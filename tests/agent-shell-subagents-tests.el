;;; agent-shell-subagents-tests.el --- Tests for native subagents and async tasks -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell-air-capabilities-meta-test ()
  "Test that AIR `_meta' carries the requested capability names."
  (let ((meta (agent-shell--air-capabilities-meta "asyncTasks" "nativeSubagentSessions")))
    (should (equal (map-nested-elt meta '(jetbrains air version)) 1))
    (should (equal (map-nested-elt meta '(jetbrains air capabilities))
                   ["asyncTasks" "nativeSubagentSessions"]))))

(ert-deftest agent-shell-air-meta-rides-inside-client-capabilities-test ()
  "Test that the initialize request nests AIR `_meta' inside `clientCapabilities'.

Agents read extension meta there (claude-agent-acp, codex-acp); a
request-level `_meta' is never consulted, and the gated features
silently fall back to plain tool calls."
  (let* ((request (acp-make-initialize-request
                   :protocol-version 1
                   :client-capabilities `((subagents . ())
                                          (_meta . ,(agent-shell--air-capabilities-meta
                                                     "asyncTasks"))))))
    (should (equal (map-nested-elt request '(:params clientCapabilities _meta
                                                    jetbrains air capabilities))
                   ["asyncTasks"]))
    (should-not (map-elt (map-elt request :params) '_meta))))

(ert-deftest agent-shell-advertises-every-air-capability-it-implements-test ()
  "The AIR capabilities we advertise must name every feature we render.

claude-agent-acp gates each feature on the client naming it in the
initialize request's `_meta.jetbrains.air.capabilities': native subagent
sessions on \"nativeSubagentSessions\" (acp-subagents.ts) and background
tasks on \"asyncTasks\" (async-tasks.ts).  A feature we render but never
name is dead code -- the agent simply never sends those notifications."
  (let ((advertised (map-nested-elt (agent-shell--air-client-capabilities-meta)
                                    '(jetbrains air capabilities))))
    (should (seq-contains-p advertised "nativeSubagentSessions"))
    (should (seq-contains-p advertised "asyncTasks"))))

(ert-deftest agent-shell-air-capabilities-are-per-agent-test ()
  "Only agents that implement the AIR extension are told about it.

The `jetbrains.air' namespace is one vendor's, so the shared handshake
advertising it to codex, gemini and the rest would grow a union of every
vendor's keys sent to everyone.  Agent configs already carry per-agent
wire extras."
  (should (agent-shell--air-client-capabilities-meta))
  ;; Claude implements it.
  (should (map-elt (agent-shell-anthropic-make-claude-code-config) :initialize-meta))
  ;; A config that never opted in advertises nothing.
  (should-not (map-elt (agent-shell-make-agent-config :identifier 'nobody)
                       :initialize-meta)))

(ert-deftest agent-shell-air-extension-supported-p-reads-the-agents-advert-test ()
  "Whether the agent implements the AIR extension is read, not assumed.

`agent-shell-fork-at-point' rides `_meta.jetbrains.air.fork', so an agent
that never advertised the extension silently forks the latest turn
instead of the message at point."
  (should (agent-shell--air-extension-supported-p
           '((_meta . ((jetbrains . ((air . ((version . 1)
                                             (capabilities . ["asyncTasks"])))))))))) 
  (should-not (agent-shell--air-extension-supported-p
               '((_meta . ((steering . ((supported . t))))))))
  (should-not (agent-shell--air-extension-supported-p nil)))

(ert-deftest agent-shell-session-bound-notification-p-recognizes-subagent-and-async-task-updates-test ()
  "Test that native subagent and async task updates count as session-bound.
Otherwise they would misfire the \"unexpected out-of-turn update\" path,
since they legitimately arrive after `end_turn' once a session/prompt
request has already settled."
  (dolist (kind '("subagent_spawned" "subagent_state_update"
                  "async_task_spawned" "async_task_progress" "async_task_state_update"))
    (should (agent-shell--session-bound-notification-p
             `((method . "session/update")
               (params . ((update . ((sessionUpdate . ,kind))))))))))

(ert-deftest agent-shell-native-subagent-registry-round-trip-test ()
  "Test that a spawned subagent's name/task can be looked back up."
  (let ((state (list (cons :native-subagents nil))))
    (agent-shell--save-native-subagent state "subagent-1" "Researcher" "Find prior art")
    (let ((registered (agent-shell--native-subagent state "subagent-1")))
      (should (equal (map-elt registered :name) "Researcher"))
      (should (equal (map-elt registered :task) "Find prior art")))
    (should-not (agent-shell--native-subagent state "subagent-unknown"))))

(ert-deftest agent-shell-async-task-registry-round-trip-test ()
  "Test that a spawned async task's name/type/description can be looked back up."
  (let ((state (list (cons :async-tasks nil))))
    (agent-shell--save-async-task state "task-1" "Build" "workflow" "Running the test suite" t t)
    (let ((registered (agent-shell--async-task state "task-1")))
      (should (equal (map-elt registered :name) "Build"))
      (should (equal (map-elt registered :task-type) "workflow"))
      (should (equal (map-elt registered :description) "Running the test suite"))
      (should (eq (map-elt registered :can-stop) t))
      (should (equal (map-elt registered :state) "running")))
    (should-not (agent-shell--async-task state "task-unknown"))))

(cl-defun agent-shell-tests--spawn-async-task (&key show-in-transcript (can-stop t))
  "Dispatch an `async_task_spawned' notification and report what it did.

SHOW-IN-TRANSCRIPT and CAN-STOP are the flags the agent sends.

Returns an alist of the tasks the stop command would offer and whether a
transcript fragment was rendered."
  (let ((state (list (cons :async-tasks nil)
                     (cons :last-entry-type "agent_message_chunk")))
        rendered)
    (cl-letf (((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest _) (setq rendered t))))
      (agent-shell--dispatch-notification
       :state state
       :acp-notification
       `((method . "session/update")
         (params (update (sessionUpdate . "async_task_spawned")
                         (asyncTaskId . "task-1")
                         (name . "Build")
                         (taskType . "workflow")
                         (description . "Running the test suite")
                         (canStop . ,can-stop)
                         (showInTranscript . ,show-in-transcript))))))
    (list (cons :stoppable (agent-shell--stoppable-async-tasks state))
          (cons :rendered rendered)
          (cons :last-entry-type (map-elt state :last-entry-type)))))

(ert-deftest agent-shell-async-task-progress-does-not-resume-a-paused-task-test ()
  "Progress is not a state transition, so it must not rewrite the state.

`async_task_progress' carries no `state' field: claude-agent-acp's
`publishMetadata' sends one for any non-terminal task, paused included
\(async-tasks.ts).  Treating it as \"running\" makes a paused task report
running in both the status icon and the registry the stop command reads."
  (let ((state (list (cons :async-tasks nil)
                     (cons :last-entry-type nil)))
        rendered-status)
    (cl-letf (((symbol-function 'agent-shell--update-fragment)
               (cl-function (lambda (&key label-left &allow-other-keys)
                              (setq rendered-status label-left))))
              ((symbol-function 'agent-shell--make-status-kind-label)
               (cl-function (lambda (&key status &allow-other-keys) status))))
      (agent-shell--save-async-task state "task-1" "Build" "workflow" "desc" t t)
      (agent-shell--set-async-task-state state "task-1" "paused")
      (agent-shell--dispatch-notification
       :state state
       :acp-notification
       '((method . "session/update")
         (params (update (sessionUpdate . "async_task_progress")
                         (asyncTaskId . "task-1")
                         (summary . "still going")))))
      (should (equal (map-elt (agent-shell--async-task state "task-1") :state) "paused"))
      (should (string-match-p "paused" rendered-status)))))

(ert-deftest agent-shell-async-task-hidden-from-transcript-is-still-stoppable-test ()
  "A task the agent hides from the transcript can still be stopped.

`showInTranscript' says whether the task gets its own transcript card --
it is false when a tool call already shows it -- not whether the task
exists.  `canStop' is a separate flag, so a hidden task the agent
advertises as stoppable has to reach `agent-shell--stoppable-async-tasks'
or `agent-shell-stop-async-task' reports nothing to stop."
  (let ((spawned (agent-shell-tests--spawn-async-task :show-in-transcript nil)))
    (should (equal (map-elt (cdar (map-elt spawned :stoppable)) :name) "Build"))
    ;; Still hidden: registering it must not put a duplicate card next to
    ;; the tool call already showing it.
    (should-not (map-elt spawned :rendered))
    ;; Nothing was drawn, so the entry type must not advance -- doing so
    ;; would split a streaming agent message in two.
    (should (equal (map-elt spawned :last-entry-type) "agent_message_chunk")))
  ;; A visible task registers and renders, as before.
  (let ((spawned (agent-shell-tests--spawn-async-task :show-in-transcript t)))
    (should (equal (map-elt (cdar (map-elt spawned :stoppable)) :name) "Build"))
    (should (map-elt spawned :rendered))
    (should (equal (map-elt spawned :last-entry-type) "async_task_spawned")))
  ;; A task the agent will not stop is registered but never offered.
  (let ((spawned (agent-shell-tests--spawn-async-task :show-in-transcript nil :can-stop nil)))
    (should-not (map-elt spawned :stoppable))))

(defun agent-shell-tests--interleaved-message-chunks (chunks)
  "Dispatch CHUNKS as agent messages and return their (BLOCK-ID . CREATE-NEW).

CHUNKS is a list of (SESSION-ID MESSAGE-ID . TEXT).  A subagent's content
arrives on its own `sessionId' but renders into the root shell, so both
sessions drive the same STATE."
  (let ((state (list (cons :session (list (cons :id "root")))
                     (cons :native-subagents nil)
                     (cons :last-entry-type nil)
                     (cons :last-agent-message-ids nil)
                     (cons :last-agent-message-block-ids nil)
                     (cons :chunked-group-count 0)
                     (cons :activity-group-sessions
                           '((:root (:last-entry-type . nil))))
                     (cons :active-requests t)
                     (cons :pending-restore nil)
                     (cons :last-activity-time nil)
                     (cons :buffer nil)))
        (calls '()))
    (agent-shell--save-native-subagent state "child" "Researcher" "Find prior art")
    (cl-letf (((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args)
                 (push (cons (plist-get args :block-id) (plist-get args :create-new)) calls)))
              ((symbol-function 'agent-shell--append-transcript) #'ignore)
              ((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell--collapse-expanded-activity-group) #'ignore)
              ((symbol-function 'agent-shell--active-requests-p) (lambda (&rest _) t))
              ((symbol-function 'agent-shell--content-block-to-markdown)
               (lambda (block) (map-elt block 'text)))
              ((symbol-function 'agent-shell--indent-markdown-headers) #'identity))
      (dolist (chunk chunks)
        (agent-shell--on-notification
         :state state
         :acp-notification
         `((method . "session/update")
           (params (sessionId . ,(nth 0 chunk))
                   (update (sessionUpdate . "agent_message_chunk")
                           (messageId . ,(nth 1 chunk))
                           (content (type . "text") (text . ,(cddr chunk)))))))))
    (nreverse calls)))

(ert-deftest agent-shell-interleaved-subagent-chunks-stay-one-message-test ()
  "A message interrupted by another session's message is not split in two.

A subagent's chunks render into the root shell, so both sessions share
one entry per session.  Given child, root, child, the second child
chunk compares its id against the root's and looks like a new message.
`agent-shell-ui-update-fragment' only reuses an existing block when
CREATE-NEW is nil, so the message gets a second block instead of
continuing its first."
  (let ((calls (agent-shell-tests--interleaved-message-chunks
                '(("child" "m-child" . "I looked")
                  ("root"  "m-root"  . "Meanwhile")
                  ("child" "m-child" . " and found it")))))
    ;; Each message starts once.
    (should (equal (mapcar #'cdr calls) '(t t nil)))
    ;; The resumed chunk goes back to the block it started.
    (should (equal (car (nth 0 calls)) (car (nth 2 calls))))
    (should-not (equal (car (nth 0 calls)) (car (nth 1 calls))))))

(ert-deftest agent-shell-interleaved-subagent-chunks-without-message-id-stay-one-message-test ()
  "Interleaved messages without ACP IDs still keep session-local blocks."
  (let ((calls (agent-shell-tests--interleaved-message-chunks
                '( ("child" nil . "I looked")
                   ("root" nil . "Meanwhile")
                   ("child" nil . " and found it")))))
    (should (equal (mapcar #'cdr calls) '(t t nil)))
    (should (equal (car (nth 0 calls)) (car (nth 2 calls))))
    (should-not (equal (car (nth 0 calls)) (car (nth 1 calls))))))

(defun agent-shell-tests--interleaved-activity-group-ids (updates)
  "Dispatch activity UPDATES and return the requested tool group IDs.

UPDATES is a list of (SESSION-ID . UPDATE), where UPDATE is a
`session/update' payload.  The helper uses the real notification dispatcher
but stubs buffer rendering, so it exercises session routing and state while
remaining independent of a live shell buffer."
  (let ((state (agent-shell--make-state))
        (tool-call-ids (seq-keep (lambda (entry)
                                   (when (equal (map-elt (cdr entry) 'sessionUpdate)
                                                "tool_call")
                                     (map-elt (cdr entry) 'toolCallId)))
                                 updates)))
    (map-put! (map-elt state :session) :id "root")
    (map-put! state :active-requests t)
    (agent-shell--save-native-subagent state "child" "Researcher" "Find prior art")
    (cl-letf (((symbol-function 'agent-shell--update-fragment) #'ignore)
              ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
              ((symbol-function 'agent-shell--append-transcript) #'ignore)
              ((symbol-function 'agent-shell--collapse-expanded-activity-group) #'ignore)
              ((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell--make-transcript-tool-call-entry)
               (lambda (&rest _) ""))
              ((symbol-function 'agent-shell--delete-fragment) #'ignore)
              ((symbol-function 'agent-shell--cancel-idle-timer) #'ignore)
              ((symbol-function 'agent-shell-make-tool-call-label)
               (lambda (&rest _) '((:status . "s") (:title . "t"))))
              ((symbol-function 'agent-shell--content-block-to-markdown)
               (lambda (block) (map-elt block 'text)))
              ((symbol-function 'agent-shell--indent-markdown-headers) #'identity))
      (dolist (entry updates)
        (agent-shell--on-notification
         :state state
         :acp-notification
         `((method . "session/update")
           (params . ((sessionId . ,(car entry))
                      (update . ,(cdr entry)))))))
      (mapcar (lambda (tool-call-id)
                (map-nested-elt state `(:tool-calls ,tool-call-id :group-id)))
              tool-call-ids))))

(ert-deftest agent-shell-interleaved-activity-groups-stay-session-local-test ()
  "Interleaved root and subagent activity runs do not share a group.

Each session keeps its own activity boundary.  A subagent tool call starts a
different group from a root tool call, while a root tool after a subagent
message continues the root's open run."
  (should (equal
           '("activity-1" "activity-child-1" "activity-1")
           (agent-shell-tests--interleaved-activity-group-ids
            `(("root" . ((sessionUpdate . "tool_call")
                          (toolCallId . "root-a")
                          (title . "root-a") (kind . "other") (status . "pending")))
              ("child" . ((sessionUpdate . "tool_call")
                           (toolCallId . "child-a")
                           (title . "child-a") (kind . "other") (status . "pending")))
              ("root" . ((sessionUpdate . "tool_call")
                          (toolCallId . "root-b")
                          (title . "root-b") (kind . "other") (status . "pending")))))))
  (should (equal
           '("activity-1" "activity-1")
           (agent-shell-tests--interleaved-activity-group-ids
            `(("root" . ((sessionUpdate . "tool_call")
                          (toolCallId . "root-a")
                          (title . "root-a") (kind . "other") (status . "pending")))
              ("child" . ((sessionUpdate . "agent_message_chunk")
                           (messageId . "child-message")
                           (content (type . "text") (text . "child reply"))))
              ("root" . ((sessionUpdate . "tool_call")
                          (toolCallId . "root-b")
                          (title . "root-b") (kind . "other") (status . "pending"))))))))
  (should (equal
           '("activity-1" "activity-child-1" "activity-2")
           (agent-shell-tests--interleaved-activity-group-ids
            `(("root" . ((sessionUpdate . "tool_call")
                          (toolCallId . "root-a")
                          (title . "root-a") (kind . "other") (status . "pending")))
              ("root" . ((sessionUpdate . "agent_message_chunk")
                          (messageId . "root-message")
                          (content (type . "text") (text . "root reply"))))
              ("child" . ((sessionUpdate . "tool_call")
                           (toolCallId . "child-a")
                           (title . "child-a") (kind . "other") (status . "pending")))
              ("root" . ((sessionUpdate . "tool_call")
                          (toolCallId . "root-b")
                          (title . "root-b") (kind . "other") (status . "pending")))))))

(ert-deftest agent-shell-interleaved-thought-chunks-stay-in-their-session-test ()
  "Interleaved thought streams keep distinct blocks and activity groups."
  (let ((state (agent-shell--make-state))
        (calls nil))
    (map-put! (map-elt state :session) :id "root")
    (map-put! state :active-requests t)
    (agent-shell--save-native-subagent state "child" "Researcher" "Find prior art")
    (cl-letf (((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args)
                 (push (list (plist-get args :block-id)
                             (plist-get args :group-id)
                             (plist-get args :create-new)
                             (plist-get args :append))
                       calls)))
              ((symbol-function 'agent-shell--refresh-activity-group-header) #'ignore)
              ((symbol-function 'agent-shell--append-transcript) #'ignore)
              ((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell--collapse-expanded-activity-group) #'ignore)
              ((symbol-function 'agent-shell--content-block-to-markdown)
               (lambda (block) (map-elt block 'text)))
              ((symbol-function 'agent-shell--indent-markdown-headers) #'identity))
      (dolist (entry
               '(("child" . ((sessionUpdate . "agent_thought_chunk")
                              (content (type . "text") (text . "child one"))))
                 ("root" . ((sessionUpdate . "agent_thought_chunk")
                             (content (type . "text") (text . "root one"))))
                 ("child" . ((sessionUpdate . "agent_thought_chunk")
                              (content (type . "text") (text . "child two"))))))
        (agent-shell--on-notification
         :state state
         :acp-notification
         `((method . "session/update")
           (params . ((sessionId . ,(car entry))
                      (update . ,(cdr entry)))))))
      (setq calls (nreverse calls)))
    (should (equal
             '( ("activity-child-1-agent_thought_chunk" "activity-child-1" nil nil)
                ("activity-1-agent_thought_chunk" "activity-1" nil nil)
                ("activity-child-1-agent_thought_chunk" "activity-child-1" nil t))
             calls))))

(ert-deftest agent-shell-format-async-task-body-prefers-summary-over-description-test ()
  "Test that SUMMARY, when present, takes priority over DESCRIPTION."
  (should (equal (agent-shell--format-async-task-body "description" "summary" nil nil)
                 "summary"))
  (should (equal (agent-shell--format-async-task-body "description" nil nil nil)
                 "description")))

(ert-deftest agent-shell-format-async-task-body-includes-tool-and-usage-lines-test ()
  "Test that the last tool name and usage tally render as extra lines."
  (let ((body (agent-shell--format-async-task-body
              "description" nil "Bash" '((totalTokens . 1500) (toolUses . 3)))))
    (should (string-match-p "Last tool: Bash" body))
    (should (string-match-p "2k tokens · 3 tool uses" body))))

(ert-deftest agent-shell-native-subagent-registry-migrates-legacy-state-test ()
  "Test that saving a subagent works even without a pre-seeded :native-subagents.
A live shell created before this key existed would otherwise hit
map-put!'s \"Cannot modify map in-place\" error on the first save."
  (let ((state (list (cons :usage nil))))
    (agent-shell--save-native-subagent state "subagent-1" "Researcher" "Find prior art")
    (should (equal (map-elt (agent-shell--native-subagent state "subagent-1") :name)
                   "Researcher"))))

(ert-deftest agent-shell-async-task-registry-migrates-legacy-state-test ()
  "Test that saving an async task works even without a pre-seeded :async-tasks.
A live shell created before this key existed would otherwise hit
map-put!'s \"Cannot modify map in-place\" error on the first save."
  (let ((state (list (cons :usage nil))))
    (agent-shell--save-async-task state "task-1" "Build" "workflow" "desc" t t)
    (should (equal (map-elt (agent-shell--async-task state "task-1") :name) "Build"))))

(ert-deftest agent-shell-set-async-task-state-updates-registered-entry-test ()
  "Test that the registry's :state field reflects the latest update."
  (let ((state (list (cons :async-tasks nil))))
    (agent-shell--save-async-task state "task-1" "Build" "workflow" "desc" t t)
    (agent-shell--set-async-task-state state "task-1" "completed")
    (should (equal (map-elt (agent-shell--async-task state "task-1") :state) "completed"))))

(ert-deftest agent-shell-set-async-task-state-ignores-unregistered-task-test ()
  "Test that updating an unknown task id is a no-op, not an error."
  (let ((state (list (cons :async-tasks nil))))
    (should-not (agent-shell--set-async-task-state state "task-unknown" "completed"))))

(ert-deftest agent-shell-stoppable-async-tasks-filters-by-can-stop-and-state-test ()
  "Test that only running, stoppable tasks are offered for stopping."
  (let ((state (list (cons :async-tasks nil))))
    (agent-shell--save-async-task state "stoppable" "Build" "workflow" "desc" t t)
    (agent-shell--save-async-task state "not-stoppable" "Deploy" "workflow" "desc" nil t)
    (agent-shell--save-async-task state "already-done" "Test" "workflow" "desc" t t)
    (agent-shell--set-async-task-state state "already-done" "completed")
    (let ((ids (mapcar #'car (agent-shell--stoppable-async-tasks state))))
      (should (equal ids '("stoppable"))))))

(ert-deftest agent-shell-async-task-stop-request-shape-test ()
  "Test that the stop request carries the session and task id in `_session/async_task/stop'."
  (let ((request (agent-shell--async-task-stop-request
                  :session-id "session-1" :async-task-id "task-1")))
    (should (equal (map-elt request :method) "_session/async_task/stop"))
    (should (equal (map-nested-elt request '(:params sessionId)) "session-1"))
    (should (equal (map-nested-elt request '(:params asyncTaskId)) "task-1"))))

(ert-deftest agent-shell-stop-async-task-errors-without-candidates-test ()
  "Test that stopping fails clearly when nothing is stoppable."
  (let ((state (list (cons :async-tasks nil))))
    (with-temp-buffer
      (setq-local major-mode 'agent-shell-mode)
      (cl-letf (((symbol-function 'agent-shell--state) (lambda () state)))
        (should-error (agent-shell-stop-async-task) :type 'user-error)))))

(ert-deftest agent-shell-notification-subagent-group-nil-for-root-session-test ()
  "Test that a root-session notification carries no subagent group."
  (let ((state (list (cons :session (list (cons :id "root-session")))
                     (cons :native-subagents nil))))
    (should-not (agent-shell--notification-subagent-group
                state `((method . "session/update")
                        (params . ((sessionId . "root-session")
                                   (update . ((sessionUpdate . "agent_message_chunk"))))))))))

(ert-deftest agent-shell-notification-subagent-group-resolves-registered-subagent-test ()
  "Test that a registered subagent's own updates resolve to its group."
  (let ((state (list (cons :session (list (cons :id "root-session")))
                     (cons :native-subagents nil))))
    (agent-shell--save-native-subagent state "subagent-1" "Researcher" "Find prior art")
    (let ((group (agent-shell--notification-subagent-group
                  state `((method . "session/update")
                          (params . ((sessionId . "subagent-1")
                                     (update . ((sessionUpdate . "agent_message_chunk")))))))))
      (should (equal (car group) "subagent-1"))
      (should (equal (map-elt (cdr group) :name) "Researcher")))))

(ert-deftest agent-shell-notification-subagent-group-nil-for-unregistered-session-test ()
  "Test that an unrecognized foreign session id doesn't crash or false-match."
  (let ((state (list (cons :session (list (cons :id "root-session")))
                     (cons :native-subagents nil))))
    (should-not (agent-shell--notification-subagent-group
                state `((method . "session/update")
                        (params . ((sessionId . "some-other-session")
                                   (update . ((sessionUpdate . "agent_message_chunk"))))))))))

(ert-deftest agent-shell-tool-call-label-prefixes-subagent-name-before-detail-test ()
  "A subagent tool label puts the name before its status and detail."
  (let* ((state '((:tool-calls . (("tool-1" . ((:kind . "read")
                                                (:status . "completed")
                                                (:title . "CONTRIBUTING.org")))))))
         (agent-shell--subagent-group
          (cons "subagent-1" (list :name "Researcher")))
         (tool-labels (agent-shell-make-tool-call-label state "tool-1"))
         (label-left
          (agent-shell--maybe-prefix-with-subagent
           (map-elt tool-labels :status)))
         (label-right (map-elt tool-labels :title)))
    (should (equal (substring-no-properties (concat label-left " " label-right))
                   "Researcher ✓ Read CONTRIBUTING.org"))
    (should (eq (get-text-property 0 'font-lock-face label-left)
                'agent-shell-subagent-name))
    (should-not (string-match-p "·" (concat label-left " " label-right)))))

(ert-deftest agent-shell-subagent-name-face-inherits-font-lock-type-face-test ()
  "The subagent name face is distinct and inherits the type face."
  (should (facep 'agent-shell-subagent-name))
  (should (eq (face-attribute 'agent-shell-subagent-name :inherit nil t)
              'font-lock-type-face)))

(ert-deftest agent-shell-subagent-name-label-includes-name-and-name-face-test ()
  "A subagent name uses its own face with a TTY fallback."
  (let ((label (agent-shell--subagent-name-label '(:name "Researcher"))))
    (should (equal (substring-no-properties label) "Researcher"))
    (should (eq (get-text-property 0 'font-lock-face label)
                'agent-shell-subagent-name))
    (should (eq (get-text-property 0 'agent-shell-subagent-label label)
                t))
    (should (equal (get-text-property 0 'display label)
                   `(when (not window-system) .
                      ,(propertize "[Researcher]" 'face 'agent-shell-subagent-name))))))

(ert-deftest agent-shell-subagent-name-label-nil-without-name-test ()
  "A nameless subagent yields no label rather than a blank one."
  (should-not (agent-shell--subagent-name-label nil)))

(ert-deftest agent-shell-maybe-prefix-with-subagent-prepends-name-test ()
  "A subagent name precedes the existing heading without a separator."
  (let ((agent-shell--subagent-group (cons "subagent-1" '(:name "Researcher"))))
    (let ((label (agent-shell--maybe-prefix-with-subagent "done Read")))
      (should (equal (substring-no-properties label) "Researcher done Read"))
      (should (eq (get-text-property 0 'font-lock-face label)
                  'agent-shell-subagent-name))
      (should-not (string-match-p "·" label)))))

(ert-deftest agent-shell-maybe-prefix-with-subagent-leaves-label-unchanged-for-root-test ()
  "Root-session content (no subagent group) is untouched."
  (let ((agent-shell--subagent-group nil))
    (should (equal (agent-shell--maybe-prefix-with-subagent "done Read")
                   "done Read"))))

(ert-deftest agent-shell-maybe-prefix-with-subagent-names-an-unlabelled-fragment-test ()
  "An unlabelled subagent fragment receives the name as its label."
  (let ((agent-shell--subagent-group (cons "subagent-1" '(:name "Researcher"))))
    (let ((label (agent-shell--maybe-prefix-with-subagent nil)))
      (should (equal (substring-no-properties label) "Researcher"))
      (should (eq (get-text-property 0 'font-lock-face label)
                  'agent-shell-subagent-name)))))

(ert-deftest agent-shell-maybe-prefix-with-subagent-keeps-nil-for-a-nameless-subagent-test ()
  "A nameless subagent invents no label."
  (let ((agent-shell--subagent-group (cons "subagent-1" nil)))
    (should-not (agent-shell--maybe-prefix-with-subagent nil))
    (should (equal (agent-shell--maybe-prefix-with-subagent "done Read")
                   "done Read"))))

(ert-deftest agent-shell-maybe-prefix-with-subagent-leaves-root-unlabelled-fragments-alone-test ()
  "The root session's unlabelled fragments gain no label."
  (let ((agent-shell--subagent-group nil))
    (should-not (agent-shell--maybe-prefix-with-subagent nil))))

(provide 'agent-shell-subagents-tests)
;;; agent-shell-subagents-tests.el ends here
