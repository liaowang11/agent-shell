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

A subagent's tool call goes to its own pane, which is its group for as
long as it lives, while a root tool call after one continues the root's
open run: neither session's activity boundary moves because the other
rendered between two of its entries."
  (should (equal
           '("activity-1" "subagent-child" "activity-1")
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
                          (title . "root-b") (kind . "other") (status . "pending")))))))
  (should (equal
           '("activity-1" "subagent-child" "activity-2")
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
                          (title . "root-b") (kind . "other") (status . "pending"))))))))

(ert-deftest agent-shell-interleaved-thought-chunks-stay-in-their-session-test ()
  "Interleaved thought streams keep distinct blocks and groups.

The subagent's thought opens its pane and relabels the pane header from
what it now holds, which is the bodyless write between the two.  Its
second chunk appends to the block the first opened, rather than starting
another one behind the root's thought."
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
             '( ("subagent-child-agent_thought_chunk" "subagent-child" nil nil)
                ("subagent-child" nil nil nil)
                ("activity-1-agent_thought_chunk" "activity-1" nil nil)
                ("subagent-child-agent_thought_chunk" "subagent-child" nil t))
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

(defun agent-shell-tests--state-with-subagents ()
  "Return a state where `child' was spawned by the root and `grandchild' by it.

`child' owns a pane; `grandchild' has none of its own and renders into
its ancestor's (see `agent-shell--subagent-pane-at')."
  (let ((state (agent-shell--make-state)))
    (map-put! (map-elt state :session) :id "root")
    (map-put! state :request-count 1)
    (map-put! state :active-requests t)
    (agent-shell--save-native-subagent state "child" "Researcher" "Find prior art")
    (let ((agent-shell--subagent-group
           (cons "child" (agent-shell--native-subagent state "child"))))
      (agent-shell--save-native-subagent state "grandchild" "Reviewer" "Check it"))
    state))

(defmacro agent-shell-tests--dispatching-as (state session-id &rest body)
  "Run BODY with SESSION-ID's subagent bound as STATE's dispatching session."
  (declare (indent 2) (debug t))
  `(let ((agent-shell--subagent-group
          (cons ,session-id (agent-shell--native-subagent ,state ,session-id))))
     ,@body))

(ert-deftest agent-shell-tool-call-label-prefixes-subagent-name-before-detail-test ()
  "A tool label shared into an ancestor's pane puts the name first.

The name is what tells two subagents' rows apart once they render side
by side under the same pane header."
  (let* ((state (agent-shell-tests--state-with-subagents)))
    (agent-shell--save-tool-call state "tool-1" '((:kind . "read")
                                                  (:status . "completed")
                                                  (:title . "CONTRIBUTING.org")))
    (agent-shell-tests--dispatching-as state "grandchild"
      (let* ((tool-labels (agent-shell-make-tool-call-label state "tool-1"))
             (label-left (agent-shell--maybe-prefix-with-subagent
                          state (map-elt tool-labels :status)))
             (label-right (map-elt tool-labels :title)))
        (should (equal (substring-no-properties (concat label-left " " label-right))
                       "Reviewer ✓ Read CONTRIBUTING.org"))
        (should (eq (get-text-property 0 'font-lock-face label-left)
                    'agent-shell-subagent-name))
        (should-not (string-match-p "·" (concat label-left " " label-right)))))))

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
  "A subagent sharing an ancestor's pane precedes its heading with its name."
  (let ((state (agent-shell-tests--state-with-subagents)))
    (agent-shell-tests--dispatching-as state "grandchild"
      (let ((label (agent-shell--maybe-prefix-with-subagent state "done Read")))
        (should (equal (substring-no-properties label) "Reviewer done Read"))
        (should (eq (get-text-property 0 'font-lock-face label)
                    'agent-shell-subagent-name))
        (should-not (string-match-p "·" label))))))

(ert-deftest agent-shell-maybe-prefix-with-subagent-leaves-a-pane-owner-unnamed-test ()
  "The subagent whose pane the content lands in is not named again on every row.

Its pane header already carries the name, so repeating it down the pane
says nothing the reader cannot see one line up."
  (let ((state (agent-shell-tests--state-with-subagents)))
    (agent-shell-tests--dispatching-as state "child"
      (should (equal (agent-shell--maybe-prefix-with-subagent state "done Read")
                     "done Read"))
      (should-not (agent-shell--maybe-prefix-with-subagent state nil)))))

(ert-deftest agent-shell-maybe-prefix-with-subagent-leaves-label-unchanged-for-root-test ()
  "Root-session content (no subagent group) is untouched."
  (let ((state (agent-shell-tests--state-with-subagents))
        (agent-shell--subagent-group nil))
    (should (equal (agent-shell--maybe-prefix-with-subagent state "done Read")
                   "done Read"))))

(ert-deftest agent-shell-maybe-prefix-with-subagent-names-an-unlabelled-fragment-test ()
  "An unlabelled subagent fragment receives the name as its label."
  (let ((state (agent-shell-tests--state-with-subagents)))
    (agent-shell-tests--dispatching-as state "grandchild"
      (let ((label (agent-shell--maybe-prefix-with-subagent state nil)))
        (should (equal (substring-no-properties label) "Reviewer"))
        (should (eq (get-text-property 0 'font-lock-face label)
                    'agent-shell-subagent-name))))))

(ert-deftest agent-shell-maybe-prefix-with-subagent-keeps-nil-for-a-nameless-subagent-test ()
  "A nameless subagent invents no label."
  (let ((state (agent-shell-tests--state-with-subagents)))
    (agent-shell--save-native-subagent state "child" nil "Find prior art")
    (let ((agent-shell--subagent-group (cons "nameless" '(:parent "child"))))
      (should-not (agent-shell--maybe-prefix-with-subagent state nil))
      (should (equal (agent-shell--maybe-prefix-with-subagent state "done Read")
                     "done Read")))))

(ert-deftest agent-shell-maybe-prefix-with-subagent-leaves-root-unlabelled-fragments-alone-test ()
  "The root session's unlabelled fragments gain no label."
  (let ((state (agent-shell-tests--state-with-subagents))
        (agent-shell--subagent-group nil))
    (should-not (agent-shell--maybe-prefix-with-subagent state nil))))

(defun agent-shell-tests--subagent-shell (dispatch)
  "Render DISPATCH's notifications into a live shell and report the result.

DISPATCH is called with two functions.  SEND takes a session id and a
`session/update' payload and dispatches it as that session.  END-TURN
ends the root's turn the way `session/prompt' succeeding does, then
prints the prompt the shell holds afterwards; PROMPT submits a new one.

Returns an alist of `:blocks' (every rendered block's qualified id, in
buffer order, deduplicated), `:text' (the buffer, properties stripped)
and `:pages' (what `shell-maker--extract-history' pairs up, which is
what the viewport pages through).

Renders for real rather than stubbing `agent-shell--update-fragment':
what breaks when a subagent outlives its turn is where fragments land in
the buffer, which a captured argument list cannot show."
  (let* ((buffer (generate-new-buffer " *agent-shell-subagent-test*"))
         (process (start-process "fake-agent" buffer "cat")))
    (set-process-query-on-exit-flag process nil)
    ;; The default sentinel reports the exit into read-only comint output,
    ;; and a failing sentinel aborts the whole ERT batch.
    (set-process-sentinel process #'ignore)
    (unwind-protect
        (with-current-buffer buffer
          (comint-mode)
          (setq-local major-mode 'agent-shell-mode)
          (setq-local comint-prompt-regexp "Claude> ")
          (setq-local comint-use-prompt-regexp t)
          (setq-local shell-maker--config
                      (make-shell-maker-config
                       :name "agent" :prompt "Claude> " :prompt-regexp "Claude> "))
          (setq-local agent-shell--state
                      (agent-shell--make-state
                       :buffer buffer
                       :agent-config '((:shell-prompt . "Claude> ")
                                       (:mode-line-name . "Claude"))))
          (let ((state agent-shell--state))
            (map-put! (map-elt state :session) :id "root")
            (map-put! state :request-count 1)
            (map-put! state :active-requests '(((:method . "session/prompt"))))
            (cl-letf (((symbol-function 'shell-maker--process) (lambda () process))
                      ((symbol-function 'shell-maker-busy) (lambda (&rest _) t))
                      ((symbol-function 'agent-shell--append-transcript) #'ignore)
                      ;; No viewport in these tests; exercise the shell path.
                      ((symbol-function 'agent-shell-viewport--buffer) #'ignore))
              (cl-flet* ((prompt (text)
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))
                             (insert (propertize
                                      "Claude> "
                                      ;; Both faces, as a live prompt carries
                                      ;; them: `shell-maker--extract-history'
                                      ;; wants the literal
                                      ;; `comint-highlight-prompt'.
                                      'font-lock-face '(agent-shell-prompt
                                                        comint-highlight-prompt)
                                      'field 'output)
                                     text))
                           (set-marker (process-mark process) (point-max))
                           (shell-maker-insert-end-of-prompt-marker)
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))
                             (insert "\n"))
                           (map-put! state :request-count
                                     (1+ (map-elt state :request-count)))
                           (map-put! state :active-requests
                                     '(((:method . "session/prompt")))))
                         (send (session-id &rest update)
                           (agent-shell--on-notification
                            :state state
                            :acp-notification
                            `((method . "session/update")
                              (params . ((sessionId . ,session-id)
                                         (update . ,update))))))
                         (end-turn ()
                           (agent-shell--forget-turn-tool-calls state)
                           (agent-shell--collapse-expanded-activity-group state)
                           (map-put! state :active-requests nil)
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))
                             (set-marker (process-mark process) (point-max))
                             (shell-maker--output-filter process "\nClaude> ")
                             ;; comint marks no face in batch, and extraction
                             ;; needs the literal `comint-highlight-prompt'.
                             (add-text-properties
                              (car comint-last-prompt) (cdr comint-last-prompt)
                              '(font-lock-face (agent-shell-prompt
                                                comint-highlight-prompt))))))
                (prompt "do research")
                ;; `prompt' advanced the count for the turn it submits; the
                ;; first turn is the one already set up above.
                (map-put! state :request-count 1)
                (funcall dispatch #'send #'end-turn #'prompt))))
          (list (cons :blocks (mapcar #'car (agent-shell-tests--rendered-blocks)))
                (cons :groups (agent-shell-tests--rendered-blocks))
                (cons :text (substring-no-properties
                             (buffer-substring (point-min) (point-max))))
                (cons :pages (shell-maker--extract-history
                              "Claude> " :trimmed nil))
                (cons :state agent-shell--state)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(defun agent-shell-tests--rendered-blocks ()
  "Return (QUALIFIED-ID . GROUP-ID) for each rendered block, in buffer order.

Runs of the same block collapse into one entry, so a block rendered
twice is two entries and says so."
  (let ((position (point-min))
        (blocks '()))
    (while (< position (point-max))
      (when-let* ((state (get-text-property position 'agent-shell-ui-state))
                  (id (map-elt state :qualified-id))
                  ((not (equal id (car-safe (car blocks))))))
        (push (cons id (map-elt state :group-id)) blocks))
      (setq position (next-single-property-change
                      position 'agent-shell-ui-state nil (point-max))))
    (nreverse blocks)))

(defun agent-shell-tests--spawn-and-work (send)
  "Dispatch a root turn that spawns `child' and leaves it working, via SEND."
  (funcall send "root" '(sessionUpdate . "tool_call") '(toolCallId . "T1")
           '(title . "Task: research") '(kind . "other") '(status . "in_progress"))
  (funcall send "root" '(sessionUpdate . "subagent_spawned")
           '(subagentSessionId . "child") '(name . "Researcher")
           '(task . "Find prior art"))
  (funcall send "child" '(sessionUpdate . "tool_call") '(toolCallId . "C1")
           '(title . "Read foo.el") '(kind . "read") '(status . "pending"))
  (funcall send "child" '(sessionUpdate . "agent_message_chunk")
           '(messageId . "m-child") '(content (type . "text") (text . "I looked")))
  (funcall send "root" '(sessionUpdate . "agent_message_chunk")
           '(messageId . "m-root") '(content (type . "text") (text . "Delegating."))))

(ert-deftest agent-shell-subagent-content-nests-under-one-pane-test ()
  "Everything a subagent does renders inside its own pane.

Its tool call, message and the task it was given are children of one
group header, so folding the header puts the whole subagent away, and
the root's own content stays outside it."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send _end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send))))
         (blocks (map-elt rendered :blocks)))
    (should (member "1-subagent-child" blocks))
    (should (member "1-subagent-child-task" blocks))
    ;; The subagent's work sits under the pane.
    (should (equal (agent-shell-tests--block-group "1-C1" rendered)
                   "1-subagent-child"))
    (should (equal (agent-shell-tests--block-group "1-m-child-agent_message_chunk" rendered)
                   "1-subagent-child"))
    ;; The root's does not.
    (should-not (agent-shell-tests--block-group "1-m-root-agent_message_chunk" rendered))
    (should (equal (agent-shell-tests--block-group "1-T1" rendered) "1-activity-1"))))

(defun agent-shell-tests--block-group (qualified-id rendered)
  "Return the group QUALIFIED-ID renders under in RENDERED, or nil."
  (map-elt (map-elt rendered :groups) qualified-id))

(ert-deftest agent-shell-subagent-pane-survives-the-turn-that-spawned-it-test ()
  "Work arriving after `end_turn' joins the pane instead of starting a new one.

A subagent outlives the turn that spawned it, so its pane is addressed
by the namespace pinned at spawn rather than by whatever the root is
doing when the update lands.  Without that, the same run renders a
second time under an `out-of-turn' header that never folds."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall end-turn)
                      (funcall send "child" '(sessionUpdate . "tool_call")
                               '(toolCallId . "C2") '(title . "Grep bar")
                               '(kind . "search") '(status . "completed"))
                      (funcall send "child" '(sessionUpdate . "agent_thought_chunk")
                               '(content (type . "text") (text . "still thinking"))))))
         (blocks (map-elt rendered :blocks)))
    (should (member "1-C2" blocks))
    (should (member "1-subagent-child-agent_thought_chunk" blocks))
    ;; One pane, and no out-of-turn twin of it.
    (should (equal (seq-count (lambda (id) (equal id "1-subagent-child")) blocks) 1))
    (should-not (seq-find (lambda (id) (string-prefix-p "out-of-turn-" id)) blocks))))

(ert-deftest agent-shell-subagent-tool-call-completes-in-place-after-the-turn-test ()
  "A subagent tool call still running at `end_turn' completes in its own row.

The turn's end drops the tool calls the root is done with, but a
subagent's are still in flight: dropping those too leaves the late
completion with no title and no group to rejoin, so it lands as a
stranger below while the original row stays pending forever."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall end-turn)
                      (funcall send "child" '(sessionUpdate . "tool_call_update")
                               '(toolCallId . "C1") '(status . "completed")))))
         (state (map-elt rendered :state)))
    ;; One row for C1, still in the pane, and still labelled.
    (should (equal (seq-count (lambda (id) (equal id "1-C1"))
                              (map-elt rendered :blocks))
                   1))
    (should (equal (map-nested-elt state '(:tool-calls "C1" :title)) "Read foo.el"))
    (should (equal (map-nested-elt state '(:tool-calls "C1" :group-id)) "subagent-child"))
    ;; The root's own tool call is released, as before.
    (should-not (map-nested-elt state '(:tool-calls "T1")))))

(ert-deftest agent-shell-subagent-state-update-relabels-one-pane-test ()
  "A lifecycle report updates the pane rather than drawing a second card.

It arrives on the root's session, possibly turns later, so only the
namespace pinned at spawn points it back at the pane the subagent has
been working in."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send end-turn prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall end-turn)
                      (funcall prompt "something else")
                      (funcall send "root" '(sessionUpdate . "subagent_state_update")
                               '(subagentSessionId . "child") '(state . "completed")))))
         (blocks (map-elt rendered :blocks)))
    (should (equal (seq-count (lambda (id) (equal id "1-subagent-child")) blocks) 1))
    (should-not (member "2-subagent-child" blocks))
    (should (equal (map-elt (agent-shell--native-subagent (map-elt rendered :state) "child")
                            :state)
                   "completed"))))

(ert-deftest agent-shell-subagent-message-spanning-the-turn-stays-one-message-test ()
  "A subagent message interrupted by `end_turn' keeps streaming into one block."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall end-turn)
                      (funcall send "child" '(sessionUpdate . "agent_message_chunk")
                               '(messageId . "m-child")
                               '(content (type . "text") (text . " and found it."))))))
         (blocks (map-elt rendered :blocks)))
    (should (equal (seq-count (lambda (id) (equal id "1-m-child-agent_message_chunk"))
                              blocks)
                   1))
    (should-not (member "out-of-turn-m-child-agent_message_chunk" blocks))
    (should (string-match-p "I looked and found it." (map-elt rendered :text)))))

(ert-deftest agent-shell-subagent-work-stays-on-the-page-that-spawned-it-test ()
  "A subagent still working through a later turn keeps its content on its page.

The viewport pages by interaction, so content rendered at the buffer end
belongs to whichever prompt came last.  A subagent the user has already
moved past would otherwise have its work read as the answer to a
question it never saw."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send end-turn prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall end-turn)
                      (funcall prompt "unrelated question")
                      (funcall send "child" '(sessionUpdate . "tool_call")
                               '(toolCallId . "C2") '(title . "Grep bar")
                               '(kind . "search") '(status . "completed"))
                      (funcall send "root" '(sessionUpdate . "agent_message_chunk")
                               '(messageId . "m-root3")
                               '(content (type . "text") (text . "Answer 2.")))
                      (funcall end-turn))))
         (pages (map-elt rendered :pages)))
    (should (equal (length pages) 2))
    (should (equal (car (nth 0 pages)) "do research"))
    (should (equal (car (nth 1 pages)) "unrelated question"))
    ;; The subagent's late tool call reads on its own page, not the new one.
    (should (string-match-p "Grep bar" (substring-no-properties (cdr (nth 0 pages)))))
    (should-not (string-match-p "Grep bar" (substring-no-properties (cdr (nth 1 pages)))))
    (should (string-match-p "Answer 2." (substring-no-properties (cdr (nth 1 pages)))))))

(ert-deftest agent-shell-latest-page-namespace-p-excludes-an-earlier-turn-test ()
  "The viewport mirror takes the live namespaces and refuses an earlier one.

It only ever shows the latest interaction, so mirroring a fragment
pinned to a turn the user has paged past would append that turn's
content to a page it does not belong to."
  (let ((state '((:request-count . 2))))
    (should (agent-shell--latest-page-namespace-p state 2))
    (should (agent-shell--latest-page-namespace-p state "out-of-turn"))
    (should-not (agent-shell--latest-page-namespace-p state 1))))

(provide 'agent-shell-subagents-tests)
;;; agent-shell-subagents-tests.el ends here
