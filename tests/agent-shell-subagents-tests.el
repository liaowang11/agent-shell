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

(ert-deftest agent-shell-async-task-progress-persists-to-registry-test ()
  "Progress is written to the registry so the subagents list can show it."
  (let ((state (list (cons :async-tasks nil)
                     (cons :last-entry-type nil)))
        changed)
    (cl-letf (((symbol-function 'agent-shell--update-fragment) #'ignore)
              ((symbol-function 'agent-shell-subagents--changed)
               (lambda (changed-state) (setq changed changed-state))))
      (agent-shell--save-async-task state "task-1" "Build" "workflow" "desc" t t)
      (agent-shell--set-async-task-state state "task-1" "paused")
      (agent-shell--dispatch-notification
       :state state
       :acp-notification
       '((method . "session/update")
         (params (update (sessionUpdate . "async_task_progress")
                         (asyncTaskId . "task-1")
                         (summary . "Compiling")
                         (lastToolName . "Bash")
                         (usage (totalTokens . 2000) (toolUses . 3))))))
      (let ((task (agent-shell--async-task state "task-1")))
        (should (equal (map-elt task :summary) "Compiling"))
        (should (equal (map-elt task :last-tool-name) "Bash"))
        (should (equal (map-elt (map-elt task :usage) 'totalTokens) 2000))
        (should (equal (map-elt task :description) "desc"))
        (should (equal (map-elt task :state) "paused")))
      (should (eq changed state)))))

(ert-deftest agent-shell-async-task-state-update-persists-summary-test ()
  "A terminal state update's summary is written to the registry."
  (let ((state (list (cons :async-tasks nil)
                     (cons :last-entry-type nil))))
    (cl-letf (((symbol-function 'agent-shell--update-fragment) #'ignore)
              ((symbol-function 'agent-shell-subagents--changed) #'ignore))
      (agent-shell--save-async-task state "task-1" "Build" "workflow" "desc" t t)
      (agent-shell--dispatch-notification
       :state state
       :acp-notification
       '((method . "session/update")
         (params (update (sessionUpdate . "async_task_state_update")
                         (asyncTaskId . "task-1")
                         (state . "completed")
                         (summary . "All tests passed")))))
      (let ((task (agent-shell--async-task state "task-1")))
        (should (equal (map-elt task :summary) "All tests passed"))
        (should (equal (map-elt task :state) "completed"))))))

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

A subagent's tool call is filed under its row, which is its group for as
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

The subagent's thought renders flat in its own buffer, then relabels its
row from what it now holds, which is the bodyless write between the two.
Its second chunk appends to the block the first opened, rather than
starting another one behind the root's thought."
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
             '( ("subagent-child-agent_thought_chunk-0" nil nil nil)
                ("subagent-child" nil nil nil)
                ("activity-1-agent_thought_chunk" "activity-1" nil nil)
                ("subagent-child-agent_thought_chunk-0" nil nil t))
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
`map-put!'s \"Cannot modify map in-place\" error on the first save."
  (let ((state (list (cons :usage nil))))
    (agent-shell--save-native-subagent state "subagent-1" "Researcher" "Find prior art")
    (should (equal (map-elt (agent-shell--native-subagent state "subagent-1") :name)
                   "Researcher"))))

(ert-deftest agent-shell-async-task-registry-migrates-legacy-state-test ()
  "Test that saving an async task works even without a pre-seeded :async-tasks.
A live shell created before this key existed would otherwise hit
`map-put!'s \"Cannot modify map in-place\" error on the first save."
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

(defun agent-shell-tests--subagent-shell (dispatch &optional transcript-file)
  "Render DISPATCH's notifications into a live shell and report the result.

TRANSCRIPT-FILE, when given, is where the shell keeps its transcript;
without one, transcript writes are dropped.

DISPATCH is called with two functions.  SEND takes a session id and a
`session/update' payload and dispatches it as that session.  END-TURN
ends the root's turn the way `session/prompt' succeeding does, then
prints the prompt the shell holds afterwards; PROMPT submits a new one.

Returns an alist of `:blocks' (every rendered block's qualified id, in
buffer order, deduplicated), `:groups' (each block's group), the
buffer's `:text' with properties stripped, `:pages' (what
`shell-maker--extract-history' pairs up, which is what the viewport
pages through), `:state', and `:subagents', mapping each subagent's
session id to the same report for its own buffer.  Subagent buffers are
killed along with the shell.

Renders for real rather than stubbing `agent-shell--update-fragment':
what breaks when a subagent outlives its turn is where fragments land in
the buffer, which a captured argument list cannot show."
  (let* (;; This shell prints its prompt when a turn ends rather than
         ;; keeping one live throughout, so there is nothing to render
         ;; above while the first turn runs and
         ;; `agent-shell--live-prompt-start' would only signal.  Rendering
         ;; above a prompt that is there is still covered: `end-turn'
         ;; leaves one, and everything after it renders above it.
         (agent-shell-persistent-prompt-enabled nil)
         (buffer (generate-new-buffer " *agent-shell-subagent-test*"))
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
          (setq-local agent-shell--transcript-file transcript-file)
          (let ((state agent-shell--state))
            (map-put! (map-elt state :session) :id "root")
            (map-put! state :request-count 1)
            (map-put! state :active-requests '(((:method . "session/prompt"))))
            (cl-letf (((symbol-function 'shell-maker--process) (lambda () process))
                      ((symbol-function 'shell-maker-busy) (lambda (&rest _) t))
                      ((symbol-function 'agent-shell--append-transcript)
                       (if transcript-file
                           (symbol-function 'agent-shell--append-transcript)
                         #'ignore))
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
                (cons :state agent-shell--state)
                (cons :subagents
                      (mapcar (lambda (entry)
                                (cons (car entry)
                                      (agent-shell-tests--subagent-buffer-report
                                       (map-elt (cdr entry) :buffer))))
                              (map-elt agent-shell--state :native-subagents)))))
      (when (process-live-p process)
        (delete-process process))
      (with-current-buffer buffer
        (agent-shell-subagents--kill-buffers agent-shell--state))
      (kill-buffer buffer))))

(defun agent-shell-tests--subagent-buffer-report (buffer)
  "Return BUFFER's `:blocks', `:groups', `:text' and `:mode', or nil.
Nil when BUFFER is dead.  The same shape `agent-shell-tests--subagent-shell'
reports the shell in."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (list (cons :blocks (mapcar #'car (agent-shell-tests--rendered-blocks)))
            (cons :groups (agent-shell-tests--rendered-blocks))
            (cons :text (substring-no-properties
                         (buffer-substring (point-min) (point-max))))
            (cons :mode major-mode)))))

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

(defun agent-shell-tests--subagent-report (rendered session-id)
  "Return SESSION-ID's buffer report from RENDERED."
  (map-elt (map-elt rendered :subagents) session-id))

(ert-deftest agent-shell-subagent-content-renders-in-its-own-buffer-test ()
  "A subagent's work renders in its buffer, and the shell keeps one row.

The row sits where the spawn happened.  The subagent's tool calls and
messages render flat in its own buffer, headed by its task, so none of
it lands in the turn the root is streaming."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send _end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send))))
         (blocks (map-elt rendered :blocks))
         (child (agent-shell-tests--subagent-report rendered "child")))
    ;; The shell: the root's own work, and one row for the subagent.
    (should (member "1-subagent-child" blocks))
    (should (member "1-T1" blocks))
    (should (member "1-m-root-agent_message_chunk" blocks))
    (should-not (member "1-C1" blocks))
    (should-not (member "1-m-child-agent_message_chunk" blocks))
    (should-not (member "1-subagent-child-task" blocks))
    (should-not (string-match-p "I looked" (map-elt rendered :text)))
    ;; The row is folded, its body holding the task and a way in.
    (should (string-match-p "Researcher" (map-elt rendered :text)))
    ;; The subagent's buffer: task first, then its work, ungrouped.
    (should (eq (map-elt child :mode) 'agent-shell-subagent-mode))
    (should (equal (seq-take (map-elt child :blocks) 3)
                   '("1-subagent-child-task" "1-C1" "1-m-child-agent_message_chunk")))
    (should-not (seq-some #'cdr (map-elt child :groups)))
    (should (string-match-p "Find prior art" (map-elt child :text)))
    (should (string-match-p "I looked" (map-elt child :text)))
    ;; The tool call stays filed under the row for its summary.
    (should (equal (map-nested-elt rendered '(:state :tool-calls "C1" :group-id))
                   "subagent-child"))))

(ert-deftest agent-shell-subagent-row-survives-the-turn-that-spawned-it-test ()
  "Work arriving after `end_turn' goes to the buffer and updates the one row.

A subagent outlives the turn that spawned it, so its row is addressed by
the namespace pinned at spawn rather than by whatever the root is doing
when the update lands.  Without that, the row renders a second time
under an `out-of-turn' namespace."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall end-turn)
                      (funcall send "child" '(sessionUpdate . "tool_call")
                               '(toolCallId . "C2") '(title . "Grep bar")
                               '(kind . "search") '(status . "completed"))
                      (funcall send "child" '(sessionUpdate . "agent_thought_chunk")
                               '(content (type . "text") (text . "still thinking"))))))
         (blocks (map-elt rendered :blocks))
         (child-blocks (map-elt (agent-shell-tests--subagent-report rendered "child")
                                :blocks)))
    (should (member "1-C2" child-blocks))
    (should (member "1-subagent-child-agent_thought_chunk-0" child-blocks))
    (should (equal (seq-count (lambda (id) (equal id "1-subagent-child")) blocks) 1))
    (should-not (seq-find (lambda (id) (string-prefix-p "out-of-turn-" id)) blocks))
    (should-not (seq-find (lambda (id) (string-prefix-p "out-of-turn-" id)) child-blocks))))

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
    (should (equal (seq-count (lambda (id) (equal id "1-C1"))
                              (map-elt (agent-shell-tests--subagent-report rendered "child")
                                       :blocks))
                   1))
    (should (equal (map-nested-elt state '(:tool-calls "C1" :title)) "Read foo.el"))
    (should (equal (map-nested-elt state '(:tool-calls "C1" :group-id)) "subagent-child"))
    ;; The root's own tool call is released, as before.
    (should-not (map-nested-elt state '(:tool-calls "T1")))))

(ert-deftest agent-shell-subagent-state-update-relabels-one-row-test ()
  "A lifecycle report updates the row rather than drawing a second one.

It arrives on the root's session, possibly turns later, so only the
namespace pinned at spawn points it back at the row drawn then.  The
terminal state also releases the subagent's tool calls, keeping the
summary they add up to."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send end-turn prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall end-turn)
                      (funcall prompt "something else")
                      (funcall send "root" '(sessionUpdate . "subagent_state_update")
                               '(subagentSessionId . "child") '(state . "completed")))))
         (blocks (map-elt rendered :blocks))
         (state (map-elt rendered :state))
         (subagent (agent-shell--native-subagent state "child")))
    (should (equal (seq-count (lambda (id) (equal id "1-subagent-child")) blocks) 1))
    (should-not (member "2-subagent-child" blocks))
    (should (equal (map-elt subagent :state) "completed"))
    (should (map-elt subagent :ended-at))
    (should (map-elt subagent :summary))
    (should (equal (map-elt subagent :tool-count) 1))
    (should-not (map-nested-elt state '(:tool-calls "C1")))
    ;; The row still reads what the subagent did.
    (should (string-match-p (regexp-quote (map-elt subagent :summary))
                            (map-elt rendered :text)))))

(ert-deftest agent-shell-subagent-message-spanning-the-turn-stays-one-message-test ()
  "A subagent message interrupted by `end_turn' keeps streaming into one block."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall end-turn)
                      (funcall send "child" '(sessionUpdate . "agent_message_chunk")
                               '(messageId . "m-child")
                               '(content (type . "text") (text . " and found it."))))))
         (child (agent-shell-tests--subagent-report rendered "child")))
    (should (equal (seq-count (lambda (id) (equal id "1-m-child-agent_message_chunk"))
                              (map-elt child :blocks))
                   1))
    (should (string-match-p "I looked and found it." (map-elt child :text)))))

(ert-deftest agent-shell-subagent-work-stays-out-of-a-later-page-test ()
  "A subagent still working through a later turn leaves that page alone.

The viewport pages by interaction, so content rendered at the shell's
end belongs to whichever prompt came last.  A subagent's work goes to
its own buffer instead, and its row stays on the page that spawned it."
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
    (should (string-match-p "Researcher" (substring-no-properties (cdr (nth 0 pages)))))
    (should-not (string-match-p "Researcher" (substring-no-properties (cdr (nth 1 pages)))))
    (should-not (string-match-p "Grep bar" (map-elt rendered :text)))
    (should (string-match-p "Grep bar" (map-elt (agent-shell-tests--subagent-report
                                                 rendered "child")
                                                :text)))
    (should (string-match-p "Answer 2." (substring-no-properties (cdr (nth 1 pages)))))))

(ert-deftest agent-shell-nested-subagent-row-renders-in-its-parent-buffer-test ()
  "A subagent spawned by a subagent gets its row in the spawner's buffer.

Its own work goes to a buffer of its own, like any subagent's, and the
shell shows only the root's direct child."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send _end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall send "child" '(sessionUpdate . "subagent_spawned")
                               '(subagentSessionId . "grandchild") '(name . "Reviewer")
                               '(task . "Check it"))
                      (funcall send "grandchild" '(sessionUpdate . "tool_call")
                               '(toolCallId . "G1") '(title . "Read bar.el")
                               '(kind . "read") '(status . "completed")))))
         (child (agent-shell-tests--subagent-report rendered "child"))
         (grandchild (agent-shell-tests--subagent-report rendered "grandchild")))
    (should-not (member "1-subagent-grandchild" (map-elt rendered :blocks)))
    (should (member "1-subagent-grandchild" (map-elt child :blocks)))
    (should-not (member "1-G1" (map-elt child :blocks)))
    (should (member "1-G1" (map-elt grandchild :blocks)))
    (should (string-match-p "Check it" (map-elt grandchild :text)))
    (should (equal (map-elt (agent-shell--native-subagent
                             (map-elt rendered :state) "grandchild")
                            :parent)
                   "child"))))

(ert-deftest agent-shell-subagent-update-registers-and-retires-a-subagent-test ()
  "The current draft's single `subagent_update' drives the whole lifecycle.

Its first sighting registers the subagent and draws its row, later ones
merge only the fields they carry, and a terminal state retires it."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send _end-turn _prompt)
                      (funcall send "root" '(sessionUpdate . "subagent_update")
                               '(subagentSessionId . "child") '(name . "Researcher")
                               '(task . "Find prior art") '(capabilities (cancel . t))
                               '(state . "running"))
                      (funcall send "child" '(sessionUpdate . "tool_call")
                               '(toolCallId . "C1") '(title . "Read foo.el")
                               '(kind . "read") '(status . "completed"))
                      (funcall send "root" '(sessionUpdate . "subagent_update")
                               '(subagentSessionId . "child") '(state . "failed")))))
         (subagent (agent-shell--native-subagent (map-elt rendered :state) "child")))
    (should (equal (map-elt subagent :name) "Researcher"))
    (should (equal (map-elt subagent :task) "Find prior art"))
    (should (equal (map-elt subagent :capabilities) '((cancel . t))))
    (should (equal (map-elt subagent :state) "failed"))
    (should (map-elt subagent :ended-at))
    (should (equal (seq-count (lambda (id) (equal id "1-subagent-child"))
                              (map-elt rendered :blocks))
                   1))
    (should (member "1-C1" (map-elt (agent-shell-tests--subagent-report rendered "child")
                                    :blocks)))))

(ert-deftest agent-shell-subagent-prompt-renders-in-its-buffer-test ()
  "A subagent's replayed prompt renders in its buffer and opens no page."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send _end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (funcall send "child" '(sessionUpdate . "user_message_chunk")
                               '(content (type . "text") (text . "Look for prior art"))))))
         (child (agent-shell-tests--subagent-report rendered "child")))
    (should (string-match-p "Look for prior art" (map-elt child :text)))
    (should-not (string-match-p "Look for prior art" (map-elt rendered :text)))
    (should (equal (length (map-elt rendered :pages)) 1))))

(ert-deftest agent-shell-subagent-permission-renders-in-both-places-test ()
  "A subagent's permission dialog shows in the shell and in its buffer.

The shell's copy names the subagent, since nothing else there says whose
request it is.  Answering it removes both copies."
  (let* ((during nil)
         (rendered
          (cl-letf (((symbol-function 'agent-shell--make-tool-call-permission-text)
                     (lambda (&rest _) "Allow reading foo.el?")))
            (agent-shell-tests--subagent-shell
             (lambda (send _end-turn _prompt)
               (agent-shell-tests--spawn-and-work send)
               (let* ((state agent-shell--state)
                      (agent-shell--subagent-group
                       (agent-shell--session-subagent-group state "child")))
                 (agent-shell--render-permission-fragment state "C1"))
               (setq during
                     (list (cons :shell (buffer-substring-no-properties
                                         (point-min) (point-max)))
                           (cons :shell-blocks (mapcar #'car (agent-shell-tests--rendered-blocks)))
                           (cons :child (agent-shell-tests--subagent-buffer-report
                                         (map-elt (agent-shell--native-subagent
                                                   agent-shell--state "child")
                                                  :buffer)))))
               (agent-shell--delete-permission-fragment agent-shell--state "C1"))))))
    (should (equal (map-nested-elt rendered '(:state :tool-calls "C1" :subagent-session-id))
                   "child"))
    (should (member "1-permission-C1" (map-elt during :shell-blocks)))
    (should (string-match-p "Researcher needs approval" (map-elt during :shell)))
    (should (member "1-permission-C1" (map-nested-elt during '(:child :blocks))))
    (should (string-match-p "Allow reading foo.el?" (map-nested-elt during '(:child :text))))
    ;; Answered: gone from both.
    (should-not (member "1-permission-C1" (map-elt rendered :blocks)))
    (should-not (member "1-permission-C1"
                        (map-elt (agent-shell-tests--subagent-report rendered "child")
                                 :blocks)))))

(ert-deftest agent-shell-unknown-session-content-gets-one-notice-test ()
  "Content from a session nobody announced renders in the shell after a notice.

The notice says whose it is once, however much of it arrives."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send _end-turn _prompt)
                      (funcall send "stranger" '(sessionUpdate . "agent_message_chunk")
                               '(messageId . "m-s") '(content (type . "text") (text . "Hello")))
                      (funcall send "stranger" '(sessionUpdate . "agent_message_chunk")
                               '(messageId . "m-s") '(content (type . "text") (text . " again"))))))
         (blocks (map-elt rendered :blocks)))
    (should (equal (seq-count (lambda (id) (equal id "1-unknown-session-stranger")) blocks)
                   1))
    (should (string-match-p "Hello again" (map-elt rendered :text)))
    (should (equal (map-nested-elt rendered '(:state :unknown-sessions)) '("stranger")))))

(ert-deftest agent-shell-subagent-buffers-die-with-the-shell-test ()
  "Killing a shell's subagent buffers leaves none behind."
  (let (buffer)
    (agent-shell-tests--subagent-shell
     (lambda (send _end-turn _prompt)
       (agent-shell-tests--spawn-and-work send)
       (setq buffer (map-elt (agent-shell--native-subagent agent-shell--state "child")
                             :buffer))
       (should (buffer-live-p buffer))))
    (should-not (buffer-live-p buffer))))

(ert-deftest agent-shell-subagent-buffer-is-remade-after-being-killed-test ()
  "A subagent buffer the user killed comes back for the subagent's next update."
  (let* ((rendered (agent-shell-tests--subagent-shell
                    (lambda (send _end-turn _prompt)
                      (agent-shell-tests--spawn-and-work send)
                      (kill-buffer (map-elt (agent-shell--native-subagent
                                             agent-shell--state "child")
                                            :buffer))
                      (funcall send "child" '(sessionUpdate . "tool_call")
                               '(toolCallId . "C2") '(title . "Grep bar")
                               '(kind . "search") '(status . "completed")))))
         (child (agent-shell-tests--subagent-report rendered "child")))
    (should child)
    (should (string-match-p "Grep bar" (map-elt child :text)))
    (should-not (string-match-p "I looked" (map-elt child :text)))))

(ert-deftest agent-shell-latest-page-namespace-p-excludes-an-earlier-turn-test ()
  "The viewport mirror takes the live namespaces and refuses an earlier one.

It only ever shows the latest interaction, so mirroring a fragment
pinned to a turn the user has paged past would append that turn's
content to a page it does not belong to."
  (let ((state '((:request-count . 2))))
    (should (agent-shell--latest-page-namespace-p state 2))
    (should (agent-shell--latest-page-namespace-p state "out-of-turn"))
    (should-not (agent-shell--latest-page-namespace-p state 1))))

(defun agent-shell-tests--file-text (file)
  "Return FILE's contents, or nil when it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defmacro agent-shell-tests--with-transcript-dir (var &rest body)
  "Bind VAR to a fresh directory for BODY and delete it afterwards."
  (declare (indent 1))
  `(let ((,var (make-temp-file "agent-shell-subagent-transcript" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(ert-deftest agent-shell-subagent-transcript-path-test ()
  "A subagent's transcript sits beside the root's, named after it."
  (should (equal (agent-shell-subagents--transcript-file-path
                  :parent-file "/t/2026-09-24.md"
                  :name "Survey subagent UI!"
                  :session-id "ab4463c3e484421df")
                 "/t/2026-09-24/subagents/survey-subagent-ui-ab4463c3e484421df.md"))
  ;; A nested subagent shares its parent's directory rather than nesting.
  (should (equal (agent-shell-subagents--transcript-file-path
                  :parent-file "/t/2026-09-24/subagents/researcher-child.md"
                  :parent-is-subagent t
                  :name nil
                  :session-id "task:generation:2")
                 "/t/2026-09-24/subagents/subagent-task-generation-2.md")))

(ert-deftest agent-shell-subagent-content-goes-to-its-own-transcript-test ()
  "A subagent writes its own transcript; the root's records it spawning and ending.

The root's transcript holds what the root session said and did, plus
an entry linking to the subagent's file when it spawns and another when
it ends.  The subagent's tool calls and messages are in its own file,
which starts with a header naming it, its task and its parent."
  (agent-shell-tests--with-transcript-dir dir
    (let* ((root-file (expand-file-name "session.md" dir))
           (rendered (agent-shell-tests--subagent-shell
                      (lambda (send _end-turn _prompt)
                        (agent-shell-tests--spawn-and-work send)
                        (funcall send "child" '(sessionUpdate . "tool_call_update")
                                 '(toolCallId . "C1") '(status . "completed"))
                        (funcall send "root" '(sessionUpdate . "subagent_state_update")
                                 '(subagentSessionId . "child") '(state . "completed")))
                      root-file))
           (child-file (map-elt (agent-shell--native-subagent (map-elt rendered :state) "child")
                                :transcript-file))
           (root-text (agent-shell-tests--file-text root-file))
           (child-text (agent-shell-tests--file-text child-file)))
      (should (equal child-file (expand-file-name "session/subagents/researcher-child.md" dir)))
      (should (string-match-p "^## Subagent: Researcher" root-text))
      (should (string-match-p (regexp-quote "(session/subagents/researcher-child.md)") root-text))
      (should (string-match-p "^## Subagent completed: Researcher" root-text))
      (should (string-match-p "Delegating\\." root-text))
      (should-not (string-match-p "I looked" root-text))
      (should-not (string-match-p "Read foo\\.el" root-text))
      (should (string-match-p "^\\*\\*Subagent:\\*\\* Researcher" child-text))
      (should (string-match-p "^\\*\\*Parent Session ID:\\*\\* root" child-text))
      (should (string-match-p (regexp-quote "[session.md](../../session.md)") child-text))
      (should (string-match-p "Find prior art" child-text))
      (should (string-match-p "I looked" child-text))
      (should (string-match-p "Tool Call \\[completed\\]: Read foo\\.el" child-text))
      (should (string-match-p "^## Subagent completed: Researcher" child-text))
      (should-not (string-match-p "Delegating\\." child-text)))))

(ert-deftest agent-shell-nested-subagent-spawn-is-logged-by-its-parent-test ()
  "A subagent spawned by a subagent is logged in that subagent's transcript."
  (agent-shell-tests--with-transcript-dir dir
    (let* ((root-file (expand-file-name "session.md" dir))
           (rendered (agent-shell-tests--subagent-shell
                      (lambda (send _end-turn _prompt)
                        (agent-shell-tests--spawn-and-work send)
                        (funcall send "child" '(sessionUpdate . "subagent_spawned")
                                 '(subagentSessionId . "grandchild") '(name . "Reviewer")
                                 '(task . "Check it"))
                        (funcall send "grandchild" '(sessionUpdate . "agent_message_chunk")
                                 '(messageId . "m-grandchild")
                                 '(content (type . "text") (text . "Checked."))))
                      root-file))
           (state (map-elt rendered :state))
           (child-text (agent-shell-tests--file-text
                        (map-elt (agent-shell--native-subagent state "child") :transcript-file)))
           (grandchild-file (map-elt (agent-shell--native-subagent state "grandchild")
                                     :transcript-file))
           (grandchild-text (agent-shell-tests--file-text grandchild-file)))
      (should (equal grandchild-file
                     (expand-file-name "session/subagents/reviewer-grandchild.md" dir)))
      (should (string-match-p "^## Subagent: Reviewer" child-text))
      (should (string-match-p (regexp-quote "(reviewer-grandchild.md)") child-text))
      (should-not (string-match-p "Reviewer" (agent-shell-tests--file-text root-file)))
      (should (string-match-p "^\\*\\*Parent Session ID:\\*\\* child" grandchild-text))
      (should (string-match-p "Checked\\." grandchild-text)))))

(defun agent-shell-tests--list-state ()
  "Return a state with a running and a finished subagent and one async task."
  (let ((state (agent-shell--make-state)))
    (map-put! (map-elt state :session) :id "root")
    (agent-shell--save-native-subagent state "done" "Reviewer" "Check it")
    (agent-shell--update-native-subagent
     state "done" (list (cons :spawned-at (time-subtract nil 120))
                        (cons :ended-at (time-subtract nil 60))
                        (cons :state "completed")))
    (agent-shell--save-native-subagent state "live" "Researcher" "Find prior art")
    (agent-shell--update-native-subagent
     state "live" (list (cons :spawned-at (time-subtract nil 30))))
    (agent-shell--save-async-task state "task-1" "npm test" "shell" "Run the tests"
                                  t t)
    state))

(ert-deftest agent-shell-subagents-header-indicator-counts-running-subagents-test ()
  "The header counts subagents still running and says nothing without any."
  (let ((state (agent-shell-tests--list-state)))
    (should (equal (substring-no-properties
                    (agent-shell-subagents--header-indicator state))
                   "1 subagent"))
    (agent-shell--save-native-subagent state "other" "Writer" "Write it")
    (should (equal (substring-no-properties
                    (agent-shell-subagents--header-indicator state))
                   "2 subagents"))
    (should-not (agent-shell-subagents--header-indicator (agent-shell--make-state)))))

(ert-deftest agent-shell-subagents-header-rebuilds-only-when-the-count-changes-test ()
  "A subagent's streaming does not rebuild the header unless its count moves."
  (let ((state (agent-shell-tests--list-state))
        (updates 0))
    (with-temp-buffer
      (setq-local major-mode 'agent-shell-mode)
      (map-put! state :buffer (current-buffer))
      (cl-letf (((symbol-function 'agent-shell--update-header-and-mode-line)
                 (lambda (&rest _) (setq updates (1+ updates))))
                ((symbol-function 'agent-shell-viewport--buffer) #'ignore))
        (agent-shell-subagents--refresh-header state)
        (agent-shell-subagents--refresh-header state)
        (should (= updates 1))
        (agent-shell--update-native-subagent
         state "live" (list (cons :ended-at (current-time))))
        (agent-shell-subagents--refresh-header state)
        (should (= updates 2))))))

(ert-deftest agent-shell-subagents-list-entries-put-running-first-test ()
  "Running subagents and tasks come first, finished ones after."
  (let ((entries (agent-shell-subagents--list-entries (agent-shell-tests--list-state))))
    (should (equal (mapcar (lambda (entry) (cons (map-elt entry :kind) (map-elt entry :id)))
                           entries)
                   '((subagent . "live") (async-task . "task-1") (subagent . "done"))))
    (should (equal (mapcar (lambda (entry) (map-elt entry :finished)) entries)
                   '(nil nil t)))
    (should (string-match-p "Researcher" (map-elt (car entries) :label-left)))
    (should (equal (map-elt (car entries) :body) "Find prior art"))
    (should (string-match-p "1m 0s" (map-elt (nth 2 entries) :label-right)))))

(ert-deftest agent-shell-subagents-list-async-task-body-shows-progress-test ()
  "An async task's row shows its description, then any recorded progress."
  (let ((body (lambda (state)
                (map-elt (seq-find (lambda (entry) (eq (map-elt entry :kind) 'async-task))
                                   (agent-shell-subagents--list-entries state))
                         :body))))
    (let ((state (agent-shell-tests--list-state)))
      (should (equal (funcall body state) "Run the tests"))
      (agent-shell--update-async-task
       state "task-1" (list :summary "Compiling"
                            :last-tool-name "Bash"
                            :usage '((totalTokens . 2000) (toolUses . 3))))
      (should (equal (funcall body state)
                     "Run the tests\nCompiling\nLast tool: Bash\n2k tokens · 3 tool uses")))
    (let ((state (agent-shell-tests--list-state)))
      (agent-shell--update-async-task state "task-1" (list :summary "Run the tests"))
      (should (equal (funcall body state) "Run the tests")))))

(ert-deftest agent-shell-subagents-elapsed-test ()
  "Elapsed time reads in the largest two units."
  (let ((start (current-time)))
    (should (equal (agent-shell-subagents--elapsed start (time-add start 42)) "42s"))
    (should (equal (agent-shell-subagents--elapsed start (time-add start 75)) "1m 15s"))
    (should (equal (agent-shell-subagents--elapsed start (time-add start 3725)) "1h 2m"))))

(defmacro agent-shell-tests--with-subagents-list (state &rest body)
  "Run BODY in a subagents list rendered from STATE."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (agent-shell-subagents-list-mode)
     (let ((shell (current-buffer)))
       (setq agent-shell-subagents--shell-buffer shell)
       (cl-letf (((symbol-function 'agent-shell-subagents--shell-state)
                  (lambda (&rest _) ,state)))
         (agent-shell-subagents--render-list ,state)
         ,@body))))

(defun agent-shell-tests--list-goto (text)
  "Move point to the row showing TEXT."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(ert-deftest agent-shell-subagents-list-renders-finished-rows-under-a-folded-group-test ()
  "Finished rows sit in a folded group named with their count."
  (let ((state (agent-shell-tests--list-state)))
    (agent-shell-tests--with-subagents-list state
      (let ((blocks (mapcar #'car (agent-shell-tests--rendered-blocks))))
        (should (equal blocks '("subagents-subagent:live" "subagents-async-task:task-1"
                                "subagents-finished" "subagents-subagent:done"))))
      (should (string-match-p "Finished (1)" (buffer-string)))
      (agent-shell-tests--list-goto "Reviewer")
      (should (get-text-property (point) 'invisible)))))

(ert-deftest agent-shell-subagents-list-refresh-keeps-folds-and-point-test ()
  "Redrawing the list keeps unfolded rows unfolded and point on its row."
  (let ((state (agent-shell-tests--list-state)))
    (agent-shell-tests--with-subagents-list state
      (should-not (string-match-p "Find prior art" (agent-shell-tests--visible-text)))
      (agent-shell-tests--list-goto "Researcher")
      (agent-shell-ui-toggle-fragment)
      (should (string-match-p "Find prior art" (agent-shell-tests--visible-text)))
      (agent-shell-tests--list-goto "npm test")
      (agent-shell-subagents-list-refresh)
      (should (string-match-p "Find prior art" (agent-shell-tests--visible-text)))
      (should (equal (map-elt (agent-shell-subagents--list-entry-at-point) :id) "task-1")))))

(defun agent-shell-tests--visible-text ()
  "Return the current buffer's text that is not invisible."
  (let ((position (point-min))
        (text ""))
    (while (< position (point-max))
      (let ((next (next-single-char-property-change position 'invisible)))
        (unless (get-char-property position 'invisible)
          (setq text (concat text (buffer-substring-no-properties position next))))
        (setq position next)))
    text))

(ert-deftest agent-shell-subagents-list-stop-is-gated-on-advertised-cancel-test ()
  "Stopping a subagent needs the agent to advertise `capabilities.cancel'."
  (let ((state (agent-shell-tests--list-state))
        (sent nil))
    (cl-letf (((symbol-function 'acp-send-notification)
               (lambda (&rest args) (push (plist-get args :notification) sent))))
      (agent-shell-tests--with-subagents-list state
        (agent-shell-tests--list-goto "Researcher")
        (should-error (agent-shell-subagents-list-stop) :type 'user-error)
        (should-not sent)
        (agent-shell--update-native-subagent
         state "live" (list (cons :capabilities '((cancel . t)))))
        (agent-shell-subagents-list-stop)
        (should (equal (map-nested-elt (car sent) '(:params sessionId)) "live"))
        (should (equal (map-elt (car sent) :method) "session/cancel"))
        ;; Finished rows have nothing to stop.
        (agent-shell-tests--list-goto "Reviewer")
        (should-error (agent-shell-subagents-list-stop) :type 'user-error)))))

(ert-deftest agent-shell-subagents-list-stops-a-stoppable-async-task-test ()
  "Stopping an async task row sends the agent's stop request for it."
  (let ((state (agent-shell-tests--list-state))
        (stopped nil))
    (cl-letf (((symbol-function 'agent-shell--send-async-task-stop)
               (lambda (_state id) (push id stopped))))
      (agent-shell-tests--with-subagents-list state
        (agent-shell-tests--list-goto "npm test")
        (agent-shell-subagents-list-stop)
        (should (equal stopped '("task-1")))
        (should-error (agent-shell-subagents-list-open) :type 'user-error)))))

(ert-deftest agent-shell-subagents-list-jump-goes-to-the-spawn-row-test ()
  "Jumping from the list lands on the subagent's row in the shell."
  (let (landed)
    (agent-shell-tests--subagent-shell
     (lambda (send _end-turn _prompt)
       (agent-shell-tests--spawn-and-work send)
       (let ((shell (current-buffer)))
         (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
           (agent-shell-subagents)
           (should (derived-mode-p 'agent-shell-subagents-list-mode))
           (agent-shell-tests--list-goto "Researcher")
           (agent-shell-subagents-list-jump)
           (should (eq (current-buffer) shell))
           (setq landed (map-elt (get-text-property (point) 'agent-shell-ui-state)
                                 :qualified-id))))))
    (should (equal landed "1-subagent-child"))))

(ert-deftest agent-shell-subagents-list-dies-with-the-shell-test ()
  "The subagents list is killed with the shell's subagent buffers."
  (let (list-buffer)
    (agent-shell-tests--subagent-shell
     (lambda (send _end-turn _prompt)
       (agent-shell-tests--spawn-and-work send)
       (save-current-buffer
         (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
           (agent-shell-subagents)
           (setq list-buffer (current-buffer))))))
    (should (bufferp list-buffer))
    (should-not (buffer-live-p list-buffer))))

(ert-deftest agent-shell-subagents-list-quit-returns-to-origin-test ()
  "Quitting the list switches back to the buffer it was opened from."
  (agent-shell-tests--subagent-shell
   (lambda (send _end-turn _prompt)
     (agent-shell-tests--spawn-and-work send)
     (let ((shell (current-buffer)))
       (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
         (agent-shell-subagents)
         (should (derived-mode-p 'agent-shell-subagents-list-mode))
         (agent-shell-subagents-list-quit)
         (should (eq (current-buffer) shell)))))))

(ert-deftest agent-shell-subagents-list-quit-falls-back-when-origin-is-gone-test ()
  "Quitting does not error when the origin buffer was killed meanwhile."
  (agent-shell-tests--subagent-shell
   (lambda (send _end-turn _prompt)
     (agent-shell-tests--spawn-and-work send)
     (let ((shell (current-buffer))
           (list-buffer nil))
       (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer))
         (agent-shell-subagents)
         (setq list-buffer (current-buffer)))
       (with-current-buffer list-buffer
         (setq-local agent-shell-subagents-list--origin (generate-new-buffer " *gone*"))
         (kill-buffer agent-shell-subagents-list--origin)
         (agent-shell-subagents-list-quit))
       (should (buffer-live-p shell))))))

(provide 'agent-shell-subagents-tests)
;;; agent-shell-subagents-tests.el ends here
