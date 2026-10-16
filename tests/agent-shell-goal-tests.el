;;; agent-shell-goal-tests.el --- Tests for agent-shell-goal -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'cl-lib)
(require 'agent-shell)
(require 'agent-shell-goal)

;;; Code:

;; Wire shapes below are claude-agent-acp's goal extension at 4d93c44:
;; the capability from src/acp-agent.ts:2950-2972, the snapshot from
;; src/acp-agent.ts:4036-4050 with the fields of
;; src/goal-extension.ts:18-29, and the request of
;; src/goal-extension.ts:31-34.  acp.el decodes JSON null as nil
;; (acp.el:1142), so a cleared goal arrives as `(goal)'.

(defconst agent-shell-goal-tests--capability-response
  '((protocolVersion . 1)
    (_meta (jetbrains (air (version . 1)
                           (capabilities . ["asyncTasks" "sessionIndex"])
                           (goal (version . 1)
                                 (controlMethod . "_session/goal")
                                 (actions . ["set" "clear"]))))
           (steering (supported . t))))
  "An `initialize' response advertising the goal extension.
From claude-agent-acp src/acp-agent.ts:2950-2972.")

(defconst agent-shell-goal-tests--snapshot
  '((objective . "Ship the change")
    (status . "active")
    (iterations . 3)
    (lastReason . "Tests still need work")
    (createdAt . 1710000000123)
    (controlMethod . "_session/goal"))
  "A goal snapshot as claude-agent-acp publishes it.
From docs/air-extensions.md \"Session state\" and
src/goal-extension.ts:18-29.")

(defun agent-shell-goal-tests--handshake (acp-response)
  "Run the initialize handshake answering ACP-RESPONSE, return the state."
  (with-temp-buffer
    (let ((agent-shell--state (agent-shell--make-state :buffer (current-buffer))))
      (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment)
                 #'ignore)
                ((symbol-function 'agent-shell--emit-event) #'ignore)
                ((symbol-function 'agent-shell--send-request)
                 (cl-function
                  (lambda (&key on-success &allow-other-keys)
                    (funcall on-success acp-response)))))
        (agent-shell--initiate-handshake
         :shell-buffer (current-buffer)
         :on-initiated #'ignore))
      agent-shell--state)))

(ert-deftest agent-shell-goal-handshake-reads-capability-test ()
  "The initialize response's `_meta.jetbrains.air.goal' sets `:goal-capability'.
An agent without the extension leaves it nil."
  (should (equal (map-elt (agent-shell-goal-tests--handshake
                           agent-shell-goal-tests--capability-response)
                          :goal-capability)
                 '((:control-method . "_session/goal")
                   (:actions . ("set" "clear")))))
  (should-not (map-elt (agent-shell-goal-tests--handshake '((protocolVersion . 1)))
                       :goal-capability))
  ;; A capability without a control method can't be used.
  (should-not (map-elt (agent-shell-goal-tests--handshake
                        '((_meta (jetbrains (air (version . 1)
                                                 (goal (version . 1)
                                                       (actions . ["set"])))))))
                       :goal-capability)))

(defun agent-shell-goal-tests--session-info (state update)
  "Drive a `session_info_update' carrying UPDATE through STATE.
Return the events emitted, oldest first."
  (let ((events nil))
    (cl-letf (((symbol-function 'agent-shell--update-header-and-mode-line) #'ignore)
              ((symbol-function 'agent-shell--emit-event)
               (cl-function
                (lambda (&key event data)
                  (push (list event data) events))))
              ((symbol-function 'agent-shell--update-transcript-title) #'ignore))
      (with-current-buffer (map-elt state :buffer)
        (agent-shell--on-notification
         :state state
         :acp-notification
         `((method . "session/update")
           (params (sessionId . "s1")
                   (update (sessionUpdate . "session_info_update") ,@update))))))
    (seq-filter (lambda (event) (eq (car event) 'goal-changed))
                (reverse events))))

(defun agent-shell-goal-tests--goal-update (goal)
  "Return a `session_info_update' body publishing GOAL (nil to clear).
Shaped like `withAirMeta' in claude-agent-acp src/air-extension.ts:80-97."
  `((_meta (jetbrains (air (version . 1) (goal . ,goal))))))

(ert-deftest agent-shell-goal-session-info-stores-and-clears-goal-test ()
  "A goal snapshot is stored, a null goal clears it, no goal key leaves it."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let* ((state (agent-shell--make-state :buffer (current-buffer)))
           (agent-shell--state state))
      (map-put! (map-elt state :session) :id "s1")
      (map-put! (map-elt state :session) :title "Kept")
      (agent-shell-goal-tests--session-info
       state (agent-shell-goal-tests--goal-update agent-shell-goal-tests--snapshot))
      (should (equal (map-elt state :goal)
                     '((:objective . "Ship the change")
                       (:status . "active")
                       (:iterations . 3)
                       (:last-reason . "Tests still need work")
                       (:created-at . 1710000000123))))
      (should (equal (map-nested-elt state '(:session :title)) "Kept"))
      ;; A title-only update says nothing about the goal.
      (agent-shell-goal-tests--session-info state '((title . "Retitled")))
      (should (equal (map-nested-elt state '(:goal :objective)) "Ship the change"))
      ;; `goal: null' (docs/air-extensions.md "Session state") clears it.
      (agent-shell-goal-tests--session-info
       state (agent-shell-goal-tests--goal-update nil))
      (should (assq :goal state))
      (should-not (map-elt state :goal)))))

(ert-deftest agent-shell-goal-session-info-works-on-state-without-goal-key-test ()
  "A shell state built before `:goal' existed still takes a goal."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let* ((state (list (cons :buffer (current-buffer))
                        (cons :session (list (cons :id "s1")))
                        (cons :last-entry-type nil)
                        (cons :last-activity-time nil)))
           (agent-shell--state state))
      (agent-shell-goal-tests--session-info
       state (agent-shell-goal-tests--goal-update agent-shell-goal-tests--snapshot))
      (should (equal (map-nested-elt state '(:goal :status)) "active")))))

(ert-deftest agent-shell-goal-changed-event-test ()
  "`goal-changed' fires when the objective or status changes, not on progress.
The runtime re-sends `active_goal' on each Stop-hook pass with a new
iteration count (claude-agent-acp src/acp-agent.ts:5735-5741), which
must not notify."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let* ((state (agent-shell--make-state :buffer (current-buffer)))
           (agent-shell--state state)
           (set-events (agent-shell-goal-tests--session-info
                        state (agent-shell-goal-tests--goal-update
                               agent-shell-goal-tests--snapshot)))
           (progress-events (agent-shell-goal-tests--session-info
                             state (agent-shell-goal-tests--goal-update
                                    (cons '(iterations . 4)
                                          agent-shell-goal-tests--snapshot))))
           (complete-events (agent-shell-goal-tests--session-info
                             state (agent-shell-goal-tests--goal-update
                                    (cons '(status . "complete")
                                          agent-shell-goal-tests--snapshot))))
           (clear-events (agent-shell-goal-tests--session-info
                          state (agent-shell-goal-tests--goal-update nil)))
           (repeat-clear-events (agent-shell-goal-tests--session-info
                                 state (agent-shell-goal-tests--goal-update nil))))
      (should (= (length set-events) 1))
      (should (equal (map-nested-elt (cadr (car set-events)) '(:goal :status))
                     "active"))
      (should-not (map-elt (cadr (car set-events)) :previous-goal))
      (should-not progress-events)
      (should (= (length complete-events) 1))
      (should (equal (map-nested-elt (cadr (car complete-events)) '(:goal :status))
                     "complete"))
      (should (equal (map-nested-elt (cadr (car complete-events))
                                     '(:previous-goal :status))
                     "active"))
      (should (= (length clear-events) 1))
      (should-not (map-elt (cadr (car clear-events)) :goal))
      (should (equal (map-nested-elt (cadr (car clear-events))
                                     '(:previous-goal :status))
                     "complete"))
      (should-not repeat-clear-events))))

(ert-deftest agent-shell-goal-indicator-test ()
  "The indicator truncates the objective, shows the status and its face.
The help echo carries the full objective, and `%' is doubled for header
and mode lines."
  (let ((indicator (agent-shell--goal-indicator
                    '((:objective . "Ship the change")
                      (:status . "active")
                      (:iterations . 3)
                      (:last-reason . "Tests still need work")))))
    (should (equal (substring-no-properties indicator)
                   "Goal: Ship the change (active)"))
    (should (eq (get-text-property 0 'face indicator) 'agent-shell-warning))
    (should (string-match-p "Ship the change"
                            (get-text-property 0 'help-echo indicator)))
    (should (string-match-p "Tests still need work"
                            (get-text-property 0 'help-echo indicator))))
  (dolist (case '(("paused" . agent-shell-pending)
                  ("blocked" . agent-shell-error)
                  ("limited" . agent-shell-error)
                  ("complete" . agent-shell-success)
                  ("unknown-future-status" . agent-shell-secondary)))
    (should (eq (get-text-property
                 0 'face (agent-shell--goal-indicator
                          `((:objective . "x") (:status . ,(car case)))))
                (cdr case))))
  (let* ((long (concat "Reach 100% coverage across "
                       (make-string 60 ?x)))
         (indicator (agent-shell--goal-indicator
                     `((:objective . ,long) (:status . "active")))))
    (should (string-prefix-p "Goal: Reach 100%% coverage"
                             (substring-no-properties indicator)))
    (should (string-match-p "\\.\\.\\. (active)\\'"
                            (substring-no-properties indicator)))
    (should (string-match-p (regexp-quote long)
                            (get-text-property 0 'help-echo indicator))))
  (should-not (agent-shell--goal-indicator nil)))

(ert-deftest agent-shell-goal-header-model-and-text-header-test ()
  "The header model carries the goal indicator; the text header shows it."
  (with-temp-buffer
    (setq-local agent-shell--state
                `((:agent-config . ((:buffer-name . "Claude Code")
                                    (:icon-name . nil)))
                  (:session . ((:id . "abc")))
                  (:goal . ((:objective . "Ship it") (:status . "active")))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state)))
      (should (equal (substring-no-properties
                      (map-elt (agent-shell--make-header-model agent-shell--state)
                               :goal-indicator))
                     "Goal: Ship it (active)"))))
  (should (equal (substring-no-properties
                  (agent-shell--text-header
                   '((:buffer-name . "Claude Code")
                     (:goal-indicator . "Goal: Ship it (active)")
                     (:project-name . "project"))))
                 " Claude Code ➤ Goal: Ship it (active) ➤ project")))

(ert-deftest agent-shell-goal-graphical-header-test ()
  "The graphical header draws the goal in its status color, `%' undoubled."
  (skip-unless (image-type-available-p 'svg))
  (with-temp-buffer
    (setq-local agent-shell--state
                '((:agent-config . ((:buffer-name . "Test")
                                    (:icon-name . nil)))
                  (:session . ((:id . "abc")))
                  (:goal . ((:objective . "Reach 100% coverage")
                            (:status . "blocked")))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--context-usage-indicator)
               (lambda () nil))
              ((symbol-function 'agent-shell--busy-indicator-frame)
               (lambda () nil))
              ((symbol-function 'agent-shell--session-id-indicator)
               (lambda () nil)))
      (let* ((agent-shell-header-style 'graphical)
             (agent-shell--header-cache nil)
             (header (agent-shell--make-header agent-shell--state))
             (svg-data (plist-get (cdr (get-text-property 1 'display header))
                                  :data)))
        (should (string-match-p
                 (format "<tspan[^>]*fill=\"%s\"[^>]*>Goal: Reach 100%% coverage (blocked)</tspan>"
                         (agent-shell--svg-fill-color 'agent-shell-error))
                 svg-data))))))

(ert-deftest agent-shell-goal-mode-line-test ()
  "The mode line shows the goal when there's no header."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (setq-local agent-shell--state
                '((:session . ((:id . "abc")))
                  (:goal . ((:objective . "Ship it") (:status . "complete")))))
    (cl-letf (((symbol-function 'agent-shell--state)
               (lambda () agent-shell--state))
              ((symbol-function 'agent-shell--busy-indicator-frame)
               (lambda () nil)))
      (let ((agent-shell-header-style nil))
        (should (string-match-p "➤ Goal: Ship it (complete)"
                                (agent-shell--mode-line-format)))))))

(ert-deftest agent-shell-goal-make-request-test ()
  "The request uses the advertised method and the adapter's params.
See claude-agent-acp src/goal-extension.ts:31-34 and 52-69."
  (should (equal (agent-shell--make-goal-request
                  :method "_session/goal" :session-id "s1"
                  :action "set" :objective "Ship it")
                 '((:method . "_session/goal")
                   (:params . ((sessionId . "s1")
                               (action . "set")
                               (objective . "Ship it"))))))
  (should (equal (agent-shell--make-goal-request
                  :method "_vendor/goal" :session-id "s1" :action "clear")
                 '((:method . "_vendor/goal")
                   (:params . ((sessionId . "s1")
                               (action . "clear"))))))
  (should-error (agent-shell--make-goal-request
                 :method "_session/goal" :session-id "s1" :action "set")))

(defun agent-shell-goal-tests--command-call (capability busy body)
  "Call BODY in an agent shell with goal CAPABILITY, BUSY or idle.
BODY gets a function returning what was sent so far, oldest first, as
`(request PLIST)' for `agent-shell--send-request' and `(prompt TEXT)'
for a prompt submitted through the shell."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (setq-local agent-shell--state (agent-shell--make-state :buffer (current-buffer)))
    (map-put! (map-elt agent-shell--state :session) :id "s1")
    (map-put! agent-shell--state :goal-capability capability)
    (map-put! agent-shell--state :goal '((:objective . "Old goal")
                                         (:status . "active")))
    (let ((sent nil))
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (lambda (&rest args) (push (list 'request args) sent)))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (cl-function
                  (lambda (&key text submit &allow-other-keys)
                    (should submit)
                    (push (list 'prompt text) sent))))
                ((symbol-function 'agent-shell--take-prompt-input) #'ignore)
                ((symbol-function 'shell-maker-busy) (lambda () busy))
                ((symbol-function 'agent-shell--prompt-queue-paused-p) #'ignore)
                ((symbol-function 'agent-shell--update-header-and-mode-line)
                 #'ignore))
        (funcall body (lambda () (reverse sent)))))))

(defconst agent-shell-goal-tests--capability
  '((:control-method . "_session/goal")
    (:actions . ("set" "clear")))
  "The normalized capability of `agent-shell-goal-tests--capability-response'.")

(ert-deftest agent-shell-goal-commands-need-capability-test ()
  "Without the capability, or an advertised action, the commands refuse."
  (agent-shell-goal-tests--command-call
   nil nil
   (lambda (sent)
     (should-error (agent-shell-set-goal "x") :type 'user-error)
     (should-error (agent-shell-clear-goal) :type 'user-error)
     (should-not (funcall sent))))
  (agent-shell-goal-tests--command-call
   '((:control-method . "_session/goal") (:actions . ("set")))
   nil
   (lambda (sent)
     (should-error (agent-shell-clear-goal) :type 'user-error)
     (should-not (funcall sent))
     (should (agent-shell--goal-supported-p "set"))
     (should-not (agent-shell--goal-supported-p "clear")))))

(ert-deftest agent-shell-goal-commands-while-busy-send-request-test ()
  "While a turn runs, the commands send the advertised control request.
The adapter steers the matching `/goal' command into the running turn
and answers at once (claude-agent-acp src/acp-agent.ts:4022-4034)."
  (agent-shell-goal-tests--command-call
   agent-shell-goal-tests--capability t
   (lambda (sent)
     (agent-shell-set-goal "  Ship it  ")
     (agent-shell-clear-goal)
     (should (equal (mapcar (lambda (call)
                              (plist-get (cadr call) :request))
                            (funcall sent))
                    '(((:method . "_session/goal")
                       (:params . ((sessionId . "s1")
                                   (action . "set")
                                   (objective . "Ship it"))))
                      ((:method . "_session/goal")
                       (:params . ((sessionId . "s1")
                                   (action . "clear")))))))
     ;; The snapshot arrives through `session_info_update', not the reply.
     (funcall (plist-get (cadr (car (funcall sent))) :on-success) nil)
     (should (equal (map-nested-elt agent-shell--state '(:goal :objective))
                    "Old goal"))
     (let ((shown nil))
       (cl-letf (((symbol-function 'message)
                  (lambda (fmt &rest args) (setq shown (apply #'format fmt args)))))
         (funcall (plist-get (cadr (car (funcall sent))) :on-failure)
                  '((code . -32602)
                    (message . "goal action \"set\" requires a non-empty objective"))
                  nil))
       (should (string-match-p "requires a non-empty objective" shown))))))

(ert-deftest agent-shell-goal-commands-while-idle-submit-prompt-test ()
  "While idle, the commands submit the `/goal' command as a prompt.
On an idle session the adapter's control request would run a whole turn
before answering (claude-agent-acp src/acp-agent.ts:4029-4031), a turn
the shell would not own.  The prompt publishes the same snapshot
\(src/acp-agent.ts:3821-3823, src/goal-extension.ts:36-50)."
  (agent-shell-goal-tests--command-call
   agent-shell-goal-tests--capability nil
   (lambda (sent)
     (agent-shell-set-goal "Ship it")
     (agent-shell-clear-goal)
     (should (equal (funcall sent)
                    '((prompt "/goal Ship it")
                      (prompt "/goal clear"))))
     (should-error (agent-shell-set-goal "   ") :type 'user-error)
     ;; "/goal clear" would clear rather than set.
     (should-error (agent-shell-set-goal "clear") :type 'user-error)
     (should (= (length (funcall sent)) 2)))))

(ert-deftest agent-shell-goal-set-goal-defaults-to-current-objective-test ()
  "Interactively, the minibuffer offers the current objective."
  (agent-shell-goal-tests--command-call
   agent-shell-goal-tests--capability t
   (lambda (sent)
     (cl-letf (((symbol-function 'read-string)
                (lambda (_prompt initial _history default)
                  (should-not initial)
                  (should (equal default "Old goal"))
                  default)))
       (call-interactively #'agent-shell-set-goal))
     (should (equal (map-nested-elt (cadr (car (funcall sent)))
                                    '(:request :params objective))
                    "Old goal")))))

(ert-deftest agent-shell-goal-commands-from-viewport-test ()
  "Setting and clearing the goal act on a viewport buffer's shell."
  (agent-shell-goal-tests--command-call
   agent-shell-goal-tests--capability nil
   (lambda (sent)
     (let ((shell-buffer (current-buffer)))
       (with-temp-buffer
         (setq-local major-mode 'agent-shell-viewport-view-mode)
         (cl-letf (((symbol-function 'agent-shell--current-shell)
                    (lambda () shell-buffer))
                   ((symbol-function 'read-string)
                    (lambda (&rest _) "Ship it")))
           (call-interactively #'agent-shell-set-goal)
           (agent-shell-clear-goal))))
     (should (equal (funcall sent)
                    '((prompt "/goal Ship it")
                      (prompt "/goal clear")))))))

(provide 'agent-shell-goal-tests)
;;; agent-shell-goal-tests.el ends here
