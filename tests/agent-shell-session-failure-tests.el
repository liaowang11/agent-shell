;;; agent-shell-session-failure-tests.el --- Tests for AIR session failures -*- lexical-binding: t; -*-

;;; Commentary:

;; claude-agent-acp reports retries, lost connections, usage limits and
;; provider errors as AIR `sessionFailure' records once the client names
;; that capability.  The payloads below copy the wire shape of
;; session-failure-extension.ts's `sessionFailureMeta'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell)

(defun agent-shell-session-failure-tests--meta (failure)
  "Return FAILURE wrapped the way `sessionFailureMeta' sends it."
  `((jetbrains (air (version . 1) (sessionFailure ,@failure)))))

(defconst agent-shell-session-failure-tests--retry
  '((id . "prompt-1:error") (revision . 1) (category . "service")
    (severity . "warning") (title . "Retrying Claude, attempt 2 of 10.")
    (details . "HTTP 529 (overloaded). Next attempt in 8s.") (actions . []))
  "An `api_retry' warning, as `apiRetryWarning' builds it.")

(defconst agent-shell-session-failure-tests--usage-limit
  '((id . "prompt-1:error") (revision . 3) (category . "limit")
    (severity . "error")
    (title . "You've hit your individual spend limit · run /usage-credits to ask your admin for a higher limit")
    (actions . []))
  "A usage-limit error, as attached to the prompt response.")

(defun agent-shell-session-failure-tests--notify (state update)
  "Dispatch session UPDATE to STATE and return the fragments it rendered."
  (let ((calls nil))
    (cl-letf (((symbol-function 'agent-shell--emit-event) #'ignore)
              ((symbol-function 'agent-shell--append-transcript) #'ignore)
              ((symbol-function 'agent-shell--set-session-title) #'ignore)
              ((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (push args calls))))
      (agent-shell--on-notification
       :state state
       :acp-notification `((method . "session/update")
                           (params (sessionId . "s1") (update ,@update)))))
    (nreverse calls)))

(defun agent-shell-session-failure-tests--state ()
  "Return a minimal shell state with no request in flight."
  (list (cons :buffer (current-buffer))
        (cons :chunked-group-count 0)
        (cons :last-entry-type nil)
        (cons :active-requests nil)
        (cons :last-activity-time nil)
        (cons :last-agent-message-ids nil)))

(ert-deftest agent-shell-advertises-session-failure-test ()
  "claude-agent-acp sends retries and turn failures only to a client naming it."
  (should (seq-contains-p (map-nested-elt (agent-shell--air-client-capabilities-meta)
                                          '(jetbrains air capabilities))
                          "sessionFailure")))

(ert-deftest agent-shell-session-failure-warning-renders-title-and-details-test ()
  "A retry warning shows its title as the label and the cause as the body."
  (let* ((state (agent-shell-session-failure-tests--state))
         (calls (agent-shell-session-failure-tests--notify
                 state
                 `((sessionUpdate . "session_info_update")
                   (_meta ,@(agent-shell-session-failure-tests--meta
                             agent-shell-session-failure-tests--retry)))))
         (call (car calls)))
    (should (= (length calls) 1))
    (should (equal (substring-no-properties (plist-get call :label-left))
                   "Retrying Claude, attempt 2 of 10."))
    (should (eq (get-text-property 0 'font-lock-face (plist-get call :label-left))
                'agent-shell-warning))
    (should (equal (plist-get call :body) "HTTP 529 (overloaded). Next attempt in 8s."))
    (should (plist-get call :above-last-prompt))))

(ert-deftest agent-shell-session-failure-revision-updates-the-same-block-test ()
  "Later revisions of one failure id replace its block instead of stacking."
  (let* ((state (agent-shell-session-failure-tests--state))
         (first (car (agent-shell-session-failure-tests--notify
                      state
                      `((sessionUpdate . "session_info_update")
                        (_meta ,@(agent-shell-session-failure-tests--meta
                                  agent-shell-session-failure-tests--retry))))))
         (second (car (agent-shell-session-failure-tests--notify
                       state
                       `((sessionUpdate . "session_info_update")
                         (_meta ,@(agent-shell-session-failure-tests--meta
                                   '((id . "prompt-1:error") (revision . 2)
                                     (severity . "warning")
                                     (title . "Retrying Claude, attempt 3 of 10."))))))))
         (other (car (agent-shell-session-failure-tests--notify
                      state
                      `((sessionUpdate . "session_info_update")
                        (_meta ,@(agent-shell-session-failure-tests--meta
                                  '((id . "prompt-2:error") (revision . 1)
                                    (severity . "warning")
                                    (title . "Retrying Claude, attempt 1 of 10.")))))))))
    (should (equal (plist-get first :block-id) (plist-get second :block-id)))
    (should-not (equal (plist-get first :block-id) (plist-get other :block-id)))
    (should-not (plist-get second :body))))

(ert-deftest agent-shell-session-info-update-without-failure-renders-nothing-test ()
  "A title-only `session_info_update' draws no failure block."
  (should-not (agent-shell-session-failure-tests--notify
               (agent-shell-session-failure-tests--state)
               '((sessionUpdate . "session_info_update") (title . "Fix the parser")))))

(ert-deftest agent-shell-prompt-response-session-failure-test ()
  "A failed turn ends `end_turn' with the failure in the response's `_meta'."
  (let ((failed `((stopReason . "end_turn")
                  (_meta ,@(agent-shell-session-failure-tests--meta
                            agent-shell-session-failure-tests--usage-limit)))))
    (should (equal (map-elt (agent-shell--session-failure (map-elt failed '_meta)) 'title)
                   (map-elt agent-shell-session-failure-tests--usage-limit 'title)))
    (should-not (agent-shell--prompt-response-success-p failed))
    (should (agent-shell--prompt-response-success-p '((stopReason . "end_turn"))))
    (should-not (agent-shell--prompt-response-success-p '((stopReason . "cancelled"))))
    (should-not (agent-shell--session-failure nil))))

(ert-deftest agent-shell-session-failure-error-uses-error-face-test ()
  "An error-severity failure is labelled with the error face."
  (let (call)
    (cl-letf (((symbol-function 'agent-shell--update-fragment)
               (lambda (&rest args) (setq call args))))
      (agent-shell--render-session-failure
       (agent-shell-session-failure-tests--state)
       agent-shell-session-failure-tests--usage-limit))
    (should (eq (get-text-property 0 'font-lock-face (plist-get call :label-left))
                'agent-shell-error))
    (should (string-prefix-p "You've hit your individual spend limit"
                             (substring-no-properties (plist-get call :label-left))))))

(ert-deftest agent-shell-auth-required-error-shows-its-reason-test ()
  "An `authRequired' rejection shows the agent's message, reason included.

claude-agent-acp's v2 agent says why the login failed in the message:
`Authentication required: <text>'.  The error dialog prints it as sent."
  (let ((message (concat "Authentication required: Failed to authenticate: "
                         "this login reached its time limit and cannot be renewed.")))
    (should (string-search message
                           (substring-no-properties
                            (agent-shell--make-error-dialog-text
                             :code -32000 :message message :raw-message nil))))))

(provide 'agent-shell-session-failure-tests)
;;; agent-shell-session-failure-tests.el ends here
