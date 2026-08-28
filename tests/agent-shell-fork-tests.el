;;; agent-shell-fork-tests.el --- Tests for message-specific session forks -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell-fork-message-meta-shape-test ()
  "Test that the fork-point `_meta' matches claude-agent-acp's wire shape.
`_meta.jetbrains.air.fork = {version: 1, messageId: MESSAGE-ID}', per
fork-session.ts's `forkPointMessageId'."
  (let ((meta (agent-shell--fork-message-meta "msg-1")))
    (should (equal (map-nested-elt meta '(jetbrains air fork version)) 1))
    (should (equal (map-nested-elt meta '(jetbrains air fork messageId)) "msg-1"))))

(cl-defun agent-shell-tests--fork-at-point (&key supports-fork-point message-id)
  "Invoke `agent-shell-fork-at-point' and report what its guards decided.

SUPPORTS-FORK-POINT is what the agent advertised at `initialize'.
MESSAGE-ID is the id stamped on the text at point, or nil for none.

Returns `forked' when the fork was attempted, or the `user-error' message
explaining why it was not."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (setq-local agent-shell--state
                (list (cons :supports-fork-point supports-fork-point)))
    (cl-letf (((symbol-function 'agent-shell--current-shell)
               (lambda (&rest _) (current-buffer)))
              ((symbol-function 'agent-shell--message-id-at-point)
               (lambda (&rest _) message-id))
              ((symbol-function 'agent-shell--fork-shell-buffer)
               (lambda (&rest _) 'forked)))
      (condition-case error
          (agent-shell-fork-at-point)
        (user-error (error-message-string error))))))

(ert-deftest agent-shell-fork-at-point-refuses-an-agent-without-the-extension-test ()
  "Forking from a message needs an agent that reads the fork point.

The point rides claude-agent-acp's AIR `_meta'.  An agent that never
advertised the extension ignores it and forks the latest turn, so the new
session still remembers everything the fork was meant to leave behind --
and nothing says so until the agent recalls it several turns later."
  (should (equal (agent-shell-tests--fork-at-point :supports-fork-point nil
                                                   :message-id "msg-1")
                 "This agent cannot fork from a specific message"))
  (should (eq (agent-shell-tests--fork-at-point :supports-fork-point t
                                                :message-id "msg-1")
              'forked))
  (should (equal (agent-shell-tests--fork-at-point :supports-fork-point t
                                                   :message-id nil)
                 "No agent message at point to fork from"))
  ;; An agent without the extension is reported first even when point
  ;; also has no message: that is why nothing is stamped, not a
  ;; cursor-placement mistake, so say so instead of blaming point.
  (should (equal (agent-shell-tests--fork-at-point :supports-fork-point nil
                                                   :message-id nil)
                 "This agent cannot fork from a specific message")))

(ert-deftest agent-shell-message-id-at-point-reads-the-property-test ()
  "Test that the ACP messageId stamped by `agent-shell--update-fragment' is
readable back at point."
  (with-temp-buffer
    (insert "before ")
    (let ((fragment-start (point)))
      (insert "agent message body")
      (put-text-property fragment-start (point) 'agent-shell-message-id "msg-1")
      (insert " after")
      (goto-char (1+ fragment-start))
      (should (equal (agent-shell--message-id-at-point) "msg-1")))))

(ert-deftest agent-shell-message-id-at-point-nil-outside-a-message-test ()
  "Test that point outside any tagged fragment yields no messageId."
  (with-temp-buffer
    (insert "plain text, no fragment here")
    (goto-char (point-min))
    (should-not (agent-shell--message-id-at-point))))

(ert-deftest agent-shell-message-id-at-point-falls-back-across-trailing-padding-test ()
  "Test that point past a stamped message still resolves its messageId.

`agent-shell-ui.el' inserts blank-line padding after every block, and
end of buffer carries no character at all -- neither position holds the
`agent-shell-message-id' property directly, so the nearest preceding
stamped run must answer instead."
  (with-temp-buffer
    (insert "agent message body")
    (put-text-property (point-min) (point) 'agent-shell-message-id "msg-1")
    (insert "\n\n")
    (goto-char (point-max))
    (should (equal (agent-shell--message-id-at-point) "msg-1"))))

(ert-deftest agent-shell-message-id-at-point-nil-when-buffer-has-no-stamp-test ()
  "Test that the padding fallback does not invent a messageId when none exists."
  (with-temp-buffer
    (insert "unstamped text\n\n")
    (goto-char (point-max))
    (should-not (agent-shell--message-id-at-point))))

(ert-deftest agent-shell-fork-at-point-errors-without-a-message-test ()
  "Test that forking at point fails clearly when point has no messageId,
from either the viewport or the plain shell buffer.  The agent supports
the extension here, so this isolates the point guard from Cause 5's
extension guard."
  (dolist (mode '(agent-shell-viewport-view-mode agent-shell-mode))
    (with-temp-buffer
      (setq-local major-mode mode)
      (setq-local agent-shell--state (list (cons :supports-fork-point t)))
      (cl-letf (((symbol-function 'agent-shell--current-shell)
                (lambda () (current-buffer))))
        (should-error (agent-shell-fork-at-point) :type 'user-error)))))

(ert-deftest agent-shell-fork-at-point-forks-with-the-message-id-at-point-test ()
  "Test that a valid point forwards its messageId to `agent-shell--fork-shell-buffer'.
From the viewport buffer, FROM-VIEWPORT should be non-nil (the mode
`derived-mode-p' matched, not literally t) so the new session also
opens in the viewport."
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-viewport-view-mode)
    (insert "agent message")
    (put-text-property (point-min) (point-max) 'agent-shell-message-id "msg-1")
    (goto-char (point-min))
    (let ((captured nil)
          (shell-buffer (generate-new-buffer " *agent-shell fork test*")))
      (with-current-buffer shell-buffer
        (setq-local agent-shell--state (list (cons :supports-fork-point t))))
      (cl-letf (((symbol-function 'agent-shell--current-shell)
                (lambda () shell-buffer))
                ((symbol-function 'agent-shell--fork-shell-buffer)
                (lambda (&rest args) (setq captured args) 'fake-new-buffer)))
        (agent-shell-fork-at-point))
      (should (eq (plist-get captured :shell-buffer) shell-buffer))
      (should (equal (plist-get captured :message-id) "msg-1"))
      (should (plist-get captured :from-viewport))
      (kill-buffer shell-buffer))))

(ert-deftest agent-shell-fork-at-point-from-shell-buffer-does-not-force-viewport-test ()
  "Test that forking at point from the plain shell buffer doesn't force
FROM-VIEWPORT, so `agent-shell--fork-shell-buffer' falls back to
`agent-shell-prefer-viewport-interaction' like `agent-shell-fork' does."
  (with-temp-buffer
    (setq-local major-mode 'agent-shell-mode)
    (insert "agent message")
    (put-text-property (point-min) (point-max) 'agent-shell-message-id "msg-1")
    (goto-char (point-min))
    (let ((captured nil)
          (shell-buffer (generate-new-buffer " *agent-shell fork test*")))
      (with-current-buffer shell-buffer
        (setq-local agent-shell--state (list (cons :supports-fork-point t))))
      (cl-letf (((symbol-function 'agent-shell--current-shell)
                (lambda () shell-buffer))
                ((symbol-function 'agent-shell--fork-shell-buffer)
                (lambda (&rest args) (setq captured args) 'fake-new-buffer)))
        (agent-shell-fork-at-point))
      (should (eq (plist-get captured :shell-buffer) shell-buffer))
      (should (equal (plist-get captured :message-id) "msg-1"))
      (should-not (plist-get captured :from-viewport))
      (kill-buffer shell-buffer))))

(ert-deftest agent-shell-fork-shell-buffer-errors-without-a-session-test ()
  "Test that forking fails clearly when the shell has no active session."
  (let ((shell-buffer (generate-new-buffer " *agent-shell-fork-test*")))
    (unwind-protect
        (with-current-buffer shell-buffer
          (setq-local agent-shell--state (list (cons :session nil)
                                               (cons :supports-session-fork t)))
          (should-error (agent-shell--fork-shell-buffer :shell-buffer shell-buffer)
                       :type 'user-error))
      (kill-buffer shell-buffer))))

(ert-deftest agent-shell-fork-shell-buffer-errors-when-agent-does-not-support-fork-test ()
  "Test that forking fails clearly when the agent never advertised `session.fork'."
  (let ((shell-buffer (generate-new-buffer " *agent-shell-fork-test*")))
    (unwind-protect
        (with-current-buffer shell-buffer
          (setq-local agent-shell--state (list (cons :session (list (cons :id "session-1")))
                                               (cons :supports-session-fork nil)))
          (should-error (agent-shell--fork-shell-buffer :shell-buffer shell-buffer)
                       :type 'user-error))
      (kill-buffer shell-buffer))))

(provide 'agent-shell-fork-tests)
;;; agent-shell-fork-tests.el ends here
