;;; agent-shell-cursor-tests.el --- Tests for agent-shell-cursor -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-cursor)

;;; Code:

(ert-deftest agent-shell-cursor--make-text-content-block-test ()
  "Test `agent-shell-cursor--make-text-content-block'."
  (should (equal (agent-shell-cursor--make-text-content-block "hello")
                 '((type . "content")
                   (content (type . "text") (text . "hello")))))
  (should-not (agent-shell-cursor--make-text-content-block ""))
  (should-not (agent-shell-cursor--make-text-content-block nil)))

(ert-deftest agent-shell-cursor--content-from-raw-output-test ()
  "Test `agent-shell-cursor--content-from-raw-output'."
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((error . "permission denied")))
                 (list '((type . "content")
                         (content (type . "text") (text . "permission denied"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((content . "file contents")))
                 (list '((type . "content")
                         (content (type . "text") (text . "file contents"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((stdout . "hello")
                    (stderr . "warning")
                    (exitCode . 0)))
                 (list '((type . "content")
                         (content (type . "text")
                                  (text . "```\nExit code: 0\n\nhello\n\nwarning\n```"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((stdout . "done")))
                 (list '((type . "content")
                         (content (type . "text") (text . "```\ndone\n```"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((exitCode . 1)))
                 (list '((type . "content")
                         (content (type . "text") (text . "```\nExit code: 1\n```"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((totalMatches . 42)))
                 (list '((type . "content")
                         (content (type . "text") (text . "42 matches"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((totalMatches . 42)
                    (truncated . t)))
                 (list '((type . "content")
                         (content (type . "text") (text . "42 matches (truncated)"))))))
  (should (equal (agent-shell-cursor--content-from-raw-output
                  '((resultCount . 7)))
                 (list '((type . "content")
                         (content (type . "text") (text . "7 results"))))))
  (should-not (agent-shell-cursor--content-from-raw-output nil))
  (should-not (agent-shell-cursor--content-from-raw-output '((unknown . "value")))))

(ert-deftest agent-shell-cursor--notification-adapter-test ()
  "Test `agent-shell-cursor--notification-adapter'."
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (stdout . "hello")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "```\nhello\n```")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (content . ((type . "content")
                                       (content (type . "text")
                                                (text . "keep me"))))
                           (rawOutput (stdout . "ignored")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   '((type . "content")
                     (content (type . "text") (text . "keep me"))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (content . ())
                           (rawOutput (stdout . "hello")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "```\nhello\n```")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "in_progress")
                           (rawOutput (error . "permission denied")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "permission denied")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (content . "file contents")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "file contents")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (totalMatches . 42)
                                      (truncated . t)))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "42 matches (truncated)")))))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed")
                           (rawOutput (unknown . "value")))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (null (map-nested-elt notification '(params update content)))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "tool_call_update")
                           (status . "completed"))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (null (map-nested-elt notification '(params update content)))))
  (let ((notification
         '((method . "session/update")
           (params (update (sessionUpdate . "agent_message_chunk")
                           (content . "hello"))))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   "hello")))
  (let ((notification
         '((method . "session/request")
           (params (foo . "bar")))))
    (agent-shell-cursor--notification-adapter :acp-notification notification)
    (should (equal notification
                   '((method . "session/request")
                     (params (foo . "bar")))))))

(ert-deftest agent-shell--adapt-notification-cursor-integration-test ()
  "Test `agent-shell--adapt-notification' with Cursor config."
  (let* ((config (agent-shell-cursor-make-agent-config))
         (state (agent-shell--make-state :agent-config config))
         (notification
          '((method . "session/update")
            (params (update (sessionUpdate . "tool_call_update")
                            (status . "completed")
                            (rawOutput (stdout . "hello")))))))
    (agent-shell--adapt-notification :state state :acp-notification notification)
    (should (equal (map-nested-elt notification '(params update content))
                   (list '((type . "content")
                           (content (type . "text")
                                    (text . "```\nhello\n```"))))))))

(provide 'agent-shell-cursor-tests)
;;; agent-shell-cursor-tests.el ends here
