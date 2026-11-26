;;; agent-shell-rewind-tests.el --- Tests for editing an earlier prompt -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-shell)
(require 'agent-shell-fakes)

;;; Code:

;;;; Fixtures

(cl-defun agent-shell-rewind-tests--insert-exchange (prompt response &key steered)
  "Insert an exchange of PROMPT and RESPONSE the way the shell renders one.
STEERED marks the prompt as steered into a running turn."
  (insert (apply #'propertize "Claude> "
                 'font-lock-face '(agent-shell-prompt comint-highlight-prompt)
                 'field 'output
                 (when steered '(agent-shell-steered-prompt t)))
          prompt
          (propertize "<shell-maker-end-of-prompt>" 'shell-maker--marker t)
          response))

;;;; Capability

(ert-deftest agent-shell-rewind-anthropic-declares-cursors-and-rewind-test ()
  "Claude Code is told this client handles cursors and `_session/rewind'.

claude-agent-acp announces no cursor unless `session.cursors' is an
object, and answers `_session/rewind' only for an AIR client naming
\"sessionRewind\".  The encoded request must carry `\"cursors\":{}',
not null, which drops every cursor."
  (let* ((config (agent-shell-anthropic-make-claude-code-config))
         (capabilities (map-elt config :client-capabilities))
         (request (acp-make-initialize-request
                   :protocol-version 1
                   :client-capabilities
                   (append capabilities
                           `((_meta . ,(map-elt config :initialize-meta))))))
         (json (acp--serialize-json (map-elt request :params))))
    (should (assq 'cursors (map-elt capabilities 'session)))
    (should (seq-contains-p (map-nested-elt config '(:initialize-meta jetbrains air capabilities))
                            "sessionRewind"))
    (should (string-match-p "\"cursors\":{}" json))
    (should (string-match-p "\"sessionRewind\"" json))))

(ert-deftest agent-shell-rewind-omp-declares-no-cursors-test ()
  "omp has no cursors, so its config does not ask for them."
  (should-not (assq 'cursors (map-elt (map-elt (agent-shell-omp-make-agent-config)
                                               :client-capabilities)
                                      'session))))

(ert-deftest agent-shell-rewind-supported-only-when-advertised-test ()
  "`:supports-session-rewind' follows the agent's AIR capabilities."
  (dolist (case '((["asyncTasks" "sessionRewind"] . t)
                  (["asyncTasks"] . nil)))
    (with-temp-buffer
      (let ((agent-shell--state (agent-shell--make-state
                                 :agent-config (agent-shell-anthropic-make-claude-code-config)
                                 :buffer (current-buffer))))
        (cl-letf (((symbol-function 'agent-shell--update-bootstrapping-fragment) #'ignore)
                  ((symbol-function 'agent-shell--emit-event) #'ignore)
                  ((symbol-function 'agent-shell--send-request)
                   (cl-function
                    (lambda (&key on-success &allow-other-keys)
                      (funcall on-success
                               `((_meta (jetbrains (air (version . 1)
                                                        (capabilities . ,(car case)))))))))))
          (agent-shell--initiate-handshake
           :shell-buffer (current-buffer)
           :on-initiated #'ignore))
        (should (eq (map-elt agent-shell--state :supports-session-rewind)
                    (cdr case)))))))

;;;; Cursor tracking

(defun agent-shell-rewind-tests--cursor-update (cursor-id &rest fields)
  "Return a `cursor_update' for CURSOR-ID with FIELDS (an alist) added.
Without FIELDS, it is rewindable and anchored before a user message."
  `((sessionUpdate . "cursor_update")
    (cursorId . ,cursor-id)
    ,@(or fields
          '((capabilities (fork) (rewind))
            (anchor (type . "user_message") (messageId . "u")
                    (position . "before"))))))

(defun agent-shell-rewind-tests--prompt-position (text)
  "Return where the prompt holding TEXT begins in the current buffer."
  (seq-find (lambda (position)
              (save-excursion
                (goto-char position)
                (search-forward text (pos-eol) t)))
            (mapcar (lambda (span) (map-elt span :position))
                    (agent-shell--turn-spans))))

(ert-deftest agent-shell-rewind-replayed-cursor-marks-its-prompt-test ()
  "A replayed `cursor_update' lands on the prompt it follows.

claude-agent-acp replays each user message, then the cursor before it.
Nothing renders for it, so the page still pairs each prompt with its
answer, and no \"Unhandled\" block shows even with logging on."
  (let ((acp-logging-enabled t))
    (agent-shell-fakes-with-restored-shell
        (:verbosity 'full
         :updates
         `(("s1" . ((sessionUpdate . "user_message_chunk")
                    (content (type . "text") (text . "PROMPTONE"))))
           ("s1" . ((sessionUpdate . "agent_message_chunk") (messageId . "m1")
                    (content (type . "text") (text . "ANSWERONE"))))
           ("s1" . ((sessionUpdate . "user_message_chunk")
                    (content (type . "text") (text . "PROMPTTWO"))))
           ("s1" . ,(agent-shell-rewind-tests--cursor-update "before:u2"))
           ("s1" . ((sessionUpdate . "agent_message_chunk") (messageId . "m2")
                    (content (type . "text") (text . "ANSWERTWO"))))
           ("s1" . ((sessionUpdate . "user_message_chunk")
                    (content (type . "text") (text . "PROMPTTHREE"))))
           ("s1" . ,(agent-shell-rewind-tests--cursor-update "before:u3"))))
      (let ((spans (agent-shell--turn-spans)))
        (should (= (length spans) 3))
        (should (equal (mapcar (lambda (span)
                                 (get-text-property (map-elt span :position)
                                                    'agent-shell-cursor-id))
                               spans)
                       '(nil "before:u2" "before:u3")))
        (should (equal (string-trim
                        (substring-no-properties
                         (map-elt (agent-shell--turn-text (nth 1 spans)) :prompt)))
                       "PROMPTTWO"))
        (let ((response (map-elt (agent-shell--turn-text (nth 1 spans)) :response)))
          (should (string-match-p "ANSWERTWO" response))
          ;; One end-of-prompt marker per prompt, the one closing it.
          (should-not (string-match-p "<shell-maker-end-of-prompt>" response))))
      (should (assq 'rewind (agent-shell--cursor-capabilities
                             agent-shell--state "before:u2")))
      (should-not (string-match-p "Unhandled" (buffer-string))))))

(ert-deftest agent-shell-rewind-unrendered-prompt-cursors-are-dropped-test ()
  "A restore that skips a turn keeps none of its cursors.
Under `first-last', the middle prompt is not in the buffer to edit."
  (agent-shell-fakes-with-restored-shell
      (:verbosity 'first-last
       :updates
       `(("s1" . ((sessionUpdate . "user_message_chunk")
                  (content (type . "text") (text . "PROMPTONE"))))
         ("s1" . ((sessionUpdate . "agent_message_chunk")
                  (content (type . "text") (text . "ANSWERONE"))))
         ("s1" . ((sessionUpdate . "user_message_chunk")
                  (content (type . "text") (text . "PROMPTTWO"))))
         ("s1" . ,(agent-shell-rewind-tests--cursor-update "before:u2"))
         ("s1" . ((sessionUpdate . "agent_message_chunk")
                  (content (type . "text") (text . "ANSWERTWO"))))
         ("s1" . ((sessionUpdate . "user_message_chunk")
                  (content (type . "text") (text . "PROMPTTHREE"))))
         ("s1" . ,(agent-shell-rewind-tests--cursor-update "before:u3"))
         ("s1" . ((sessionUpdate . "agent_message_chunk")
                  (content (type . "text") (text . "ANSWERTHREE"))))))
    (should-not (assoc "before:u2" (map-elt agent-shell--state :cursors)))
    (should (assoc "before:u3" (map-elt agent-shell--state :cursors)))
    (should (equal (get-text-property
                    (agent-shell-rewind-tests--prompt-position "PROMPTTHREE")
                    'agent-shell-cursor-id)
                   "before:u3"))))

(defmacro agent-shell-rewind-tests--with-shell (&rest body)
  "Run BODY in a bare shell buffer with two turns and fresh state.
The turns' prompts are \"one\" and \"two\"; \"one\"'s answer is stamped
with messageId \"m1\"."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq-local comint-prompt-regexp "Claude> ")
     (setq-local shell-maker--config
                 (make-shell-maker-config :name "agent" :prompt "Claude> "
                                          :prompt-regexp "Claude> "))
     (setq major-mode 'agent-shell-mode)
     (setq-local agent-shell--state
                 (agent-shell--make-state :agent-config nil :buffer (current-buffer)))
     (map-put! (map-elt agent-shell--state :session) :id "s1")
     (agent-shell-rewind-tests--insert-exchange "one" "\n\n")
     (let ((start (point)))
       (insert "answer one")
       (put-text-property start (point) 'agent-shell-message-id "m1"))
     (insert "\n\n")
     (agent-shell-rewind-tests--insert-exchange "two" "\n\nanswer two")
     ,@body))

(defun agent-shell-rewind-tests--dispatch (update)
  "Dispatch `session/update' UPDATE for session s1 in the current shell."
  (agent-shell--dispatch-notification
   :state agent-shell--state
   :acp-notification `((method . "session/update")
                       (params (sessionId . "s1") (update . ,update)))))

(defun agent-shell-rewind-tests--live-update (cursor-id)
  "Return a live-shaped `cursor_update' for CURSOR-ID."
  (agent-shell-rewind-tests--cursor-update
   cursor-id
   '(capabilities (fork) (rewind))
   '(anchor (type . "agent_message") (messageId . "m1") (position . "after"))))

(ert-deftest agent-shell-rewind-live-cursor-marks-the-running-prompt-test ()
  "A live cursor, anchored after the last agent entity, marks the latest prompt.
Not the live input prompt below it, which the persistent prompt keeps
at the end of the buffer for the whole turn."
  (let ((acp-logging-enabled t)
        (agent-shell-persistent-prompt-enabled t))
    (agent-shell-rewind-tests--with-shell
      (insert "\n\n")
      (let ((start (point)))
        (insert (propertize "Claude> "
                            'font-lock-face '(agent-shell-prompt comint-highlight-prompt)
                            'field 'output))
        (setq-local comint-last-prompt (cons (copy-marker start) (copy-marker (point))))
        (should (agent-shell--live-prompt-start)))
      (agent-shell-rewind-tests--dispatch
       (agent-shell-rewind-tests--live-update "before:u2"))
      (should (equal (get-text-property (agent-shell-rewind-tests--prompt-position "two")
                                        'agent-shell-cursor-id)
                     "before:u2"))
      (should-not (get-text-property (agent-shell-rewind-tests--prompt-position "one")
                                     'agent-shell-cursor-id))
      (should-not (string-match-p "Unhandled" (buffer-string))))))

(ert-deftest agent-shell-rewind-live-cursor-keeps-an-earlier-one-test ()
  "A second live cursor for a turn that has one already is not attached."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--dispatch
     (agent-shell-rewind-tests--live-update "before:u2"))
    (agent-shell-rewind-tests--dispatch
     (agent-shell-rewind-tests--live-update "before:other"))
    (should (equal (get-text-property (agent-shell-rewind-tests--prompt-position "two")
                                      'agent-shell-cursor-id)
                   "before:u2"))
    (should-not (assoc "before:other" (map-elt agent-shell--state :cursors)))))

(ert-deftest agent-shell-rewind-live-cursor-skips-a-steered-prompt-test ()
  "A live cursor whose nearest prompt was steered marks nothing.
Only prompts that open a turn are editable."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--insert-exchange "[steer] more" "\n\nok" :steered t)
    (agent-shell-rewind-tests--dispatch
     (agent-shell-rewind-tests--live-update "before:steer"))
    (should-not (get-text-property (agent-shell-rewind-tests--prompt-position "two")
                                   'agent-shell-cursor-id))
    (should-not (map-elt agent-shell--state :cursors))))

(ert-deftest agent-shell-rewind-repeated-cursor-replaces-capabilities-test ()
  "A repeated cursorId replaces its capabilities; null clears; omitted keeps."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--dispatch
     (agent-shell-rewind-tests--live-update "before:u2"))
    (agent-shell-rewind-tests--dispatch
     (agent-shell-rewind-tests--cursor-update "before:u2" '(capabilities (fork))))
    (should (equal (agent-shell--cursor-capabilities agent-shell--state "before:u2")
                   '((fork))))
    (agent-shell-rewind-tests--dispatch
     (agent-shell-rewind-tests--cursor-update "before:u2" '(_meta)))
    (should (equal (agent-shell--cursor-capabilities agent-shell--state "before:u2")
                   '((fork))))
    (agent-shell-rewind-tests--dispatch
     (agent-shell-rewind-tests--cursor-update "before:u2" '(capabilities)))
    (should-not (agent-shell--cursor-capabilities agent-shell--state "before:u2"))
    ;; Still the prompt's cursor: only what it accepts changed.
    (should (equal (get-text-property (agent-shell-rewind-tests--prompt-position "two")
                                      'agent-shell-cursor-id)
                   "before:u2"))))

(ert-deftest agent-shell-rewind-subagent-cursor-is-ignored-test ()
  "A subagent's cursor is not on the shell's own timeline."
  (agent-shell-rewind-tests--with-shell
    (cl-letf (((symbol-function 'agent-shell--subagent-row)
               (lambda (_state) '((:session-id . "child")))))
      (agent-shell-rewind-tests--dispatch
       (agent-shell-rewind-tests--live-update "before:child")))
    (should-not (map-elt agent-shell--state :cursors))))

(provide 'agent-shell-rewind-tests)
;;; agent-shell-rewind-tests.el ends here
