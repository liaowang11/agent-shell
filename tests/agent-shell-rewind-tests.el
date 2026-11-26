;;; agent-shell-rewind-tests.el --- Tests for editing an earlier prompt -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-shell)
(require 'agent-shell-fakes)

;; In agent-shell-tests.el, which the suite loads alongside this file.
(declare-function agent-shell-tests--compose-send-commands "agent-shell-tests")

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

;;;; Editing

(cl-defun agent-shell-rewind-tests--submit (&key (prompt "edited") (answers '(t))
                                                 response error on-done)
  "Submit PROMPT as an edit of prompt \"two\" in the current shell.

ANSWERS are what successive `y-or-n-p' calls return.  RESPONSE or ERROR,
when given, answer the first request sent.  ON-DONE is passed through.

Returns an alist of what happened: `:requests' sent, `:forks' (the
arguments of each `agent-shell--fork-shell-buffer' call), `:inserted'
\(each `agent-shell--insert-to-shell-buffer' call), `:synced' (each
`agent-shell--rewind-sync' call), `:questions' asked and `:error', the
`user-error' message signaled, if any."
  (let (requests forks inserted synced questions caught)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (question)
                 (push question questions)
                 (pop answers)))
              ((symbol-function 'agent-shell--send-request)
               (cl-function
                (lambda (&key request on-success on-failure &allow-other-keys)
                  (push request requests)
                  (cond (response (funcall on-success response))
                        (error (funcall on-failure error nil))))))
              ((symbol-function 'agent-shell--fork-shell-buffer)
               (lambda (&rest args) (push args forks) 'new-shell))
              ((symbol-function 'agent-shell--insert-to-shell-buffer)
               (lambda (&rest args) (push args inserted) nil))
              ((symbol-function 'agent-shell--rewind-sync)
               (lambda (&rest args) (push args synced) 'new-shell))
              ((symbol-function 'agent-shell--make-error-handler)
               (lambda (&rest _) #'ignore)))
      (condition-case err
          (agent-shell--edit-prompt-submit
           :shell-buffer (current-buffer)
           :position (agent-shell-rewind-tests--prompt-position "two")
           :prompt prompt
           :on-done on-done)
        (user-error (setq caught (error-message-string err)))))
    `((:requests . ,(nreverse requests))
      (:forks . ,(nreverse forks))
      (:inserted . ,(nreverse inserted))
      (:synced . ,(nreverse synced))
      (:questions . ,(nreverse questions))
      (:error . ,caught))))

(defun agent-shell-rewind-tests--add-cursor (capabilities)
  "Mark prompt \"two\" with cursor \"before:u2\" accepting CAPABILITIES."
  (agent-shell-rewind-tests--dispatch
   (agent-shell-rewind-tests--cursor-update
    "before:u2" `(capabilities ,@capabilities)
    '(anchor (type . "agent_message") (messageId . "m1") (position . "after")))))

(ert-deftest agent-shell-rewind-edit-sends-exactly-a-rewind-test ()
  "Editing a rewindable prompt sends `_session/rewind' and nothing else."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (map-put! agent-shell--state :supports-session-fork t)
    (map-put! agent-shell--state :supports-fork-point t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let ((result (agent-shell-rewind-tests--submit)))
      (should-not (map-elt result :error))
      (should (equal (map-elt result :requests)
                     '(((:method . "_session/rewind")
                        (:params . ((sessionId . "s1")
                                    (cursorId . "before:u2")))))))
      (should-not (map-elt result :forks))
      ;; Asked every time: file edits from the dropped turns stay.
      (should (seq-find (lambda (question) (string-match-p "[Ff]ile" question))
                        (map-elt result :questions))))))

(ert-deftest agent-shell-rewind-edit-declined-confirmation-sends-nothing-test ()
  "Answering no to the warning leaves everything as it was."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let ((result (agent-shell-rewind-tests--submit :answers '(nil))))
      (should-not (map-elt result :requests))
      (should-not (map-elt result :forks)))))

(ert-deftest agent-shell-rewind-edit-forks-at-a-fork-only-cursor-test ()
  "A cursor without `rewind' but with `fork' forks at that cursor."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (map-put! agent-shell--state :supports-session-fork t)
    (map-put! agent-shell--state :supports-fork-point t)
    (agent-shell-rewind-tests--add-cursor '((fork)))
    (let* ((done nil)
           (result (agent-shell-rewind-tests--submit
                    :on-done (lambda () (setq done t)))))
      (should-not (map-elt result :requests))
      (should (equal (plist-get (car (map-elt result :forks)) :cursor-id) "before:u2"))
      (should-not (plist-get (car (map-elt result :forks)) :message-id))
      (should (eq (plist-get (car (map-elt result :inserted)) :shell-buffer) 'new-shell))
      (should (equal (plist-get (car (map-elt result :inserted)) :text) "edited"))
      (should (plist-get (car (map-elt result :inserted)) :submit))
      (should done))))

(ert-deftest agent-shell-rewind-edit-forks-at-the-preceding-message-test ()
  "Without a usable cursor, the fork ends at the agent message before the prompt.
Covers a prompt with no cursor, and an agent (omp) that cannot rewind
even though the cursor says it could."
  (dolist (setup (list (lambda ())
                       (lambda ()
                         (agent-shell-rewind-tests--add-cursor '((rewind)))
                         (map-put! agent-shell--state :supports-session-rewind nil))))
    (agent-shell-rewind-tests--with-shell
      (map-put! agent-shell--state :supports-session-fork t)
      (map-put! agent-shell--state :supports-fork-point t)
      (funcall setup)
      (let ((result (agent-shell-rewind-tests--submit)))
        (should-not (map-elt result :requests))
        (should (equal (plist-get (car (map-elt result :forks)) :message-id) "m1"))
        (should-not (plist-get (car (map-elt result :forks)) :cursor-id))
        (should (equal (plist-get (car (map-elt result :inserted)) :text) "edited"))))))

(ert-deftest agent-shell-rewind-edit-without-any-way-errors-test ()
  "An agent that can neither rewind nor fork at a point says so."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-fork t)
    (let ((result (agent-shell-rewind-tests--submit)))
      (should (equal (map-elt result :error) "This agent cannot edit an earlier prompt"))
      (should-not (map-elt result :requests))
      (should-not (map-elt result :forks)))
    (goto-char (agent-shell-rewind-tests--prompt-position "two"))
    (should-error (agent-shell-edit-prompt-at-point) :type 'user-error)))

(ert-deftest agent-shell-rewind-refusal-changes-nothing-and-offers-fork-test ()
  "`rewound: false' keeps the buffer, the cursors and the draft.
For `beforeCompaction' it asks whether to fork instead."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (map-put! agent-shell--state :supports-session-fork t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let* ((before-text (buffer-string))
           (before-cursors (copy-tree (map-elt agent-shell--state :cursors)))
           (done nil)
           (result (agent-shell-rewind-tests--submit
                    :answers '(t nil)
                    :response '((rewound) (reason . "beforeCompaction"))
                    :on-done (lambda () (setq done t)))))
      (should (equal (buffer-string) before-text))
      (should (equal (map-elt agent-shell--state :cursors) before-cursors))
      (should-not (map-elt result :synced))
      (should-not (map-elt result :forks))
      (should (= (length (map-elt result :questions)) 2))
      (should (string-match-p "[Ff]ork" (nth 1 (map-elt result :questions))))
      (should-not done))
    ;; Saying yes forks at the cursor.
    (let ((result (agent-shell-rewind-tests--submit
                   :answers '(t t)
                   :response '((rewound) (reason . "beforeCompaction")))))
      (should (equal (plist-get (car (map-elt result :forks)) :cursor-id) "before:u2")))))

(ert-deftest agent-shell-rewind-busy-refusal-offers-no-fork-test ()
  "A `busy' refusal keeps the draft without offering a fork."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let ((result (agent-shell-rewind-tests--submit
                   :response '((rewound) (reason . "busy")))))
      (should (= (length (map-elt result :questions)) 1))
      (should-not (map-elt result :forks)))))

(ert-deftest agent-shell-rewind-success-syncs-with-the-edited-prompt-test ()
  "`rewound: true' syncs the buffer and hands it the edited prompt."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let* ((done nil)
           (result (agent-shell-rewind-tests--submit
                    :response '((rewound . t))
                    :on-done (lambda () (setq done t)))))
      (should (equal (map-elt result :synced)
                     `((:shell-buffer ,(current-buffer) :prompt "edited"
                                      :from-viewport nil))))
      (should done))))

(ert-deftest agent-shell-rewind-failed-sync-keeps-the-draft-test ()
  "A shell that could not reload never got the prompt, so the draft stays."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let ((done nil))
      (cl-letf (((symbol-function 'agent-shell-restart)
                 (lambda (&rest _) (user-error "Cancelled")))
                ((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'agent-shell--send-request)
                 (cl-function
                  (lambda (&key on-success &allow-other-keys)
                    (funcall on-success '((rewound . t))))))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest _) (error "Must not submit"))))
        (agent-shell--edit-prompt-submit
         :shell-buffer (current-buffer)
         :position (agent-shell-rewind-tests--prompt-position "two")
         :prompt "edited"
         :on-done (lambda () (setq done t))))
      (should-not done))))

(ert-deftest agent-shell-rewind-sync-reloads-the-session-with-the-prompt-test ()
  "The sync restarts on the same session and submits the edited prompt there.
The transcript keeps what it had and records the rewind."
  (agent-shell-rewind-tests--with-shell
    (let ((transcript (make-temp-file "agent-shell-rewind-transcript"))
          restarted inserted)
      (unwind-protect
          (progn
            (setq-local agent-shell--transcript-file transcript)
            (with-temp-file transcript (insert "## User\n\nold\n\n"))
            (cl-letf (((symbol-function 'agent-shell-restart)
                       (cl-function
                        (lambda (&key session-id)
                          (push session-id restarted)
                          'new-shell)))
                      ((symbol-function 'agent-shell--insert-to-shell-buffer)
                       (lambda (&rest args) (push args inserted) nil)))
              (agent-shell--rewind-sync :shell-buffer (current-buffer) :prompt "edited"))
            (should (equal restarted '("s1")))
            (should (eq (plist-get (car inserted) :shell-buffer) 'new-shell))
            (should (equal (plist-get (car inserted) :text) "edited"))
            (should (plist-get (car inserted) :submit))
            (let ((text (with-temp-buffer
                          (insert-file-contents transcript)
                          (buffer-string))))
              (should (string-prefix-p "## User\n\nold\n\n" text))
              (should (string-match-p "^## Rewound " text))))
        (delete-file transcript)))))

(ert-deftest agent-shell-rewind-unknown-cursor-is-dropped-test ()
  "-32602 means the cursor is gone: forget it and keep the draft."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let ((result (agent-shell-rewind-tests--submit
                   :error '((code . -32602) (message . "bad cursor")))))
      (should (equal (map-nested-elt (car (map-elt result :requests)) '(:method))
                     "_session/rewind"))
      (should-not (map-elt result :forks))
      (should-not (map-elt result :synced)))
    (should-not (map-elt agent-shell--state :cursors))
    (should-not (get-text-property (agent-shell-rewind-tests--prompt-position "two")
                                   'agent-shell-cursor-id))))

(ert-deftest agent-shell-rewind-method-not-found-clears-capability-test ()
  "-32601 means the agent cannot rewind: stop offering it and fork instead."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (map-put! agent-shell--state :supports-session-fork t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let ((result (agent-shell-rewind-tests--submit
                   :answers '(t t)
                   :error '((code . -32601) (message . "Method not found")))))
      (should-not (map-elt agent-shell--state :supports-session-rewind))
      (should (equal (plist-get (car (map-elt result :forks)) :cursor-id) "before:u2")))))

(ert-deftest agent-shell-rewind-refuses-a-busy-shell-test ()
  "A running turn or a non-empty queue refuses before any request."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let ((result (let ((shell-maker--busy t))
                    (agent-shell-rewind-tests--submit))))
      (should (string-match-p "[Ii]nterrupt" (map-elt result :error)))
      (should-not (map-elt result :requests))
      (should-not (map-elt result :questions)))
    (map-put! agent-shell--state :pending-prompts '("queued"))
    (let ((result (agent-shell-rewind-tests--submit)))
      (should (string-match-p "queue" (map-elt result :error)))
      (should-not (map-elt result :requests)))))

;;;; Editing in the compose buffer

(cl-defun agent-shell-rewind-tests--compose (command &key args (draft "two more") on-submit)
  "Call compose COMMAND with ARGS on DRAFT, an edit of prompt \"two\".

Runs in a compose buffer of the current shell, with the normal send paths
stubbed to fail.  ON-SUBMIT, when given, is called with the arguments of
`agent-shell--edit-prompt-submit'.

Returns an alist: `:submitted' (each `agent-shell--edit-prompt-submit'
call), `:error' (the `user-error' message, if any), `:position' (the
edit state left) and `:draft' (the text left)."
  (let ((shell-buffer (current-buffer))
        (position (copy-marker (agent-shell-rewind-tests--prompt-position "two")))
        submitted caught)
    (with-temp-buffer
      (insert draft)
      (setq agent-shell-viewport--edit-past-position position)
      (setq major-mode 'agent-shell-viewport-edit-mode)
      (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                 (lambda (&rest _) shell-buffer))
                ((symbol-function 'agent-shell-viewport--hold-p)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell--edit-prompt-submit)
                 (lambda (&rest args)
                   (push args submitted)
                   (when on-submit
                     (apply on-submit args))))
                ((symbol-function 'agent-shell--prompt-send)
                 (lambda (&rest _) (error "Sent as a new prompt")))
                ((symbol-function 'agent-shell--prompt-steer)
                 (lambda (&rest _) (error "Steered as a new prompt")))
                ((symbol-function 'agent-shell--prompt-queue-replace)
                 (lambda (&rest _) (error "Replaced a queued prompt")))
                ((symbol-function 'agent-shell--prompt-queue-steer-at)
                 (lambda (&rest _) (error "Steered a queued prompt")))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest _) (error "Sent as a new prompt")))
                ((symbol-function 'agent-shell-viewport--initialize)
                 (lambda (&rest _) (erase-buffer)))
                ((symbol-function 'agent-shell-viewport--dismiss) #'ignore)
                ((symbol-function 'agent-shell-viewport-view-last) #'ignore)
                ((symbol-function 'kill-buffer) #'ignore)
                ((symbol-function 'pop-to-buffer) #'ignore))
        (condition-case err
            (apply command args)
          (user-error (setq caught (error-message-string err))))
        `((:submitted . ,(nreverse submitted))
          (:error . ,caught)
          (:position . ,agent-shell-viewport--edit-past-position)
          (:draft . ,(buffer-string)))))))

(defun agent-shell-rewind-tests--should-submit-edit (result)
  "Assert RESULT (see `agent-shell-rewind-tests--compose') ran the edit."
  (should-not (map-elt result :error))
  (should (= (length (map-elt result :submitted)) 1))
  (let ((args (car (map-elt result :submitted))))
    (should (equal (plist-get args :prompt) "two more"))
    (should (= (plist-get args :position)
               (agent-shell-rewind-tests--prompt-position "two")))
    (should (plist-get args :from-viewport)))
  ;; Nothing confirmed it yet, so the draft and its target stay.
  (should (map-elt result :position))
  (should (equal (map-elt result :draft) "two more")))

(defun agent-shell-rewind-tests--should-refuse-edit (result)
  "Assert RESULT (see `agent-shell-rewind-tests--compose') refused the edit."
  (should (map-elt result :error))
  (should-not (map-elt result :submitted))
  (should (map-elt result :position))
  (should (equal (map-elt result :draft) "two more")))

(ert-deftest agent-shell-rewind-compose-send-runs-the-edit-test ()
  "\\[agent-shell-viewport-compose-send] on a past prompt's edit runs the edit."
  (agent-shell-rewind-tests--with-shell
    (dolist (settings '((nil . nil) (t . nil) (nil . t)))
      (let ((agent-shell-viewport-dismiss-on-send (car settings))
            (agent-shell-prefer-viewport-interaction (cdr settings)))
        (agent-shell-rewind-tests--should-submit-edit
         (agent-shell-rewind-tests--compose #'agent-shell-viewport-compose-send))))))

(ert-deftest agent-shell-rewind-compose-send-keep-composing-runs-the-edit-test ()
  "\\[universal-argument] \\[agent-shell-viewport-compose-send] runs the edit too."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--should-submit-edit
     (agent-shell-rewind-tests--compose #'agent-shell-viewport-compose-send :args '(t)))))

(ert-deftest agent-shell-rewind-compose-send-override-runs-the-edit-test ()
  "The override route has no turn to pick a route for, so it runs the edit."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--should-submit-edit
     (agent-shell-rewind-tests--compose #'agent-shell-viewport-compose-send-override))))

(ert-deftest agent-shell-rewind-compose-send-and-kill-runs-the-edit-test ()
  "`agent-shell-viewport-compose-send-and-kill' runs the edit."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--should-submit-edit
     (agent-shell-rewind-tests--compose #'agent-shell-viewport-compose-send-and-kill))))

(ert-deftest agent-shell-rewind-compose-send-and-dismiss-runs-the-edit-test ()
  "`agent-shell-viewport-compose-send-and-dismiss' runs the edit."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--should-submit-edit
     (agent-shell-rewind-tests--compose #'agent-shell-viewport-compose-send-and-dismiss))))

(ert-deftest agent-shell-rewind-compose-send-and-wait-runs-the-edit-test ()
  "`agent-shell-viewport-compose-send-and-wait-for-response' runs the edit."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--should-submit-edit
     (agent-shell-rewind-tests--compose
      #'agent-shell-viewport-compose-send-and-wait-for-response))))

(ert-deftest agent-shell-rewind-compose-queue-refuses-the-edit-test ()
  "Queueing a past prompt's edit is refused: no turn runs to queue behind."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--should-refuse-edit
     (agent-shell-rewind-tests--compose #'agent-shell-viewport-compose-queue))))

(ert-deftest agent-shell-rewind-compose-steer-refuses-the-edit-test ()
  "Steering a past prompt's edit is refused: no turn runs to steer into."
  (agent-shell-rewind-tests--with-shell
    (agent-shell-rewind-tests--should-refuse-edit
     (agent-shell-rewind-tests--compose #'agent-shell-viewport-compose-steer))))

(ert-deftest agent-shell-rewind-every-compose-send-handles-the-edit-test ()
  "Every compose send command runs a past prompt's edit or refuses it.
None sends it as a new prompt.  Enumerated, so a new send command that
skips `agent-shell-viewport--compose-route' fails here."
  (agent-shell-rewind-tests--with-shell
    (dolist (command (agent-shell-tests--compose-send-commands))
      (let ((result (agent-shell-rewind-tests--compose command)))
        (should (equal (list command
                             (or (map-elt result :error)
                                 (length (map-elt result :submitted))))
                       (list command
                             (if (memq command '(agent-shell-viewport-compose-queue
                                                 agent-shell-viewport-compose-steer))
                                 (map-elt result :error)
                               1))))
        (should (map-elt result :position))))))

(ert-deftest agent-shell-rewind-compose-edit-clears-once-sent-test ()
  "Once the edit is on its way, the draft and the edit state go."
  (agent-shell-rewind-tests--with-shell
    (let ((result (agent-shell-rewind-tests--compose
                   #'agent-shell-viewport-compose-send-and-kill
                   :on-submit (cl-function
                               (lambda (&key on-done &allow-other-keys)
                                 (funcall on-done))))))
      (should (= (length (map-elt result :submitted)) 1))
      (should-not (map-elt result :position))
      (should (equal (map-elt result :draft) "")))))

(ert-deftest agent-shell-rewind-compose-edit-refused-can-be-sent-again-test ()
  "A refused edit keeps the draft as an edit, so sending again retries it.
Uses the real rewind flow: the agent refuses with `busy' the first time."
  (agent-shell-rewind-tests--with-shell
    (map-put! agent-shell--state :supports-session-rewind t)
    (agent-shell-rewind-tests--add-cursor '((fork) (rewind)))
    (let ((shell-buffer (current-buffer))
          (requests 0))
      (with-temp-buffer
        (insert "two more")
        (setq agent-shell-viewport--edit-past-position
              (with-current-buffer shell-buffer
                (copy-marker (agent-shell-rewind-tests--prompt-position "two"))))
        (setq major-mode 'agent-shell-viewport-edit-mode)
        (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                   (lambda (&rest _) shell-buffer))
                  ((symbol-function 'y-or-n-p) (lambda (_) t))
                  ((symbol-function 'agent-shell--send-request)
                   (cl-function
                    (lambda (&key on-success &allow-other-keys)
                      (setq requests (1+ requests))
                      (funcall on-success '((rewound) (reason . "busy"))))))
                  ((symbol-function 'agent-shell--insert-to-shell-buffer)
                   (lambda (&rest _) (error "Sent as a new prompt")))
                  ((symbol-function 'kill-buffer) #'ignore)
                  ((symbol-function 'pop-to-buffer) #'ignore))
          (agent-shell-viewport-compose-send-and-kill)
          (should (equal (buffer-string) "two more"))
          (should agent-shell-viewport--edit-past-position)
          (agent-shell-viewport-compose-send-and-kill)
          (should (= requests 2)))))))

(defmacro agent-shell-rewind-tests--with-viewport (&rest body)
  "Run BODY in a shell from `agent-shell-rewind-tests--with-shell' with a viewport.
The viewport's rendering is stubbed; it is killed afterwards."
  (declare (indent 0) (debug t))
  `(agent-shell-rewind-tests--with-shell
     (rename-buffer (generate-new-buffer-name " *agent-shell rewind shell*"))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell-viewport--initialize)
                    (lambda (&rest _) (erase-buffer)))
                   ((symbol-function 'agent-shell-viewport--update-header) #'ignore)
                   ((symbol-function 'agent-shell--display-buffer) #'ignore)
                   ((symbol-function 'agent-shell-viewport--recenter) #'ignore))
           ,@body)
       (when-let* ((viewport (agent-shell-viewport--buffer :shell-buffer (current-buffer)
                                                           :existing-only t)))
         (with-current-buffer viewport
           (let ((agent-shell-viewport--clean-up nil))
             (kill-buffer viewport)))))))

(ert-deftest agent-shell-rewind-edit-opens-the-compose-buffer-test ()
  "Editing a prompt opens the shell's compose buffer on it, marked as an edit.
A disposition left from an earlier draft does not carry over."
  (agent-shell-rewind-tests--with-viewport
    (map-put! agent-shell--state :supports-session-fork t)
    (agent-shell-rewind-tests--add-cursor '((fork)))
    (with-current-buffer (agent-shell-viewport--buffer :shell-buffer (current-buffer))
      (setq agent-shell-viewport--compose-disposition 'steer))
    (goto-char (+ (agent-shell-rewind-tests--prompt-position "two") 9))
    (agent-shell-edit-prompt-at-point)
    (with-current-buffer (agent-shell-viewport--buffer :shell-buffer (current-buffer)
                                                       :existing-only t)
      (should (derived-mode-p 'agent-shell-viewport-edit-mode))
      (should (equal (buffer-string) "two"))
      (should-not agent-shell-viewport--compose-disposition)
      (should-not agent-shell-viewport--edit-pending-index)
      (should (= (marker-position agent-shell-viewport--edit-past-position)
                 (with-current-buffer (marker-buffer agent-shell-viewport--edit-past-position)
                   (agent-shell-rewind-tests--prompt-position "two")))))))

(ert-deftest agent-shell-rewind-edit-from-the-viewport-edits-its-page-test ()
  "From the viewport, the prompt edited is the one of the page shown."
  (agent-shell-rewind-tests--with-viewport
    (map-put! agent-shell--state :supports-session-fork t)
    (map-put! agent-shell--state :supports-fork-point t)
    (let ((shell-buffer (current-buffer))
          (viewport (agent-shell-viewport--buffer :shell-buffer (current-buffer))))
      (with-current-buffer viewport
        (agent-shell-viewport-view-mode)
        (cl-letf (((symbol-function 'agent-shell--current-shell)
                   (lambda () shell-buffer))
                  ((symbol-function 'agent-shell-viewport--position)
                   (lambda (&rest _) '((:current . 2) (:total . 2)))))
          (agent-shell-edit-prompt-at-point))
        (should (derived-mode-p 'agent-shell-viewport-edit-mode))
        (should (equal (buffer-string) "two"))
        (should (= (marker-position agent-shell-viewport--edit-past-position)
                   (with-current-buffer shell-buffer
                     (agent-shell-rewind-tests--prompt-position "two"))))))))

(ert-deftest agent-shell-rewind-edit-asks-before-replacing-a-draft-test ()
  "Editing a prompt over a draft asks first; declining keeps the draft."
  (agent-shell-rewind-tests--with-viewport
    (map-put! agent-shell--state :supports-session-fork t)
    (agent-shell-rewind-tests--add-cursor '((fork)))
    (let ((viewport (agent-shell-viewport--buffer :shell-buffer (current-buffer))))
      (with-current-buffer viewport
        (insert "my draft"))
      (goto-char (+ (agent-shell-rewind-tests--prompt-position "two") 9))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (should-error (agent-shell-edit-prompt-at-point) :type 'user-error))
      (with-current-buffer viewport
        (should (equal (buffer-string) "my draft"))
        (should-not agent-shell-viewport--edit-past-position)))))

(ert-deftest agent-shell-rewind-compose-edit-survives-initialize-test ()
  "Clearing the compose buffer keeps the edit: paging clears it this way.
`agent-shell-viewport--initialize' runs whenever a page or the compose
page is shown, and the parked draft it leaves behind is still the edit."
  (with-temp-buffer
    (let ((marker (copy-marker 1)))
      (setq agent-shell-viewport--edit-past-position marker)
      (cl-letf (((symbol-function 'agent-shell-viewport--ensure-buffer) #'ignore)
                ((symbol-function 'agent-shell-viewport--position) #'ignore)
                ((symbol-function 'agent-shell-viewport--update-header) #'ignore)
                ((symbol-function 'agent-shell-viewport--sync-layout-spacer) #'ignore)
                ((symbol-function 'agent-shell--prompt-suggestion-refresh) #'ignore))
        (agent-shell-viewport--initialize))
      (should (eq agent-shell-viewport--edit-past-position marker)))))

(ert-deftest agent-shell-rewind-compose-edit-survives-paging-test ()
  "Leaving the compose page and coming back keeps a draft an edit.
A blank draft parks nothing, so leaving drops the edit rather than hand
it to the next draft."
  (agent-shell-rewind-tests--with-viewport
    (with-current-buffer (agent-shell-viewport--buffer :shell-buffer (current-buffer))
      (let ((marker (copy-marker 1)))
        (insert "two more")
        (setq agent-shell-viewport--edit-past-position marker)
        (agent-shell-viewport-view-mode)
        (agent-shell-viewport-edit-mode)
        (should (eq agent-shell-viewport--edit-past-position marker))
        (erase-buffer)
        (agent-shell-viewport-view-mode)
        (should-not agent-shell-viewport--edit-past-position)))))

(ert-deftest agent-shell-rewind-compose-override-drops-the-edit-test ()
  "A draft that overrides the edit is sent as a new prompt, not as the edit."
  (agent-shell-rewind-tests--with-viewport
    (let ((shell-buffer (current-buffer))
          (viewport (agent-shell-viewport--buffer :shell-buffer (current-buffer))))
      (with-current-buffer viewport
        (insert "two more")
        (setq agent-shell-viewport--edit-past-position (copy-marker 1)))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'agent-shell--context) #'ignore))
        (agent-shell-viewport--show-buffer :shell-buffer shell-buffer
                                           :override "something else"))
      (with-current-buffer viewport
        (should (equal (buffer-string) "something else"))
        (should (eq (agent-shell-viewport--compose-route
                     :pending (lambda (&rest _) 'pending)
                     :past (lambda () (error "Sent as the edit"))
                     :send #'ignore)
                    t))))))

(ert-deftest agent-shell-rewind-compose-cancel-clears-the-edit-test ()
  "Cancelling the compose buffer drops the edit with the draft."
  (agent-shell-rewind-tests--with-viewport
    (let ((shell-buffer (current-buffer))
          (viewport (agent-shell-viewport--buffer :shell-buffer (current-buffer))))
      (with-current-buffer viewport
        (insert "two more")
        (setq agent-shell-viewport--edit-past-position (copy-marker 1))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t))
                  ((symbol-function 'agent-shell-viewport--shell-buffer)
                   (lambda (&rest _) shell-buffer))
                  ((symbol-function 'shell-maker-history-position) #'ignore)
                  ((symbol-function 'bury-buffer) #'ignore))
          (agent-shell-viewport-compose-cancel))
        (should-not agent-shell-viewport--edit-past-position)))))

(ert-deftest agent-shell-rewind-queued-edit-replaces-a-past-edit-test ()
  "Editing a queued prompt over a past prompt's edit drops the past edit."
  (agent-shell-rewind-tests--with-viewport
    (let ((viewport (agent-shell-viewport--buffer :shell-buffer (current-buffer))))
      (with-current-buffer viewport
        (setq agent-shell-viewport--edit-past-position (copy-marker 1)))
      (agent-shell-viewport--prefill-edit :shell-buffer (current-buffer)
                                          :index 0 :text "queued")
      (with-current-buffer viewport
        (should (= agent-shell-viewport--edit-pending-index 0))
        (should-not agent-shell-viewport--edit-past-position)))))

(ert-deftest agent-shell-rewind-compose-header-shows-the-edit-test ()
  "The compose header says the draft edits an earlier prompt."
  (agent-shell-rewind-tests--with-shell
    (let ((shell-buffer (current-buffer)))
      (with-temp-buffer
        (setq major-mode 'agent-shell-viewport-edit-mode)
        (cl-letf (((symbol-function 'agent-shell-viewport--busy-p) #'ignore)
                  ((symbol-function 'agent-shell-viewport--position-label) #'ignore))
          (let ((plain (agent-shell-viewport--header-inputs shell-buffer)))
            (should-not (string-match-p
                         "[Ee]arlier"
                         (map-elt (agent-shell-viewport--make-header-model shell-buffer)
                                  :status)))
            (setq agent-shell-viewport--edit-past-position (copy-marker 1))
            (should-not (equal (agent-shell-viewport--header-inputs shell-buffer) plain))
            (should (string-match-p
                     "[Ee]arlier"
                     (map-elt (agent-shell-viewport--make-header-model shell-buffer)
                              :status)))))))))

(ert-deftest agent-shell-rewind-fork-request-carries-the-cursor-test ()
  "A fork at a cursor sends `cursorId' and no AIR fork point."
  (let ((request (agent-shell--make-session-fork-request
                  :session-id "s1" :cwd "/tmp" :cursor-id "before:u2")))
    (should (equal (map-elt request :method) "session/fork"))
    (should (equal (map-nested-elt request '(:params cursorId)) "before:u2"))
    (should-not (map-nested-elt request '(:params _meta jetbrains air fork)))))

(provide 'agent-shell-rewind-tests)
;;; agent-shell-rewind-tests.el ends here
