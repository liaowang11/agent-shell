;;; agent-shell-prompt-suggestion-tests.el --- Tests for agent-shell-prompt-suggestion -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'cl-lib)
(require 'agent-shell)
(require 'agent-shell-prompt-suggestion)
(require 'agent-shell-viewport)

;;; Code:

;; The wire shape is claude-agent-acp's at v0.93.0: src/prompt-suggestions.ts
;; and docs/air-extensions.md "Prompt suggestions".  acp.el decodes JSON
;; arrays as vectors (acp.el:1142, `json-parse-string' defaults).

(defun agent-shell-prompt-suggestion-tests--update (suggestions)
  "Return a `session_info_update' body carrying SUGGESTIONS, a vector."
  `((_meta (promptSuggestions (suggestions . ,suggestions)))))

(defun agent-shell-prompt-suggestion-tests--session-info (state update)
  "Drive a `session_info_update' carrying UPDATE through STATE."
  (cl-letf (((symbol-function 'agent-shell--update-header-and-mode-line) #'ignore)
            ((symbol-function 'agent-shell--emit-event) #'ignore)
            ((symbol-function 'agent-shell--update-transcript-title) #'ignore))
    (with-current-buffer (map-elt state :buffer)
      (agent-shell--on-notification
       :state state
       :acp-notification
       `((method . "session/update")
         (params (sessionId . "s1")
                 (update (sessionUpdate . "session_info_update") ,@update)))))))

(defmacro agent-shell-prompt-suggestion-tests--with-shell (&rest body)
  "Run BODY in a fake shell buffer ending in a live, empty `Claude> ' prompt.
`state' is bound to the shell's state."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (setq major-mode 'agent-shell-mode)
     (let* ((state (agent-shell--make-state :buffer (current-buffer)))
            (agent-shell--state state)
            (agent-shell-prompt-suggestions t))
       (setq-local agent-shell--state state)
       (setq-local comint-input-ring (make-ring 10))
       (setq-local comint-input-ring-index nil)
       (map-put! (map-elt state :session) :id "s1")
       (insert "Earlier answer\n")
       (let ((start (point-marker)))
         (insert "Claude> ")
         (setq-local comint-last-prompt (cons start (point-marker))))
       ,@body)))

(defun agent-shell-prompt-suggestion-tests--ghost ()
  "Return the gray text shown in the current buffer, or nil."
  (when-let* ((overlay (seq-find (lambda (overlay)
                                   (overlay-get overlay 'agent-shell-prompt-suggestion))
                                 (overlays-in (point-min) (point-max)))))
    (substring-no-properties (overlay-get overlay 'before-string))))

(defun agent-shell-prompt-suggestion-tests--display-strings (pos)
  "Return the overlay strings drawn at POS, in the order Emacs draws them.
Follows `compare_overlay_entries' in xdisp.c: after-strings of overlays
ending at POS come first, by decreasing priority, then before-strings of
overlays starting there, by increasing priority."
  (let (after before)
    (dolist (overlay (overlays-in pos pos))
      (let ((priority (or (overlay-get overlay 'priority) 0)))
        (when (and (= (overlay-end overlay) pos) (overlay-get overlay 'after-string))
          (push (cons priority (overlay-get overlay 'after-string)) after))
        (when (and (= (overlay-start overlay) pos) (overlay-get overlay 'before-string))
          (push (cons priority (overlay-get overlay 'before-string)) before))))
    (mapcar (lambda (entry) (substring-no-properties (cdr entry)))
            (append (sort after (lambda (a b) (> (car a) (car b))))
                    (sort before (lambda (a b) (< (car a) (car b))))))))

(defun agent-shell-prompt-suggestion-tests--input ()
  "Return the text typed at the fake shell's prompt."
  (buffer-substring-no-properties (agent-shell--prompt-input-start) (point-max)))

;;; Reading the update

(ert-deftest agent-shell-prompt-suggestion-from-acp-test ()
  "The first non-blank suggestion is kept, trimmed; none means nil."
  (should (equal (agent-shell--prompt-suggestion-from-acp
                  '((suggestions . [((text . "  Run the tests\n"))])))
                 "Run the tests"))
  (should (equal (agent-shell--prompt-suggestion-from-acp
                  '((suggestions . [((text . " ")) ((text . "Commit it"))])))
                 "Commit it"))
  (should-not (agent-shell--prompt-suggestion-from-acp '((suggestions . []))))
  (should-not (agent-shell--prompt-suggestion-from-acp '((suggestions . [((text . "  "))]))))
  (should-not (agent-shell--prompt-suggestion-from-acp '((suggestions . [((text))]))))
  (should-not (agent-shell--prompt-suggestion-from-acp nil)))

(ert-deftest agent-shell-prompt-suggestion-session-info-stores-and-clears-test ()
  "A suggestion is stored, `[]' clears it, an update without the key leaves it."
  (agent-shell-prompt-suggestion-tests--with-shell
    (agent-shell-prompt-suggestion-tests--session-info
     state (agent-shell-prompt-suggestion-tests--update [((text . "Run the tests"))]))
    (should (equal (map-elt state :prompt-suggestion) "Run the tests"))
    ;; Title-only and goal-only updates say nothing about the suggestion.
    (agent-shell-prompt-suggestion-tests--session-info state '((title . "Retitled")))
    (agent-shell-prompt-suggestion-tests--session-info
     state '((_meta (jetbrains (air (version . 1) (goal))))))
    (should (equal (map-elt state :prompt-suggestion) "Run the tests"))
    (agent-shell-prompt-suggestion-tests--session-info
     state (agent-shell-prompt-suggestion-tests--update []))
    (should-not (map-elt state :prompt-suggestion))))

(ert-deftest agent-shell-prompt-suggestion-works-on-state-without-key-test ()
  "A shell state built before `:prompt-suggestion' existed still takes one."
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let* ((state (list (cons :buffer (current-buffer))
                        (cons :session (list (cons :id "s1")))
                        (cons :last-entry-type nil)
                        (cons :last-activity-time nil)))
           (agent-shell--state state))
      (agent-shell-prompt-suggestion-tests--session-info
       state (agent-shell-prompt-suggestion-tests--update [((text . "Go on"))]))
      (should (equal (map-elt state :prompt-suggestion) "Go on")))))

;;; Gray text in the shell

(ert-deftest agent-shell-prompt-suggestion-ghost-shows-at-empty-prompt-test ()
  "The suggestion's first line shows in gray at an empty live prompt, before point."
  (agent-shell-prompt-suggestion-tests--with-shell
    (agent-shell--set-prompt-suggestion state "Run the tests\nthen commit")
    (should (equal (agent-shell-prompt-suggestion-tests--ghost) "Run the tests"))
    (let ((overlay (seq-find (lambda (o) (overlay-get o 'agent-shell-prompt-suggestion))
                             (overlays-in (point-min) (point-max)))))
      (should (= (overlay-start overlay) (point-max)))
      (should (get-text-property 0 'cursor (overlay-get overlay 'before-string)))
      (should (eq (get-text-property 0 'face (overlay-get overlay 'before-string))
                  'agent-shell-prompt-suggestion)))
    ;; Gray text is not buffer text.
    (should (equal (agent-shell-prompt-suggestion-tests--input) ""))))

(ert-deftest agent-shell-prompt-suggestion-ghost-hides-while-typing-test ()
  "Typing hides the gray text; deleting back to empty shows it again."
  (agent-shell-prompt-suggestion-tests--with-shell
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (goto-char (point-max))
    (insert "x")
    (should-not (agent-shell-prompt-suggestion-tests--ghost))
    (delete-char -1)
    (should (equal (agent-shell-prompt-suggestion-tests--ghost) "Run the tests"))))

(ert-deftest agent-shell-prompt-suggestion-ghost-needs-live-prompt-test ()
  "No gray text while the prompt is stale, such as when output streams below it."
  (agent-shell-prompt-suggestion-tests--with-shell
    (goto-char (point-max))
    (insert (propertize "streaming" 'field 'output))
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (should-not (agent-shell-prompt-suggestion-tests--ghost))))

(ert-deftest agent-shell-prompt-suggestion-ghost-follows-setting-test ()
  "`history' and nil draw nothing; clearing the suggestion removes the gray text."
  (agent-shell-prompt-suggestion-tests--with-shell
    (let ((agent-shell-prompt-suggestions 'history))
      (agent-shell--set-prompt-suggestion state "Run the tests")
      (should-not (agent-shell-prompt-suggestion-tests--ghost)))
    (let ((agent-shell-prompt-suggestions nil))
      (agent-shell--set-prompt-suggestion state "Run the tests")
      (should-not (agent-shell-prompt-suggestion-tests--ghost)))
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (should (agent-shell-prompt-suggestion-tests--ghost))
    (agent-shell--set-prompt-suggestion state nil)
    (should-not (agent-shell-prompt-suggestion-tests--ghost))))

(ert-deftest agent-shell-prompt-suggestion-ghost-redraws-after-prompt-test ()
  "A suggestion that arrived before the prompt shows once the prompt is printed."
  (agent-shell-prompt-suggestion-tests--with-shell
    (goto-char (point-max))
    (insert (propertize "answer" 'field 'output))
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (should-not (agent-shell-prompt-suggestion-tests--ghost))
    (let ((start (point-marker)))
      (insert "\nClaude> ")
      (setq-local comint-last-prompt (cons start (point-marker))))
    (run-hooks 'shell-maker-finish-output-hook)
    (should (equal (agent-shell-prompt-suggestion-tests--ghost) "Run the tests"))))

(ert-deftest agent-shell-prompt-suggestion-ghost-survives-chat-mode-test ()
  "Chat mode's relabeling keeps the gray text, and adds nothing beside it.
Chat mode hides the prompt behind overlays and indents the draft with an
`after-string' (`agent-shell-chat--draft-reindent'), at the same position
as the gray text when nothing is typed."
  (agent-shell-prompt-suggestion-tests--with-shell
    (map-put! state :agent-config '((:mode-line-name . "Claude")))
    (setq-local agent-shell-chat--labeled t)
    ;; A finished turn and a fresh prompt, fontified the way shell-maker
    ;; prints them, which is what chat mode finds turns by.
    (erase-buffer)
    (let ((prompt (propertize "Claude> " 'font-lock-face
                              '(comint-highlight-prompt comint-highlight-prompt))))
      (insert prompt "typed\n")
      (insert (propertize "<shell-maker-end-of-prompt>" 'invisible t 'shell-maker--marker t))
      (insert "reply\n\n")
      (let ((start (point-marker)))
        (insert prompt)
        (setq-local comint-last-prompt (cons start (point-marker)))))
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (agent-shell-chat--relabel)
    ;; Chat mode did label the prompt.
    (should (seq-find (lambda (overlay) (overlay-get overlay 'agent-shell-chat--tag))
                      (overlays-in (point-min) (point-max))))
    (should (equal (agent-shell-prompt-suggestion-tests--ghost) "Run the tests"))
    (dolist (overlay (overlays-in (point-max) (point-max)))
      (unless (overlay-get overlay 'agent-shell-prompt-suggestion)
        (should (string-empty-p (or (overlay-get overlay 'after-string) "")))))))

;;; M-n / M-p in the shell

(ert-deftest agent-shell-prompt-suggestion-next-input-inserts-suggestion-test ()
  "M-n at an empty prompt inserts the suggestion as input, without sending."
  (agent-shell-prompt-suggestion-tests--with-shell
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (cl-letf (((symbol-function 'shell-maker-submit)
               (lambda (&rest _) (error "Must not send"))))
      (agent-shell-next-input 1))
    (should (equal (agent-shell-prompt-suggestion-tests--input) "Run the tests"))
    (should (= (point) (point-max)))
    (should-not (agent-shell-prompt-suggestion-tests--ghost))
    (should-error (agent-shell-next-input 1) :type 'user-error)))

(ert-deftest agent-shell-prompt-suggestion-next-input-saves-draft-test ()
  "M-n over a draft replaces it; M-p from the suggestion restores it exactly."
  (agent-shell-prompt-suggestion-tests--with-shell
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (goto-char (point-max))
    (insert "half a thought")
    (agent-shell-next-input 1)
    (should (equal (agent-shell-prompt-suggestion-tests--input) "Run the tests"))
    ;; Edited suggestions go back to the draft too, as comint does with
    ;; edited history entries.
    (insert " now")
    (agent-shell-previous-input 1)
    (should (equal (agent-shell-prompt-suggestion-tests--input) "half a thought"))
    ;; Back at the draft, M-n offers the suggestion again.
    (agent-shell-next-input 1)
    (should (equal (agent-shell-prompt-suggestion-tests--input) "Run the tests"))))

(ert-deftest agent-shell-prompt-suggestion-next-input-falls-through-to-comint-test ()
  "Without a suggestion, or while browsing history, M-n and M-p are comint's."
  (agent-shell-prompt-suggestion-tests--with-shell
    (let ((calls nil))
      (cl-letf (((symbol-function 'comint-next-input)
                 (lambda (arg) (push (list 'next arg) calls)))
                ((symbol-function 'comint-previous-input)
                 (lambda (arg) (push (list 'previous arg) calls))))
        ;; No suggestion.
        (agent-shell-next-input 1)
        (agent-shell-previous-input 1)
        ;; A suggestion, but browsing history: M-n walks back down first.
        (agent-shell--set-prompt-suggestion state "Run the tests")
        (setq comint-input-ring-index 0)
        (agent-shell-next-input 1)
        (setq comint-input-ring-index nil)
        ;; A suggestion, not shown yet: M-p goes into history.
        (agent-shell-previous-input 2))
      (should (equal (reverse calls)
                     '((next 1) (previous 1) (next 1) (previous 2))))
      (should (equal (agent-shell-prompt-suggestion-tests--input) "")))))

(ert-deftest agent-shell-prompt-suggestion-next-input-ignores-off-setting-test ()
  "With the setting off, M-n is comint's even when a suggestion is stored."
  (agent-shell-prompt-suggestion-tests--with-shell
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (let ((agent-shell-prompt-suggestions nil)
          (called nil))
      (cl-letf (((symbol-function 'comint-next-input)
                 (lambda (_arg) (setq called t))))
        (agent-shell-next-input 1))
      (should called))
    ;; `history' still offers it on M-n.
    (let ((agent-shell-prompt-suggestions 'history))
      (agent-shell-next-input 1))
    (should (equal (agent-shell-prompt-suggestion-tests--input) "Run the tests"))))

(ert-deftest agent-shell-prompt-suggestion-spent-on-submit-test ()
  "Sending a prompt spends the suggestion and forgets the saved draft."
  (agent-shell-prompt-suggestion-tests--with-shell
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (agent-shell-next-input 1)
    (agent-shell--prompt-suggestion-spend state)
    (should-not (map-elt state :prompt-suggestion))
    (should-not agent-shell--prompt-suggestion-draft)
    (should-not (agent-shell-prompt-suggestion-tests--ghost))))

(ert-deftest agent-shell-prompt-suggestion-busy-submit-leaves-history-test ()
  "Steering or queueing a recalled history entry leaves history, as RET does.
`comint-send-input' resets `comint-input-ring-index' on an idle submit; a
busy one never calls it, and a stale index would have M-n walk history
instead of offering the suggestion that arrives after the turn."
  (agent-shell-prompt-suggestion-tests--with-shell
    (goto-char (point-max))
    (insert "recalled prompt")
    (setq comint-input-ring-index 1)
    (cl-letf (((symbol-function 'agent-shell--prompt-submittable-p) #'ignore)
              ((symbol-function 'agent-shell--ensure-session-not-archived) #'ignore)
              ((symbol-function 'agent-shell--busy-submit) #'ignore))
      (agent-shell-submit))
    (should-not comint-input-ring-index)
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (agent-shell-next-input 1)
    (should (equal (agent-shell-prompt-suggestion-tests--input) "Run the tests"))))

;;; Viewport compose buffer

(defmacro agent-shell-prompt-suggestion-tests--with-viewport (&rest body)
  "Run BODY in a compose buffer whose shell has a suggestion stored.
`shell-buffer' and `state' are bound."
  (declare (indent 0) (debug t))
  `(let ((shell-buffer (generate-new-buffer " *suggestion shell*"))
         (viewport-buffer (generate-new-buffer " *suggestion shell* [viewport]"))
         (agent-shell-prompt-suggestions t))
     (unwind-protect
         (let ((state nil))
           (with-current-buffer shell-buffer
             (setq major-mode 'agent-shell-mode)
             (setq state (agent-shell--make-state :buffer shell-buffer))
             (setq-local agent-shell--state state)
             (setq-local comint-input-ring (make-ring 10)))
           (with-current-buffer viewport-buffer
             (cl-letf (((symbol-function 'agent-shell-viewport--update-header) #'ignore)
                       ((symbol-function 'agent-shell-viewport--position) #'ignore)
                       ((symbol-function 'agent-shell-viewport--sync-layout-spacer) #'ignore)
                       ((symbol-function 'agent-shell-viewport--ensure-buffer) #'ignore)
                       ((symbol-function 'agent-shell-viewport--shell-buffer)
                        (lambda (&rest _) shell-buffer))
                       ((symbol-function 'agent-shell-viewport--buffer)
                        (lambda (&rest _) viewport-buffer)))
               (agent-shell-viewport-edit-mode)
               ,@body)))
       (kill-buffer viewport-buffer)
       (kill-buffer shell-buffer))))

(ert-deftest agent-shell-prompt-suggestion-viewport-ghost-test ()
  "An empty compose buffer shows the suggestion in gray; typing hides it."
  (agent-shell-prompt-suggestion-tests--with-viewport
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (should (equal (agent-shell-prompt-suggestion-tests--ghost) "Run the tests"))
    (insert "x")
    (should-not (agent-shell-prompt-suggestion-tests--ghost))
    (delete-char -1)
    (should (equal (agent-shell-prompt-suggestion-tests--ghost) "Run the tests"))
    (agent-shell--set-prompt-suggestion state nil)
    (should-not (agent-shell-prompt-suggestion-tests--ghost))))

(ert-deftest agent-shell-prompt-suggestion-viewport-ghost-below-spacer-test ()
  "With a text header, the gray text sits after the spacer's newline, beside point.
Drawn before it, the gray text would show a line above the cursor."
  (let ((sync-spacer (symbol-function 'agent-shell-viewport--sync-layout-spacer))
        (agent-shell-header-style 'text))
    (agent-shell-prompt-suggestion-tests--with-viewport
      (funcall sync-spacer)
      (agent-shell--set-prompt-suggestion state "Run the tests")
      (should (equal (agent-shell-prompt-suggestion-tests--display-strings (point-min))
                     '("\n" "Run the tests"))))))

(ert-deftest agent-shell-prompt-suggestion-viewport-next-history-test ()
  "M-n past the newest entry saves the draft and inserts the suggestion.
M-p from the suggestion restores the draft; M-n at the suggestion stops."
  (agent-shell-prompt-suggestion-tests--with-viewport
    (with-current-buffer shell-buffer
      (ring-insert comint-input-ring "older prompt"))
    (agent-shell--set-prompt-suggestion state "Run the tests")
    (insert "my draft")
    (goto-char 3)
    (agent-shell-viewport-next-history)
    (should (equal (buffer-string) "Run the tests"))
    (should (= (point) (point-max)))
    (should-error (agent-shell-viewport-next-history) :type 'user-error)
    (agent-shell-viewport-previous-history)
    (should (equal (buffer-string) "my draft"))
    (should (= (point) 3))
    ;; The next M-p goes into history, and M-n comes back through the draft.
    (agent-shell-viewport-previous-history)
    (should (equal (buffer-string) "older prompt"))
    (agent-shell-viewport-next-history)
    (should (equal (buffer-string) "my draft"))
    (agent-shell-viewport-next-history)
    (should (equal (buffer-string) "Run the tests"))))

(ert-deftest agent-shell-prompt-suggestion-viewport-next-history-without-suggestion-test ()
  "With no suggestion, M-n at the bottom still says there is no more history."
  (agent-shell-prompt-suggestion-tests--with-viewport
    (insert "my draft")
    (should-error (agent-shell-viewport-next-history) :type 'user-error)
    (should (equal (buffer-string) "my draft"))))

;;; Opting out

(ert-deftest agent-shell-prompt-suggestion-anthropic-opt-out-test ()
  "With the setting nil, the Claude config asks the agent not to predict."
  (let ((agent-shell-prompt-suggestions nil))
    (should (eq (map-nested-elt
                 (map-elt (agent-shell-anthropic-make-claude-code-config) :session-meta)
                 '(claudeCode options promptSuggestions))
                :false)))
  (dolist (setting '(t history))
    (let ((agent-shell-prompt-suggestions setting))
      (should-not (assq 'promptSuggestions
                        (map-nested-elt
                         (map-elt (agent-shell-anthropic-make-claude-code-config) :session-meta)
                         '(claudeCode options))))
      ;; The rest of the options stay.
      (should (map-nested-elt
               (map-elt (agent-shell-anthropic-make-claude-code-config) :session-meta)
               '(claudeCode options thinking))))))

(ert-deftest agent-shell-prompt-suggestion-anthropic-loads-the-setting-test ()
  "Loading the Claude config's file loads the setting it reads.
The config can be built before `agent-shell' is loaded."
  (let ((entry (seq-find (lambda (entry)
                           (and (stringp (car entry))
                                (string-match-p "/agent-shell-anthropic\\.elc?\\'"
                                                (car entry))))
                         load-history)))
    (should entry)
    (should (member '(require . agent-shell-prompt-suggestion) (cdr entry)))))

(provide 'agent-shell-prompt-suggestion-tests)

;;; agent-shell-prompt-suggestion-tests.el ends here
