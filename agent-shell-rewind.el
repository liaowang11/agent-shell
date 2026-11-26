;;; agent-shell-rewind.el --- Edit an earlier prompt in agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; Edit a prompt submitted earlier and run the edited text in its place.
;;
;; Session cursors (ACP's unstable session cursors RFD) name positions in
;; a session that a method can act at.  claude-agent-acp announces one
;; before each user prompt in a `cursor_update', to a client that declared
;; `clientCapabilities.session.cursors'.  The cursor always accepts
;; `session/fork', and also accepts claude-agent-acp's `_session/rewind'
;; when the client named AIR's "sessionRewind" and the conversation can
;; still be cut there.  Its id is opaque: it is stored as announced, on
;; the prompt it precedes, and never built here.
;;
;; `agent-shell-edit-prompt-at-point' opens the prompt in the viewport
;; compose buffer, marked as an edit of it.  Sending rewinds the same
;; session to before the prompt and submits the edited text there.
;; Without a rewindable cursor it forks instead: at the cursor when it
;; accepts `session/fork', else at the agent message before the prompt
;; (the AIR fork point), and submits the edited text in the new shell.
;; A rewind does not revert file edits.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'subr-x)

(defvar agent-shell--state)
(defvar agent-shell--transcript-file)
(defvar shell-maker--config)
(defvar comint-prompt-regexp)
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--current-shell "agent-shell")
(declare-function agent-shell--ensure-state-key "agent-shell")
(declare-function agent-shell--send-request "agent-shell")
(declare-function agent-shell--make-error-handler "agent-shell")
(declare-function agent-shell--acp-error-text "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--fork-shell-buffer "agent-shell")
(declare-function agent-shell--message-id-at-point "agent-shell")
(declare-function agent-shell--flush-stream-render "agent-shell")
(declare-function agent-shell--subagent-row "agent-shell")
(declare-function agent-shell--steered-prompt-p "agent-shell")
(declare-function agent-shell--turn-spans "agent-shell")
(declare-function agent-shell--turn-text "agent-shell")
(declare-function agent-shell--turn-begin-position "agent-shell")
(declare-function agent-shell--append-transcript "agent-shell")
(declare-function agent-shell-restart "agent-shell")
(declare-function agent-shell-reload "agent-shell")
(declare-function agent-shell--live-prompt-start "agent-shell-prompt")
(declare-function agent-shell--prompt-face-p "agent-shell-prompt")
(declare-function agent-shell--prompt-submittable-p "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-migrate "agent-shell-prompt-queue")
(declare-function agent-shell-viewport--position "agent-shell-viewport")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--prefill-past-edit "agent-shell-viewport")
(declare-function agent-shell-viewport--recenter "agent-shell-viewport")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function shell-maker-prompt-regexp "shell-maker")

;;;; Cursors

(defun agent-shell--cursor-capabilities (state cursor-id)
  "Return the capabilities STATE's cursor CURSOR-ID last announced.

An alist keyed by method, values decoded from empty JSON objects:

  (agent-shell--cursor-capabilities state \"before:u2\")
  ;; => ((fork) (rewind))

Nil for an unknown cursor, or one whose capabilities were cleared."
  (map-nested-elt (map-elt state :cursors) (list cursor-id :capabilities)))

(defun agent-shell--latest-prompt-position ()
  "Return where the newest submitted prompt begins, or nil.

Searches back from the live input prompt, which is never the answer:
with a persistent prompt it sits below the turn the cursor belongs to.
Only text carrying the prompt face counts, so a tool result echoing the
prompt string is skipped."
  (let ((regexp (if shell-maker--config
                    (shell-maker-prompt-regexp shell-maker--config)
                  comint-prompt-regexp))
        (limit (or (condition-case nil
                       (agent-shell--live-prompt-start)
                     (error nil))
                   (point-max)))
        found)
    (unless (string-match-p regexp "")
      (save-excursion
        (save-restriction
          (narrow-to-region (point-min) limit)
          (goto-char (point-max))
          (while (and (not found)
                      (re-search-backward regexp nil t))
            (when (agent-shell--prompt-face-p
                   (get-text-property (match-beginning 0) 'font-lock-face))
              (setq found (match-beginning 0)))))))
    found))

(defun agent-shell--replayed-cursor-p (update)
  "Return non-nil when cursor UPDATE comes from a history replay.

A replay sends each cursor right after its user message, anchored
before it.  A live cursor is anchored after the agent entity before the
prompt, since claude-agent-acp does not report the prompt the client
sent."
  (and (equal (map-nested-elt update '(anchor type)) "user_message")
       (equal (map-nested-elt update '(anchor position)) "before")))

(defun agent-shell--forget-cursor (state cursor-id)
  "Drop STATE's cursor CURSOR-ID and its mark on the prompt."
  (when-let* ((entry (assoc cursor-id (map-elt state :cursors))))
    (when-let* ((marker (map-elt (cdr entry) :position))
                (buffer (marker-buffer marker)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (remove-text-properties marker (1+ marker) '(agent-shell-cursor-id nil)))))
    (map-put! state :cursors (seq-remove (lambda (cursor)
                                           (equal (car cursor) cursor-id))
                                         (map-elt state :cursors)))))

(defun agent-shell--attach-cursor (state cursor-id update)
  "Record a new cursor CURSOR-ID from UPDATE on the newest prompt in STATE.

A replayed cursor always takes the prompt just rendered.  A live one
only takes a prompt that has none yet, and never a steered prompt: only
a prompt that opens a turn is editable."
  (with-current-buffer (map-elt state :buffer)
    (agent-shell--flush-stream-render state)
    (when-let* ((position (agent-shell--latest-prompt-position))
                ((not (agent-shell--steered-prompt-p position)))
                (previous (or (get-text-property position 'agent-shell-cursor-id) t))
                ((or (eq previous t)
                     (agent-shell--replayed-cursor-p update))))
      (unless (eq previous t)
        (agent-shell--forget-cursor state previous))
      (let ((inhibit-read-only t))
        (put-text-property position (1+ position) 'agent-shell-cursor-id cursor-id))
      (map-put! state :cursors
                (cons (cons cursor-id
                            (list (cons :capabilities (map-elt update 'capabilities))
                                  (cons :position (copy-marker position))))
                      (map-elt state :cursors))))))

(defun agent-shell--handle-cursor-update (state update)
  "Track `cursor_update' UPDATE in STATE.

A known cursor's capabilities are replaced when UPDATE carries them,
cleared when they are null, and kept when the key is absent.  A new one
is attached to the newest prompt (see `agent-shell--attach-cursor').  A
subagent's cursors are not on the shell's timeline and are ignored.

Renders nothing."
  (agent-shell--ensure-state-key state :cursors)
  (when-let* (((not (agent-shell--subagent-row state)))
              (cursor-id (map-elt update 'cursorId)))
    (if-let* ((entry (assoc cursor-id (map-elt state :cursors))))
        (when (assq 'capabilities update)
          (map-put! (cdr entry) :capabilities (map-elt update 'capabilities)))
      (agent-shell--attach-cursor state cursor-id update))))

;;;; Editing

(defun agent-shell--edit-prompt-way (position &optional no-rewind)
  "Return how the current shell can edit the prompt at POSITION, or nil.

An alist with `:kind', `rewind' or `fork', and the `:cursor-id' or
`:message-id' to act at.  Prefers a rewind, then a fork at the prompt's
cursor, then a fork up to the agent message before the prompt.  With
NO-REWIND, only a fork counts.

  (agent-shell--edit-prompt-way 1200)
  ;; => ((:kind . rewind) (:cursor-id . \"before:u2\"))"
  (let* ((state (agent-shell--state))
         (cursor-id (get-text-property position 'agent-shell-cursor-id))
         (capabilities (and cursor-id (agent-shell--cursor-capabilities state cursor-id)))
         (message-id (and (> position (point-min))
                          (save-excursion
                            (goto-char (1- position))
                            (agent-shell--message-id-at-point)))))
    (cond
     ((and (not no-rewind)
           (map-elt state :supports-session-rewind)
           (assq 'rewind capabilities))
      (list (cons :kind 'rewind) (cons :cursor-id cursor-id)))
     ((and (map-elt state :supports-session-fork)
           (assq 'fork capabilities))
      (list (cons :kind 'fork) (cons :cursor-id cursor-id)))
     ((and (map-elt state :supports-session-fork)
           (map-elt state :supports-fork-point)
           message-id)
      (list (cons :kind 'fork) (cons :message-id message-id))))))

(cl-defun agent-shell--edit-prompt-fork (&key shell-buffer way prompt from-viewport on-done)
  "Fork SHELL-BUFFER's session as WAY says and submit PROMPT in the fork.
FROM-VIEWPORT shows the fork in the viewport.  Calls ON-DONE after."
  (let ((new-shell (agent-shell--fork-shell-buffer
                    :shell-buffer shell-buffer
                    :cursor-id (map-elt way :cursor-id)
                    :message-id (map-elt way :message-id)
                    :from-viewport from-viewport)))
    ;; Waits for the fork's session, so it lands as its first prompt.
    (agent-shell--insert-to-shell-buffer
     :shell-buffer new-shell :text prompt :submit t :no-focus t)
    (when on-done
      (funcall on-done))))

(defun agent-shell--rewind-refusal-text (reason)
  "Return why a rewind refused with REASON, for the echo area.

  (agent-shell--rewind-refusal-text \"busy\")
  ;; => \"The agent is busy\""
  (pcase reason
    ("busy" "The agent is busy")
    ("backgroundWork" "A background task or subagent is running")
    ("beforeCompaction" "The prompt is before the latest compaction")
    ("clearedContext" "The session's context was cleared")
    ("providerRefused" "Claude Code refused the cut")
    ("persistFailed" "The rewind point could not be saved")
    (_ (format "Refused (%s)" (or reason "no reason")))))

(cl-defun agent-shell--rewind-sync (&key shell-buffer prompt from-viewport)
  "Bring SHELL-BUFFER in line with its rewound session, then submit PROMPT.

Reloads the session in a fresh shell, which replays only the history
before the cut, and queues PROMPT as its first prompt.  FROM-VIEWPORT
reloads from the shell's viewport, so the new shell shows there.  The
transcript is append-only, so it keeps the dropped turns and records
the rewind.

Returns the new shell, or nil when the shell could not reload and PROMPT
was not sent."
  (let ((session-id (map-nested-elt (buffer-local-value 'agent-shell--state shell-buffer)
                                    '(:session :id)))
        (new-shell nil))
    (with-current-buffer shell-buffer
      (agent-shell--append-transcript
       :text (format "## Rewound (%s)\n\n" (format-time-string "%F %T"))
       :file-path agent-shell--transcript-file))
    ;; `agent-shell-restart' shows the new shell where it is called from.
    (with-current-buffer (or (and from-viewport
                                  (agent-shell-viewport--buffer :shell-buffer shell-buffer
                                                                :existing-only t))
                             shell-buffer)
      (condition-case err
          (setq new-shell (agent-shell-restart :session-id session-id))
        (user-error
         (message "Rewound, but this shell could not reload (%s).  \
It shows dropped turns: run M-x agent-shell-reload"
                  (error-message-string err)))))
    (when new-shell
      (agent-shell--insert-to-shell-buffer
       :shell-buffer new-shell :text prompt :submit t :no-focus t)
      new-shell)))

(cl-defun agent-shell--offer-fork (&key shell-buffer position prompt from-viewport on-done question)
  "Ask QUESTION and, on yes, fork SHELL-BUFFER before the prompt at POSITION.
PROMPT, FROM-VIEWPORT and ON-DONE are as in `agent-shell--edit-prompt-fork'.
Return non-nil when it forked."
  (when-let* ((way (with-current-buffer shell-buffer
                     (agent-shell--edit-prompt-way position t)))
              ((y-or-n-p question)))
    (agent-shell--edit-prompt-fork
     :shell-buffer shell-buffer :way way :prompt prompt
     :from-viewport from-viewport :on-done on-done)
    t))

(cl-defun agent-shell--edit-prompt-rewind (&key shell-buffer position cursor-id prompt
                                                from-viewport on-done)
  "Rewind SHELL-BUFFER's session to CURSOR-ID and submit PROMPT there.

POSITION is where the edited prompt begins.  Refuses while a turn runs
or prompts are queued, and asks first, since file edits made by the
dropped turns stay.  ON-DONE is called once PROMPT is on its way; when
the rewind does not happen, it is not, so the draft stays."
  (let ((state (agent-shell--state))
        (args (list :shell-buffer shell-buffer :position position :prompt prompt
                    :from-viewport from-viewport :on-done on-done)))
    (unless (agent-shell--prompt-submittable-p)
      (user-error "Interrupt the running turn first"))
    (agent-shell--prompt-queue-migrate)
    (when (map-elt state :pending-prompts)
      (user-error "Clear the prompt queue first: queued prompts would run after the rewind"))
    (unless (y-or-n-p "Rewind drops this prompt and every turn after it.  \
File edits those turns made stay in place.  Rewind? ")
      (user-error "Cancelled"))
    (agent-shell--send-request
     :state state
     :client (map-elt state :client)
     :request `((:method . "_session/rewind")
                (:params . ((sessionId . ,(map-nested-elt state '(:session :id)))
                            (cursorId . ,cursor-id))))
     :buffer shell-buffer
     :on-success
     (lambda (response)
       (if (eq (map-elt response 'rewound) t)
           ;; The draft goes only once the reloaded shell has the prompt.
           (when (and (agent-shell--rewind-sync :shell-buffer shell-buffer :prompt prompt
                                                :from-viewport from-viewport)
                      on-done)
             (funcall on-done))
         (let* ((reason (map-elt response 'reason))
                (text (agent-shell--rewind-refusal-text reason)))
           (unless (and (member reason '("beforeCompaction" "clearedContext"))
                        (apply #'agent-shell--offer-fork
                               :question (format "%s, so it cannot be rewound.  Fork from it instead? "
                                                 text)
                               args))
             (message "Not rewound: %s.  The edit is kept" text)))))
     :on-failure
     (lambda (acp-error raw-message)
       (pcase (map-elt acp-error 'code)
         (-32601
          (map-put! state :supports-session-rewind nil)
          (unless (apply #'agent-shell--offer-fork
                         :question "This agent cannot rewind.  Fork from this prompt instead? "
                         args)
            (message "This agent cannot rewind.  The edit is kept")))
         (-32602
          (agent-shell--forget-cursor state cursor-id)
          (message "This prompt can no longer be rewound.  The edit is kept"))
         ((or -32603 -32002)
          (message "The rewind failed and the session ended: %s"
                   (agent-shell--acp-error-text acp-error))
          (when (y-or-n-p "The session ended.  Reload it? ")
            (with-current-buffer shell-buffer
              (agent-shell-reload))))
         (_
          (funcall (agent-shell--make-error-handler :state state :shell-buffer shell-buffer)
                   acp-error raw-message)))))))

(cl-defun agent-shell--edit-prompt-submit (&key shell-buffer position prompt from-viewport on-done)
  "Run PROMPT in place of SHELL-BUFFER's prompt beginning at POSITION.

Rewinds the session when the prompt's cursor allows it, else forks
before the prompt (see `agent-shell--edit-prompt-way').  FROM-VIEWPORT
shows a fork in the viewport.  ON-DONE is called once PROMPT is on its
way, and not when the edit is refused, so the caller can keep its draft."
  (when (string-empty-p (string-trim prompt))
    (user-error "Nothing to send"))
  (with-current-buffer shell-buffer
    (let ((way (or (agent-shell--edit-prompt-way position)
                   (user-error "This agent cannot edit an earlier prompt"))))
      (if (eq (map-elt way :kind) 'rewind)
          (agent-shell--edit-prompt-rewind
           :shell-buffer shell-buffer :position position
           :cursor-id (map-elt way :cursor-id) :prompt prompt
           :from-viewport from-viewport :on-done on-done)
        (agent-shell--edit-prompt-fork
         :shell-buffer shell-buffer :way way :prompt prompt
         :from-viewport from-viewport :on-done on-done)))))

;;;; Command

(defun agent-shell--turn-span-for-edit (shell-buffer from-viewport)
  "Return the turn span (see `agent-shell--turn-spans') to edit in SHELL-BUFFER.
FROM-VIEWPORT takes the page the viewport shows, else the turn at point."
  (let ((index (when from-viewport
                 (map-elt (agent-shell-viewport--position) :current)))
        (begin (unless from-viewport
                 (agent-shell--turn-begin-position))))
    (with-current-buffer shell-buffer
      (let ((spans (agent-shell--turn-spans)))
        (if index
            (nth (1- index) spans)
          (seq-find (lambda (span) (eql (map-elt span :position) begin))
                    spans))))))

;;;###autoload
(defun agent-shell-edit-prompt-at-point ()
  "Edit the submitted prompt at point and run the edited text in its place.

In the viewport, edits the prompt of the page shown.  The prompt opens
in the viewport compose buffer, where @ and / complete as usual and the
header says the draft edits an earlier prompt.  Sending it with
\\<agent-shell-viewport-edit-mode-map>\\[agent-shell-viewport-compose-send] \
drops the prompt and every turn after it from the session, then submits
the edited text: the same session is rewound when the agent allows it
\(claude-agent-acp's `_session/rewind'), and the shell reloads to show
the shortened conversation.  Otherwise the session is forked before the
prompt and the edited text runs in the new shell, leaving this one as it
was.  A rewind does not undo file edits the dropped turns made.  The
edit cannot be queued or steered, and cancelling the draft drops it.

Only a prompt that started a turn can be edited, not one steered into a
running turn."
  (declare (modes agent-shell-mode agent-shell-viewport-view-mode))
  (interactive)
  (let* ((from-viewport (and (derived-mode-p 'agent-shell-viewport-view-mode) t))
         (shell-buffer (or (agent-shell--current-shell)
                           (user-error "Not in a shell or viewport buffer")))
         (span (or (agent-shell--turn-span-for-edit shell-buffer from-viewport)
                   (user-error "No submitted prompt at point")))
         (position (map-elt span :position))
         (text (with-current-buffer shell-buffer
                 (unless (agent-shell--edit-prompt-way position)
                   (user-error "This agent cannot edit an earlier prompt"))
                 (string-trim (substring-no-properties
                               (or (map-elt (agent-shell--turn-text span) :prompt) "")))))
         (viewport-buffer (agent-shell-viewport--prefill-past-edit
                           :shell-buffer shell-buffer :position position :text text)))
    (agent-shell--display-buffer viewport-buffer)
    (with-current-buffer viewport-buffer
      (agent-shell-viewport--recenter))))

(provide 'agent-shell-rewind)
;;; agent-shell-rewind.el ends here
