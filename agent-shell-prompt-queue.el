;;; agent-shell-prompt-queue.el --- Prompt queueing for agent-shell. -*- lexical-binding: t; -*-

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
;; Queue prompts while an `agent-shell' is busy and manage the pending
;; queue (submit, view, resume, remove).
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(require 'map)
(require 'ring)
(require 'agent-shell-faces)
(eval-when-compile (require 'cl-lib))

(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--make-button "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--echo "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--inject-or-queue "agent-shell-inject")
(declare-function agent-shell--inject-strict "agent-shell-inject")
(declare-function agent-shell--inject-available-p "agent-shell-inject")
(declare-function agent-shell-prompt-inject-queued "agent-shell-inject")
(declare-function agent-shell-viewport--prefill-edit "agent-shell-viewport")
(declare-function shell-maker-busy "shell-maker")

(defvar agent-shell--state)
(defvar agent-shell-prefer-viewport-interaction)
(defvar comint-input-ring)

(defcustom agent-shell-prompt-while-busy 'inject
  "What sending a prompt does while the agent is working on a turn.

`inject'  Prefer delivering the prompt to the turn already running, so
          the agent reads it at its next stopping point and changes course
          without losing the work already done.  Agents that cannot take
          it, and shells waiting on a permission answer, queue instead.
`queue'   Hold the prompt until the turn ends, then send it as a new
          turn.

Only `agent-shell-prompt-send' and the compose buffer's send key consult
this.  Generic send queues when preferred injection is unavailable.
`agent-shell-prompt-queue' queues only while work is in progress, and
`agent-shell-prompt-inject' errors when a running turn cannot be steered."
  :type '(choice (const :tag "Deliver to the running turn" inject)
                 (const :tag "Queue until the turn ends" queue))
  :group 'agent-shell)

;; The queueing commands were renamed to the `agent-shell-prompt-queue'
;; namespace.  A package upgrade reloads this file into a running session
;; (see `package--reload-previously-loaded'), which redefines the new
;; names but leaves the old ones bound to stale definitions.  Unbind them
;; so they no longer show up in `M-x' or run outdated code.
;; TODO: Remove after 2026-08-28.
(dolist (command '(agent-shell-queue-request
                   agent-shell-resume-pending-requests
                   agent-shell-remove-pending-request))
  (fmakunbound command))

;; These injection commands were renamed into the `agent-shell-prompt'
;; namespace for the same reason.
;; TODO: Remove after 2026-09-22.
(dolist (command '(agent-shell-inject-prompt
                   agent-shell-prompt-queue-inject))
  (fmakunbound command))

(defun agent-shell--prompt-queue-migrate ()
  "Bring prompt queue state in a live shell up to date.

Preserve queued prompts in shells created before `:pending-requests' was
renamed, and add `:prompt-queue-paused' to shells created before prompt
injection could pause automatic delivery.

TODO: Remove only the `:pending-requests' migration after 2026-08-28."
  (when (and (assq :pending-requests agent-shell--state)
             (not (assq :pending-prompts agent-shell--state)))
    (nconc agent-shell--state
           (list (cons :pending-prompts
                       (map-elt agent-shell--state :pending-requests)))))
  (unless (assq :prompt-queue-paused agent-shell--state)
    (nconc agent-shell--state (list (cons :prompt-queue-paused nil)))))

(cl-defun agent-shell--prompt-queue-process-next ()
  "Process the next pending prompt from the queue if available."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (agent-shell--prompt-queue-migrate)
  (unless (map-elt agent-shell--state :prompt-queue-paused)
    (when-let* ((pending (map-elt agent-shell--state :pending-prompts))
                (next-prompt (car pending)))
      (map-put! agent-shell--state :pending-prompts (cdr pending))
      (agent-shell--insert-to-shell-buffer
       :text next-prompt
       :submit t
       :no-focus t))))

(defun agent-shell--prompt-queue-paused-p ()
  "Return non-nil when automatic prompt delivery is paused in this shell."
  (agent-shell--prompt-queue-migrate)
  (map-elt agent-shell--state :prompt-queue-paused))

(defun agent-shell--prompt-queue-actions ()
  "Return the queue action buttons, to be clicked or invoked with RET.

Each action's key is bound in a keymap shared by all the buttons, so any
of them accepts every key (as the permission dialog does).

For example, in a terminal frame:

  \"[ Edit (e) ] [ Resume (r) ] [ Remove (d) ]\""
  (let* ((actions '(((:label . "Edit")
                     (:char . "e")
                     (:description . "edit a pending prompt")
                     (:command . agent-shell-prompt-queue-edit))
                    ((:label . "Inject")
                     (:char . "i")
                     (:description . "inject a pending prompt into the running turn")
                     (:command . agent-shell-prompt-inject-queued))
                    ((:label . "Resume")
                     (:char . "r")
                     (:description . "resume pending prompts")
                     (:command . agent-shell-prompt-queue-resume))
                    ((:label . "Remove")
                     (:char . "d")
                     (:description . "remove pending prompts")
                     (:command . agent-shell-prompt-queue-remove))))
         (keymap (let ((map (make-sparse-keymap)))
                   (dolist (action actions)
                     (define-key map (kbd (map-elt action :char))
                                 (map-elt action :command)))
                   map)))
    (mapconcat
     (lambda (action)
       (agent-shell--make-button
        :text (format "%s (%s)" (map-elt action :label) (map-elt action :char))
        :help (format "Press RET or %s to %s (M-x %s)"
                      (map-elt action :char)
                      (map-elt action :description)
                      (map-elt action :command))
        :kind 'prompt-queue
        :keymap keymap
        :action (map-elt action :command)))
     actions
     " ")))

(defun agent-shell--prompt-queue-display ()
  "Display pending prompts in the shell buffer if queue is not empty."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (agent-shell--prompt-queue-migrate)
  (unless (seq-empty-p (map-elt agent-shell--state :pending-prompts))
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id (format "%s-pending-prompts"
                       (map-elt (agent-shell--state) :request-count))
     :body (format "Pending prompts: %d

%s

  %s
"
                   (seq-length (map-elt agent-shell--state :pending-prompts))
                   (mapconcat
                    (lambda (idx-prompt)
                      (let ((idx (cdr idx-prompt))
                            (first-line (car (split-string (car idx-prompt) "\n" t))))
                        (format "  %d: \"%s\""
                                (1+ idx)
                                (truncate-string-to-width first-line 80 nil nil "..."))))
                    (seq-map-indexed #'cons (map-elt agent-shell--state :pending-prompts))
                    "\n")
                   (agent-shell--prompt-queue-actions))
     :create-new t)))

(cl-defun agent-shell--prompt-queue-echo (&key active-prompt pending-prompts)
  "Message the in-progress prompt and PENDING-PROMPTS to the echo area.

ACTIVE-PROMPT is the prompt currently running, or nil if none.

PENDING-PROMPTS is a list of pending prompt strings, in the same form as
the :pending-prompts entry in variable `agent-shell--state'.

Each prompt is shown on a single line, prefixed by a status column
\(\"active\" or \"queued\"), and truncated to fit the frame width so it
never wraps.

For example, given:

  :active-prompt \"Find that nasty bug\"
  :pending-prompts (\"Next prompt text\" \"Second prompt\")

messages:

  active  Find that nasty bug
  queued  Next prompt text
  queued  Second prompt"
  (if (and (not active-prompt) (seq-empty-p pending-prompts))
      (agent-shell--echo "No pending prompts")
    (let ((available (- (frame-width) 8)))
      (agent-shell--echo
       "%s"
       (mapconcat
        (lambda (row)
          (concat
           (propertize (string-pad (map-elt row :status) 6)
                       'face (map-elt row :face))
           "  "
           (truncate-string-to-width
            (or (car (split-string (map-elt row :prompt) "\n" t)) "")
            available nil nil t)))
        (append
         (when active-prompt
           (list `((:status . "active")
                   (:face . success)
                   (:prompt . ,active-prompt))))
         (seq-map (lambda (prompt)
                    `((:status . "queued")
                      (:face . agent-shell-secondary)
                      (:prompt . ,prompt)))
                  pending-prompts))
        "\n")))))

(cl-defun agent-shell--prompt-queue-enqueue (&key prompt)
  "Add PROMPT to the pending prompts queue and echo the resulting queue.

The running prompt (the most recent `comint-input-ring' entry) is shown
as \"active\" and the queued prompts, PROMPT included, as \"queued\"."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (agent-shell--prompt-queue-migrate)
  (map-put! agent-shell--state :pending-prompts
            (append (map-elt agent-shell--state :pending-prompts)
                    (list prompt)))
  (agent-shell--prompt-queue-echo
   :active-prompt (when (and (bound-and-true-p comint-input-ring)
                             (not (ring-empty-p comint-input-ring)))
                    (ring-ref comint-input-ring 0))
   :pending-prompts (map-elt agent-shell--state :pending-prompts)))

(defvar agent-shell-prompt-queue-setup-minibuffer-functions nil
  "Abnormal hook run while reading a queued prompt from the minibuffer.

Each function is called with a single alist containing:

  :shell-buffer - the shell the prompt is bound for

and runs with the minibuffer current, so it can decorate or extend the
prompt the way that shell renders its own.  The shell is carried rather
than looked up: it is resolved for the project, so it need not be the
buffer the minibuffer was entered from.")

(cl-defun agent-shell--prompt-queue-read (&key initial)
  "Read a queue prompt from the minibuffer.

When INITIAL is non-nil, prefill the minibuffer with it and leave
point at the end (ready to type below the prefill).

While reading, @ completes project files and / completes available
agent commands when the agent has reported them."
  (let ((shell-buffer (current-buffer)))
    (minibuffer-with-setup-hook
        (lambda ()
          (run-hook-with-args 'agent-shell-prompt-queue-setup-minibuffer-functions
                              `((:shell-buffer . ,shell-buffer)))
          (when initial
            (insert initial)))
      (read-string (or (map-nested-elt (agent-shell--state) '(:agent-config :shell-prompt))
                       "Enqueue prompt: ")))))

(cl-defun agent-shell--prompt-send (&key prompt disposition on-delivered)
  "Send PROMPT to the shell in the current buffer.

DISPOSITION decides what happens while a turn is running: `inject'
delivers PROMPT to that turn, `queue' holds it until the turn ends, and
nil follows `agent-shell-prompt-while-busy'.  An explicit DISPOSITION is
how an entry point keeps its own promise regardless of the setting.
Explicit `inject' errors when injection is unavailable and never falls
back after an agent declines it; a nil DISPOSITION with an `inject'
preference remains forgiving and queues.

Injection is tried first when `agent-shell--inject-available-p' says the
prompt can really reach a running turn.

Otherwise PROMPT queues while a turn is running, and also while automatic
delivery is paused: a paused queue means an untracked turn is still going
even though the shell looks idle (see
`agent-shell--prompt-queue-paused-p'), and submitting into that fires a
prompt at a working agent.  Pausing does not block injection, though,
since it exists to hold back automatic delivery into an untracked turn
rather than to stop the user steering a tracked one.

With nothing running and nothing paused, PROMPT is submitted.  Queueing
then would leave it pending until `agent-shell-prompt-queue-resume'.

ON-DELIVERED (lambda ()) is called once the prompt reached the agent, so
a caller holding its own copy (an edited pending prompt) can drop it.
Queueing does not call it: the queue now owns that copy."
  (agent-shell--prompt-queue-migrate)
  (let ((effective-disposition
         (or disposition agent-shell-prompt-while-busy)))
    (cond
     ((eq disposition 'inject)
      (agent-shell--inject-strict :prompt prompt :on-delivered on-delivered))
     ((and (eq effective-disposition 'inject)
           (agent-shell--inject-available-p))
      (agent-shell--inject-or-queue :prompt prompt :on-delivered on-delivered))
     ((or (shell-maker-busy) (agent-shell--prompt-queue-paused-p))
      (agent-shell--prompt-queue-enqueue :prompt prompt))
     (t
      (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)
      (when on-delivered
        (funcall on-delivered))))))

(defun agent-shell-prompt-send (prompt)
  "Send PROMPT, injecting or queueing it when a turn is already running.

Read PROMPT from the minibuffer and act on the current project's shell,
resolving it via `agent-shell--shell-buffer' so this works even when
invoked outside a shell buffer.

`agent-shell-prompt-while-busy' decides which it is.  Use
`agent-shell-prompt-queue' or `agent-shell-prompt-inject' to pick one
outright for a single prompt.

While reading, @ completes project files and / completes available agent
commands when the agent has reported them."
  (interactive
   (list (with-current-buffer (agent-shell--shell-buffer :no-create t)
           (agent-shell--prompt-queue-read))))
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-send :prompt prompt)))

(defun agent-shell-prompt-queue (prompt)
  "Queue or immediately send a prompt depending on shell busy state.

Read PROMPT from the minibuffer and act on the current project's shell,
resolving it via `agent-shell--shell-buffer' so this works even when
invoked outside a shell buffer.  If the shell is busy or automatic queue
delivery is paused, add PROMPT to the pending prompts queue.  Otherwise,
submit it immediately.  Queued prompts are automatically sent when the
current prompt completes or the queue resumes.

While reading, @ completes project files and / completes available agent
commands when the agent has reported them."
  (interactive
   (list (with-current-buffer (agent-shell--shell-buffer :no-create t)
           (agent-shell--prompt-queue-read))))
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-queue-migrate)
    (if (or (shell-maker-busy)
            (agent-shell--prompt-queue-paused-p))
        (agent-shell--prompt-queue-enqueue :prompt prompt)
      (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t))))

(defun agent-shell-prompt-queue-resume ()
  "Resume processing pending prompts in the queue.

Acts on the current project's shell, resolving it via
`agent-shell--shell-buffer' so this works even when invoked outside a
shell buffer."
  (interactive)
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-queue-migrate)
    (when (seq-empty-p (map-elt agent-shell--state :pending-prompts))
      (user-error "No pending prompts"))
    (when (eq (map-elt agent-shell--state :prompt-queue-paused) 'steering)
      (user-error "Prompt injection is still pending"))
    (map-put! agent-shell--state :prompt-queue-paused nil)
    (if (shell-maker-busy)
        (message "Shell is busy, prompts will auto-resume when ready")
      (agent-shell--prompt-queue-process-next))))

(defun agent-shell--prompt-queue-choices ()
  "Return an alist of (LABEL . INDEX) for the pending prompts.

LABEL is the prompt's 1-based position and a truncated copy of its text;
INDEX is the 0-based position in `:pending-prompts'.  Returns nil when the
queue is empty.

For example, with pending prompts \"first\" and \"second\":

  ((\"1: first\" . 0) (\"2: second\" . 1))"
  (seq-map-indexed
   (lambda (prompt idx)
     (cons (format "%d: %s" (1+ idx)
                   (truncate-string-to-width prompt 60 nil nil "..."))
           idx))
   (map-elt agent-shell--state :pending-prompts)))

(cl-defun agent-shell--prompt-queue-replace (&key index prompt expected)
  "Replace the pending prompt at INDEX with PROMPT, preserving its position.

When PROMPT is nil or only whitespace, the prompt is left unchanged (no
error).  When INDEX is out of range (e.g. the queue drained while editing),
nothing is changed.  When EXPECTED is non-nil, also leave the queue unchanged
unless the prompt at INDEX still equals EXPECTED.  Otherwise PROMPT is spliced
in at INDEX."
  (agent-shell--prompt-queue-migrate)
  (let ((pending (map-elt agent-shell--state :pending-prompts)))
    (cond
     ((or (null prompt) (string-empty-p (string-trim prompt)))
      (message "Pending prompt unchanged"))
     ((or (null index) (< index 0) (>= index (length pending)))
      (message "Prompt no longer pending"))
     ((and expected (not (equal (nth index pending) expected)))
      (message "Prompt queue changed; edit not applied"))
     (t
      (let ((new-pending (append (seq-take pending index)
                                 (list prompt)
                                 (seq-drop pending (1+ index)))))
        (map-put! agent-shell--state :pending-prompts new-pending)
        (message "Prompt updated (%d pending)" (length new-pending)))))))

(cl-defun agent-shell--prompt-queue-remove-at (&key index expected)
  "Remove the pending prompt at INDEX, leaving the rest of the queue in order.

When INDEX is out of range (e.g. the queue drained meanwhile), nothing is
changed.  When EXPECTED is non-nil, also leave the queue unchanged unless
the prompt at INDEX still equals EXPECTED, so a queue that shifted under
us doesn't lose the wrong prompt.

For example, with pending prompts \"first\" and \"second\":

  (agent-shell--prompt-queue-remove-at :index 0 :expected \"first\")

leaves (\"second\")."
  (agent-shell--prompt-queue-migrate)
  (let ((pending (map-elt agent-shell--state :pending-prompts)))
    (cond
     ((or (null index) (< index 0) (>= index (length pending)))
      (message "Prompt no longer pending"))
     ((and expected (not (equal (nth index pending) expected)))
      (message "Prompt queue changed; prompt left pending"))
     (t
      (map-put! agent-shell--state :pending-prompts
                (append (seq-take pending index)
                        (seq-drop pending (1+ index))))
      (message "Prompt sent (%d pending)"
               (length (map-elt agent-shell--state :pending-prompts)))))))

(cl-defun agent-shell--prompt-queue-take-at (&key index expected)
  "Remove and return the pending prompt at INDEX when it equals EXPECTED.

Return nil and leave the queue unchanged when INDEX is invalid or its
prompt no longer equals EXPECTED.  Unlike
`agent-shell--prompt-queue-remove-at', do not announce delivery: callers
use this to claim a prompt while asynchronous delivery is unresolved."
  (agent-shell--prompt-queue-migrate)
  (let* ((pending (map-elt agent-shell--state :pending-prompts))
         (prompt (and index (>= index 0) (nth index pending))))
    (when (and prompt (or (null expected) (equal prompt expected)))
      (map-put! agent-shell--state :pending-prompts
                (append (seq-take pending index)
                        (seq-drop pending (1+ index))))
      prompt)))

(cl-defun agent-shell--prompt-queue-insert-at (&key index prompt)
  "Insert PROMPT into the pending queue at INDEX and return PROMPT.

Clamp INDEX to the current queue bounds.  This preserves the selected
prompt's relative position when steering declines after other queue edits."
  (agent-shell--prompt-queue-migrate)
  (let* ((pending (map-elt agent-shell--state :pending-prompts))
         (position (min (max (or index 0) 0) (length pending))))
    (map-put! agent-shell--state :pending-prompts
              (append (seq-take pending position)
                      (list prompt)
                      (seq-drop pending position)))
    prompt))

(defun agent-shell-prompt-queue-edit (index)
  "Edit the pending prompt at INDEX, replacing it in place.

Acts on the current project's shell, resolving it via
`agent-shell--shell-buffer' so this works even when invoked outside a
shell buffer.  When called interactively, prompt to choose a pending
prompt (or use the only one when there is just one).  The current prompt
text is prefilled for editing: in a viewport-edit buffer when
`agent-shell-prefer-viewport-interaction' is non-nil, otherwise in the
minibuffer.  Submitting empty text leaves the prompt unchanged.

While editing, @ completes project files and / completes available agent
commands when the agent has reported them."
  (interactive
   (with-current-buffer (agent-shell--shell-buffer :no-create t)
     (agent-shell--prompt-queue-migrate)
     (let ((choices (agent-shell--prompt-queue-choices)))
       (when (seq-empty-p choices)
         (user-error "No pending prompts"))
       (list (if (cdr choices)
                 (cdr (assoc (completing-read "Edit: " choices nil t) choices))
               (cdar choices))))))
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-queue-migrate)
    (let ((current (nth index (map-elt agent-shell--state :pending-prompts))))
      (if agent-shell-prefer-viewport-interaction
          (agent-shell--display-buffer
           (agent-shell-viewport--prefill-edit
            :shell-buffer (current-buffer) :index index :text current))
        (agent-shell--prompt-queue-replace
         :index index
         :expected current
         :prompt (agent-shell--prompt-queue-read :initial current))))))

(defun agent-shell-prompt-queue-remove (&optional remove-index)
  "Remove all pending prompts or a specific prompt by REMOVE-INDEX.

Acts on the current project's shell, resolving it via
`agent-shell--shell-buffer' so this works even when invoked outside a
shell buffer.  When called interactively with pending prompts, prompt to
either remove all or select a specific prompt to remove."
  (interactive
   (with-current-buffer (agent-shell--shell-buffer :no-create t)
     (agent-shell--prompt-queue-migrate)
     (when (seq-empty-p (map-elt agent-shell--state :pending-prompts))
       (user-error "No pending prompts"))
     (let* ((choices (append '(("Remove all" . remove-all))
                             (agent-shell--prompt-queue-choices)))
            (selection (cdr (assoc (completing-read "Remove: " choices nil t) choices))))
       (list (unless (eq selection 'remove-all) selection)))))
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (if remove-index
        (when (y-or-n-p (format "Remove \"%s\"?"
                                (nth remove-index
                                     (map-elt agent-shell--state :pending-prompts))))
          (let* ((pending (map-elt agent-shell--state :pending-prompts))
                 (new-pending (append (seq-take pending remove-index)
                                      (seq-drop pending (1+ remove-index)))))
            (map-put! agent-shell--state :pending-prompts new-pending)
            (message "Removed (%d remaining)"
                     (length new-pending))))
      (when (y-or-n-p (format "Remove %d pending prompts?"
                              (length (map-elt agent-shell--state :pending-prompts))))
        (map-put! agent-shell--state :pending-prompts nil)
        (message "Removed all pending prompts")))))

(provide 'agent-shell-prompt-queue)

;;; agent-shell-prompt-queue.el ends here
