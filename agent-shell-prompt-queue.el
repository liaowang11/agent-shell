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
(declare-function agent-shell--ensure-state-key "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-steering-supported-p "agent-shell")
(declare-function agent-shell-experimental--send-steering "agent-shell-experimental")
(declare-function agent-shell-completion--setup-minibuffer "agent-shell-completion")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell-viewport--prefill-edit "agent-shell-viewport")
(declare-function shell-maker-busy "shell-maker")

(defvar agent-shell--state)
(defvar agent-shell-prefer-viewport-interaction)
(defvar comint-input-ring)

(defcustom agent-shell-prompt-while-busy 'steer
  "What `agent-shell-prompt-send' does while the agent is working on a turn.

`steer'  Hand the prompt to the turn already running, so the agent reads
         it at its next stopping point instead of after the turn ends.
         Agents that cannot steer, agents that decline, and shells
         waiting on a permission answer queue instead.
`queue'  Hold the prompt until the turn ends, then send it as a new turn.

Steering is not additive.  An agent that interrupts what it is generating
may drop the instruction it was working on, so a steer can replace what
the agent was already doing rather than adding to it.  Set this to
`queue' if that trade is the wrong one for how you work.

Only `agent-shell-prompt-send', its DWIM variant, and the compose
buffer's send key read this.  `agent-shell-prompt-queue' always queues
and `agent-shell-prompt-steer' always steers, so a binding on either
never depends on this setting."
  :type '(choice (const :tag "Steer the running turn" steer)
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

;; Steering was called injection here before upstream settled on the name.
;; Same reload problem: the old names stay bound to stale definitions.
;; TODO: Remove after 2026-09-29.
(dolist (command '(agent-shell-prompt-inject
                   agent-shell-prompt-inject-dwim
                   agent-shell-prompt-inject-queued
                   agent-shell-viewport-compose-inject))
  (fmakunbound command))

(defun agent-shell--prompt-queue-migrate ()
  "Bring prompt queue state in a live shell up to date.

Preserves queued prompts in shells created before `:pending-requests' was
renamed to `:pending-prompts', and adds `:prompt-queue-paused' to shells
created before steering could pause automatic delivery.  A package
upgrade reloads this file into a running session, and `map-put!' fails on
an absent alist key as it does for any other.

TODO: Remove only the `:pending-requests' migration after 2026-08-28."
  (when (and (assq :pending-requests agent-shell--state)
             (not (assq :pending-prompts agent-shell--state)))
    (nconc agent-shell--state
           (list (cons :pending-prompts
                       (map-elt agent-shell--state :pending-requests)))))
  (agent-shell--ensure-state-key agent-shell--state :prompt-queue-paused))

(cl-defun agent-shell--prompt-queue-process-next ()
  "Process the next pending prompt from the queue if available.

Does nothing while delivery is paused (see
`agent-shell--prompt-queue-paused-p'), because a pause means work is
still running even though the shell does not show as busy."
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
  "Return non-nil when automatic prompt delivery is paused in this shell.

Returns `steering' while a steer request is unresolved, and
`detached-turn' once one came back `startedNewTurn' and left a turn
running that no `session/prompt' owns.  Either way a turn is in progress
while `shell-maker-busy' reports nothing, so sending now would fire a
prompt at a working agent."
  (agent-shell--prompt-queue-migrate)
  (map-elt agent-shell--state :prompt-queue-paused))

(defun agent-shell--prompt-queue-actions ()
  "Return the queue action buttons, to be clicked or invoked with RET.

Each action's key is bound in a keymap shared by all the buttons, so any
of them accepts every key (as the permission dialog does).

For example, in a terminal frame:

  \"[ Edit (e) ] [ Steer (s) ] [ Resume (r) ] [ Remove (d) ]\""
  (let* ((actions '(((:label . "Edit")
                     (:char . "e")
                     (:description . "edit a pending prompt")
                     (:command . agent-shell-prompt-queue-edit))
                    ((:label . "Steer")
                     (:char . "s")
                     (:description . "steer a pending prompt into the running turn")
                     (:command . agent-shell-prompt-queue-steer))
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

(defun agent-shell--prompt-submittable-p ()
  "Return non-nil when a prompt can be submitted as a new turn right now.

Both a `shell-maker' turn and a paused queue mean work is in progress.
The second is the one that is easy to miss: a steer answered with
`startedNewTurn' leaves a turn running that no `session/prompt' owns, so
the shell looks idle while the agent works."
  (and (not (shell-maker-busy))
       (not (agent-shell--prompt-queue-paused-p))))

(defun agent-shell--prompt-steerable-p ()
  "Return non-nil when a prompt would really reach the turn now running.

Needs a turn in flight (there is nothing to steer otherwise) and an agent
that advertised the capability at `initialize'.

Requiring status `busy' also rules out a `blocked' shell, and that is
deliberate: a blocked shell waits on a permission answer, and no
implementation defines what an agent does with a prompt steered in while
a tool sits on that question.

This is the forgiving predicate, used where an unsteerable shell has
somewhere else to put the prompt.  `agent-shell--prompt-steer' asks the
user about a blocked shell instead of refusing outright."
  (and (shell-maker-busy)
       (eq (agent-shell-status) 'busy)
       (agent-shell-steering-supported-p)
       t))

(defun agent-shell--prompt-steer-confirmed-p ()
  "Return non-nil unless steering a blocked shell is declined.

A shell waiting on a permission answer has no defined behaviour for a
prompt steered in, and a declined steer interrupts the turn, which
rejects that permission with it.  Only a blocked shell asks; every other
state answers yes without a question."
  (or (not (eq (agent-shell-status) 'blocked))
      (y-or-n-p
       "Shell is pending user action (Steering may cancel work).  Steer anyway?")))

(cl-defun agent-shell--prompt-steer (&key prompt on-delivered on-declined confirmed)
  "Steer PROMPT into the running turn, without falling back to the queue.

Submits PROMPT as a new turn when nothing is running.  Signals a
`user-error' when a turn is running that cannot be steered, and asks
first when the shell is blocked on a permission answer: a declined steer
interrupts the turn, which rejects that permission along with it.

ON-DELIVERED (lambda (outcome)) runs once the agent took PROMPT.
ON-DECLINED (lambda (reason outcome)) replaces the report-and-interrupt
`agent-shell-experimental--send-steering' does by default.  With neither,
a declined steer is reported in the shell and the turn interrupted.

Nothing here queues.  A steer turned into a queued prompt would reach the
agent long after the user asked for it."
  (agent-shell--prompt-queue-migrate)
  (if (agent-shell--prompt-submittable-p)
      (let ((submitted (agent-shell--insert-to-shell-buffer
                        :text prompt :submit t :no-focus t)))
        (when on-delivered
          (funcall on-delivered 'submitted))
        submitted)
    (unless (agent-shell-steering-supported-p)
      (user-error "This agent does not support steering"))
    (unless (or confirmed (agent-shell--prompt-steer-confirmed-p))
      (user-error "Steering cancelled"))
    ;; Steering is the one path that never reaches shell-maker, which would
    ;; otherwise absorb a blank prompt by reprinting its own.
    (when (string-empty-p (string-trim prompt))
      (user-error "No prompt to steer"))
    (agent-shell-experimental--send-steering
     :state (agent-shell--state)
     :prompt prompt
     :on-delivered on-delivered
     :on-declined on-declined)))

(cl-defun agent-shell--prompt-steer-or-queue (&key prompt on-delivered)
  "Steer PROMPT into the running turn, queueing it when the agent will not.

Only call with `agent-shell--prompt-steerable-p' non-nil.  Unlike
`agent-shell--prompt-steer', a declined steer neither interrupts the turn
nor loses the prompt: it goes on the queue and is sent when the turn
ends.  That is the trade a plain send makes, where the user asked to send
a prompt rather than to steer with one.

ON-DELIVERED (lambda (outcome)) runs once the agent took PROMPT."
  (agent-shell-experimental--send-steering
   :state (agent-shell--state)
   :prompt prompt
   :on-delivered on-delivered
   :on-declined
   (lambda (reason _outcome)
     (agent-shell--prompt-queue-enqueue :prompt prompt)
     (agent-shell--echo "Steer declined (%s): prompt queued"
                        (or reason "no outcome"))
     ;; The tracked turn can end while the steer is in flight, where the
     ;; hook that drains the queue on turn completion saw the pause and did
     ;; nothing.  Nothing else is coming to drain it, so without this the
     ;; prompt waits for a manual `agent-shell-prompt-queue-resume'.
     (unless (shell-maker-busy)
       (agent-shell--prompt-queue-process-next)))))

(cl-defun agent-shell--prompt-send (&key prompt disposition on-delivered)
  "Send PROMPT to the shell in the current buffer.

DISPOSITION decides what happens while a turn is running: `steer' hands
PROMPT to that turn, `queue' holds it until the turn ends, and nil
follows `agent-shell-prompt-while-busy'.  An explicit DISPOSITION is how
an entry point keeps its own promise whatever the setting says.

The two differ in what happens when steering is not on offer.  Explicit
`steer' errors, and never falls back once the agent has declined, because
the user asked to steer and silently doing something else is worse than
saying so.  A nil DISPOSITION stays forgiving and queues, because the
user asked to send.

With nothing running and nothing paused, PROMPT is submitted.  Queueing
then would leave it pending until `agent-shell-prompt-queue-resume'.

ON-DELIVERED (lambda (outcome)) is called once PROMPT reached the agent,
so a caller holding its own copy can drop it.  Queueing does not call it:
the queue owns that copy now."
  (agent-shell--prompt-queue-migrate)
  (cond
   ((eq disposition 'steer)
    (agent-shell--prompt-steer :prompt prompt :on-delivered on-delivered))
   ((and (not (eq disposition 'queue))
         (eq agent-shell-prompt-while-busy 'steer)
         (agent-shell--prompt-steerable-p))
    (agent-shell--prompt-steer-or-queue :prompt prompt :on-delivered on-delivered))
   ((not (agent-shell--prompt-submittable-p))
    (agent-shell--prompt-queue-enqueue :prompt prompt))
   (t
    (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)
    (when on-delivered
      (funcall on-delivered 'submitted)))))

(defun agent-shell-prompt-steer (&optional prompt)
  "Steer PROMPT into the turn the agent is currently running.

Unlike `agent-shell-prompt-queue', the prompt reaches the agent while
it's already working on a submitted prompt, so it can change course
instead of finishing first.  Signals a `user-error' when a turn is running
and the agent cannot steer -- use `agent-shell-prompt-queue' for that one.

With no turn running there is nothing to steer into, so PROMPT is simply
submitted and starts the next turn.

Steering is not additive: an agent that interrupts what it is generating
may drop the instruction it was working on, so what the agent was already
doing can be lost.  Whether that happens is the agent's choice, not ours.

When the agent declines the steer, the running turn is interrupted rather
than left to carry on in a direction you believe you already corrected.

Steering a shell awaiting a permission answer asks for confirmation
first: no implementation defines what an agent does with a prompt steered
in while a tool sits on that question, and a declined steer interrupts
the turn, which rejects that permission along with it.

While reading, @ completes project files and / completes available agent
commands when the agent has reported them."
  (interactive)
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    ;; Both guards run before the prompt is asked for, so refusing costs no
    ;; typing.  The turn can end while the prompt is written, since the
    ;; message pump drains on a timer and timers run from the minibuffer's
    ;; own wait for input -- hence `confirmed' below rather than trusting
    ;; the state to still be blocked by the time the send checks.
    (let ((confirmed (when (shell-maker-busy)
                       (unless (agent-shell-steering-supported-p)
                         (user-error "This agent does not support steering"))
                       (unless (agent-shell--prompt-steer-confirmed-p)
                         (user-error "Steering cancelled"))
                       t)))
      (setq prompt (or prompt (agent-shell--prompt-queue-read)))
      ;; A blank prompt is refused on either path.  Steering bypasses
      ;; shell-maker, which would otherwise absorb it by reprinting its
      ;; own prompt, and submitting one starts nothing at all.
      (when (string-empty-p (string-trim prompt))
        (user-error "No prompt given"))
      (agent-shell--prompt-steer
       :prompt prompt
       :confirmed confirmed))))

(defun agent-shell-prompt-send (prompt)
  "Send PROMPT, steering or queueing it when a turn is already running.

Read PROMPT from the minibuffer and act on the current project's shell,
resolving it via `agent-shell--shell-buffer' so this works even when
invoked outside a shell buffer.

`agent-shell-prompt-while-busy' decides which it is.  Use
`agent-shell-prompt-queue' or `agent-shell-prompt-steer' to pick one
outright for a single prompt.

Nothing is lost when steering is not on offer: an agent that never
advertised it, an agent that declines, and a shell waiting on a
permission answer all get the prompt queued instead.

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
invoked outside a shell buffer.  If the shell is busy, or automatic
delivery is paused, add PROMPT to the pending prompts queue.  Otherwise,
submit it immediately.  Queued prompts are automatically sent when the
current prompt completes or the queue resumes.

To hand PROMPT to the agent mid-turn instead of waiting, see
`agent-shell-prompt-steer'.

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
    ;; A steer still in flight settles the pause itself, and clearing it
    ;; here would race that answer into an untracked turn.
    (when (eq (map-elt agent-shell--state :prompt-queue-paused) 'steering)
      (user-error "Steer still pending"))
    (map-put! agent-shell--state :prompt-queue-paused nil)
    (if (shell-maker-busy)
        (message "Shell is busy, prompts will auto-resume when ready")
      (agent-shell--prompt-queue-process-next))))

(defun agent-shell--prompt-queue-read-index (prompt)
  "Read the index of a pending prompt, asking with PROMPT.

Skips the question when only one prompt is pending, and signals a
`user-error' when none is.  Call from an `interactive' form; resolves the
shell itself so the caller need not."
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-queue-migrate)
    (let ((choices (agent-shell--prompt-queue-choices)))
      (when (seq-empty-p choices)
        (user-error "No pending prompts"))
      (if (cdr choices)
          (cdr (assoc (completing-read prompt choices nil t) choices))
        (cdar choices)))))

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
us does not lose the wrong prompt.

For example, with pending prompts \"first\" and \"second\":

  (agent-shell--prompt-queue-remove-at :index 0 :expected \"first\")

leaves (\"second\")."
  (agent-shell--prompt-queue-migrate)
  ;; Read before the claim below mutates the queue, so the failure
  ;; branches can still tell a drained queue from a shifted one.
  (let ((in-range (when-let* ((pending (map-elt agent-shell--state :pending-prompts)))
                    (and index (>= index 0) (< index (length pending))))))
    (cond
     ;; One splice, shared with `agent-shell--prompt-queue-take-at': this
     ;; differs only in announcing the outcome.
     ((agent-shell--prompt-queue-take-at :index index :expected expected)
      (message "Prompt sent (%d pending)"
               (length (map-elt agent-shell--state :pending-prompts))))
     (in-range
      (message "Prompt queue changed; prompt left pending"))
     (t
      (message "Prompt no longer pending")))))

(cl-defun agent-shell--prompt-queue-take-at (&key index expected)
  "Remove and return the pending prompt at INDEX when it equals EXPECTED.

Return nil and leave the queue unchanged when INDEX is invalid or its
prompt no longer equals EXPECTED.  Unlike
`agent-shell--prompt-queue-remove-at', announce nothing: callers use this
to claim a prompt while delivery is still unresolved, and it may yet go
back."
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

Clamps INDEX to the current queue bounds, which preserves the prompt's
relative position when a steer is declined after other queue edits."
  (agent-shell--prompt-queue-migrate)
  (let* ((pending (map-elt agent-shell--state :pending-prompts))
         (position (min (max (or index 0) 0) (length pending))))
    (map-put! agent-shell--state :pending-prompts
              (append (seq-take pending position)
                      (list prompt)
                      (seq-drop pending position)))
    prompt))

(cl-defun agent-shell--prompt-queue-steer-at (&key index prompt expected
                                                   on-delivered on-declined)
  "Steer the pending prompt at INDEX into the running turn.

PROMPT is the text to send, which may be an edit of the queued one.
EXPECTED is the text currently stored at INDEX; the queue is left alone
when it no longer matches.

Claims EXPECTED out of the queue before sending, so a turn completing
meanwhile cannot also submit it, and puts PROMPT back at the same
position when the agent declines or the request fails.  The prompt is
therefore neither lost nor run twice.  Calls ON-DELIVERED or ON-DECLINED
after that queue ownership change, never before."
  (agent-shell--prompt-queue-migrate)
  (let ((current (nth index (map-elt agent-shell--state :pending-prompts))))
    (unless (and current (equal current expected))
      (user-error "Prompt no longer pending"))
    (cond
     ((agent-shell--prompt-submittable-p)
      (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)
      (agent-shell--prompt-queue-remove-at :index index :expected expected)
      (when on-delivered
        (funcall on-delivered 'submitted)))
     ((not (agent-shell--prompt-steerable-p))
      (user-error "Prompt cannot be steered into the current turn"))
     (t
      (unless (agent-shell--prompt-queue-take-at :index index :expected expected)
        (user-error "Prompt no longer pending"))
      (condition-case err
          (agent-shell--prompt-steer
           :prompt prompt
           :on-delivered
           (lambda (outcome)
             (message "Prompt sent (%d pending)"
                      (length (map-elt agent-shell--state :pending-prompts)))
             (when on-delivered
               (funcall on-delivered outcome)))
           :on-declined
           (lambda (reason outcome)
             (agent-shell--prompt-queue-insert-at :index index :prompt prompt)
             (agent-shell--echo "Steer declined (%s): prompt stays queued"
                                (or reason "no outcome"))
             (when on-declined
               (funcall on-declined reason outcome))))
        (error
         (agent-shell--prompt-queue-insert-at :index index :prompt prompt)
         (signal (car err) (cdr err))))))))

(defun agent-shell-prompt-queue-steer (index)
  "Steer the pending prompt at INDEX into the turn already running.

Acts on the current project's shell, resolving it via
`agent-shell--shell-buffer' so this works even when invoked outside a
shell buffer.  When called interactively, prompt to choose a pending
prompt (or use the only one when there is just one).

The prompt is claimed from the queue while delivery is unresolved, so
normal turn completion cannot also submit it.  A steer the agent took
consumes it; decline or failure restores it at the same position.  With
no turn running the prompt is submitted instead.  Signals a `user-error'
and leaves the queue unchanged when a running turn cannot be steered."
  (interactive
   (list (agent-shell--prompt-queue-read-index "Steer: ")))
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-queue-migrate)
    (let ((prompt (nth index (map-elt agent-shell--state :pending-prompts))))
      (unless prompt
        (user-error "Prompt no longer pending"))
      (agent-shell--prompt-queue-steer-at
       :index index :prompt prompt :expected prompt))))

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
   (list (agent-shell--prompt-queue-read-index "Edit: ")))
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
