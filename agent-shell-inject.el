;;; agent-shell-inject.el --- Mid-turn prompt injection for agent-shell. -*- lexical-binding: t; -*-

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
;; Deliver a prompt to the turn that is already running, instead of
;; queueing it until the turn ends (see `agent-shell-prompt-queue').
;;
;; The agent reads the prompt at its next stopping point and changes course
;; without losing the work already done, so a wrong direction can be
;; corrected while it is still wrong.
;;
;; This rides the `_session/steering' ACP extension request, which is not
;; part of the ACP schema.  Agents that support it say so in the
;; `initialize' response at the top-level `_meta.steering.supported' (a
;; sibling of `agentCapabilities').  Agents that don't (or turns that
;; already ended) fall back to queueing, so a prompt is never lost.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues

;;; Code:

(require 'map)
(require 'seq)
(require 'agent-shell-faces)
(eval-when-compile (require 'cl-lib))

(declare-function agent-shell--append-transcript "agent-shell")
(declare-function agent-shell--build-content-blocks "agent-shell")
(declare-function agent-shell--echo "agent-shell")
(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--expand-truncated-regions "agent-shell")
(declare-function agent-shell--indent-markdown-headers "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--prompt-queue-choices "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-enqueue "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-insert-at "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-migrate "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-process-next "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-read "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-remove-at "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-replace "agent-shell-prompt-queue")
(declare-function agent-shell--prompt-queue-take-at "agent-shell-prompt-queue")
(declare-function agent-shell--send-request "agent-shell")
(declare-function agent-shell--separate-transcript-after-agent-message "agent-shell")
(declare-function agent-shell--shell-buffer "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function agent-shell--update-text "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function shell-maker-busy "shell-maker")

(defvar agent-shell--state)
(defvar agent-shell--transcript-file)

(defconst agent-shell-inject--method "_session/steering"
  "ACP extension request that delivers a prompt into the running turn.

Not in the ACP schema.  The standards-track equivalent is `session/inject'
\(https://github.com/agentclientprotocol/agent-client-protocol/pull/1261),
still open, so the extension is the only thing usable today.")

(defconst agent-shell-inject--entry-type "injected_user_message"
  "`:last-entry-type' left after rendering an injected prompt.

This must differ from `user_message_chunk', which makes notification
dispatch insert another end-of-prompt marker on the next update.")

(defun agent-shell--inject-capability (acp-response)
  "Return non-nil when ACP-RESPONSE advertises prompt injection support.

ACP-RESPONSE is an `initialize' response.  The capability lives at the
top-level `_meta.steering.supported', a sibling of `agentCapabilities'
rather than a member of it.

For example, given:

  ((_meta . ((steering . ((supported . t))))))

returns t."
  (and (map-nested-elt acp-response '(_meta steering supported)) t))

(defun agent-shell--inject-outcome (acp-response)
  "Return ACP-RESPONSE's injection outcome as an internal symbol.

Returns `injected', `prompt-required', `started-new-turn', or `failed'.
Unknown and missing outcomes return `failed', so callers keep the prompt
instead of treating a new response shape as successful."
  (pcase (map-elt acp-response 'outcome)
    ("injected" 'injected)
    ("promptRequired" 'prompt-required)
    ("startedNewTurn" 'started-new-turn)
    (_ 'failed)))

(cl-defun agent-shell--inject-request (&key session-id prompt)
  "Return a `_session/steering' request for SESSION-ID carrying PROMPT.

PROMPT is a sequence of ACP content blocks, as built by
`agent-shell--build-content-blocks'.  It is normalized to a vector for
JSON array serialization.

`_meta.steering.idleBehavior' opts into the host-owned fallback: an agent
whose turn already ended hands the prompt back (outcome `promptRequired')
instead of starting a turn of its own, leaving the client free to queue
it.

Only the Claude adapter honors it.  Codex's parameter parser passes
unknown keys through and ignores them, so a request there that races the
end of a turn still comes back `startedNewTurn' and the agent runs a turn
nobody asked for (see `agent-shell--inject-report-detached-turn').

For example, given:

  :session-id \"session-1\"
  :prompt [((type . \"text\") (text . \"steer me\"))]

returns:

  ((:method . \"_session/steering\")
   (:params . ((sessionId . \"session-1\")
               (prompt . [((type . \"text\") (text . \"steer me\"))])
               (_meta . ((steering . ((idleBehavior . \"promptRequired\"))))))))"
  (unless session-id
    (error ":session-id is required"))
  (unless prompt
    (error ":prompt is required"))
  `((:method . ,agent-shell-inject--method)
    (:params . ((sessionId . ,session-id)
                (prompt . ,(vconcat prompt))
                (_meta . ((steering . ((idleBehavior . "promptRequired")))))))))

(defun agent-shell--inject-migrate ()
  "Add the injection state keys when a live shell predates them.

A package upgrade reloads agent-shell into a running session whose state
was built without :supports-prompt-injection and :injected-prompt-count.
Without this, `map-put!' fails on those shells, as it does for any absent
alist key."
  (dolist (key '(:supports-prompt-injection :injected-prompt-count))
    (unless (assq key (agent-shell--state))
      (nconc (agent-shell--state)
             (list (cons key (when (eq key :injected-prompt-count) 0)))))))

(defun agent-shell--inject-available-p ()
  "Return non-nil when a prompt can be injected into the running turn.

Needs a turn in flight (nothing to steer otherwise), a session, and an
agent that advertised the capability at `initialize'.

Requiring status `busy' also rules out a `blocked' shell, and that is
deliberate rather than incidental: a blocked shell is waiting on a
permission answer, and no implementation defines what an agent does with
a prompt injected while a tool sits on that question.  Those prompts
queue instead."
  (and (eq (agent-shell-status) 'busy)
       (map-elt (agent-shell--state) :supports-prompt-injection)
       (map-nested-elt (agent-shell--state) '(:session :id))
       t))

(cl-defun agent-shell--inject-render (&key prompt)
  "Render PROMPT as a labeled block inside the running turn.

The running turn's `session/prompt' is still in flight, and a
`user_message_chunk' echo is suppressed while it is (see
`agent-shell--dispatch-notification'), so the injected prompt is rendered
here or the user never sees what they sent.

Renders inside the turn instead of as another shell prompt.  A
prompt-shaped line reads as an interaction boundary for
`shell-maker-narrow-to-prompt', splitting one steered turn into two
interactions and hiding everything past the injection from the viewport,
which rebuilds from `agent-shell-interaction-at-point'.  The label is
what tells the prompt apart from the agent output around it."
  (agent-shell--separate-transcript-after-agent-message
   :last-entry-type (map-elt (agent-shell--state) :last-entry-type)
   :file-path agent-shell--transcript-file)
  (agent-shell--append-transcript
   :text (format "## User (injected) (%s)\n\n%s\n\n"
                 (format-time-string "%F %T")
                 (agent-shell--indent-markdown-headers prompt))
   :file-path agent-shell--transcript-file)
  (agent-shell--inject-migrate)
  (map-put! (agent-shell--state) :injected-prompt-count
            (1+ (map-elt (agent-shell--state) :injected-prompt-count)))
  (agent-shell--update-text
   :state (agent-shell--state)
   :block-id (format "%s-injected-user-message"
                     (map-elt (agent-shell--state) :injected-prompt-count))
   :text (concat
          (propertize "Injected prompt\n"
                      'font-lock-face 'agent-shell-section-heading)
          (propertize (substring-no-properties prompt)
                      'font-lock-face 'agent-shell-input))
   :create-new t)
  (map-put! (agent-shell--state) :last-entry-type
            agent-shell-inject--entry-type))

(cl-defun agent-shell--inject-report-detached-turn (&key prompt)
  "Explain in the buffer that PROMPT started a turn this shell did not ask for.

The `startedNewTurn' outcome means the turn ended before the request
arrived and the agent ran PROMPT as a turn of its own.  Its output then
arrives with no request in flight, and the shell does not show as busy
while it runs.

Rendered into the buffer rather than echoed: the output being explained
shows up later, by which time an echo is gone and the user is left with
agent output nobody asked for and no account of where it came from.  The
queue is left paused by the caller, and the instruction to resume it has
to survive until the turn actually finishes, which is the other reason
this outlives an echo."
  (agent-shell--update-fragment
   :state (agent-shell--state)
   :block-id (format "%s-injected-detached-turn"
                     (map-elt (agent-shell--state) :injected-prompt-count))
   :label-left (propertize "Injected prompt started a new turn"
                           'font-lock-face 'agent-shell-section-heading)
   :body (format "The turn ended before this prompt reached the agent, so the
agent started one of its own for it:

  %s

Its output arrives out of turn, and the shell does not show as busy
while it runs.  Pending prompts stay paused until you run M-x
agent-shell-prompt-queue-resume, so they are not sent into it." prompt)
   :create-new t))

(cl-defun agent-shell--inject-send (&key prompt on-delivered on-declined)
  "Deliver PROMPT to the running turn, calling back with the outcome.

Call ON-DELIVERED (lambda ()) when the agent took the prompt, and
ON-DECLINED (lambda (reason outcome)) when it did not, REASON being a
string fit for the echo area and OUTCOME the normalized outcome symbol.
Both are optional.

Outcomes, per the steering extension:

  injected        joined the running turn.
  startedNewTurn  the turn had ended, so the agent ran the prompt as a
                  new turn.  Consumed either way, so it counts as
                  delivered and must not be sent again.
  promptRequired  NOT consumed: the client owns the prompt and delivers
                  it through a normal `session/prompt'.
  failed          NOT consumed.

Only call with `agent-shell--inject-available-p' non-nil."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (let* ((expanded-prompt (agent-shell--expand-truncated-regions prompt))
         (content-blocks
          (condition-case nil
              (agent-shell--build-content-blocks expanded-prompt)
            (error `[((type . "text")
                      (text . ,(substring-no-properties expanded-prompt)))]))))
    (agent-shell--prompt-queue-migrate)
    ;; A turn can complete before its steering response arrives.  Pause
    ;; automatic queue delivery until the outcome says which side consumed
    ;; the prompt.
    (map-put! (agent-shell--state) :prompt-queue-paused 'steering)
    (condition-case err
        (agent-shell--send-request
         :state (agent-shell--state)
         :client (map-elt (agent-shell--state) :client)
         :request (agent-shell--inject-request
                   :session-id (map-nested-elt (agent-shell--state) '(:session :id))
                   :prompt content-blocks)
         :buffer (current-buffer)
         :on-success
         (lambda (acp-response)
           (let ((outcome (agent-shell--inject-outcome acp-response)))
             (map-put! (agent-shell--state) :prompt-queue-paused
                       (when (eq outcome 'started-new-turn) 'detached-turn))
             (agent-shell--emit-event
              :event 'prompt-injected
              :data (list (cons :prompt (substring-no-properties prompt))
                          (cons :outcome outcome)))
             (if (memq outcome '(injected started-new-turn))
                 (progn
                   (agent-shell--inject-render :prompt prompt)
                   (when (eq outcome 'started-new-turn)
                     (agent-shell--inject-report-detached-turn :prompt prompt))
                   (when on-delivered
                     (funcall on-delivered))
                   ;; If the tracked turn completed while steering was pending,
                   ;; its normal queue hook observed the pause.  Resume now that
                   ;; an `injected' outcome confirms the turn consumed the prompt.
                   (when (and (eq outcome 'injected)
                              (not (shell-maker-busy)))
                     (agent-shell--prompt-queue-process-next)))
               (when on-declined
                 (funcall on-declined
                          (format "agent declined the injection (%s)"
                                  (or (map-elt acp-response 'outcome) "no outcome"))
                          outcome)))))
         :on-failure
         (lambda (acp-error _raw-message)
           (map-put! (agent-shell--state) :prompt-queue-paused nil)
           (agent-shell--emit-event
            :event 'prompt-injected
            :data (list (cons :prompt (substring-no-properties prompt))
                        (cons :outcome 'failed)))
           (when on-declined
             (funcall on-declined
                      (format "injection failed (%s)"
                              (or (map-elt acp-error 'message)
                                  (map-elt acp-error 'code)
                                  "unknown error"))
                      'failed))))
      (error
       (map-put! (agent-shell--state) :prompt-queue-paused nil)
       (signal (car err) (cdr err))))))

(cl-defun agent-shell--inject-or-queue (&key prompt on-delivered)
  "Inject PROMPT into the running turn, or queue it when that is not possible.

Call ON-DELIVERED (lambda ()) once the prompt was injected or submitted,
so callers can drop their own queued copy.  Queueing does not call it.

Falls back to `agent-shell-prompt-queue' behavior whenever the prompt
cannot be injected: no turn in flight, an agent that never advertised the
capability, or an agent that declined.  The prompt is never dropped."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond
   ((and (not (shell-maker-busy))
         (not (map-elt (agent-shell--state) :prompt-queue-paused)))
    (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)
    (when on-delivered
      (funcall on-delivered)))
   ((not (agent-shell--inject-available-p))
    (agent-shell--prompt-queue-enqueue :prompt prompt)
    (agent-shell--echo "Prompt cannot be injected now: prompt queued"))
   (t
    (agent-shell--inject-send
     :prompt prompt
     :on-delivered on-delivered
     :on-declined
     (lambda (reason outcome)
       (let ((submit-now (and (eq outcome 'prompt-required)
                              (not (shell-maker-busy)))))
         (if submit-now
             (progn
               (agent-shell--insert-to-shell-buffer
                :text prompt :submit t :no-focus t)
               (when on-delivered
                 (funcall on-delivered)))
           (agent-shell--prompt-queue-enqueue :prompt prompt))
         (agent-shell--echo "%s: prompt %s"
                            reason
                            (if submit-now "submitted" "queued"))))))))

(cl-defun agent-shell--inject-strict (&key prompt on-delivered on-declined)
  "Inject PROMPT without falling back to another delivery method.

Submit normally when no turn is running.  Signal a `user-error' when a
running or untracked turn cannot accept injection.  Once steering starts,
call ON-DELIVERED with no arguments if the agent consumes PROMPT.  Call
ON-DECLINED with REASON and OUTCOME if it declines or the request fails.
A declined prompt is reported but never queued or submitted by this
function."
  (unless (derived-mode-p 'agent-shell-mode)
    (error "Not in a shell"))
  (cond
   ((and (not (shell-maker-busy))
         (not (map-elt (agent-shell--state) :prompt-queue-paused)))
    (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)
    (when on-delivered
      (funcall on-delivered)))
   ((not (agent-shell--inject-available-p))
    (user-error "Prompt cannot be injected into the current turn"))
   (t
    (agent-shell--inject-send
     :prompt prompt
     :on-delivered on-delivered
     :on-declined
     (lambda (reason outcome)
       (agent-shell--echo "%s: prompt not sent" reason)
       (when on-declined
         (funcall on-declined reason outcome)))))))

(cl-defun agent-shell--inject-pending-prompt (&key index prompt expected
                                                   on-delivered on-declined)
  "Inject an edited or unedited pending PROMPT at INDEX.

EXPECTED is the text currently stored at INDEX.  Reject unavailable
injection without changing that entry.  When steering is attempted, claim
EXPECTED before sending; successful delivery consumes it, while decline or
failure restores PROMPT at the same queue position.  Call ON-DELIVERED or
ON-DECLINED after applying that queue ownership change."
  (let ((current (nth index (map-elt (agent-shell--state) :pending-prompts))))
    (unless (and current (equal current expected))
      (user-error "Prompt no longer pending"))
    (cond
     ((and (not (shell-maker-busy))
           (not (map-elt (agent-shell--state) :prompt-queue-paused)))
      (agent-shell--insert-to-shell-buffer :text prompt :submit t :no-focus t)
      (agent-shell--prompt-queue-remove-at :index index :expected expected)
      (when on-delivered
        (funcall on-delivered)))
     ((not (agent-shell--inject-available-p))
      (user-error "Prompt cannot be injected into the current turn"))
     (t
      (unless (agent-shell--prompt-queue-take-at :index index :expected expected)
        (user-error "Prompt no longer pending"))
      (condition-case err
          (agent-shell--inject-send
           :prompt prompt
           :on-delivered
           (lambda ()
             (message "Prompt sent (%d pending)"
                      (length (map-elt (agent-shell--state) :pending-prompts)))
             (when on-delivered
               (funcall on-delivered)))
           :on-declined
           (lambda (reason outcome)
             (agent-shell--prompt-queue-insert-at :index index :prompt prompt)
             (agent-shell--echo "%s: prompt stays queued" reason)
             (when on-declined
               (funcall on-declined reason outcome))))
        (error
         (agent-shell--prompt-queue-insert-at :index index :prompt prompt)
         (signal (car err) (cdr err))))))))

(defun agent-shell-prompt-inject (prompt)
  "Deliver PROMPT to the turn that is already running.

Read PROMPT from the minibuffer and act on the current project's shell,
resolving it via `agent-shell--shell-buffer' so this works even when
invoked outside a shell buffer.  Unlike `agent-shell-prompt-queue', the
prompt reaches the agent mid-turn, so it can change course without the
turn being interrupted and restarted.

When the shell is idle the prompt is submitted normally.  When a running
turn cannot accept injection, signal a `user-error'.  If the agent declines
or the request fails after steering starts, report that outcome without
submitting or queueing the prompt.

While reading, @ completes project files and / completes available agent
commands when the agent has reported them."
  (interactive
   (list (with-current-buffer (agent-shell--shell-buffer :no-create t)
           (agent-shell--prompt-queue-read))))
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--inject-strict :prompt prompt)))

(defun agent-shell-prompt-inject-queued (index)
  "Deliver the pending prompt at INDEX to the running turn.

Acts on the current project's shell, resolving it via
`agent-shell--shell-buffer' so this works even when invoked outside a
shell buffer.  When called interactively, prompt to choose a pending
prompt (or use the only one when there is just one).

The prompt is claimed from the queue while delivery is unresolved, so
normal turn completion cannot also submit it.  Successful injection
consumes it; decline or failure restores it at the same position.  With no
turn running the prompt is submitted instead.  If a running turn cannot
accept injection, signal a `user-error' and leave the queue unchanged."
  (interactive
   (with-current-buffer (agent-shell--shell-buffer :no-create t)
     (agent-shell--prompt-queue-migrate)
     (let ((choices (agent-shell--prompt-queue-choices)))
       (when (seq-empty-p choices)
         (user-error "No pending prompts"))
       (list (if (cdr choices)
                 (cdr (assoc (completing-read "Inject: " choices nil t) choices))
               (cdar choices))))))
  (with-current-buffer (agent-shell--shell-buffer :no-create t)
    (agent-shell--prompt-queue-migrate)
    (let ((prompt (nth index (map-elt (agent-shell--state) :pending-prompts))))
      (unless prompt
        (user-error "Prompt no longer pending"))
      (agent-shell--inject-pending-prompt
       :index index :prompt prompt :expected prompt))))

(provide 'agent-shell-inject)

;;; agent-shell-inject.el ends here
