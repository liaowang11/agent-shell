;;; agent-shell-goal.el --- Session goals for agent-shell -*- lexical-binding: t; -*-

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
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Commentary:
;;
;; Session goals, from claude-agent-acp's AIR goal extension: a
;; long-running objective the agent keeps working towards across turns.
;;
;; The agent advertises the extension in its `initialize' response, under
;; the top-level `_meta.jetbrains.air.goal', to any client that sent AIR
;; `_meta'.  It publishes the current goal in `session_info_update' as
;; `_meta.jetbrains.air.goal', null once cleared, and accepts a control
;; request (`_session/goal') to set or clear it.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-faces)

(defvar agent-shell--state)
(defvar agent-shell-persistent-prompt-enabled)
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--current-shell "agent-shell")
(declare-function agent-shell--ensure-state-key "agent-shell")
(declare-function agent-shell--emit-event "agent-shell")
(declare-function agent-shell--update-header-and-mode-line "agent-shell")
(declare-function agent-shell--send-request "agent-shell")
(declare-function agent-shell--acp-error-text "agent-shell")
(declare-function agent-shell--ensure-session-not-archived "agent-shell")
(declare-function agent-shell--insert-to-shell-buffer "agent-shell")
(declare-function agent-shell--take-prompt-input "agent-shell-prompt")
(declare-function agent-shell--prompt-queue-paused-p "agent-shell-prompt-queue")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--update-header "agent-shell-viewport")
(declare-function shell-maker-busy "shell-maker")

(defun agent-shell--goal-capability (acp-response)
  "Return the goal capability ACP-RESPONSE advertises, or nil.

ACP-RESPONSE is an `initialize' response.  claude-agent-acp advertises
the extension under the response's top-level `_meta.jetbrains.air.goal',
naming its control method and the actions it supports:

  (agent-shell--goal-capability
   \='((_meta (jetbrains (air (goal (version . 1)
                                   (controlMethod . \"_session/goal\")
                                   (actions . [\"set\" \"clear\"])))))))
  ;; => ((:control-method . \"_session/goal\")
  ;;     (:actions . (\"set\" \"clear\")))"
  (when-let* ((goal (map-nested-elt acp-response '(_meta jetbrains air goal)))
              ((listp goal))
              (method (map-elt goal 'controlMethod))
              ((stringp method)))
    (list (cons :control-method method)
          (cons :actions (append (map-elt goal 'actions) nil)))))

(defun agent-shell--goal-supported-p (&optional action)
  "Return non-nil when the current shell's agent supports goals.
With ACTION, a string such as \"set\", only when it also advertises it."
  (when-let* (((derived-mode-p 'agent-shell-mode))
              (capability (map-elt agent-shell--state :goal-capability)))
    (or (not action)
        (and (member action (map-elt capability :actions)) t))))

(defun agent-shell--goal-from-acp (acp-goal)
  "Return ACP-GOAL, a goal snapshot from the agent, as internal state.

Keeps the fields that describe the goal, leaving out those the agent
did not send:

  (agent-shell--goal-from-acp
   \='((objective . \"Ship it\") (status . \"active\") (iterations . 3)
     (controlMethod . \"_session/goal\")))
  ;; => ((:objective . \"Ship it\") (:status . \"active\")
  ;;     (:iterations . 3))"
  (when (and (listp acp-goal) (map-elt acp-goal 'objective))
    (seq-filter #'cdr
                (list (cons :objective (map-elt acp-goal 'objective))
                      (cons :status (map-elt acp-goal 'status))
                      (cons :iterations (map-elt acp-goal 'iterations))
                      (cons :last-reason (map-elt acp-goal 'lastReason))
                      (cons :created-at (map-elt acp-goal 'createdAt))))))

(defun agent-shell--set-goal (state acp-goal)
  "Record ACP-GOAL as STATE's goal, nil when the agent cleared it.

Refreshes the header, and emits `goal-changed' when the objective or
status changed.  Progress alone, such as another iteration, doesn't
emit: the agent reports it after every pass towards the goal."
  (agent-shell--ensure-state-key state :goal)
  (let ((previous (map-elt state :goal))
        (goal (agent-shell--goal-from-acp acp-goal)))
    (map-put! state :goal goal)
    (with-current-buffer (map-elt state :buffer)
      (when (derived-mode-p 'agent-shell-mode)
        (agent-shell--update-header-and-mode-line))
      (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                    :shell-buffer (current-buffer)
                                    :existing-only t)))
        (with-current-buffer viewport-buffer
          (agent-shell-viewport--update-header)))
      (unless (and (equal (map-elt previous :objective) (map-elt goal :objective))
                   (equal (map-elt previous :status) (map-elt goal :status)))
        (agent-shell--emit-event :event 'goal-changed
                                 :data (list (cons :goal goal)
                                             (cons :previous-goal previous)))))))

(defun agent-shell--goal-face (status)
  "Return the face showing a goal with STATUS."
  (pcase status
    ("active" 'agent-shell-warning)
    ("paused" 'agent-shell-pending)
    ((or "blocked" "limited") 'agent-shell-error)
    ("complete" 'agent-shell-success)
    (_ 'agent-shell-secondary)))

(defun agent-shell--goal-description (goal)
  "Return GOAL described in full, one field per line."
  (string-join
   (delq nil
         (list (format "Goal: %s" (map-elt goal :objective))
               (when-let* ((status (map-elt goal :status)))
                 (format "Status: %s" status))
               (when-let* ((iterations (map-elt goal :iterations)))
                 (format "Iterations: %s" iterations))
               (when-let* ((reason (map-elt goal :last-reason)))
                 (format "Last reason: %s" reason))))
   "\n"))

(defun agent-shell--goal-indicator (goal)
  "Return the header indicator for GOAL, or nil when there is none.

The objective is cut to 30 columns and followed by the status, in a
face per status.  The help echo has the full goal.  `%' is doubled,
since header and mode lines read it as a construct.

  (agent-shell--goal-indicator
   \='((:objective . \"Ship it\") (:status . \"active\")))
  ;; => \"Goal: Ship it (active)\""
  (when-let* ((objective (map-elt goal :objective))
              ((stringp objective)))
    (propertize (concat "Goal: "
                        (string-replace
                         "%" "%%"
                         (truncate-string-to-width
                          (string-trim
                           (replace-regexp-in-string "[[:space:]]+" " " objective))
                          30 nil nil "..."))
                        (when-let* ((status (map-elt goal :status)))
                          (format " (%s)" status)))
                'face (agent-shell--goal-face (map-elt goal :status))
                'help-echo (agent-shell--goal-description goal))))

(cl-defun agent-shell--make-goal-request (&key method session-id action objective)
  "Return a goal control request for SESSION-ID.

METHOD is the control method the agent advertised.  ACTION is \"set\",
which needs OBJECTIVE, or \"clear\"."
  (unless method
    (error ":method is required"))
  (unless session-id
    (error ":session-id is required"))
  (when (and (equal action "set") (not objective))
    (error ":objective is required to set a goal"))
  `((:method . ,method)
    (:params . ((sessionId . ,session-id)
                (action . ,action)
                ,@(when objective
                    `((objective . ,objective)))))))

(defun agent-shell--goal-shell ()
  "Return the shell a goal command acts on.
That is the current shell buffer, or a viewport buffer's shell."
  (or (agent-shell--current-shell)
      (user-error "Not in a shell or viewport buffer")))

(defun agent-shell--goal-session-id (action)
  "Return the current shell's session id for goal ACTION.
Signal a `user-error' outside a shell, when the agent did not advertise
ACTION, when the session is archived or when there is no session yet."
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (unless (agent-shell--goal-supported-p action)
    (user-error "Agent does not support the goal action \"%s\"" action))
  (agent-shell--ensure-session-not-archived)
  (or (map-nested-elt (agent-shell--state) '(:session :id))
      (user-error "No active session")))

(cl-defun agent-shell--send-goal-action (&key action objective)
  "Ask the agent to apply goal ACTION, with OBJECTIVE when setting one.

While a turn runs, sends the control request the agent advertised,
which steers the agent's `/goal' command into that turn.  While idle,
submits the `/goal' command as a prompt instead: there, the control
request runs a whole turn before it answers, and the shell would not
own that turn.  Either way the agent publishes the new goal through
`session_info_update'.

Acts on `agent-shell--goal-shell'."
  (with-current-buffer (agent-shell--goal-shell)
    (let ((session-id (agent-shell--goal-session-id action))
          (state (agent-shell--state))
          (command (if (equal action "set")
                       (concat "/goal " objective)
                     "/goal clear")))
      (if (or (shell-maker-busy) (agent-shell--prompt-queue-paused-p))
          (agent-shell--send-request
           :state state
           :client (map-elt state :client)
           :request (agent-shell--make-goal-request
                     :method (map-nested-elt state '(:goal-capability :control-method))
                     :session-id session-id
                     :action action
                     :objective objective)
           :buffer (map-elt state :buffer)
           :on-success #'ignore
           :on-failure (lambda (acp-error _raw-message)
                         (message "Failed to %s goal: %s" action
                                  (agent-shell--acp-error-text acp-error))))
        ;; Like the prompt queue: keep what the user is typing out of the
        ;; command, and give it back afterwards.
        (let ((draft (and agent-shell-persistent-prompt-enabled
                          (agent-shell--take-prompt-input))))
          (agent-shell--insert-to-shell-buffer :shell-buffer (current-buffer)
                                               :text command
                                               :submit t
                                               :no-focus t)
          (when draft
            (goto-char (point-max))
            (insert draft)))))))

(defun agent-shell-set-goal (objective)
  "Set the current session's goal to OBJECTIVE.

The agent keeps working towards OBJECTIVE across turns until it's met or
cleared with `agent-shell-clear-goal'.  Needs an agent with
claude-agent-acp's goal extension.  The header shows the goal once the
agent reports it.

Works from both shell and viewport buffers."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive
   (with-current-buffer (agent-shell--goal-shell)
     (agent-shell--goal-session-id "set")
     (let ((current (map-nested-elt (agent-shell--state) '(:goal :objective))))
       (list (read-string (format-prompt "Goal" current) nil nil current)))))
  (let ((objective (string-trim (or objective ""))))
    (when (string-empty-p objective)
      (user-error "Goal can't be blank"))
    (when (equal objective "clear")
      (user-error "Use `agent-shell-clear-goal' to clear the goal"))
    (agent-shell--send-goal-action :action "set" :objective objective)))

(defun agent-shell-clear-goal ()
  "Clear the current session's goal.
Needs an agent with claude-agent-acp's goal extension.

Works from both shell and viewport buffers."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (agent-shell--send-goal-action :action "clear"))

(provide 'agent-shell-goal)

;;; agent-shell-goal.el ends here
