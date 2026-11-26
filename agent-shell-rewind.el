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
;; Session cursors (ACP's unstable session cursors RFD) name positions in
;; a session that a method can act at.  claude-agent-acp announces one
;; before each user prompt in a `cursor_update', to a client that declared
;; `clientCapabilities.session.cursors'.  The cursor always accepts
;; `session/fork', and also accepts claude-agent-acp's `_session/rewind'
;; when the client named AIR's "sessionRewind" and the conversation can
;; still be cut there.  Its id is opaque: it is stored as announced, on
;; the prompt it precedes, and never built here.

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

(provide 'agent-shell-rewind)
;;; agent-shell-rewind.el ends here
