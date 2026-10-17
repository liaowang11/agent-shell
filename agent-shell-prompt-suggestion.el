;;; agent-shell-prompt-suggestion.el --- Predicted next prompts for agent-shell -*- lexical-binding: t; -*-

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
;; The prompt the agent predicts the user sends next, offered as "future
;; history": one step past the newest entry of prompt history, the way the
;; minibuffer offers its defaults on M-n.
;;
;;   oldest ... M-p <- newest <- [what you typed] -> M-n -> suggestion
;;
;; An empty prompt shows the suggestion in gray.  M-n inserts it as
;; ordinary input, saving what was typed; M-p from there restores it.
;;
;; claude-agent-acp publishes the prediction after a turn ends, as
;; `session_info_update' with `_meta.promptSuggestions =
;; {suggestions: [{text}]}', and clears it with an empty list.

;;; Code:

(require 'comint)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-faces)
(require 'agent-shell-prompt)

(defvar agent-shell--state)
(declare-function agent-shell--ensure-state-key "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")

(defcustom agent-shell-prompt-suggestions t
  "How to offer the prompt the agent predicts you will send next.

t        Show it in gray in an empty prompt, and insert it with
         \\[agent-shell-next-input] past the newest history entry.
`history' Insert it with \\[agent-shell-next-input] only; draw nothing.
nil      Off.  Claude Code is also asked not to make the prediction,
         for shells started after the change."
  :type '(choice (const :tag "Show in gray and insert with M-n" t)
                 (const :tag "Insert with M-n only" history)
                 (const :tag "Off" nil))
  :group 'agent-shell)

(defvar-local agent-shell--prompt-suggestion-draft nil
  "What the prompt held before \\[agent-shell-next-input] put the suggestion there.
Nil while the prompt is not showing the suggestion.  \"\" when nothing
was typed.  \\[agent-shell-previous-input] puts it back.")

(defvar-local agent-shell--prompt-suggestion-overlay nil
  "The overlay showing the suggestion in gray, or nil.")
;; Survives the viewport's edit <-> view mode switches, so the switch's
;; `change-major-mode-hook' can still find and delete it.
(put 'agent-shell--prompt-suggestion-overlay 'permanent-local t)

(defun agent-shell--prompt-suggestion-from-acp (acp-value)
  "Return the suggestion in ACP-VALUE, or nil when it holds none.

ACP-VALUE is the `_meta.promptSuggestions' of a `session_info_update'.
The first non-blank text wins, trimmed:

  (agent-shell--prompt-suggestion-from-acp
   \\='((suggestions . [((text . \"  \")) ((text . \"Run the tests\\n\"))])))
  ;; => \"Run the tests\""
  (seq-some (lambda (suggestion)
              (when-let* (((listp suggestion))
                          (text (map-elt suggestion 'text))
                          ((stringp text))
                          (text (string-trim text))
                          ((not (string-empty-p text))))
                text))
            (and (listp acp-value) (map-elt acp-value 'suggestions))))

(defun agent-shell--prompt-suggestion-text ()
  "Return the suggestion to offer in the current buffer, or nil.
The current buffer is a shell, or a viewport compose buffer reading its
shell's suggestion."
  (when agent-shell-prompt-suggestions
    (cond
     ((derived-mode-p 'agent-shell-mode)
      (map-elt agent-shell--state :prompt-suggestion))
     ((derived-mode-p 'agent-shell-viewport-edit-mode)
      (when-let* ((shell-buffer (agent-shell-viewport--shell-buffer))
                  ((buffer-live-p shell-buffer)))
        (map-elt (buffer-local-value 'agent-shell--state shell-buffer)
                 :prompt-suggestion))))))

(defun agent-shell--prompt-suggestion-position ()
  "Return where gray text goes in the current buffer, or nil for nowhere.
In a shell, the input area of a live prompt with nothing typed.  In a
compose buffer, the start of an empty buffer."
  (cond
   ((derived-mode-p 'agent-shell-mode)
    (when-let* ((start (agent-shell--prompt-input-start))
                ((= start (point-max))))
      start))
   ((derived-mode-p 'agent-shell-viewport-edit-mode)
    (when (= (buffer-size) 0)
      (point-min)))))

(defun agent-shell--prompt-suggestion-ghost (suggestion)
  "Return SUGGESTION's first line as the gray text to display.
The `cursor' property keeps point drawn before the text, where typing
goes, rather than after it."
  (let ((ghost (propertize (car (split-string suggestion "\n"))
                           'face 'agent-shell-prompt-suggestion)))
    (put-text-property 0 1 'cursor t ghost)
    ghost))

(defun agent-shell--prompt-suggestion-remove ()
  "Delete the current buffer's gray suggestion, if any."
  (when (overlayp agent-shell--prompt-suggestion-overlay)
    (delete-overlay agent-shell--prompt-suggestion-overlay))
  (setq agent-shell--prompt-suggestion-overlay nil))

(defun agent-shell--prompt-suggestion-refresh ()
  "Show the suggestion in gray in the current buffer, or hide it.
Shows it when `agent-shell-prompt-suggestions' is t, a suggestion is
stored and there is an empty prompt to put it in."
  (agent-shell--prompt-suggestion-remove)
  (when-let* (((eq agent-shell-prompt-suggestions t))
              (suggestion (agent-shell--prompt-suggestion-text))
              (position (agent-shell--prompt-suggestion-position))
              (overlay (make-overlay position position nil t nil)))
    (overlay-put overlay 'agent-shell-prompt-suggestion t)
    ;; Emacs draws the strings of other overlays at POSITION first: their
    ;; after-strings, then before-strings by increasing priority.  As the
    ;; highest-priority before-string, the gray text comes last, beside
    ;; point, even after the viewport's text-header spacer newline.
    (overlay-put overlay 'priority most-positive-fixnum)
    (overlay-put overlay 'before-string (agent-shell--prompt-suggestion-ghost suggestion))
    (setq agent-shell--prompt-suggestion-overlay overlay)))

(defun agent-shell--prompt-suggestion-after-change (&rest _)
  "Redraw the gray suggestion after a buffer change.
Typing hides it, and deleting back to an empty prompt shows it again.
Does nothing in a buffer that has neither a suggestion nor gray text,
which is what most changes during a turn's output look like."
  (when (or agent-shell--prompt-suggestion-overlay
            (agent-shell--prompt-suggestion-text))
    (agent-shell--prompt-suggestion-refresh)))

(defun agent-shell--prompt-suggestion-watch ()
  "Redraw the gray suggestion when the current shell buffer changes.
`shell-maker-finish-output-hook' covers a suggestion that arrives
before the prompt it belongs in is printed."
  (add-hook 'after-change-functions #'agent-shell--prompt-suggestion-after-change nil t)
  (add-hook 'shell-maker-finish-output-hook #'agent-shell--prompt-suggestion-refresh nil t))

(defun agent-shell--set-prompt-suggestion (state suggestion)
  "Record SUGGESTION as STATE's predicted prompt, nil when there is none.
Redraws the gray text in the shell and in its viewport compose buffer."
  (agent-shell--ensure-state-key state :prompt-suggestion)
  (map-put! state :prompt-suggestion suggestion)
  (when-let* ((shell-buffer (map-elt state :buffer))
              ((buffer-live-p shell-buffer)))
    (with-current-buffer shell-buffer
      (agent-shell--prompt-suggestion-watch)
      (agent-shell--prompt-suggestion-refresh)
      (when-let* ((viewport-buffer (agent-shell-viewport--buffer
                                    :shell-buffer shell-buffer
                                    :existing-only t)))
        (with-current-buffer viewport-buffer
          (agent-shell--prompt-suggestion-refresh))))))

(defun agent-shell--prompt-suggestion-spend (state)
  "Forget STATE's suggestion: a prompt was sent, so it no longer applies.
claude-agent-acp clears it too once the prompt reaches it, but not before
the turn has started."
  (when-let* ((shell-buffer (map-elt state :buffer))
              ((buffer-live-p shell-buffer)))
    (with-current-buffer shell-buffer
      (setq agent-shell--prompt-suggestion-draft nil)))
  (agent-shell--set-prompt-suggestion state nil))

(defun agent-shell--replace-prompt-input (text)
  "Replace what is typed at the live prompt with TEXT, leaving point after it."
  (let ((start (agent-shell--prompt-input-start)))
    (delete-region start (point-max))
    (goto-char (point-max))
    (insert text)))

(defun agent-shell-next-input (arg)
  "Cycle forwards through input history, then to the suggested prompt.

Past the newest entry, inserts the prompt the agent predicts you will
send next, replacing what is typed; \\[agent-shell-previous-input]
brings that back.  See `agent-shell-prompt-suggestions'.  Otherwise this
is `comint-next-input', moving ARG entries."
  (interactive "*p")
  (cond
   (agent-shell--prompt-suggestion-draft
    (user-error "End of future history"))
   ((and (null comint-input-ring-index)
         (agent-shell--prompt-input-start)
         (agent-shell--prompt-suggestion-text))
    (let ((suggestion (agent-shell--prompt-suggestion-text))
          (start (agent-shell--prompt-input-start)))
      (setq agent-shell--prompt-suggestion-draft (buffer-substring start (point-max)))
      (agent-shell--replace-prompt-input suggestion)))
   (t
    (comint-next-input arg))))

(defun agent-shell-previous-input (arg)
  "Cycle backwards through input history, saving input.

From the suggested prompt that \\[agent-shell-next-input] inserted,
restores what was typed before it, edits to the suggestion included.
Otherwise this is `comint-previous-input', moving ARG entries."
  (interactive "*p")
  (if-let* ((draft agent-shell--prompt-suggestion-draft)
            ((agent-shell--prompt-input-start)))
      (progn
        (agent-shell--replace-prompt-input draft)
        (setq agent-shell--prompt-suggestion-draft nil))
    (comint-previous-input arg)))

(provide 'agent-shell-prompt-suggestion)

;;; agent-shell-prompt-suggestion.el ends here
