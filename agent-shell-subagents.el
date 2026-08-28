;;; agent-shell-subagents.el --- Native subagent buffers, list and transcripts -*- lexical-binding: t; -*-

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
;; An agent that reports native subagent sessions (ACP's draft
;; `subagents' client capability) streams each subagent's content over
;; the root session's connection, tagged with the subagent's own
;; session id.  The root shell owns all of that state; this file holds
;; what the subagents get on top of it:
;;
;; - A read-only buffer per subagent, which its content renders into
;;   instead of the root shell.  The root shows a one-line row where the
;;   subagent was spawned, which opens this buffer.
;; - A transcript file per subagent, beside the root's.
;; - `agent-shell-subagents', a list of the shell's subagents and
;;   background tasks, running first, to open, jump to or stop them,
;;   and a count of the running ones in the shell's header.
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-shell-ui)

(defvar agent-shell--state)
(defvar comint-prompt-regexp)
(declare-function agent-shell--filter-buffer-substring "agent-shell")
(declare-function agent-shell--native-subagent "agent-shell")
(declare-function agent-shell--render-markdown-body "agent-shell")
(declare-function agent-shell--subagent-row-at "agent-shell")
(declare-function agent-shell--subagent-row-label "agent-shell")
(declare-function agent-shell--update-native-subagent "agent-shell")
(declare-function agent-shell--async-task "agent-shell")
(declare-function agent-shell--format-async-task-body "agent-shell")
(declare-function agent-shell--make-status-kind-label "agent-shell")
(declare-function agent-shell--send-async-task-stop "agent-shell")
(declare-function agent-shell--update-header-and-mode-line "agent-shell")
(declare-function agent-shell-viewport--buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--update-header "agent-shell-viewport")
(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")
(declare-function agent-shell-viewport--position "agent-shell-viewport")
(declare-function agent-shell-viewport-next-page "agent-shell-viewport")
(declare-function agent-shell-viewport-previous-page "agent-shell-viewport")
(declare-function acp-send-notification "acp")
(declare-function acp-make-session-cancel-notification "acp")
(declare-function shell-maker--re-search-forward-prompt "shell-maker")

(defvar-local agent-shell-subagents--shell-buffer nil
  "The shell buffer whose subagent this buffer shows.")

(defvar-local agent-shell-subagents--session-id nil
  "The ACP session id of the subagent this buffer shows.")

(defvar-local agent-shell-subagents--running-count nil
  "How many subagents the shell's header last counted as running.
Kept in the shell buffer, so a subagent's streaming only rebuilds the
header when the count it shows changes.")

(defvar-local agent-shell-subagents-list--origin nil
  "The buffer the subagents list was last opened from.
`agent-shell-subagents-list-jump' pages the viewport when this is one.")

(defvar-local agent-shell-subagents-list--entries nil
  "Alist of each rendered row's qualified fragment id to its entry.
See `agent-shell-subagents--list-entries' for an entry's shape.")

(defvar-local agent-shell-subagents-list--refresh-timer nil
  "Timer for the pending refresh of this subagents list, or nil.")

(defvar agent-shell-subagent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'agent-shell-ui-forward-block)
    (define-key map (kbd "p") #'agent-shell-ui-backward-block)
    (define-key map (kbd "TAB") #'agent-shell-ui-forward-block)
    (define-key map (kbd "<backtab>") #'agent-shell-ui-backward-block)
    (define-key map (kbd "^") #'agent-shell-subagents-parent)
    (define-key map (kbd "S") #'agent-shell-subagents)
    map)
  "Keymap for `agent-shell-subagent-mode'.")

(define-derived-mode agent-shell-subagent-mode special-mode "Agent Shell Subagent"
  "Major mode for following one native subagent's work (read-only).

The subagent's thoughts, tool calls, messages and permission requests
render here as they stream, while the shell that spawned it shows one
row for it.  There is nothing to type: agents do not accept prompts
for a subagent's session.

\{agent-shell-subagent-mode-map}"
  (agent-shell-ui-mode +1)
  (add-hook 'agent-shell-ui-post-expand-fragment-at-point-hook
            #'agent-shell--render-markdown-body nil t)
  (setq-local filter-buffer-substring-function #'agent-shell--filter-buffer-substring)
  ;; Rendered code blocks carry their language's syntax table as a
  ;; `syntax-table' text property (see `agent-shell-markdown--apply-syntax-from').
  (setq-local parse-sexp-lookup-properties t)
  (setq-local header-line-format '(:eval (agent-shell-subagents--header-line))))

(defun agent-shell-subagents--shell-state (shell-buffer)
  "Return SHELL-BUFFER's agent-shell state, or nil when it is gone."
  (when (buffer-live-p shell-buffer)
    (buffer-local-value 'agent-shell--state shell-buffer)))

(defun agent-shell-subagents--face-from-font-lock (text)
  "Return TEXT with each `font-lock-face' also set as its `face'.

A header line does not run font lock, so text styled for a buffer
through `font-lock-face' shows unstyled there."
  (let ((text (copy-sequence text))
        (position 0))
    (while (< position (length text))
      (let ((next (next-single-property-change position 'font-lock-face text (length text))))
        (when-let* ((face (get-text-property position 'font-lock-face text)))
          (put-text-property position next 'face face text))
        (setq position next)))
    text))

(defun agent-shell-subagents--header-line ()
  "Return the header line of the current subagent buffer.

Reads as the subagent's row in its parent, then the shell it belongs
to, e.g. \=`◔ Researcher Read 2 files  Claude Agent @ project'."
  (when-let* ((shell-buffer agent-shell-subagents--shell-buffer)
              (state (agent-shell-subagents--shell-state shell-buffer))
              (row (agent-shell--subagent-row-at state agent-shell-subagents--session-id)))
    (agent-shell-subagents--face-from-font-lock
     (concat " "
             (agent-shell--subagent-row-label state row)
             "  "
             (propertize (buffer-name shell-buffer) 'font-lock-face 'shadow)))))

(defun agent-shell-subagents--buffer-name (state subagent)
  "Return the buffer name for SUBAGENT, a registry entry in STATE.

  (agent-shell-subagents--buffer-name state \='((:name . \"Researcher\")))
  ;; => \"Claude Agent @ project [subagent: Researcher]\""
  (format "%s [subagent: %s]"
          (if (buffer-live-p (map-elt state :buffer))
              (buffer-name (map-elt state :buffer))
            "agent-shell")
          (or (map-elt subagent :name) "unnamed")))

(cl-defun agent-shell-subagents--buffer (&key state session-id create)
  "Return the buffer showing subagent SESSION-ID of STATE's shell, or nil.

With CREATE, make the buffer when there is none yet, or when the user
killed it.  A recreated buffer only shows what arrives from then on;
the subagent's transcript file keeps everything."
  (when-let* ((subagent (agent-shell--native-subagent state session-id)))
    (let ((buffer (map-elt subagent :buffer)))
      (cond
       ((buffer-live-p buffer) buffer)
       (create
        (setq buffer (generate-new-buffer
                      (agent-shell-subagents--buffer-name state subagent)))
        (with-current-buffer buffer
          (agent-shell-subagent-mode)
          (setq agent-shell-subagents--shell-buffer (map-elt state :buffer)
                agent-shell-subagents--session-id session-id)
          (when (buffer-live-p (map-elt state :buffer))
            (setq default-directory
                  (buffer-local-value 'default-directory (map-elt state :buffer)))))
        (agent-shell--update-native-subagent
         state session-id (list (cons :buffer buffer)))
        buffer)))))

(defun agent-shell-subagents--changed (state)
  "Refresh what summarizes STATE's subagents after one of them changed.
That is each subagent buffer's header line, which reads its row, the
shell's header count, and the subagents list."
  (dolist (entry (map-elt state :native-subagents))
    (when-let* ((buffer (map-elt (cdr entry) :buffer))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (force-mode-line-update))))
  (agent-shell-subagents--refresh-header state)
  (agent-shell-subagents--schedule-list-refresh state))

(defun agent-shell-subagents--kill-buffers (state)
  "Kill every subagent buffer STATE's shell made, and its subagents list."
  (dolist (entry (map-elt state :native-subagents))
    (when-let* ((buffer (map-elt (cdr entry) :buffer))
                ((buffer-live-p buffer)))
      (kill-buffer buffer)))
  (when-let* ((buffer (agent-shell-subagents--list-buffer state)))
    (with-current-buffer buffer
      (when (timerp agent-shell-subagents-list--refresh-timer)
        (cancel-timer agent-shell-subagents-list--refresh-timer)))
    (kill-buffer buffer)))

(defun agent-shell-subagents-display (state session-id)
  "Display subagent SESSION-ID of STATE's shell and select its window."
  (pop-to-buffer (or (agent-shell-subagents--buffer
                      :state state :session-id session-id :create t)
                     (user-error "No such subagent: %s" session-id))))

(defun agent-shell-subagents-parent ()
  "Switch to the session that spawned this buffer's subagent.
That is the shell for a subagent the root spawned, or the spawning
subagent's own buffer for a nested one."
  (declare (modes agent-shell-subagent-mode))
  (interactive)
  (let* ((shell-buffer (or agent-shell-subagents--shell-buffer
                           (user-error "Not in a subagent buffer")))
         (state (or (agent-shell-subagents--shell-state shell-buffer)
                    (user-error "The shell this subagent belongs to is gone")))
         (parent (map-elt (agent-shell--native-subagent
                           state agent-shell-subagents--session-id)
                          :parent)))
    (if (stringp parent)
        (agent-shell-subagents-display state parent)
      (pop-to-buffer shell-buffer))))

;;; Subagents list

(defconst agent-shell-subagents--async-task-terminal-states
  '("completed" "failed" "stopped")
  "Async task states after which a task no longer runs.")

(defun agent-shell-subagents--running (state)
  "Return STATE's native subagents that have not reached a terminal state."
  (seq-remove (lambda (entry) (map-elt (cdr entry) :ended-at))
              (map-elt state :native-subagents)))

(defun agent-shell-subagents--header-indicator (state)
  "Return the header's count of STATE's running subagents, or nil.

  (agent-shell-subagents--header-indicator state)
  ;; => \"2 subagents\""
  (let ((count (length (agent-shell-subagents--running state))))
    (when (> count 0)
      (propertize (format "%d subagent%s" count (if (= count 1) "" "s"))
                  'face 'agent-shell-subagent-name
                  'help-echo "Running subagents: M-x agent-shell-subagents lists them"))))

(defun agent-shell-subagents--refresh-header (state)
  "Rebuild the headers of STATE's shell and viewport if the count changed."
  (when-let* ((shell-buffer (map-elt state :buffer))
              ((buffer-live-p shell-buffer)))
    (with-current-buffer shell-buffer
      (let ((count (length (agent-shell-subagents--running state))))
        (unless (equal count agent-shell-subagents--running-count)
          (setq agent-shell-subagents--running-count count)
          (when (derived-mode-p 'agent-shell-mode)
            (agent-shell--update-header-and-mode-line))
          (when-let* ((viewport (agent-shell-viewport--buffer
                                 :shell-buffer shell-buffer :existing-only t))
                      ((buffer-live-p viewport)))
            (with-current-buffer viewport
              (agent-shell-viewport--update-header))))))))

(defun agent-shell-subagents--list-buffer-name (state)
  "Return the name of the subagents list for STATE's shell.

  (agent-shell-subagents--list-buffer-name state)
  ;; => \"Claude Agent @ project [subagents]\""
  (format "%s [subagents]"
          (if (buffer-live-p (map-elt state :buffer))
              (buffer-name (map-elt state :buffer))
            "agent-shell")))

(defun agent-shell-subagents--list-buffer (state)
  "Return STATE's live subagents list buffer, or nil."
  (when-let* ((buffer (get-buffer (agent-shell-subagents--list-buffer-name state)))
              ((eq (buffer-local-value 'agent-shell-subagents--shell-buffer buffer)
                   (map-elt state :buffer))))
    buffer))

(defun agent-shell-subagents--schedule-list-refresh (state)
  "Refresh STATE's subagents list shortly, if it is open.
Debounced, so a subagent streaming tool calls redraws the list once
per burst rather than once per update."
  (when-let* ((buffer (agent-shell-subagents--list-buffer state)))
    (with-current-buffer buffer
      (unless (timerp agent-shell-subagents-list--refresh-timer)
        (setq agent-shell-subagents-list--refresh-timer
              (run-with-timer
               0.2 nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq agent-shell-subagents-list--refresh-timer nil)
                     (agent-shell-subagents--render-list state))))))))))

(defun agent-shell-subagents--elapsed (start end)
  "Return the time from START to END, or to now, as a short string.

  (agent-shell-subagents--elapsed (time-subtract nil 75) nil)
  ;; => \"1m 15s\""
  (let ((seconds (max 0 (floor (float-time (time-subtract (or end (current-time))
                                                          start))))))
    (cond ((< seconds 60) (format "%ds" seconds))
          ((< seconds 3600) (format "%dm %ds" (/ seconds 60) (% seconds 60)))
          (t (format "%dh %dm" (/ seconds 3600) (/ (% seconds 3600) 60))))))

(defun agent-shell-subagents--subagent-entry (state session-id subagent)
  "Return the list entry for SUBAGENT, registered as SESSION-ID in STATE."
  (let* ((parent (map-elt subagent :parent))
         (parent-name (when (stringp parent)
                        (or (map-elt (agent-shell--native-subagent state parent) :name)
                            "a subagent"))))
    (list (cons :kind 'subagent)
          (cons :id session-id)
          (cons :finished (and (map-elt subagent :ended-at) t))
          (cons :label-left (agent-shell--subagent-row-label
                             state (agent-shell--subagent-row-at state session-id)))
          (cons :label-right
                (propertize
                 (string-join
                  (delq nil (list (when parent-name (format "from %s" parent-name))
                                  (when (map-elt subagent :spawned-at)
                                    (agent-shell-subagents--elapsed
                                     (map-elt subagent :spawned-at)
                                     (map-elt subagent :ended-at)))))
                  " · ")
                 'font-lock-face 'agent-shell-secondary))
          (cons :body (string-trim (or (map-elt subagent :task) ""))))))

(defun agent-shell-subagents--async-task-entry (async-task-id task)
  "Return the list entry for async TASK, registered as ASYNC-TASK-ID."
  (let ((task-state (or (map-elt task :state) "running")))
    (list (cons :kind 'async-task)
          (cons :id async-task-id)
          (cons :finished (and (member task-state
                                       agent-shell-subagents--async-task-terminal-states)
                               t))
          (cons :label-left
                (concat (agent-shell--make-status-kind-label :status task-state :kind "background")
                        " "
                        (propertize (or (map-elt task :name)
                                        (map-elt task :task-type)
                                        "Background task")
                                    'font-lock-face 'agent-shell-async-task-name)))
          (cons :label-right (propertize "background task"
                                         'font-lock-face 'agent-shell-secondary))
          (cons :body
                (let ((description (map-elt task :description))
                      (summary (map-elt task :summary)))
                  (string-trim
                   (string-join
                    (delq nil
                          (list description
                                (unless (equal summary description) summary)
                                (agent-shell--format-async-task-body
                                 nil nil
                                 (map-elt task :last-tool-name)
                                 (map-elt task :usage))))
                    "\n")))))))

(defun agent-shell-subagents--list-entries (state)
  "Return the rows STATE's subagents list shows, running ones first.

Each is an alist of `:kind' (`subagent' or `async-task'), `:id',
`:finished', `:label-left', `:label-right' and `:body'.  Subagents come
in the order they were spawned, then async tasks, which the registry
holds newest first."
  (let ((entries
         (append
          (mapcar (lambda (entry)
                    (agent-shell-subagents--subagent-entry state (car entry) (cdr entry)))
                  (sort (copy-sequence (map-elt state :native-subagents))
                        (lambda (a b)
                          (time-less-p (or (map-elt (cdr a) :spawned-at) 0)
                                       (or (map-elt (cdr b) :spawned-at) 0)))))
          (mapcar (lambda (entry)
                    (agent-shell-subagents--async-task-entry (car entry) (cdr entry)))
                  (reverse (map-elt state :async-tasks))))))
    (append (seq-remove (lambda (entry) (map-elt entry :finished)) entries)
            (seq-filter (lambda (entry) (map-elt entry :finished)) entries))))

(defun agent-shell-subagents--list-entry-block-id (entry)
  "Return the fragment block id ENTRY's row renders with."
  (format "%s:%s" (map-elt entry :kind) (map-elt entry :id)))

(defun agent-shell-subagents--fold-states ()
  "Return an alist of each fragment's qualified id to its `:collapsed' state."
  (let ((position (point-min))
        (folds '()))
    (while (< position (point-max))
      (when-let* ((ui-state (get-text-property position 'agent-shell-ui-state))
                  (id (map-elt ui-state :qualified-id))
                  ((not (assoc id folds))))
        (push (cons id (map-elt ui-state :collapsed)) folds))
      (setq position (next-single-property-change
                      position 'agent-shell-ui-state nil (point-max))))
    folds))

(defun agent-shell-subagents--goto-fragment (qualified-id)
  "Move point to the start of fragment QUALIFIED-ID, when it is rendered.
Return the position, or nil."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward
                      'agent-shell-ui-state nil
                      (lambda (_ ui-state)
                        (equal (map-elt ui-state :qualified-id) qualified-id))
                      t)))
    (goto-char (prop-match-beginning match))))

(defun agent-shell-subagents--render-list (state)
  "Redraw the current subagents list from STATE.

Every row is drawn afresh, so a finished subagent moves under the
folded \"Finished\" group.  What the user unfolded stays unfolded, and
point stays on the row it was on."
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)
         (folds (agent-shell-subagents--fold-states))
         (point-id (map-elt (get-text-property (point) 'agent-shell-ui-state)
                            :qualified-id))
         (entries (agent-shell-subagents--list-entries state))
         (finished-count (seq-count (lambda (entry) (map-elt entry :finished)) entries)))
    (erase-buffer)
    (setq agent-shell-subagents-list--entries nil)
    (if (null entries)
        (insert (propertize "No subagents or background tasks in this session yet.\n"
                            'font-lock-face 'agent-shell-secondary))
      (dolist (entry entries)
        (let* ((block-id (agent-shell-subagents--list-entry-block-id entry))
               (qualified-id (format "subagents-%s" block-id))
               (fold (assoc qualified-id folds)))
          (push (cons qualified-id entry) agent-shell-subagents-list--entries)
          (agent-shell-ui-update-fragment
           (agent-shell-ui-make-fragment-model
            :namespace-id "subagents"
            :block-id block-id
            :label-left (map-elt entry :label-left)
            :label-right (map-elt entry :label-right)
            :body (unless (string-empty-p (map-elt entry :body))
                    (map-elt entry :body))
            :group-id (when (map-elt entry :finished) "finished")
            :group-label (format "Finished (%d)" finished-count)
            :group-expanded (when-let* ((group-fold (assoc "subagents-finished" folds)))
                              (not (cdr group-fold))))
           :create-new t
           :navigation 'always
           :expanded (and fold (not (cdr fold)))
           :no-undo t))))
    (unless (and point-id (agent-shell-subagents--goto-fragment point-id))
      (goto-char (point-min)))))

(defun agent-shell-subagents--current-shell ()
  "Return the shell buffer the current buffer belongs to.
That is the current buffer in a shell, the shell a viewport, subagent
buffer or subagents list belongs to, or an error elsewhere."
  (or (cond ((derived-mode-p 'agent-shell-mode) (current-buffer))
            ((buffer-live-p agent-shell-subagents--shell-buffer)
             agent-shell-subagents--shell-buffer)
            ((derived-mode-p 'agent-shell-viewport-view-mode
                             'agent-shell-viewport-edit-mode)
             (agent-shell-viewport--shell-buffer)))
      (user-error "Not in an agent-shell buffer")))

(defvar agent-shell-subagents-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'agent-shell-ui-forward-block)
    (define-key map (kbd "p") #'agent-shell-ui-backward-block)
    (define-key map (kbd "o") #'agent-shell-subagents-list-open)
    (define-key map (kbd "j") #'agent-shell-subagents-list-jump)
    (define-key map (kbd "x") #'agent-shell-subagents-list-stop)
    (define-key map (kbd "g") #'agent-shell-subagents-list-refresh)
    (define-key map (kbd "q") #'agent-shell-subagents-list-quit)
    map)
  "Keymap for `agent-shell-subagents-list-mode'.")

(define-derived-mode agent-shell-subagents-list-mode special-mode "Agent Shell Subagents"
  "Major mode listing one shell's native subagents and background tasks.

Running rows come first.  Finished ones collect under a folded
\"Finished\" group, kept for the whole session.  Unfold a row to see
its task.

\{agent-shell-subagents-list-mode-map}"
  (agent-shell-ui-mode +1)
  (setq-local header-line-format
              '(:eval (agent-shell-subagents--list-header-line))))

(defun agent-shell-subagents--list-header-line ()
  "Return the subagents list's header line: whose list, and its keys."
  (concat
   (when (buffer-live-p agent-shell-subagents--shell-buffer)
     (concat (buffer-name agent-shell-subagents--shell-buffer) "  "))
   (propertize "o open  j jump to spawn  x stop  g refresh  q quit"
               'face 'agent-shell-secondary)))

;;;###autoload
(defun agent-shell-subagents ()
  "List the current shell's native subagents and background tasks.

Works from the shell, its viewport, and its subagent buffers.  See
`agent-shell-subagents-list-mode' for what the list offers."
  (interactive)
  (let* ((origin (current-buffer))
         (shell-buffer (agent-shell-subagents--current-shell))
         (state (or (agent-shell-subagents--shell-state shell-buffer)
                    (user-error "The shell is gone")))
         (buffer (or (agent-shell-subagents--list-buffer state)
                     (get-buffer-create (agent-shell-subagents--list-buffer-name state)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-shell-subagents-list-mode)
        (agent-shell-subagents-list-mode))
      (setq agent-shell-subagents--shell-buffer shell-buffer
            agent-shell-subagents-list--origin origin
            default-directory (buffer-local-value 'default-directory shell-buffer))
      (agent-shell-subagents--render-list state))
    (pop-to-buffer buffer)))

(defun agent-shell-subagents--list-entry-at-point ()
  "Return the list entry of the row at point, or signal a user error."
  (or (map-elt agent-shell-subagents-list--entries
               (map-elt (get-text-property (point) 'agent-shell-ui-state)
                        :qualified-id))
      (user-error "No subagent at point")))

(defun agent-shell-subagents--list-state ()
  "Return the state of the shell this subagents list belongs to."
  (or (and (buffer-live-p agent-shell-subagents--shell-buffer)
           (agent-shell-subagents--shell-state agent-shell-subagents--shell-buffer))
      (user-error "The shell this list belongs to is gone")))

(defun agent-shell-subagents-list-quit ()
  "Quit this subagents list and switch back to where it was opened.

Falls back to `quit-window' if that buffer is gone."
  (declare (modes agent-shell-subagents-list-mode))
  (interactive)
  (bury-buffer)
  (if (buffer-live-p agent-shell-subagents-list--origin)
      (switch-to-buffer agent-shell-subagents-list--origin)
    (quit-window)))

(defun agent-shell-subagents-list-refresh ()
  "Redraw this subagents list."
  (declare (modes agent-shell-subagents-list-mode))
  (interactive)
  (agent-shell-subagents--render-list (agent-shell-subagents--list-state)))

(defun agent-shell-subagents-list-open ()
  "Open the buffer of the subagent at point."
  (declare (modes agent-shell-subagents-list-mode))
  (interactive)
  (let ((entry (agent-shell-subagents--list-entry-at-point)))
    (unless (eq (map-elt entry :kind) 'subagent)
      (user-error "Background tasks have no buffer of their own"))
    (agent-shell-subagents-display (agent-shell-subagents--list-state)
                                   (map-elt entry :id))))

(defun agent-shell-subagents--page-index (position)
  "Return the one-based page of the current shell that POSITION is on.
Zero when POSITION is above the first prompt."
  (save-excursion
    (goto-char (point-min))
    (let ((index 0))
      (while (and (shell-maker--re-search-forward-prompt comint-prompt-regexp)
                  (< (match-beginning 0) position))
        (setq index (1+ index)))
      index)))

(defun agent-shell-subagents-list-jump ()
  "Show where the subagent at point was spawned.

That is its row in the shell, or in the spawning subagent's buffer for
a nested one.  Opened from the viewport, the list pages the viewport to
the interaction the row is on instead."
  (declare (modes agent-shell-subagents-list-mode))
  (interactive)
  (let* ((entry (agent-shell-subagents--list-entry-at-point))
         (state (agent-shell-subagents--list-state))
         (origin agent-shell-subagents-list--origin)
         (row (or (and (eq (map-elt entry :kind) 'subagent)
                       (agent-shell--subagent-row-at state (map-elt entry :id)))
                  (user-error "Background tasks have no spawn row")))
         (parent (map-elt row :parent))
         (target (if (stringp parent)
                     (agent-shell-subagents--buffer :state state :session-id parent :create t)
                   (map-elt state :buffer)))
         (qualified-id (format "%s-%s" (map-elt row :namespace-id) (map-elt row :block-id)))
         (position (with-current-buffer target
                     (save-excursion
                       (agent-shell-subagents--goto-fragment qualified-id)))))
    (unless position
      (user-error "The row for %s is no longer in its buffer"
                  (or (map-elt row :name) (map-elt entry :id))))
    (if (and (not (stringp parent))
             (buffer-live-p origin)
             (with-current-buffer origin
               (derived-mode-p 'agent-shell-viewport-view-mode
                               'agent-shell-viewport-edit-mode)))
        (let ((page (with-current-buffer target
                      (agent-shell-subagents--page-index position))))
          (pop-to-buffer origin)
          (when (derived-mode-p 'agent-shell-viewport-edit-mode)
            (agent-shell-viewport-previous-page))
          (let ((current (map-elt (agent-shell-viewport--position :force-refresh t)
                                  :current)))
            (when (and (> page 0) current (/= page current))
              (agent-shell-viewport-next-page :n (- page current) :start-at-top t)))
          (agent-shell-subagents--goto-fragment qualified-id))
      (pop-to-buffer target)
      (goto-char position))))

(defun agent-shell-subagents-list-stop ()
  "Stop the subagent or background task at point.

Only offered for what the agent says a client may stop: a subagent
whose spawn advertised `capabilities.cancel', or an async task with
`canStop'.  Its row updates once the agent reports it stopped."
  (declare (modes agent-shell-subagents-list-mode))
  (interactive)
  (let* ((entry (agent-shell-subagents--list-entry-at-point))
         (state (agent-shell-subagents--list-state))
         (id (map-elt entry :id)))
    (when (map-elt entry :finished)
      (user-error "Already finished"))
    (pcase (map-elt entry :kind)
      ('subagent
       (let ((subagent (agent-shell--native-subagent state id)))
         (unless (eq (map-nested-elt subagent '(:capabilities cancel)) t)
           (user-error "The agent does not let clients stop %s"
                       (or (map-elt subagent :name) "this subagent")))
         (acp-send-notification
          :client (map-elt state :client)
          :notification (acp-make-session-cancel-notification
                         :session-id id :reason "User cancelled"))
         (message "Asked the agent to stop %s" (or (map-elt subagent :name) id))))
      ('async-task
       (unless (map-elt (agent-shell--async-task state id) :can-stop)
         (user-error "The agent does not let clients stop this task"))
       (agent-shell--send-async-task-stop state id)))))

;;;###autoload
(defun agent-shell-subagents-switch ()
  "Open one of the current shell's subagent buffers, picked by name."
  (interactive)
  (let* ((shell-buffer (agent-shell-subagents--current-shell))
         (state (or (agent-shell-subagents--shell-state shell-buffer)
                    (user-error "The shell is gone")))
         (choices (mapcar (lambda (entry)
                            (let ((subagent (cdr entry)))
                              (cons (format "%s (%s) %s"
                                            (or (map-elt subagent :name) "Unnamed")
                                            (or (map-elt subagent :state) "running")
                                            (car entry))
                                    (car entry))))
                          (reverse (map-elt state :native-subagents)))))
    (unless choices
      (user-error "No subagents in this session"))
    (agent-shell-subagents-display
     state (cdr (assoc (completing-read "Subagent: " choices nil t) choices)))))

;;; Transcripts

(defun agent-shell-subagents--file-slug (text)
  "Return TEXT reduced to a lowercase, dash-separated file name part.

For example:

  (agent-shell-subagents--file-slug \"Survey subagent UI!\")
  ;; => \"survey-subagent-ui\"

Returns \"subagent\" when nothing usable is left, and keeps at most 40
characters so a long task description does not become a long path."
  (let ((slug (string-trim
               (replace-regexp-in-string
                "[^[:alnum:]]+" "-" (downcase (or text "")))
               "-+" "-+")))
    (if (string-empty-p slug)
        "subagent"
      (string-trim-right (truncate-string-to-width slug 40) "-+"))))

(defun agent-shell-subagents--transcript-directory (parent-file parent-is-subagent)
  "Return the directory subagent transcripts live in, given PARENT-FILE.

The root's subagents all go to one directory named after the root
transcript, so they sit beside it the way Claude Code keeps
`<session>/subagents/'.  A nested subagent goes to the same directory
as the subagent that spawned it (PARENT-IS-SUBAGENT non-nil), so the
tree stays one level deep whatever the nesting.

  (agent-shell-subagents--transcript-directory \"/t/2026-09-24.md\" nil)
  ;; => \"/t/2026-09-24/subagents/\"

  (agent-shell-subagents--transcript-directory
   \"/t/2026-09-24/subagents/researcher-1.md\" t)
  ;; => \"/t/2026-09-24/subagents/\""
  (if parent-is-subagent
      (file-name-directory parent-file)
    (file-name-as-directory
     (expand-file-name "subagents" (file-name-sans-extension parent-file)))))

(cl-defun agent-shell-subagents--transcript-file-path (&key parent-file parent-is-subagent name session-id)
  "Return the transcript path for subagent SESSION-ID named NAME.

PARENT-FILE and PARENT-IS-SUBAGENT locate the directory (see
`agent-shell-subagents--transcript-directory').  The file name joins
NAME with SESSION-ID, so two subagents sharing a name do not share a
file.  Only the last 24 characters of a long id are kept.

  (agent-shell-subagents--transcript-file-path
   :parent-file \"/t/2026-09-24.md\"
   :name \"Researcher\"
   :session-id \"ab4463c3e484421df\")
  ;; => \"/t/2026-09-24/subagents/researcher-ab4463c3e484421df.md\""
  (let ((id (agent-shell-subagents--file-slug session-id)))
    (expand-file-name
     (format "%s-%s.md"
             (agent-shell-subagents--file-slug name)
             (if (> (length id) 24)
                 (substring id (- (length id) 24))
               id))
     (agent-shell-subagents--transcript-directory parent-file parent-is-subagent))))

(defun agent-shell-subagents--transcript-link (from-file to-file)
  "Return a markdown link from FROM-FILE's directory to TO-FILE.

  (agent-shell-subagents--transcript-link
   \"/t/2026-09-24.md\" \"/t/2026-09-24/subagents/researcher-1.md\")
  ;; => \"[researcher-1.md](2026-09-24/subagents/researcher-1.md)\""
  (format "[%s](%s)"
          (file-name-nondirectory to-file)
          (file-relative-name to-file (file-name-directory from-file))))

(cl-defun agent-shell-subagents--transcript-header (&key name task session-id parent-session-id parent-file file)
  "Return the header a subagent's transcript FILE starts with.

Plays the part Claude Code's `agent-<id>.meta.json' does: NAME, TASK,
SESSION-ID and PARENT-SESSION-ID say what the subagent was and who
spawned it, and the link back to PARENT-FILE makes the file navigable
on its own."
  (format "# Agent Shell Subagent Transcript

**Subagent:** %s
**Started:** %s
**Session ID:** %s
**Parent Session ID:** %s
**Parent Transcript:** %s

## Task

%s

---

"
          (or name "Unnamed")
          (format-time-string "%F %T")
          session-id
          (or parent-session-id "unknown")
          (agent-shell-subagents--transcript-link file parent-file)
          (string-trim (or task ""))))

(cl-defun agent-shell-subagents--transcript-spawn-entry (&key name task file parent-file)
  "Return the entry a parent transcript records a spawned subagent with.

NAME and TASK describe the subagent; the link points from PARENT-FILE to
its own transcript at FILE, when it has one.  For a subagent named
\"Researcher\" spawned from \"/t/a.md\", the entry reads:

  ## Subagent: Researcher (2026-09-24 15:00:00)

  **Transcript:** [researcher-1.md](a/subagents/researcher-1.md)

  Find prior art"
  (concat
   (format "\n\n## Subagent: %s (%s)\n\n"
           (or name "Unnamed")
           (format-time-string "%F %T"))
   (when (and file parent-file)
     (format "**Transcript:** %s\n\n"
             (agent-shell-subagents--transcript-link parent-file file)))
   (unless (string-empty-p (string-trim (or task "")))
     (format "%s\n\n" (string-trim task)))))

(cl-defun agent-shell-subagents--transcript-state-entry (&key name state)
  "Return the entry recording that subagent NAME reached STATE.

  (agent-shell-subagents--transcript-state-entry
   :name \"Researcher\" :state \"completed\")
  ;; => \"\\n\\n## Subagent completed: Researcher (2026-09-24 15:04:12)\\n\\n\""
  (format "\n\n## Subagent %s: %s (%s)\n\n"
          state
          (or name "Unnamed")
          (format-time-string "%F %T")))

(provide 'agent-shell-subagents)

;;; agent-shell-subagents.el ends here
