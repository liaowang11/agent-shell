;;; agent-shell-omp.el --- Oh My Pi (omp) agent configurations -*- lexical-binding: t; -*-

;; Author: Sam Kleinman https://tychoish.com
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
;; This file includes Oh My Pi (omp) agent-specific configurations for
;; agent-shell.
;;
;; This integration calls `omp acp' as the ACP client command.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'color)
(require 'ansi-color)
(require 'json)
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--air-client-capabilities-meta "agent-shell")
(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-welcome-message "agent-shell")
(declare-function agent-shell--interpolate-gradient "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(defcustom agent-shell-omp-acp-command
  '("omp" "acp")
  "Command and parameters for the Oh My Pi (omp) ACP client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-omp-environment
  nil
  "Environment variables for the Oh My Pi (omp) client.

This should be a list of environment variables to be used when
starting the omp client process.

Example usage:

  (setq agent-shell-omp-environment
        (agent-shell-make-environment-variables
         \"<KEY>\" \"<VALUE>\"))"
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-omp-make-agent-config ()
  "Create an Oh My Pi (omp) agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'omp
   :mode-line-name "OMP"
   :buffer-name "OMP"
   :shell-prompt "OMP> "
   :shell-prompt-regexp "OMP> "
   :welcome-function #'agent-shell-omp--welcome-message
   ;; omp relays task subagents in the subagents RFD form once it has the
   ;; RFD; an older omp answers with AIR's `subagent_spawned' draft.
   ;; `session.notices' (session notices RFD) gets omp's notices -- idle
   ;; recap, retries, fallbacks, config warnings -- as `notice' updates
   ;; instead of bold agent message lines; omp sends the idle recap only
   ;; to a client that declares it.
   :client-capabilities '((subagents . ())
                          (session . ((notices . ()))))
   ;; omp gates background tasks, the session index and the fork point
   ;; on the client naming them here, as claude-agent-acp does.
   ;; `stateUpdate' asks it for ACP v2's `state_update' (see
   ;; `state-changed' in `agent-shell-subscribe-to').
   :initialize-meta (append (agent-shell--air-client-capabilities-meta)
                            '((stateUpdate . t)))
   :client-maker (lambda (buffer)
                   (agent-shell-omp-make-client :buffer buffer))
   :install-instructions "See https://github.com/can1357/oh-my-pi omp (Oh My Pi) installation."))

(defun agent-shell-omp-start ()
  "Start an interactive Oh My Pi (omp) agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-omp-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-omp-make-client (&key buffer)
  "Create an Oh My Pi (omp) ACP client with BUFFER as context."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (agent-shell--make-acp-client :command (car agent-shell-omp-acp-command)
                                :command-params (cdr agent-shell-omp-acp-command)
                                :environment-variables agent-shell-omp-environment
                                :context-buffer buffer))

(defun agent-shell-omp--welcome-message (config)
  "Return OMP welcome message using `shell-maker' CONFIG."
  (concat "\n\n"
          (agent-shell--indent-string 4 (agent-shell-omp--ascii-art))
          "\n\n"
          (agent-shell-welcome-message config)))

(defun agent-shell-omp--ascii-art ()
  "Oh My Pi ASCII art with gradient coloring."
  (let* ((text (string-trim "
  ██████╗ ██╗  ██╗     ███╗   ███╗ ██╗   ██╗      ██████╗ ██╗
 ██╔═══██╗██║  ██║     ████╗ ████║ ╚██╗ ██╔╝     ██╔═══██╗██║
 ██║   ██║███████║     ██╔████╔██║  ╚████╔╝█████╗ ██████╔╝██║
 ██║   ██║██╔══██║     ██║╚██╔╝██║   ╚██╔╝ ╚════╝ ██╔═══╝ ██║
 ╚██████╔╝██║  ██║     ██║ ╚═╝ ██║    ██║         ██║     ██║
  ╚═════╝ ╚═╝  ╚═╝     ╚═╝     ╚═╝    ╚═╝         ╚═╝     ╚═╝
" "\n"))
         (is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (gradient-colors (if is-dark
                              '("#7aa2f7" "#bb9af7" "#f7768e")
                            '("#3d6fd6" "#9b6dce" "#d6456e")))
         (lines (split-string text "\n"))
         (result ""))
    (seq-do (lambda (line)
              (let ((line-length (length line))
                    (propertized-line ""))
                (dotimes (i line-length)
                  (let* ((char (substring line i (1+ i)))
                         (progress (/ (float i) (max line-length 1)))
                         (color (agent-shell--interpolate-gradient gradient-colors progress)))
                    (setq propertized-line
                          (concat propertized-line
                                  (propertize char 'font-lock-face
                                              `(:foreground ,color :inherit fixed-pitch))))))
                (setq result (concat result propertized-line "\n"))))
            lines)
    (string-trim-right result)))

;;; Figures

;; omp's TUI draws ```mermaid fences as text art with its native
;; renderer; `omp figure' exposes that renderer on the command line.
;; These functions run it from `agent-shell-markdown-render-functions'
;; in omp shells, so a fence looks the same here as in the TUI.
;;
;; The render hook is synchronous, so a fence omp has not drawn yet is
;; claimed: frozen in place (source visible) while `omp figure' runs in
;; the background, then replaced by the art when it answers.  Answers are
;; cached by source and width, so a replay or a re-render of the same
;; fence draws synchronously.  A fence omp cannot draw is styled as an
;; ordinary code block.

(declare-function agent-shell-markdown--carry-properties "agent-shell-markdown")
(declare-function agent-shell-markdown--watermark-markers "agent-shell-markdown")
(declare-function agent-shell-markdown--restore-watermarks "agent-shell-markdown")
(declare-function agent-shell-markdown--style-source-blocks "agent-shell-markdown")
(declare-function agent-shell-ui--block-range "agent-shell-ui")
(declare-function agent-shell-ui--body-range "agent-shell-ui")
(declare-function agent-shell-ui--apply-trailing-whitespace-invisible "agent-shell-ui")
(declare-function agent-shell-get-config "agent-shell")
(defvar agent-shell-markdown-render-functions)
(defvar agent-shell-highlight-blocks)

(defcustom agent-shell-omp-figures t
  "Whether omp shells draw figures: mermaid, svg and LaTeX math.
Mermaid and math go through `omp figure'.  Turn on `acp.renderMermaid'
and `acp.renderSvg' in omp's settings too, or the agent is not told it
may draw diagrams."
  :type 'boolean
  :group 'agent-shell)

(defcustom agent-shell-omp-figure-command '("omp" "figure")
  "Command that draws figures, run locally even for a remote shell.
The figure kind (\"mermaid\" or \"math\") and its flags are appended."
  :type '(repeat string)
  :group 'agent-shell)

(defface agent-shell-omp-figure
  '((t :inherit fixed-pitch))
  "Face for figure art drawn by `omp figure'.
Box-drawing art only lines up in a monospaced font."
  :group 'agent-shell)

(defvar agent-shell-omp--figure-request-function
  #'agent-shell-omp--figure-request-process
  "Function that draws a figure: (KIND INPUT WIDTH CALLBACK).
CALLBACK is called once with the drawn text, or nil on failure.")

(defvar agent-shell-omp--figure-cache (make-hash-table :test 'equal)
  "Drawn figures by (KIND WIDTH INPUT): the text, or `failed'.")

(defvar agent-shell-omp--figure-waiters (make-hash-table :test 'equal)
  "Claims waiting for a figure, by (KIND WIDTH INPUT).
Each claim is (START . END), markers in the claiming buffer.")

(defvar-local agent-shell-omp--figure-requests-allowed nil
  "Non-nil in buffers that may start `omp figure' (live omp shells).
Elsewhere (work buffers, viewports) only cached figures are drawn.")

(defconst agent-shell-omp--figure-cache-limit 256
  "Cached figures kept before the cache is cleared.")

(defun agent-shell-omp-clear-figure-cache ()
  "Forget drawn and failed figures, e.g. after fixing the figure command.
Failures are remembered so a diagram omp cannot draw is not retried on
every chunk; figures rendered from now on ask omp again."
  (interactive)
  (clrhash agent-shell-omp--figure-cache)
  (message "agent-shell-omp: figure cache cleared"))

(defun agent-shell-omp--setup-figures ()
  "Draw omp figures in the current shell when it runs omp."
  (when (and agent-shell-omp-figures
             (eq (map-elt (agent-shell-get-config (current-buffer)) :identifier)
                 'omp))
    (setq-local agent-shell-omp--figure-requests-allowed t)
    (add-hook 'agent-shell-markdown-render-functions
              #'agent-shell-omp--render-figures nil t)
    (add-hook 'window-size-change-functions
              #'agent-shell-omp--figures-on-resize nil t)))

(add-hook 'agent-shell-mode-hook #'agent-shell-omp--setup-figures)

(defun agent-shell-omp--figure-width (pos)
  "Columns a figure starting at POS may use, or nil when not shown."
  (when-let* ((window (get-buffer-window (current-buffer) t)))
    (let ((prefix (get-text-property pos 'line-prefix)))
      (max 20 (- (window-max-chars-per-line window 'agent-shell-omp-figure)
                 (if (stringp prefix) (string-width prefix) 0)
                 1)))))

(defun agent-shell-omp--render-figures (context)
  "Draw the figures in the current region, given CONTEXT.
Closed ```mermaid fences become art, ```svg fences images, and LaTeX
math (```math fences, `$$' blocks, inline spans) Unicode text.  For
`agent-shell-markdown-render-functions'."
  (let ((math nil))
    (dolist (block (map-elt context :source-blocks))
      (when-let* (((map-elt block :complete))
                  (language (map-elt block :language))
                  ((member language '("mermaid" "svg" "math" "latex" "tex")))
                  (start (save-excursion
                           ;; The block range starts at the blank lines above
                           ;; the fence; keep them.
                           (goto-char (map-nested-elt block '(:block :start)))
                           (skip-chars-forward "\n")
                           (point)))
                  (end (map-nested-elt block '(:block :end)))
                  ((not (get-text-property start 'agent-shell-markdown-frozen))))
        (pcase language
          ("svg"
           (when (agent-shell-omp--svg-displayable-p)
             (agent-shell-omp--draw-svg start end (map-elt block :body))))
          ("mermaid"
           (when-let* ((key (agent-shell-omp--claim-figure
                             :kind 'mermaid :input (map-elt block :body)
                             :start start :end end
                             :source (buffer-substring-no-properties start end))))
             (agent-shell-omp--request-figure key)))
          (_ (push (list (copy-marker start) (copy-marker end) t (map-elt block :body))
                   math)))))
    (let* ((scan (agent-shell-omp--math-scan
                  (append (mapcar (lambda (block)
                                    (cons (map-nested-elt block '(:block :start))
                                          (map-nested-elt block '(:block :end))))
                                  (map-elt context :source-blocks))
                          (map-elt context :inline-code-ranges))))
           (keys nil))
      (pcase-dolist (`(,start ,end ,display ,src)
                     (append (nreverse math)
                             (mapcar (lambda (claim)
                                       (cons (copy-marker (nth 0 claim))
                                             (cons (copy-marker (nth 1 claim))
                                                   (nthcdr 2 claim))))
                                     (car scan))))
        (when-let* (((< start end))
                    (key (agent-shell-omp--claim-figure
                          :kind 'math :input (cons display src)
                          :start start :end end
                          :source (buffer-substring-no-properties start end))))
          (push key keys))
        (set-marker start nil)
        (set-marker end nil))
      (when keys
        (agent-shell-omp--request-math (nreverse keys)))
      ;; Hold an open span only while more text can still close it.
      (when (and (cdr scan) (not (map-elt context :complete)))
        (list (cons :watermark (cdr scan)))))))

(defun agent-shell-omp--escaped-p (pos)
  "Whether an odd run of backslashes ends just before POS."
  (let ((count 0))
    (while (and (> (- pos count) (point-min))
                (eq (char-before (- pos count)) ?\\))
      (setq count (1+ count)))
    (cl-oddp count)))

(defconst agent-shell-omp--math-hint "[\\^_{}]"
  "Text an unclosed opener other than an own-line block needs to hold
rendering: \\[docs] stays an escaped bracket until LaTeX shows up.")

(defun agent-shell-omp--math-scan (skip)
  "Find the closed math spans of the current region, outside SKIP.
SKIP lists (START . END) ranges (code).  Follows omp's grammar
\(`math-delimiters.ts'): own-line `$$' / `\\[' blocks, inline `$$',
`\\[', `\\(' and `$' spans, the last with pandoc's anti-currency rules.
Returns (CLAIMS . WATERMARK): CLAIMS are (START END DISPLAY SRC) in
order, WATERMARK the opener of a span that may still close, or nil."
  (let ((claims nil)
        (watermark nil)
        (pos (point-min)))
    (save-excursion
      (catch 'stop
        (while (and (< pos (point-max))
                    (progn (goto-char pos)
                           (re-search-forward "\\$\\|\\\\[[(]" nil t)))
          (let* ((at (match-beginning 0))
                 (range (seq-find (lambda (range)
                                    (and (<= (car range) at) (< at (cdr range))))
                                  skip)))
            (cond
             (range (setq pos (max (1+ at) (cdr range))))
             ((get-text-property at 'agent-shell-markdown-frozen)
              (setq pos (next-single-property-change
                         at 'agent-shell-markdown-frozen nil (point-max))))
             ((agent-shell-omp--escaped-p at) (setq pos (1+ at)))
             (t
              (pcase (or (agent-shell-omp--math-block-at at)
                         (agent-shell-omp--math-span-at at))
                (`(open . ,from) (setq watermark from) (throw 'stop nil))
                (`(,start ,end ,display ,src)
                 (if (or (seq-some (lambda (range)
                                     (and (< (car range) end) (< start (cdr range))))
                                   skip)
                         (text-property-not-all start end
                                                'agent-shell-markdown-frozen nil))
                     ;; A span reaching into code or rendered text is prose.
                     (setq pos (1+ at))
                   (push (list start end display src) claims)
                   (setq pos end)))
                (_ (setq pos (1+ at))))))))))
    (cons (nreverse claims) watermark)))

(defun agent-shell-omp--math-block-at (at)
  "The own-line display block whose opener is at AT.
Returns (START END t SRC), (open . LINE-START) while its closer line
is missing, or nil when AT opens no block."
  (save-excursion
    (goto-char at)
    (let ((line-start (line-beginning-position)))
      (when (and (<= (- at line-start) 3)
                 (string-blank-p (buffer-substring-no-properties line-start at))
                 (looking-at "\\(\\$\\$\\|\\\\\\[\\)[ \t]*\n"))
        (let ((body-start (match-end 0))
              (closer (if (equal (match-string 1) "$$") "$$" "\\]")))
          (goto-char body-start)
          (if (re-search-forward (concat "^ \\{0,3\\}" (regexp-quote closer)
                                         "[ \t]*\\(?:\n\\|\\'\\)")
                                 nil t)
              (let ((src (buffer-substring-no-properties
                          body-start (max body-start (1- (match-beginning 0))))))
                (unless (string-blank-p src)
                  (list line-start (match-end 0) t src)))
            (cons 'open line-start)))))))

(defun agent-shell-omp--math-span-at (at)
  "The inline math span opened at AT.
Returns (START END DISPLAY SRC), (open . AT) for a span that may still
close, or nil.  A display span alone on its line renders as a block."
  (let ((first (char-after at))
        (second (char-after (1+ at))))
    (cond
     ((and (eq first ?$) (eq second ?$)) (agent-shell-omp--math-closed-span at "$$" t))
     ((eq first ?$) (agent-shell-omp--math-dollar-span at))
     ((eq second ?\() (agent-shell-omp--math-closed-span at "\\)" nil))
     ((eq second ?\[) (agent-shell-omp--math-closed-span at "\\]" t)))))

(defun agent-shell-omp--math-closed-span (at closer display)
  "The span opened at AT that ends at the next unescaped CLOSER.
DISPLAY marks the display forms; see `agent-shell-omp--math-span-at'."
  (save-excursion
    (let ((body-start (+ at 2))
          (close nil))
      (goto-char body-start)
      (while (and (not close) (search-forward closer nil t))
        (unless (agent-shell-omp--escaped-p (match-beginning 0))
          (setq close (match-beginning 0))))
      (cond
       (close
        (let ((src (buffer-substring-no-properties body-start close))
              (end (+ close 2)))
          (unless (and (equal closer "$$") (string-blank-p src))
            (list at end
                  (and display
                       (save-excursion
                         (goto-char at)
                         (string-blank-p (buffer-substring-no-properties
                                          (line-beginning-position) at)))
                       (save-excursion
                         (goto-char end)
                         (string-blank-p (buffer-substring-no-properties
                                          end (line-end-position)))))
                  src))))
       ((progn (goto-char body-start)
               (re-search-forward agent-shell-omp--math-hint nil t))
        (cons 'open at))))))

(defun agent-shell-omp--math-dollar-span (at)
  "The `$...$' span opened at AT, by pandoc's anti-currency rules.
The opener must not be followed by whitespace, the closer must not be
preceded by whitespace nor followed by a digit, and the span stays on
one line.  A closer ending the region waits for what follows it."
  (let ((after (char-after (1+ at))))
    (unless (or (null after) (memq after '(?\s ?\t ?\n ?$)))
      (save-excursion
        (goto-char (1+ at))
        (catch 'done
          (while (< (point) (point-max))
            (pcase (char-after)
              (?\\ (forward-char (min 2 (- (point-max) (point)))))
              (?\n (throw 'done nil))
              (?$
               (let ((before (char-before))
                     (next (char-after (1+ (point)))))
                 (cond
                  ((memq before '(?\s ?\t)) (throw 'done nil))
                  ((null next) (throw 'done nil))
                  ((<= ?0 next ?9) (forward-char 1))
                  (t (throw 'done
                            (list at (1+ (point)) nil
                                  (buffer-substring-no-properties (1+ at) (point))))))))
              (_ (forward-char 1))))
          nil)))))

(defun agent-shell-omp--svg-displayable-p ()
  "Whether this frame can show SVG images."
  (and (display-graphic-p) (image-type-available-p 'svg)))

(defun agent-shell-omp--face-color (face attribute)
  "Return FACE's ATTRIBUTE color as #rrggbb, or nil when unspecified."
  (when-let* ((color (face-attribute face attribute nil t))
              ((stringp color))
              (rgb (color-name-to-rgb color)))
    (apply #'color-rgb-to-hex (append rgb '(2)))))

(defun agent-shell-omp--svg-palette ()
  "Colors for omp's figure tokens (`var(--fg)', ...), from the theme.
The tokens are those omp's prompt gives the agent; c1..c6 are a series."
  (let ((fg (or (agent-shell-omp--face-color 'default :foreground) "#000000")))
    (seq-filter
     #'cdr
     `(("fg" . ,fg)
       ("muted" . ,(agent-shell-omp--face-color 'shadow :foreground))
       ("border" . ,(or (agent-shell-omp--face-color 'vertical-border :foreground)
                        (agent-shell-omp--face-color 'shadow :foreground)))
       ("surface" . ,(agent-shell-omp--face-color 'highlight :background))
       ("accent" . ,(agent-shell-omp--face-color 'link :foreground))
       ("success" . ,(agent-shell-omp--face-color 'success :foreground))
       ("warning" . ,(agent-shell-omp--face-color 'warning :foreground))
       ("error" . ,(agent-shell-omp--face-color 'error :foreground))
       ("c1" . ,(agent-shell-omp--face-color 'font-lock-function-name-face :foreground))
       ("c2" . ,(agent-shell-omp--face-color 'font-lock-string-face :foreground))
       ("c3" . ,(agent-shell-omp--face-color 'font-lock-keyword-face :foreground))
       ("c4" . ,(agent-shell-omp--face-color 'font-lock-type-face :foreground))
       ("c5" . ,(agent-shell-omp--face-color 'font-lock-constant-face :foreground))
       ("c6" . ,(agent-shell-omp--face-color 'font-lock-builtin-face :foreground))))))

(defun agent-shell-omp--prepare-svg (svg palette)
  "Return SVG ready for librsvg in PALETTE's colors.
Mirrors omp's `prepareSvg': `var(--name[, fallback])' becomes the
PALETTE color (an unknown name takes its fallback, else fg), and the
root gets `color' (for `currentColor'), a sans-serif font and `xmlns'.
Also drops what a figure must not carry: scripts, `<foreignObject>',
and `href's to anything but a fragment or inline data."
  (let* ((fg (or (map-elt palette "fg") "currentColor"))
         (case-fold-search t)
         (svg (replace-regexp-in-string
               "<script\\b[^>]*\\(?:/>\\|>\\(?:.\\|\n\\)*?</script>\\)" "" svg t t))
         (svg (replace-regexp-in-string
               "<foreignObject\\b[^>]*\\(?:/>\\|>\\(?:.\\|\n\\)*?</foreignObject>\\)" "" svg t t))
         (svg (replace-regexp-in-string
               "\\s-\\(?:xlink:\\)?href\\s-*=\\s-*\\(\"[^\"]*\"\\|'[^']*'\\)"
               (lambda (attribute)
                 (let ((value (substring (match-string 1 attribute) 1 -1)))
                   (if (string-match-p "\\`\\(?:#\\|data:\\)" value) attribute "")))
               svg t t))
         (svg (replace-regexp-in-string
               "var(\\s-*--\\([[:alnum:]_-]+\\)\\s-*\\(?:,\\s-*\\(\\(?:[^()]\\|([^()]*)\\)*\\)\\)?)"
               (lambda (reference)
                 (let ((name (match-string 1 reference))
                       (fallback (match-string 2 reference)))
                   (or (map-elt palette name)
                       (and fallback (not (string-blank-p fallback))
                            (string-trim fallback))
                       fg)))
               svg t t)))
    (if (string-match "<\\(?:[[:alnum:]._-]+:\\)?svg\\(?:[[:space:]/>]\\)" svg)
        (let* ((tag-start (match-beginning 0))
               (tag-end (or (string-search ">" svg tag-start) (length svg)))
               (tag (substring svg tag-start tag-end))
               (added (concat
                       (unless (string-match-p "\\s-color\\s-*=" tag)
                         (format " color=\"%s\"" fg))
                       (unless (string-match-p "\\s-font-family\\s-*=" tag)
                         " font-family=\"sans-serif\"")
                       (unless (string-match-p "\\s-xmlns\\s-*=" tag)
                         " xmlns=\"http://www.w3.org/2000/svg\"")
                       (when (and (string-search "xlink:" svg)
                                  (not (string-match-p "\\s-xmlns:xlink\\s-*=" tag)))
                         " xmlns:xlink=\"http://www.w3.org/1999/xlink\""))))
          (string-match "\\`<[^[:space:]/>]+" tag)
          (concat (substring svg 0 tag-start)
                  (substring tag 0 (match-end 0)) added (substring tag (match-end 0))
                  (substring svg tag-end)))
      svg)))

(defun agent-shell-omp--draw-svg (start end svg)
  "Replace the ```svg fence at START..END with SVG drawn as an image."
  (let* ((source (buffer-substring-no-properties start end))
         (window (get-buffer-window (current-buffer) t))
         (image (create-image
                 (encode-coding-string
                  (agent-shell-omp--prepare-svg svg (agent-shell-omp--svg-palette))
                  'utf-8)
                 'svg t
                 :max-width (and window (window-body-width window t))))
         (inhibit-read-only t)
         (carried (agent-shell-markdown--carry-properties start))
         (placeholder (concat " " (if (eq (char-before end) ?\n) "\n" ""))))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert placeholder)
      (let ((placeholder-end (+ start (length placeholder))))
        (when carried
          (add-text-properties start placeholder-end carried))
        (put-text-property start (1+ start) 'display image)
        (add-text-properties
         start placeholder-end
         `(agent-shell-markdown-frozen t
                                       agent-shell-markdown-source ,source
                                       agent-shell-omp-figure (svg ,svg)
                                       agent-shell-non-trimmable t
                                       rear-nonsticky (display
                                                       agent-shell-markdown-frozen
                                                       agent-shell-markdown-source
                                                       agent-shell-omp-figure
                                                       agent-shell-non-trimmable)))))))

(cl-defun agent-shell-omp--claim-figure (&key kind input start end source)
  "Draw INPUT as a KIND figure over START..END, whose markdown is SOURCE.
Draws now when cached.  Otherwise, in a buffer that may ask omp,
freezes the region and registers it; returns the cache key when no
request for it is in flight yet, for the caller to send.  Math omp
does not draw is frozen as written, so LaTeX keeps its `_' and `*'."
  (let* ((width (unless (eq kind 'math) (agent-shell-omp--figure-width start)))
         (key (list kind width input))
         (cached (gethash key agent-shell-omp--figure-cache))
         (inhibit-read-only t))
    (cond
     ((stringp cached)
      (agent-shell-omp--swap-figure start end cached
                                    :kind kind :input input
                                    :source source :width width)
      nil)
     ((or (eq cached 'failed) (not agent-shell-omp--figure-requests-allowed))
      (when (eq kind 'math)
        (with-silent-modifications
          (add-text-properties start end '(agent-shell-markdown-frozen t))))
      nil)
     (t
      (with-silent-modifications
        (add-text-properties start end
                             `(agent-shell-markdown-frozen t
                                                           agent-shell-omp-figure-pending ,key
                                                           agent-shell-omp-figure-source ,source)))
      (let ((first (not (gethash key agent-shell-omp--figure-waiters))))
        (push (cons (copy-marker start) (copy-marker end))
              (gethash key agent-shell-omp--figure-waiters))
        (when first key))))))

(defun agent-shell-omp--request-figure (key)
  "Ask omp to draw the figure KEY names, one request."
  (funcall agent-shell-omp--figure-request-function
           (nth 0 key) (nth 2 key) (nth 1 key)
           (lambda (text) (agent-shell-omp--figure-arrived key text))))

(defun agent-shell-omp--request-math (keys)
  "Ask omp to render the math KEYS name, in one batch."
  (funcall agent-shell-omp--figure-request-function
           'math
           (mapconcat (lambda (key)
                        (json-encode `((src . ,(cdr (nth 2 key)))
                                       (display . ,(if (car (nth 2 key)) t :json-false)))))
                      keys "\n")
           nil
           (lambda (text)
             (let ((lines (and text (split-string text "\n" t))))
               (dolist (key keys)
                 (agent-shell-omp--figure-arrived
                  key
                  (when-let* ((line (pop lines))
                              (item (ignore-errors
                                      (json-parse-string line :object-type 'alist)))
                              (rendered (map-elt item 'text))
                              ((stringp rendered)))
                    rendered)))))))

(defun agent-shell-omp--figure-arrived (key text)
  "Store TEXT (nil on failure) for KEY and draw it over each claim."
  (when (>= (hash-table-count agent-shell-omp--figure-cache)
            agent-shell-omp--figure-cache-limit)
    (clrhash agent-shell-omp--figure-cache))
  (puthash key (or text 'failed) agent-shell-omp--figure-cache)
  (let ((claims (gethash key agent-shell-omp--figure-waiters)))
    (remhash key agent-shell-omp--figure-waiters)
    (pcase-dolist (`(,start . ,end) claims)
      (when-let* ((buffer (marker-buffer start)))
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (when (and (< start end)
                       (equal key (get-text-property
                                   start 'agent-shell-omp-figure-pending)))
              (condition-case err
                  (agent-shell-omp--settle-claim start end key text)
                (error (message "agent-shell-omp: drawing a figure failed: %S"
                                err)))))))
      (set-marker start nil)
      (set-marker end nil))))

(defun agent-shell-omp--settle-claim (start end key text)
  "Replace the claim START..END for KEY with TEXT, or style it as code."
  (let ((source (get-text-property start 'agent-shell-omp-figure-source))
        (inhibit-read-only t)
        (buffer-undo-list t)
        (modified (buffer-modified-p))
        (watermarks (agent-shell-markdown--watermark-markers)))
    (unwind-protect
        (save-excursion
          (cond
           (text
            (agent-shell-omp--swap-figure start end text
                                          :kind (nth 0 key) :input (nth 2 key)
                                          :source source :width (nth 1 key)))
           ((eq (nth 0 key) 'math)
            ;; Keep the LaTeX as written, still frozen.
            (remove-text-properties start end
                                    '(agent-shell-omp-figure-pending nil
                                                                     agent-shell-omp-figure-source nil)))
           (t
            ;; Hand the fence back to the source-block pass.
            (remove-text-properties start end
                                    '(agent-shell-markdown-frozen nil
                                                                  agent-shell-omp-figure-pending nil
                                                                  agent-shell-omp-figure-source nil))
            (save-restriction
              (narrow-to-region start end)
              (agent-shell-markdown--style-source-blocks
               :highlight-blocks agent-shell-highlight-blocks))))
          (agent-shell-omp--rehide-body-tail start))
      (agent-shell-markdown--restore-watermarks watermarks)
      (restore-buffer-modified-p modified))))

(cl-defun agent-shell-omp--swap-figure (start end text &key kind input source width)
  "Replace START..END with TEXT, the KIND figure of INPUT at WIDTH.
SOURCE is the markdown it stands for, returned by copying.  Keeps the
region's caller properties (fragment body, `read-only').  Block
figures take `agent-shell-omp-figure'; inline math keeps the prose
face.  SGR colors in TEXT (LaTeX `\\textcolor') become faces."
  (let* ((inhibit-read-only t)
         (inline (and (eq kind 'math) (not (car input))))
         (carried (agent-shell-markdown--carry-properties start))
         (art (agent-shell-omp--sgr-to-faces
               (concat (if inline text (string-trim-right text "\n"))
                       (if (eq (char-before end) ?\n) "\n" "")))))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert art)
      (let ((art-end (+ start (length art))))
        (when carried
          (add-text-properties start art-end carried))
        (remove-text-properties start art-end
                                '(agent-shell-omp-figure-pending nil
                                                                 agent-shell-omp-figure-source nil))
        (unless inline
          (add-face-text-property start art-end 'agent-shell-omp-figure t))
        ;; Mirror `face' onto `font-lock-face' so re-fontification keeps it.
        (let ((pos start))
          (while (< pos art-end)
            (let ((next (next-single-property-change pos 'face nil art-end)))
              (put-text-property pos next 'font-lock-face (get-text-property pos 'face))
              (setq pos next))))
        (add-text-properties
         start art-end
         `(fontified t
                     agent-shell-markdown-frozen t
                     agent-shell-markdown-source ,source
                     agent-shell-omp-figure (,kind ,input)
                     agent-shell-omp-figure-width ,width
                     agent-shell-non-trimmable t
                     rear-nonsticky (face font-lock-face
                                          agent-shell-markdown-frozen
                                          agent-shell-markdown-source
                                          agent-shell-omp-figure
                                          agent-shell-omp-figure-width
                                          agent-shell-non-trimmable)))))))

(defun agent-shell-omp--sgr-to-faces (text)
  "Return TEXT with its SGR color escapes turned into `face' properties."
  (if (not (string-search "\e[" text))
      text
    (let ((colored (ansi-color-apply text)))
      (let ((pos 0))
        (while (< pos (length colored))
          (let ((next (or (next-single-property-change pos 'font-lock-face colored)
                          (length colored))))
            (when-let* ((face (get-text-property pos 'font-lock-face colored)))
              (put-text-property pos next 'face face colored))
            (setq pos next))))
      colored)))

(defun agent-shell-omp--rehide-body-tail (pos)
  "Re-hide the trailing whitespace of the fragment body holding POS.
Swapping text at a body's end drops the `invisible' tail its renderer
keeps (see `agent-shell--render-markdown-body')."
  (when-let* (((eq (get-text-property pos 'agent-shell-ui-section) 'body))
              (block (agent-shell-ui--block-range :position pos))
              (body (agent-shell-ui--body-range (map-elt block :start)
                                                (map-elt block :end))))
    (agent-shell-ui--apply-trailing-whitespace-invisible
     (map-elt body :start) (map-elt body :end))))

(defun agent-shell-omp--figure-request-process (kind input width callback)
  "Run `agent-shell-omp-figure-command' for KIND on INPUT at WIDTH.
Calls CALLBACK with its output, or nil when it fails."
  (let* ((default-directory temporary-file-directory)
         (output (generate-new-buffer " *omp-figure*" t))
         (errors (generate-new-buffer " *omp-figure-stderr*" t))
         (done nil)
         (finish (lambda (text)
                   (unless done
                     (setq done t)
                     (when (buffer-live-p output) (kill-buffer output))
                     (when (buffer-live-p errors) (kill-buffer errors))
                     (funcall callback text)))))
    (condition-case err
        (let ((process
               (make-process
                :name "omp-figure"
                :command (append agent-shell-omp-figure-command
                                 (list (symbol-name kind))
                                 (when width
                                   (list "--width" (number-to-string width))))
                :buffer output
                :stderr errors
                :connection-type 'pipe
                :coding 'utf-8
                :noquery t
                :sentinel
                (lambda (process _event)
                  (unless (process-live-p process)
                    (funcall finish
                             (when (and (eq (process-status process) 'exit)
                                        (zerop (process-exit-status process))
                                        (buffer-live-p output))
                               (with-current-buffer output (buffer-string)))))))))
          (process-send-string process input)
          (process-send-eof process))
      (error
       (message "agent-shell-omp: cannot run %s: %s"
                (string-join agent-shell-omp-figure-command " ")
                (error-message-string err))
       (funcall finish nil)))))

(defvar-local agent-shell-omp--figure-resize-timer nil
  "Pending idle timer that redraws this buffer's figures for a new width.")

(defun agent-shell-omp--figures-on-resize (window)
  "Redraw WINDOW's figures for its new width once Emacs is idle.
On `window-size-change-functions', buffer-locally."
  (when (window-live-p window)
    (when (timerp agent-shell-omp--figure-resize-timer)
      (cancel-timer agent-shell-omp--figure-resize-timer))
    (setq agent-shell-omp--figure-resize-timer
          (run-with-idle-timer 0.2 nil #'agent-shell-omp--redraw-figures
                               (current-buffer)))))

(defun agent-shell-omp--redraw-figures (buffer)
  "Redraw BUFFER's figures drawn for another width."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq agent-shell-omp--figure-resize-timer nil)
      (save-restriction
        (widen)
        (let ((stale nil)
              (pos (point-min)))
          (while (setq pos (text-property-not-all pos (point-max)
                                                  'agent-shell-omp-figure nil))
            (let ((end (next-single-property-change
                        pos 'agent-shell-omp-figure nil (point-max))))
              (unless (or (not (eq 'mermaid (car (get-text-property
                                                   pos 'agent-shell-omp-figure))))
                          (eql (get-text-property pos 'agent-shell-omp-figure-width)
                               (agent-shell-omp--figure-width pos)))
                (push (list (copy-marker pos) (copy-marker end)
                            (get-text-property pos 'agent-shell-omp-figure)
                            (get-text-property pos 'agent-shell-markdown-source))
                      stale))
              (setq pos end)))
          (pcase-dolist (`(,start ,end (,kind ,input) ,source) stale)
            (let ((inhibit-read-only t)
                  (buffer-undo-list t)
                  (modified (buffer-modified-p))
                  (watermarks (agent-shell-markdown--watermark-markers)))
              (unwind-protect
                  ;; Clear the old art's tag so the claim treats it as new;
                  ;; a failed redraw keeps showing the old art.
                  (progn
                    (with-silent-modifications
                      (remove-text-properties start end '(agent-shell-omp-figure nil)))
                    (when-let* ((key (agent-shell-omp--claim-figure
                                      :kind kind :input input
                                      :start start :end end
                                      :source source)))
                      (agent-shell-omp--request-figure key)))
                (agent-shell-markdown--restore-watermarks watermarks)
                (restore-buffer-modified-p modified)))
            (set-marker start nil)
            (set-marker end nil)))))))

(provide 'agent-shell-omp)

;;; agent-shell-omp.el ends here
