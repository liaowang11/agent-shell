;;; agent-shell-omp-figure-tests.el --- Tests for omp figure rendering -*- lexical-binding: t; -*-

(require 'ert)
(require 'agent-shell)
(require 'agent-shell-omp)

;;; Code:

(defconst agent-shell-omp-figure-tests--art
  "┌───┐     ┌───┐\n│ A ├────►│ B │\n└───┘     └───┘\n"
  "Art the stub renderer returns for any mermaid source.")

(defvar agent-shell-omp-figure-tests--requests nil
  "Requests the stub renderer received, newest first: (KIND INPUT WIDTH CALLBACK).")

(defmacro agent-shell-omp-figure-tests--with-shell (&rest body)
  "Run BODY in a fragment buffer that renders omp figures.
Requests go to a stub that records them; BODY answers them with
`agent-shell-omp-figure-tests--answer'."
  (declare (indent 0) (debug t))
  `(let ((agent-shell-omp-figure-tests--requests nil)
         (agent-shell-omp--figure-cache (make-hash-table :test 'equal))
         (agent-shell-omp--figure-waiters (make-hash-table :test 'equal))
         (agent-shell-omp--figure-request-function
          (lambda (kind input width callback)
            (push (list kind input width callback)
                  agent-shell-omp-figure-tests--requests))))
     (with-temp-buffer
       (agent-shell-ui-mode 1)
       (setq-local agent-shell-omp--figure-requests-allowed t)
       (add-hook 'agent-shell-markdown-render-functions
                 #'agent-shell-omp--render-figures nil t)
       ,@body)))

(defun agent-shell-omp-figure-tests--stream (chunk &optional create)
  "Stream CHUNK into the test fragment and render its body as the shell does.
CREATE starts the fragment instead of appending to it."
  (let ((inhibit-read-only t))
    (agent-shell-ui-update-fragment
     (agent-shell-ui-make-fragment-model :block-id "msg" :body chunk)
     :append (not create) :expanded t)
    (let* ((block (agent-shell-ui--block-range
                   :position (text-property-not-all (point-min) (point-max)
                                                    'agent-shell-ui-state nil)))
           (body (agent-shell-ui--body-range (map-elt block :start)
                                             (map-elt block :end))))
      (save-restriction
        (narrow-to-region (map-elt body :start) (map-elt body :end))
        (agent-shell--render-markdown-body)))))

(defun agent-shell-omp-figure-tests--answer (text)
  "Answer the oldest pending stub request with TEXT (nil for a failure)."
  (let ((request (car (last agent-shell-omp-figure-tests--requests))))
    (setq agent-shell-omp-figure-tests--requests
          (butlast agent-shell-omp-figure-tests--requests))
    (funcall (nth 3 request) text)))

(defun agent-shell-omp-figure-tests--text ()
  "Return the visible text of the current buffer."
  (let ((text "")
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (next-single-char-property-change pos 'invisible nil (point-max))))
        (unless (get-char-property pos 'invisible)
          (setq text (concat text (buffer-substring-no-properties pos next))))
        (setq pos next)))
    text))

(ert-deftest agent-shell-omp-figure-mermaid-swaps-in-art-test ()
  "A closed mermaid fence is drawn as art once the renderer answers.
The art joins the fragment body, so later chunks still append to it,
and copying the art gives back the fence."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream "Flow:\n\n```mermaid\ngraph LR\n  A --> B\n```\n" t)
    (should (equal 1 (length agent-shell-omp-figure-tests--requests)))
    (should (equal "graph LR\n  A --> B"
                   (nth 1 (car agent-shell-omp-figure-tests--requests))))
    ;; Pending: the source stays visible and no code panel is drawn yet.
    (should (string-match-p "```mermaid" (agent-shell-omp-figure-tests--text)))
    (agent-shell-omp-figure-tests--answer agent-shell-omp-figure-tests--art)
    (should (string-match-p "│ A ├────►│ B │" (agent-shell-omp-figure-tests--text)))
    (should-not (string-match-p "```" (agent-shell-omp-figure-tests--text)))
    (goto-char (point-min))
    (search-forward "│ A ├")
    (let ((pos (match-beginning 0)))
      (should (eq 'body (get-text-property pos 'agent-shell-ui-section)))
      (should (get-text-property pos 'read-only))
      (should (equal "```mermaid\ngraph LR\n  A --> B\n```\n"
                     (get-text-property pos 'agent-shell-markdown-source))))
    (agent-shell-omp-figure-tests--stream "Done **now**.")
    (let ((text (agent-shell-omp-figure-tests--text)))
      (should (string-match-p "Flow:\n\n┌───┐     ┌───┐\n│ A ├────►│ B │\n└───┘     └───┘\nDone now\\." text)))))

(ert-deftest agent-shell-omp-figure-mermaid-failure-falls-back-to-code-test ()
  "A diagram omp cannot draw is shown as an ordinary code block."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream "```mermaid\npie\n  \"a\": 1\n```\n" t)
    (agent-shell-omp-figure-tests--answer nil)
    (let ((text (agent-shell-omp-figure-tests--text)))
      (should (string-match-p "mermaid ⧉" text))
      (should (string-match-p "\"a\": 1" text))
      (should-not (string-match-p "```" text)))
    ;; The failure is remembered: re-rendering asks nothing again.
    (agent-shell-omp-figure-tests--stream "More.")
    (should-not agent-shell-omp-figure-tests--requests)))

(ert-deftest agent-shell-omp-figure-mermaid-one-request-per-diagram-test ()
  "Chunks streamed while a diagram renders don't start more requests."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream "```mermaid\ngraph LR\n  A --> B\n```\n" t)
    (agent-shell-omp-figure-tests--stream "Some ")
    (agent-shell-omp-figure-tests--stream "more _text_.")
    (should (equal 1 (length agent-shell-omp-figure-tests--requests)))
    (agent-shell-omp-figure-tests--answer agent-shell-omp-figure-tests--art)
    (should (string-match-p "│ B │\n└───┘     └───┘\nSome more text\\."
                            (agent-shell-omp-figure-tests--text)))))

(ert-deftest agent-shell-omp-figure-mermaid-cached-art-renders-anywhere-test ()
  "Art already drawn renders synchronously, even off-shell (replay, convert)."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream "```mermaid\ngraph LR\n  A --> B\n```\n" t)
    (agent-shell-omp-figure-tests--answer agent-shell-omp-figure-tests--art)
    ;; Bound from a buffer without the shell's local hook, so the global
    ;; value is what the conversion's work buffer sees.
    (with-temp-buffer
      (let ((agent-shell-markdown-render-functions
             (list #'agent-shell-omp--render-figures)))
        (should (string-match-p
                 "│ A ├────►│ B │"
                 (substring-no-properties
                  (agent-shell-markdown-convert
                   "```mermaid\ngraph LR\n  A --> B\n```\n"))))))
    (should-not agent-shell-omp-figure-tests--requests)))

(ert-deftest agent-shell-omp-figure-no-request-outside-shell-test ()
  "Off-shell buffers never spawn a renderer; the fence renders as code."
  (let ((agent-shell-omp--figure-cache (make-hash-table :test 'equal))
        (agent-shell-omp--figure-waiters (make-hash-table :test 'equal))
        (requested nil)
        (agent-shell-omp--figure-request-function
         (lambda (&rest _) (setq requested t)))
        (agent-shell-markdown-render-functions
         (list #'agent-shell-omp--render-figures)))
    (should (string-match-p
             "mermaid ⧉"
             (substring-no-properties
              (agent-shell-markdown-convert "```mermaid\ngraph LR\n  A --> B\n```\n"))))
    (should-not requested)))

(ert-deftest agent-shell-omp-figure-redraws-for-a-new-width-test ()
  "A resize redraws art at the new width, keeping it in the body."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream "```mermaid\ngraph LR\n  A --> B\n```\nAfter." t)
    (agent-shell-omp-figure-tests--answer agent-shell-omp-figure-tests--art)
    (cl-letf (((symbol-function 'agent-shell-omp--figure-width) (lambda (_pos) 12)))
      (agent-shell-omp--redraw-figures (current-buffer))
      (should (equal 12 (nth 2 (car agent-shell-omp-figure-tests--requests))))
      ;; The old art stays up while the new width renders.
      (should (string-match-p "│ A ├────►│ B │" (agent-shell-omp-figure-tests--text)))
      (agent-shell-omp-figure-tests--answer "A\n│\n▼\nB\n"))
    (should (string-match-p "A\n│\n▼\nB\nAfter\\." (agent-shell-omp-figure-tests--text)))
    (goto-char (point-min))
    (search-forward "▼")
    (should (eq 'body (get-text-property (match-beginning 0) 'agent-shell-ui-section)))
    (should (equal 12 (get-text-property (match-beginning 0)
                                         'agent-shell-omp-figure-width)))))

(defun agent-shell-omp-figure-tests--run-process (command input)
  "Run the real request function with COMMAND on INPUT; return its answer."
  (let ((agent-shell-omp-figure-command command)
        (answer 'pending))
    (agent-shell-omp--figure-request-process 'mermaid input 80
                                             (lambda (text) (setq answer text)))
    (with-timeout (10 (error "Renderer did not answer"))
      (while (eq answer 'pending)
        (accept-process-output nil 0.05)))
    answer))

(ert-deftest agent-shell-omp-figure-request-process-test ()
  "The renderer process gets the source on stdin and its kind and width
as arguments; a failing exit answers nil."
  (should (equal "graph LR|mermaid --width 80"
                 (agent-shell-omp-figure-tests--run-process
                  '("sh" "-c" "printf '%s|%s' \"$(cat)\" \"$*\"" "sh")
                  "graph LR")))
  (should-not (agent-shell-omp-figure-tests--run-process
               '("sh" "-c" "echo partial; exit 1" "sh") "graph LR"))
  (should-not (agent-shell-omp-figure-tests--run-process
               '("agent-shell-omp-no-such-command") "graph LR")))

(ert-deftest agent-shell-omp-figure-svg-draws-themed-image-test ()
  "A closed ```svg fence becomes an inline image in the theme's colors,
with scripts and external references removed; copying gives back the
fence."
  (agent-shell-omp-figure-tests--with-shell
    (cl-letf (((symbol-function 'agent-shell-omp--svg-displayable-p) (lambda () t))
              ((symbol-function 'agent-shell-omp--svg-palette)
               (lambda () '(("fg" . "#111111") ("c1" . "#aa0000")))))
      (let ((fence (concat "```svg\n"
                           "<svg viewBox=\"0 0 10 10\"><script>alert(1)</script>"
                           "<rect fill=\"var(--c1)\" stroke=\"var(--nope, red)\"/>"
                           "<text fill=\"var(--fg)\">x</text>"
                           "<image href=\"file:///etc/passwd\"/><use href=\"#a\"/></svg>\n"
                           "```\n")))
        (agent-shell-omp-figure-tests--stream (concat "Figure:\n\n" fence) t)
        (agent-shell-omp-figure-tests--stream "After.")
        (goto-char (point-min))
        (let* ((pos (text-property-not-all (point-min) (point-max)
                                           'agent-shell-omp-figure nil))
               (image (get-text-property pos 'display))
               (svg (decode-coding-string (plist-get (cdr image) :data) 'utf-8)))
          (should (eq 'image (car image)))
          (should (eq 'svg (plist-get (cdr image) :type)))
          (should (string-match-p "fill=\"#aa0000\"" svg))
          (should (string-match-p "stroke=\"red\"" svg))
          (should (string-match-p "fill=\"#111111\"" svg))
          (should (string-match-p "<svg color=\"#111111\"" svg))
          (should (string-match-p "xmlns=\"http://www.w3.org/2000/svg\"" svg))
          (should-not (string-match-p "var(\\|<script\\|alert\\|passwd" svg))
          (should (string-match-p "href=\"#a\"" svg))
          (should (equal fence (get-text-property pos 'agent-shell-markdown-source)))
          (should (eq 'body (get-text-property pos 'agent-shell-ui-section))))
        (should (string-match-p "Figure:\n\n.*\nAfter\\." (agent-shell-omp-figure-tests--text)))))))

(ert-deftest agent-shell-omp-figure-svg-stays-code-without-graphics-test ()
  "Without SVG display (a tty frame) the fence renders as code."
  (agent-shell-omp-figure-tests--with-shell
    (cl-letf (((symbol-function 'agent-shell-omp--svg-displayable-p) (lambda () nil)))
      (agent-shell-omp-figure-tests--stream "```svg\n<svg/>\n```\n" t)
      (should (string-match-p "svg ⧉" (agent-shell-omp-figure-tests--text))))))

(ert-deftest agent-shell-omp-figure-resize-leaves-svg-alone-test ()
  "Resizing never asks omp to redraw an SVG image (it has no width)."
  (agent-shell-omp-figure-tests--with-shell
    (cl-letf (((symbol-function 'agent-shell-omp--svg-displayable-p) (lambda () t))
              ((symbol-function 'agent-shell-omp--figure-width) (lambda (_pos) 40)))
      (agent-shell-omp-figure-tests--stream "```svg\n<svg/>\n```\n" t)
      (agent-shell-omp--redraw-figures (current-buffer))
      (should-not agent-shell-omp-figure-tests--requests)
      (should (eq 'image (car (get-text-property
                               (text-property-not-all (point-min) (point-max)
                                                      'agent-shell-omp-figure nil)
                               'display)))))))

(defun agent-shell-omp-figure-tests--answer-math (render)
  "Answer the oldest math request, rendering each item with RENDER.
RENDER gets (SRC DISPLAY) and returns the text."
  (let* ((request (car (last agent-shell-omp-figure-tests--requests)))
         (items (mapcar (lambda (line) (json-parse-string line :object-type 'alist))
                        (split-string (nth 1 request) "\n" t))))
    (should (eq 'math (nth 0 request)))
    (agent-shell-omp-figure-tests--answer
     (mapconcat (lambda (item)
                  (json-encode
                   `((text . ,(funcall render (map-elt item 'src)
                                       (eq t (map-elt item 'display)))))))
                items "\n"))))

(ert-deftest agent-shell-omp-figure-math-renders-spans-in-one-request-test ()
  "Inline and display math go to omp in one batch and come back as text.
LaTeX is frozen while it renders, so emphasis passes don't eat `_'."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream
     (concat "Let $x_i + y_i$ be it, and \\(a_b\\) too.\n\n"
             "$$\n\\frac{a}{b}\n$$\n\nEnd.\n")
     t)
    (should (equal 1 (length agent-shell-omp-figure-tests--requests)))
    ;; Pending: raw LaTeX, underscores intact.
    (should (string-match-p "\\$x_i \\+ y_i\\$" (agent-shell-omp-figure-tests--text)))
    (agent-shell-omp-figure-tests--answer-math
     (lambda (src display) (if display "FRAC\nLINE" (concat "<" src ">"))))
    (let ((text (agent-shell-omp-figure-tests--text)))
      (should (string-match-p "Let <x_i \\+ y_i> be it, and <a_b> too\\." text))
      (should (string-match-p "\n\nFRAC\nLINE\n\nEnd\\." text)))
    (goto-char (point-min))
    (search-forward "<x_i")
    (should (equal "$x_i + y_i$"
                   (get-text-property (match-beginning 0) 'agent-shell-markdown-source)))
    (should (eq 'body (get-text-property (match-beginning 0) 'agent-shell-ui-section)))))

(ert-deftest agent-shell-omp-figure-math-leaves-prose-and-code-test ()
  "Prices, escaped dollars and code spans are not math."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream
     "It costs $5 and $10, \\$x\\$ stays, and `$y_z$` is code.\n" t)
    (should-not agent-shell-omp-figure-tests--requests)
    (should (string-match-p "costs \\$5 and \\$10" (agent-shell-omp-figure-tests--text)))))

(ert-deftest agent-shell-omp-figure-math-holds-open-display-block-test ()
  "An unclosed $$ block holds rendering until its closer streams in."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream "Here:\n\n$$\nx_1 *" t)
    (agent-shell-omp-figure-tests--stream " y_1\n")
    (should-not agent-shell-omp-figure-tests--requests)
    (should (string-match-p "x_1 \\* y_1" (agent-shell-omp-figure-tests--text)))
    (agent-shell-omp-figure-tests--stream "$$\n")
    (should (equal "x_1 * y_1"
                   (string-trim
                    (map-elt (json-parse-string
                              (nth 1 (car agent-shell-omp-figure-tests--requests))
                              :object-type 'alist)
                             'src))))))

(ert-deftest agent-shell-omp-figure-math-releases-open-block-at-turn-end-test ()
  "A $$ that never closes stops holding the message once the turn ends."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream "Here:\n\n$$\nx_1 *\n\nThen **bold** text.\n" t)
    (should (string-match-p "\\*\\*bold\\*\\*" (agent-shell-omp-figure-tests--text)))
    (let* ((block (agent-shell-ui--block-range
                   :position (text-property-not-all (point-min) (point-max)
                                                    'agent-shell-ui-state nil)))
           (body (agent-shell-ui--body-range (map-elt block :start)
                                             (map-elt block :end))))
      (save-restriction
        (narrow-to-region (map-elt body :start) (map-elt body :end))
        (let ((inhibit-read-only t))
          (agent-shell--render-markdown-body :complete t))))
    (should (string-match-p "Then bold text\\." (agent-shell-omp-figure-tests--text)))
    (should-not agent-shell-omp-figure-tests--requests)))

(ert-deftest agent-shell-omp-figure-math-colors-become-faces-test ()
  "SGR colors in omp's math output become faces, not escape text."
  (agent-shell-omp-figure-tests--with-shell
    (agent-shell-omp-figure-tests--stream "See $\\textcolor{red}{x}$ here.\n" t)
    (agent-shell-omp-figure-tests--answer-math
     (lambda (_src _display) "\e[38;2;255;0;0mx\e[39m"))
    (should (string-match-p "See x here\\." (agent-shell-omp-figure-tests--text)))
    (goto-char (point-min))
    (search-forward "See ")
    (should (string-equal-ignore-case
             "#ff0000"
             (let ((face (get-text-property (point) 'face)))
               (seq-some (lambda (f) (and (consp f) (plist-get f :foreground)))
                         (if (and (consp face) (not (keywordp (car face))))
                             face
                           (list face))))))))

(provide 'agent-shell-omp-figure-tests)

;;; agent-shell-omp-figure-tests.el ends here
