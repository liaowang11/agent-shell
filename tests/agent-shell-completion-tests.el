;;; agent-shell-completion-tests.el --- Tests for agent-shell completion -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-shell-completion)

;;; Code:

(ert-deftest agent-shell--completion-bounds-ignores-path-separators-test ()
  "Test `/` in file paths does not trigger command completion."
  (let ((command-chars "[:alnum:]_-")
        (path-chars "[:alnum:]/_.-"))
    (with-temp-buffer
      (insert "@path/abc")
      (goto-char (point-max))
      (should-not (agent-shell--completion-bounds command-chars ?/))
      (let ((bounds (agent-shell--completion-bounds path-chars ?@)))
        (should bounds)
        (should (equal (map-elt bounds :start) 2))
        (should (equal (map-elt bounds :end) 10)))))

  (with-temp-buffer
    (insert " /help")
    (goto-char (point-max))
    (let ((bounds (agent-shell--completion-bounds "[:alnum:]_-" ?/)))
      (should bounds)
      (should (equal (map-elt bounds :start) 3))
      (should (equal (map-elt bounds :end) 7)))))

(provide 'agent-shell-completion-tests)
;;; agent-shell-completion-tests.el ends here
