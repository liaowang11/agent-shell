;;; agent-shell-usage-tests.el --- Tests for agent-shell-usage -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-shell-usage)

;;; Code:

(defun agent-shell-usage-tests--model-usage-meta (&rest model-and-totals)
  "Build a PromptResponse `_meta' alist with `quota.model_usage' rows.
MODEL-AND-TOTALS is a flat list of (MODEL . TOTAL-TOKENS) pairs."
  (list (cons 'quota
              (list (cons 'model_usage
                          (mapcar (lambda (pair)
                                    (list (cons 'model (car pair))
                                          (cons 'token_count
                                                (list (cons 'totalTokens (cdr pair))))))
                                  model-and-totals))))))

(ert-deftest agent-shell-usage-save-usage-reads-model-usage-from-meta-test ()
  "Test that per-model usage is read from PromptResponse `_meta.quota'."
  (let ((state (list (cons :usage (list (cons :total-tokens 0) (cons :model-usage nil))))))
    (agent-shell--save-usage
     :state state
     :acp-usage '((totalTokens . 100))
     :acp-meta (agent-shell-usage-tests--model-usage-meta
                '("claude-opus-5[1m]" . 70)
                '("claude-sonnet-5" . 30)))
    (let ((rows (map-nested-elt state '(:usage :model-usage))))
      (should (equal (map-elt (nth 0 rows) :model) "claude-opus-5[1m]"))
      (should (equal (map-elt (nth 0 rows) :total-tokens) 70))
      (should (equal (map-elt (nth 1 rows) :model) "claude-sonnet-5"))
      (should (equal (map-elt (nth 1 rows) :total-tokens) 30)))))

(ert-deftest agent-shell-usage-save-usage-without-meta-leaves-model-usage-nil-test ()
  "Test that omitting `_meta' (older agents) leaves `:model-usage' unset."
  (let ((state (list (cons :usage (list (cons :total-tokens 0) (cons :model-usage nil))))))
    (agent-shell--save-usage :state state :acp-usage '((totalTokens . 100)))
    (should-not (map-nested-elt state '(:usage :model-usage)))
    (should (equal (map-nested-elt state '(:usage :total-tokens)) 100))))

(ert-deftest agent-shell-usage-format-usage-includes-models-line-when-multiple-test ()
  "Test that the multiline format shows a `Models:' row for >1 model."
  (let ((usage (list (cons :model-usage
                           (list (list :model "claude-opus-5[1m]" :total-tokens 70000)
                                 (list :model "claude-sonnet-5" :total-tokens 30000))))))
    (let ((text (agent-shell--format-usage usage t)))
      (should (string-match-p "Models:" text))
      (should (string-match-p "claude-opus-5\\[1m\\] 70k" text))
      (should (string-match-p "claude-sonnet-5 30k" text)))))

(ert-deftest agent-shell-usage-format-usage-omits-models-line-when-single-test ()
  "Test that a single-model breakdown doesn't add a redundant `Models:' row."
  (let ((usage (list (cons :model-usage
                           (list (list :model "claude-opus-5[1m]" :total-tokens 70000))))))
    (should-not (string-match-p "Models:" (agent-shell--format-usage usage t)))))

(provide 'agent-shell-usage-tests)
;;; agent-shell-usage-tests.el ends here
