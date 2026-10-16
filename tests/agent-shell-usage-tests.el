;;; agent-shell-usage-tests.el --- Tests for agent-shell-usage -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'cl-lib)
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

(ert-deftest agent-shell-usage-save-usage-migrates-state-without-model-usage-test ()
  "Test that a shell from before `:model-usage' existed still records it.
`map-put!' refuses to add a missing key, so a live session across a
package upgrade would fail on the first per-model report."
  (let ((state (list (cons :usage (list (cons :total-tokens 0))))))
    (agent-shell--save-usage
     :state state
     :acp-usage '((totalTokens . 100))
     :acp-meta (agent-shell-usage-tests--model-usage-meta
                '("claude-sonnet-5" . 100)))
    (should (equal (map-elt (car (map-nested-elt state '(:usage :model-usage)))
                            :total-tokens)
                   100))))

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

;;; Rate limits (claude-agent-acp's `_meta._claude/rateLimit')

(defun agent-shell-usage-tests--rate-limit-update (&rest info)
  "Build a `usage_update' carrying INFO as `_meta._claude/rateLimit'.
INFO is a plist of `status', `rateLimitType', `utilization' and
`resetsAt' values, as the SDK's `rate_limit_info' sends them."
  (list (cons 'sessionUpdate "usage_update")
        (cons 'used 1000)
        (cons 'size 200000)
        (cons '_meta
              (list (cons '_claude/rateLimit
                          (map-into info 'alist))))))

(defun agent-shell-usage-tests--usage-state ()
  "Return a fresh state whose `:usage' carries `:rate-limits'."
  (list (cons :usage (list (cons :context-used 0)
                           (cons :context-size 0)
                           (cons :rate-limits nil)))))

(defun agent-shell-usage-tests--future (seconds)
  "Return the epoch seconds SECONDS from now, as the SDK sends `resetsAt'."
  (+ (truncate (float-time)) seconds))

(ert-deftest agent-shell-usage-rate-limit-keeps-latest-per-type-test ()
  "Test that each `rateLimitType' keeps only its latest report."
  (let ((state (agent-shell-usage-tests--usage-state))
        (resets (agent-shell-usage-tests--future 3600)))
    (cl-letf (((symbol-function 'agent-shell--emit-event) #'ignore))
      (dolist (info `((status "allowed_warning" rateLimitType "five_hour"
                              utilization 0.8 resetsAt ,resets)
                      (status "allowed_warning" rateLimitType "seven_day"
                              utilization 0.75 resetsAt ,(+ resets 86400))
                      (status "allowed_warning" rateLimitType "five_hour"
                              utilization 0.92 resetsAt ,resets)))
        (agent-shell--update-usage-from-notification
         :state state
         :acp-update (apply #'agent-shell-usage-tests--rate-limit-update info))))
    (let ((limits (map-nested-elt state '(:usage :rate-limits))))
      (should (equal (mapcar (lambda (limit) (map-elt limit :type)) limits)
                     '("five_hour" "seven_day")))
      (should (equal (map-elt (car limits) :utilization) 0.92))
      (should (equal (map-elt (car limits) :status) "allowed_warning"))
      (should (equal (map-elt (car limits) :resets-at) resets)))
    ;; The context fields still come from the same notification.
    (should (equal (map-nested-elt state '(:usage :context-used)) 1000))))

(ert-deftest agent-shell-usage-rate-limit-typeless-allowed-clears-warnings-test ()
  "Test that an `allowed' report without a type clears every warning.
The CLI strips `rateLimitType', `utilization' and `resetsAt' once a limit
is no longer hit, so the typed warning would otherwise stick."
  (let ((state (agent-shell-usage-tests--usage-state)))
    (cl-letf (((symbol-function 'agent-shell--emit-event) #'ignore))
      (agent-shell--update-usage-from-notification
       :state state
       :acp-update (agent-shell-usage-tests--rate-limit-update
                    'status "allowed_warning" 'rateLimitType "five_hour"
                    'utilization 0.92
                    'resetsAt (agent-shell-usage-tests--future 3600)))
      (agent-shell--update-usage-from-notification
       :state state
       :acp-update (agent-shell-usage-tests--rate-limit-update
                    'status "allowed")))
    (let ((limits (map-nested-elt state '(:usage :rate-limits))))
      (should (equal (length limits) 1))
      (should (equal (map-elt (car limits) :type) "five_hour"))
      (should (equal (map-elt (car limits) :status) "allowed"))
      ;; Last known numbers stay for `agent-shell-show-usage'.
      (should (equal (map-elt (car limits) :utilization) 0.92)))))

(ert-deftest agent-shell-usage-rate-limit-typeless-warning-is-kept-test ()
  "Test that a warning without `rateLimitType' is still recorded."
  (let ((state (agent-shell-usage-tests--usage-state)))
    (cl-letf (((symbol-function 'agent-shell--emit-event) #'ignore))
      (agent-shell--update-usage-from-notification
       :state state
       :acp-update (agent-shell-usage-tests--rate-limit-update
                    'status "allowed_warning" 'utilization 0.9
                    'resetsAt (agent-shell-usage-tests--future 3600))))
    (let ((limits (map-nested-elt state '(:usage :rate-limits))))
      (should (equal (length limits) 1))
      (should-not (map-elt (car limits) :type))
      (should (equal (map-elt (car limits) :status) "allowed_warning")))))

(ert-deftest agent-shell-usage-rate-limit-migrates-state-without-key-test ()
  "Test that a shell from before `:rate-limits' existed still records them."
  (let ((state (list (cons :usage (list (cons :context-used 0)
                                        (cons :context-size 0))))))
    (cl-letf (((symbol-function 'agent-shell--emit-event) #'ignore))
      (agent-shell--update-usage-from-notification
       :state state
       :acp-update (agent-shell-usage-tests--rate-limit-update
                    'status "allowed" 'rateLimitType "five_hour"
                    'utilization 0.1)))
    (should (equal (map-elt (car (map-nested-elt state '(:usage :rate-limits)))
                            :type)
                   "five_hour"))))

(ert-deftest agent-shell-usage-rate-limit-without-meta-leaves-limits-test ()
  "Test that a plain `usage_update' leaves recorded limits alone."
  (let ((state (agent-shell-usage-tests--usage-state)))
    (cl-letf (((symbol-function 'agent-shell--emit-event) #'ignore))
      (agent-shell--update-usage-from-notification
       :state state
       :acp-update (agent-shell-usage-tests--rate-limit-update
                    'status "allowed_warning" 'rateLimitType "five_hour"
                    'utilization 0.9))
      (agent-shell--update-usage-from-notification
       :state state
       :acp-update '((sessionUpdate . "usage_update") (used . 5) (size . 10))))
    (should (equal (map-elt (car (map-nested-elt state '(:usage :rate-limits)))
                            :status)
                   "allowed_warning"))))

(ert-deftest agent-shell-usage-rate-limit-rejected-notifies-once-test ()
  "Test that entering `rejected' notifies once, not on every repeat."
  (let ((state (agent-shell-usage-tests--usage-state))
        (events nil)
        (resets (agent-shell-usage-tests--future 3600)))
    (cl-letf (((symbol-function 'agent-shell--emit-event)
               (lambda (&rest args) (push args events)))
              ((symbol-function 'message) #'ignore))
      (dotimes (_ 2)
        (agent-shell--update-usage-from-notification
         :state state
         :acp-update (agent-shell-usage-tests--rate-limit-update
                      'status "rejected" 'rateLimitType "five_hour"
                      'utilization 1.0 'resetsAt resets)))
      (should (equal (length events) 1))
      (should (eq (plist-get (car events) :event) 'rate-limit-rejected))
      (should (equal (map-elt (plist-get (car events) :data) :type) "five_hour"))
      ;; Leaving and re-entering `rejected' notifies again.
      (agent-shell--update-usage-from-notification
       :state state
       :acp-update (agent-shell-usage-tests--rate-limit-update 'status "allowed"))
      (agent-shell--update-usage-from-notification
       :state state
       :acp-update (agent-shell-usage-tests--rate-limit-update
                    'status "rejected" 'rateLimitType "five_hour"
                    'utilization 1.0 'resetsAt resets))
      (should (equal (length events) 2)))))

(ert-deftest agent-shell-usage-format-rate-limit-reset-test ()
  "Test reset times: clock time today, weekday and clock time otherwise."
  (let ((now (encode-time '(0 0 9 16 10 2026 nil nil t))))
    ;; 14:30 UTC the same day.
    (should (equal (agent-shell--format-rate-limit-reset
                    (+ (truncate (float-time now)) (* 5.5 3600)) now t)
                   "14:30"))
    ;; Two days later, a Sunday.
    (should (equal (agent-shell--format-rate-limit-reset
                    (+ (truncate (float-time now)) (* 2 86400)) now t)
                   "Sun 09:00"))))

(ert-deftest agent-shell-usage-rate-limit-indicator-hidden-when-allowed-test ()
  "Test that the indicator stays hidden unless a limit warns or rejects."
  (should-not (agent-shell--rate-limit-indicator nil))
  (should-not (agent-shell--rate-limit-indicator
               `((:rate-limits . (((:type . "five_hour") (:status . "allowed")
                                   (:utilization . 0.4)
                                   (:resets-at . ,(agent-shell-usage-tests--future 3600)))))))))

(ert-deftest agent-shell-usage-rate-limit-indicator-hidden-after-reset-test ()
  "Test that a warning whose window has already reset no longer shows."
  (should-not (agent-shell--rate-limit-indicator
               `((:rate-limits . (((:type . "five_hour") (:status . "rejected")
                                   (:utilization . 1.0)
                                   (:resets-at . ,(agent-shell-usage-tests--future -60)))))))))

(ert-deftest agent-shell-usage-rate-limit-indicator-warning-test ()
  "Test the warning indicator's text and face."
  (let* ((resets (agent-shell-usage-tests--future 3600))
         (indicator (agent-shell--rate-limit-indicator
                     `((:rate-limits . (((:type . "five_hour")
                                         (:status . "allowed_warning")
                                         (:utilization . 0.92)
                                         (:resets-at . ,resets))
                                        ((:type . "seven_day")
                                         (:status . "allowed")
                                         (:utilization . 0.3)
                                         (:resets-at . ,(+ resets 86400)))))))))
    ;; `%%' because header and mode lines read `%' as a construct.
    (should (equal (substring-no-properties indicator)
                   (format "5h 92%%%% resets %s"
                           (agent-shell--format-rate-limit-reset resets))))
    (should (eq (get-text-property 0 'face indicator) 'agent-shell-warning))
    (should (string-match-p "7d" (get-text-property 0 'help-echo indicator)))))

(ert-deftest agent-shell-usage-rate-limit-indicator-rejected-test ()
  "Test that any rejected limit gives the indicator the error face."
  (let* ((resets (agent-shell-usage-tests--future 3600))
         (indicator (agent-shell--rate-limit-indicator
                     `((:rate-limits . (((:type . "seven_day")
                                         (:status . "allowed_warning")
                                         (:utilization . 0.8)
                                         (:resets-at . ,(+ resets 86400)))
                                        ((:type . "five_hour")
                                         (:status . "rejected")
                                         (:utilization . 1.0)
                                         (:resets-at . ,resets))))))))
    (should (string-match-p "\\`7d 80%% resets .* · 5h 100%% resets "
                            (substring-no-properties indicator)))
    (should (eq (get-text-property 0 'face indicator) 'agent-shell-error))))

(ert-deftest agent-shell-usage-format-usage-lists-every-rate-limit-test ()
  "Test that the multiline usage shows every known limit with its numbers."
  (let* ((resets (agent-shell-usage-tests--future 3600))
         (text (agent-shell--format-usage
                `((:rate-limits . (((:type . "five_hour")
                                    (:status . "allowed")
                                    (:utilization . 0.42)
                                    (:resets-at . ,resets))
                                   ((:type . "seven_day_opus")
                                    (:status . "rejected")
                                    (:utilization . 1.0)
                                    (:resets-at . ,(+ resets 86400))))))
                t)))
    (should (string-match-p "Limits: 5h 42.0% allowed, resets " text))
    (should (string-match-p "7d Opus 100.0% rejected, resets " text))
    (should (string-match-p (regexp-quote
                             (format-time-string "%a %b %-d %H:%M" resets))
                            text))))

(provide 'agent-shell-usage-tests)
;;; agent-shell-usage-tests.el ends here
