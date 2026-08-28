;;; agent-shell-subagents-tests.el --- Tests for native subagents and async tasks -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-shell)

;;; Code:

(ert-deftest agent-shell-air-capabilities-meta-test ()
  "Test that AIR `_meta' carries the requested capability names."
  (let ((meta (agent-shell--air-capabilities-meta "asyncTasks" "nativeSubagentSessions")))
    (should (equal (map-nested-elt meta '(jetbrains air version)) 1))
    (should (equal (map-nested-elt meta '(jetbrains air capabilities))
                   ["asyncTasks" "nativeSubagentSessions"]))))

(ert-deftest agent-shell-air-meta-rides-inside-client-capabilities-test ()
  "Test that the initialize request nests AIR `_meta' inside `clientCapabilities'.

Agents read extension meta there (claude-agent-acp, codex-acp); a
request-level `_meta' is never consulted, and the gated features
silently fall back to plain tool calls."
  (let* ((request (acp-make-initialize-request
                   :protocol-version 1
                   :client-capabilities `((subagents . ())
                                          (_meta . ,(agent-shell--air-capabilities-meta
                                                     "asyncTasks"))))))
    (should (equal (map-nested-elt request '(:params clientCapabilities _meta
                                                    jetbrains air capabilities))
                   ["asyncTasks"]))
    (should-not (map-elt (map-elt request :params) '_meta))))

(ert-deftest agent-shell-session-bound-notification-p-recognizes-subagent-and-async-task-updates-test ()
  "Test that native subagent and async task updates count as session-bound.
Otherwise they would misfire the \"unexpected out-of-turn update\" path,
since they legitimately arrive after `end_turn' once a session/prompt
request has already settled."
  (dolist (kind '("subagent_spawned" "subagent_state_update"
                  "async_task_spawned" "async_task_progress" "async_task_state_update"))
    (should (agent-shell--session-bound-notification-p
             `((method . "session/update")
               (params . ((update . ((sessionUpdate . ,kind))))))))))

(ert-deftest agent-shell-native-subagent-registry-round-trip-test ()
  "Test that a spawned subagent's name/task can be looked back up."
  (let ((state (list (cons :native-subagents nil))))
    (agent-shell--save-native-subagent state "subagent-1" "Researcher" "Find prior art")
    (let ((registered (agent-shell--native-subagent state "subagent-1")))
      (should (equal (map-elt registered :name) "Researcher"))
      (should (equal (map-elt registered :task) "Find prior art")))
    (should-not (agent-shell--native-subagent state "subagent-unknown"))))

(ert-deftest agent-shell-async-task-registry-round-trip-test ()
  "Test that a spawned async task's name/type/description can be looked back up."
  (let ((state (list (cons :async-tasks nil))))
    (agent-shell--save-async-task state "task-1" "Build" "workflow" "Running the test suite")
    (let ((registered (agent-shell--async-task state "task-1")))
      (should (equal (map-elt registered :name) "Build"))
      (should (equal (map-elt registered :task-type) "workflow"))
      (should (equal (map-elt registered :description) "Running the test suite")))
    (should-not (agent-shell--async-task state "task-unknown"))))

(ert-deftest agent-shell-format-async-task-body-prefers-summary-over-description-test ()
  "Test that SUMMARY, when present, takes priority over DESCRIPTION."
  (should (equal (agent-shell--format-async-task-body "description" "summary" nil nil)
                 "summary"))
  (should (equal (agent-shell--format-async-task-body "description" nil nil nil)
                 "description")))

(ert-deftest agent-shell-format-async-task-body-includes-tool-and-usage-lines-test ()
  "Test that the last tool name and usage tally render as extra lines."
  (let ((body (agent-shell--format-async-task-body
              "description" nil "Bash" '((totalTokens . 1500) (toolUses . 3)))))
    (should (string-match-p "Last tool: Bash" body))
    (should (string-match-p "2k tokens · 3 tool uses" body))))

(provide 'agent-shell-subagents-tests)
;;; agent-shell-subagents-tests.el ends here
