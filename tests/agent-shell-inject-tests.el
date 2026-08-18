;;; agent-shell-inject-tests.el --- Tests for agent-shell prompt injection -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'agent-shell)
(require 'agent-shell-inject)
(require 'agent-shell-viewport)

;;; Code:

(defun agent-shell-inject-tests--state (&rest overrides)
  "Return a shell state alist for tests, merged with OVERRIDES.

OVERRIDES is an alist whose entries replace the defaults, e.g.

  (agent-shell-inject-tests--state \\='(:supports-prompt-injection . nil))"
  (let ((state (list (cons :agent-config (list (cons :shell-prompt "Claude> ")))
                     (cons :buffer nil)
                     (cons :session (list (cons :id "session-1")))
                     (cons :supports-prompt-injection t)
                     (cons :chunked-group-count 0)
                     (cons :last-entry-type "agent_message_chunk")
                     (cons :request-count 1)
                     (cons :pending-prompts nil)
                     (cons :client 'fake-client))))
    (dolist (override overrides)
      (setf (alist-get (car override) state) (cdr override)))
    state))

(defmacro agent-shell-inject-tests--with-shell (state busy &rest body)
  "Evaluate BODY in a shell buffer whose state is STATE and busy state BUSY.

Binds `agent-shell--shell-buffer' and `agent-shell--state' to the temp
buffer, stubs rendering (`agent-shell--update-text',
`agent-shell--append-transcript') and leaves the ACP send stubbed by the
caller."
  (declare (indent 2))
  `(let* ((shell-state ,state)
          (shell-buffer (generate-new-buffer " *agent-shell inject test*")))
     (with-current-buffer shell-buffer
       ;; Claim the mode without running it: `agent-shell-mode' starts a
       ;; comint process, which these tests neither need nor can drive.
       (setq-local major-mode 'agent-shell-mode)
       ;; Live shells reach the same alist through the variable and the
       ;; function.  Queue helpers read the variable, injection the function.
       (setq-local agent-shell--state shell-state)
       (map-put! shell-state :buffer shell-buffer))
     (unwind-protect
         (cl-letf (((symbol-function 'agent-shell--shell-buffer)
                    (lambda (&rest _) shell-buffer))
                   ((symbol-function 'agent-shell--state)
                    (lambda () shell-state))
                   ((symbol-function 'shell-maker-busy)
                    (lambda () ,busy))
                   ((symbol-function 'agent-shell-status)
                    (lambda (&rest _) (if ,busy 'busy 'ready)))
                   ((symbol-function 'agent-shell--update-fragment)
                    (lambda (&rest _) nil))
                   ((symbol-function 'agent-shell--update-text)
                    (lambda (&rest _) nil))
                   ((symbol-function 'agent-shell--append-transcript)
                    (lambda (&rest _) nil))
                   ((symbol-function 'agent-shell--separate-transcript-after-agent-message)
                    (lambda (&rest _) nil))
                   ((symbol-function 'agent-shell--emit-event)
                    (lambda (&rest _) nil))
                   ((symbol-function 'shell-maker-insert-end-of-prompt-marker)
                    (lambda (&rest _) nil))
                   ((symbol-function 'agent-shell--build-content-blocks)
                    (lambda (prompt)
                      (vector (list (cons 'type "text")
                                    (cons 'text prompt))))))
           ,@body)
       (kill-buffer shell-buffer))))

(defun agent-shell-inject-tests--outcome-sender (outcome captured)
  "Return a fake `agent-shell--send-request' resolving with OUTCOME.

CAPTURED is a cons cell whose car receives the sent request.  When
OUTCOME is the symbol `error', the failure handler runs instead."
  (lambda (&rest args)
    (setcar captured (plist-get args :request))
    (if (eq outcome 'error)
        (funcall (plist-get args :on-failure) '((code . -32601)) "method not found")
      (funcall (plist-get args :on-success) (list (cons 'outcome outcome))))))

(ert-deftest agent-shell--inject-request-test ()
  "Test the steering request carries the session, prompt and idle behavior.

The wire method is the `_session/steering' ACP extension, and
`_meta.steering.idleBehavior' opts into the host-owned fallback so an
agent that already settled the turn hands the prompt back instead of
starting a detached turn."
  (let ((request (agent-shell--inject-request
                  :session-id "session-1"
                  :prompt (vector (list (cons 'type "text")
                                        (cons 'text "steer me"))))))
    (should (equal (map-elt request :method) "_session/steering"))
    (should (equal (map-nested-elt request '(:params sessionId)) "session-1"))
    (should (equal (map-nested-elt request '(:params prompt))
                   (vector (list (cons 'type "text")
                                 (cons 'text "steer me")))))
    (should (equal (map-nested-elt request '(:params _meta steering idleBehavior))
                   "promptRequired"))))

(ert-deftest agent-shell--inject-request-serializes-built-content-blocks-test ()
  "Test a steering request serializes content blocks built from a prompt."
  (let* ((agent-shell--state '((:prompt-capabilities)))
         (request (agent-shell--inject-request
                   :session-id "session-1"
                   :prompt (agent-shell--build-content-blocks "steer me"))))
    (should (vectorp (map-nested-elt request '(:params prompt))))
    (should (stringp (acp--serialize-json request)))))

(ert-deftest agent-shell--inject-capability-test ()
  "Test injection support is read from the initialize response `_meta'.

The capability is advertised at the top-level `_meta.steering.supported',
a sibling of `agentCapabilities', not nested inside it."
  (should (agent-shell--inject-capability
           '((_meta . ((steering . ((supported . t))))))))
  ;; JSON false arrives as nil (acp.el parses with :false-object nil).
  (should-not (agent-shell--inject-capability
               '((_meta . ((steering . ((supported . nil))))))))
  (should-not (agent-shell--inject-capability '((_meta . nil))))
  (should-not (agent-shell--inject-capability
               '((agentCapabilities . ((_meta . ((steering . ((supported . t)))))))))))

(ert-deftest agent-shell--inject-outcome-test ()
  "Test steering responses normalize to internal outcome symbols."
  (should (eq (agent-shell--inject-outcome '((outcome . "injected")))
              'injected))
  (should (eq (agent-shell--inject-outcome '((outcome . "promptRequired")))
              'prompt-required))
  (should (eq (agent-shell--inject-outcome '((outcome . "startedNewTurn")))
              'started-new-turn))
  (should (eq (agent-shell--inject-outcome '((outcome . "failed")))
              'failed))
  (should (eq (agent-shell--inject-outcome '((outcome . "unknown")))
              'failed))
  (should (eq (agent-shell--inject-outcome nil) 'failed)))

(ert-deftest agent-shell-prompt-inject-sends-steering-request-when-busy-test ()
  "Test `agent-shell-prompt-inject' injects into the running turn."
  (let ((captured (cons nil nil))
        enqueued)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "injected" captured))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (agent-shell-prompt-inject "steer me")
        (should (equal (map-elt (car captured) :method) "_session/steering"))
        (should (equal (map-nested-elt (car captured) '(:params sessionId)) "session-1"))
        (should (equal (map-nested-elt (car captured) '(:params prompt))
                       (vector (list (cons 'type "text")
                                     (cons 'text "steer me")))))
        (should-not enqueued)))))

(ert-deftest agent-shell-prompt-inject-renders-injected-prompt-test ()
  "Test an injected prompt is shown in the shell buffer and transcript.

The running turn's own `session/prompt' echo is suppressed mid-turn, so
the injected prompt is rendered by the adapter-independent path here or
the user never sees what they sent."
  (let ((captured (cons nil nil))
        rendered
        transcribed)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "injected" captured))
                ((symbol-function 'agent-shell--update-text)
                 (lambda (&rest args) (setq rendered (plist-get args :text))))
                ((symbol-function 'agent-shell--append-transcript)
                 (lambda (&rest args)
                   (setq transcribed (concat transcribed (plist-get args :text))))))
        (agent-shell-prompt-inject "steer me")
        (should (string-match-p "steer me" rendered))
        (should (string-match-p "## User" transcribed))
        (should (string-match-p "steer me" transcribed))))))

(ert-deftest agent-shell-prompt-inject-expands-truncated-regions-test ()
  "Test injection sends stored region text instead of its visible preview."
  (let ((captured (cons nil nil))
        (prompt (propertize "preview"
                           'agent-shell-region-id "region-1"
                           'agent-shell-region-text "complete region text")))
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "injected" captured)))
        (agent-shell-prompt-inject prompt)
        (should (equal (map-nested-elt (car captured) '(:params prompt 0 text))
                       "complete region text"))))))

(ert-deftest agent-shell-prompt-inject-submits-when-idle-test ()
  "Test `agent-shell-prompt-inject' submits normally with no turn running."
  (let (inserted sent)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) nil
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest args) (setq inserted args))))
        (agent-shell-prompt-inject "just ask")
        (should (equal (plist-get inserted :text) "just ask"))
        (should (plist-get inserted :submit))
        (should-not sent)))))

(ert-deftest agent-shell-prompt-inject-errors-when-unsupported-test ()
  "Test explicit injection errors when the agent never advertised it."
  (let (enqueued sent)
    (agent-shell-inject-tests--with-shell
        (agent-shell-inject-tests--state '(:supports-prompt-injection . nil)) t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (should-error (agent-shell-prompt-inject "steer me") :type 'user-error)
        (should-not enqueued)
        (should-not sent)))))

(ert-deftest agent-shell-prompt-inject-errors-when-blocked-test ()
  "Test a permission-blocked shell does not receive an injected prompt.

A blocked shell is waiting on a permission answer, and no implementation
defines what an agent does with a prompt injected while a tool sits on
that question.  `agent-shell--inject-available-p' rules it out by
requiring status `busy', so a refactor to `shell-maker-busy' (true while
blocked) would regress this."
  (let (enqueued sent)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell-status) (lambda (&rest _) 'blocked))
                ((symbol-function 'agent-shell--send-request)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (should-error (agent-shell-prompt-inject "steer me") :type 'user-error)
        (should-not enqueued)
        (should-not sent)))))

(ert-deftest agent-shell-prompt-inject-does-not-queue-when-prompt-required-test ()
  "Test explicit injection does not queue a declined prompt."
  (let ((captured (cons nil nil))
        enqueued)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "promptRequired" captured))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (agent-shell-prompt-inject "steer me")
        (should-not enqueued)))))

(ert-deftest agent-shell-prompt-inject-does-not-submit-when-declined-after-turn-test ()
  "Test explicit injection does not submit a prompt declined after turn completion."
  (let ((busy t)
        enqueued
        inserted)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) busy
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (lambda (&rest args)
                   (setq busy nil)
                   (funcall (plist-get args :on-success)
                            '((outcome . "promptRequired")))))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt))))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest args) (setq inserted args))))
        (agent-shell-prompt-inject "steer me")
        (should-not inserted)
        (should-not enqueued)))))

(ert-deftest agent-shell-prompt-inject-emits-outcome-event-test ()
  "Test an attempted injection emits the prompt and normalized outcome."
  (let ((captured (cons nil nil))
        emitted)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "injected" captured))
                ((symbol-function 'agent-shell--emit-event)
                 (lambda (&rest args) (setq emitted args))))
        (agent-shell-prompt-inject "steer me")
        (should (eq (plist-get emitted :event) 'prompt-injected))
        (should (equal (map-elt (plist-get emitted :data) :prompt) "steer me"))
        (should (eq (map-elt (plist-get emitted :data) :outcome) 'injected))))))

(ert-deftest agent-shell-prompt-inject-keeps-started-new-turn-test ()
  "Test a `startedNewTurn' outcome is treated as delivered.

Unlike `promptRequired', this outcome means the agent consumed the prompt
and started a turn for it (the Codex adapter's behavior), so re-queueing
it would send it twice."
  (let ((captured (cons nil nil))
        enqueued)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "startedNewTurn" captured))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (agent-shell-prompt-inject "steer me")
        (should-not enqueued)))))

(ert-deftest agent-shell-prompt-inject-explains-started-new-turn-test ()
  "Test a `startedNewTurn' outcome is explained in the buffer, not the echo area.

The agent runs a turn this shell never asked for, so its output arrives
with no request in flight and the shell does not show as busy while it
runs.  An echo would be gone by the time that output shows up, leaving it
unexplained."
  (let ((captured (cons nil nil))
        fragment)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "startedNewTurn" captured))
                ((symbol-function 'agent-shell--update-fragment)
                 (lambda (&rest args) (setq fragment args))))
        (agent-shell-prompt-inject "steer me")
        (should fragment)
        (should (string-match-p "new turn" (plist-get fragment :label-left)))
        (should (string-match-p "steer me" (plist-get fragment :body)))))))

(ert-deftest agent-shell-prompt-inject-does-not-queue-on-request-failure-test ()
  "Test a failed explicit steering request does not queue the prompt."
  (let ((captured (cons nil nil))
        enqueued)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender 'error captured))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (agent-shell-prompt-inject "steer me")
        (should-not enqueued)))))

(ert-deftest agent-shell-prompt-inject-queued-pops-delivered-prompt-test ()
  "Test injecting a pending prompt removes it from the queue."
  (let* ((captured (cons nil nil))
         (state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first" "second")))))
    (agent-shell-inject-tests--with-shell state t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "injected" captured)))
        (agent-shell-prompt-inject-queued 0)
        (should (equal (map-nested-elt (car captured) '(:params prompt))
                       (vector (list (cons 'type "text")
                                     (cons 'text "first")))))
        (should (equal (map-elt state :pending-prompts) '("second")))))))

(ert-deftest agent-shell-prompt-inject-queued-claims-prompt-until-outcome-test ()
  "Test a pending prompt cannot auto-submit while steering is unresolved.

The running turn can finish before the steering response arrives.  Queue
processing at turn completion must not submit the selected prompt normally
while the agent may also consume it through steering."
  (let* ((busy t)
         (state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first" "second"))))
         on-success
         submitted)
    (agent-shell-inject-tests--with-shell state busy
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (lambda (&rest args)
                   (setq on-success (plist-get args :on-success))))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest args)
                   (push (plist-get args :text) submitted))))
        (agent-shell-prompt-inject-queued 0)
        (should (equal (map-elt state :pending-prompts) '("second")))
        (should-error (agent-shell-prompt-queue-resume) :type 'user-error)
        (should (eq (map-elt state :prompt-queue-paused) 'steering))
        (setq busy nil)
        (with-current-buffer shell-buffer
          (agent-shell--prompt-queue-process-next))
        (should-not submitted)
        (funcall on-success '((outcome . "startedNewTurn")))
        (should (equal (map-elt state :pending-prompts) '("second")))
        (should (eq (map-elt state :prompt-queue-paused) 'detached-turn))
        (agent-shell-prompt-queue-resume)
        (should (equal submitted '("second")))
        (should-not (map-elt state :pending-prompts))
        (should-not (map-elt state :prompt-queue-paused))))))

(ert-deftest agent-shell-prompt-inject-queued-restores-on-send-error-test ()
  "Test a synchronous steering send error restores the claimed prompt."
  (let* ((state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first" "second")))))
    (agent-shell-inject-tests--with-shell state t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (lambda (&rest _)
                   (error "Cannot send steering request"))))
        (should-error (agent-shell-prompt-inject-queued 0))
        (should (equal (map-elt state :pending-prompts) '("first" "second")))
        (should-not (map-elt state :prompt-queue-paused))))))

(ert-deftest agent-shell-prompt-inject-queued-keeps-declined-prompt-test ()
  "Test a declined injection leaves the pending prompt in place.

The prompt is already queued, so it must be neither dropped nor queued a
second time."
  (let* ((captured (cons nil nil))
         (state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first" "second"))))
         enqueued)
    (agent-shell-inject-tests--with-shell state t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "promptRequired" captured))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (agent-shell-prompt-inject-queued 0)
        (should (equal (map-elt state :pending-prompts) '("first" "second")))
        (should-not enqueued)))))

(ert-deftest agent-shell-prompt-inject-queued-restores-prompt-required-after-turn-test ()
  "Test a queued prompt declined after turn completion stays queued."
  (let* ((busy t)
         (state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first" "second"))))
         inserted)
    (agent-shell-inject-tests--with-shell state busy
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (lambda (&rest args)
                   (setq busy nil)
                   (funcall (plist-get args :on-success)
                            '((outcome . "promptRequired")))))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest args) (setq inserted args))))
        (agent-shell-prompt-inject-queued 0)
        (should-not inserted)
        (should (equal (map-elt state :pending-prompts) '("first" "second")))))))

(ert-deftest agent-shell-prompt-inject-queued-keeps-prompt-when-unsupported-test ()
  "Test an agent without injection support leaves the queue untouched."
  (let* ((state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first"))
                 '(:supports-prompt-injection . nil)))
         sent)
    (agent-shell-inject-tests--with-shell state t
      (cl-letf (((symbol-function 'agent-shell--send-request)
                 (lambda (&rest _) (setq sent t))))
        (should-error (agent-shell-prompt-inject-queued 0) :type 'user-error)
        (should (equal (map-elt state :pending-prompts) '("first")))
        (should-not sent)))))

(ert-deftest agent-shell-prompt-inject-queued-submits-when-idle-test ()
  "Test injecting a pending prompt with no turn running submits and pops it."
  (let* ((state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first" "second"))))
         inserted)
    (agent-shell-inject-tests--with-shell state nil
      (cl-letf (((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest args) (setq inserted args))))
        (agent-shell-prompt-inject-queued 0)
        (should (equal (plist-get inserted :text) "first"))
        (should (plist-get inserted :submit))
        (should (equal (map-elt state :pending-prompts) '("second")))))))

(ert-deftest agent-shell--inject-migrate-test ()
  "Test the injection state keys are added to a shell that predates them.

A package upgrade reloads agent-shell into a running session, whose state
alist has neither key, and `map-put!' cannot add one."
  (let ((state (list (cons :session (list (cons :id "session-1"))))))
    (cl-letf (((symbol-function 'agent-shell--state) (lambda () state)))
      (agent-shell--inject-migrate)
      (should (assq :supports-prompt-injection state))
      (should (equal (map-elt state :injected-prompt-count) 0))
      (map-put! state :injected-prompt-count 3)
      ;; Migrating again preserves what is already there.
      (agent-shell--inject-migrate)
      (should (equal (map-elt state :injected-prompt-count) 3)))))

(ert-deftest agent-shell--prompt-queue-remove-at-test ()
  "Test removing a pending prompt by index, guarded by its expected text."
  (let ((agent-shell--state (list (cons :pending-prompts '("first" "second")))))
    (agent-shell--prompt-queue-remove-at :index 0 :expected "first")
    (should (equal (map-elt agent-shell--state :pending-prompts) '("second"))))
  ;; A shifted queue is left untouched rather than dropping the wrong prompt.
  (let ((agent-shell--state (list (cons :pending-prompts '("first" "second")))))
    (agent-shell--prompt-queue-remove-at :index 0 :expected "stale")
    (should (equal (map-elt agent-shell--state :pending-prompts) '("first" "second"))))
  (let ((agent-shell--state (list (cons :pending-prompts '("first")))))
    (agent-shell--prompt-queue-remove-at :index 7 :expected "first")
    (should (equal (map-elt agent-shell--state :pending-prompts) '("first")))))

(ert-deftest agent-shell-viewport-compose-inject-test ()
  "Test the compose buffer injects its draft and clears itself.

`C-c C-c' queues the draft; `C-c C-i' injects it into the running turn.
An empty draft is rejected either way.  Keeps composing so the assertions
read the cleared buffer rather than whatever
`agent-shell-viewport--compose-dispose' would leave behind."
  (let ((agent-shell-header-style 'graphical)
        injected)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                 (lambda (&rest _) shell-buffer))
                ((symbol-function 'agent-shell--inject-strict)
                 (lambda (&rest args)
                   (setq injected (plist-get args :prompt))
                   (when-let* ((delivered (plist-get args :on-delivered)))
                     (funcall delivered))))
                ((symbol-function 'agent-shell-viewport--position)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell-viewport--update-header)
                 (lambda (&rest _) nil)))
        (with-temp-buffer
          (agent-shell-viewport-edit-mode)
          (insert "steer me")
          (agent-shell-viewport-compose-inject t)
          (should (equal injected "steer me"))
          (should (string-empty-p (string-trim (buffer-string)))))
        (setq injected nil)
        (with-temp-buffer
          (agent-shell-viewport-edit-mode)
          (should-error (agent-shell-viewport-compose-inject t))
          (should-not injected))))))

(ert-deftest agent-shell-viewport-compose-inject-pops-edited-prompt-test ()
  "Test injecting an edited pending prompt drops it from the queue.

Editing a pending prompt in the compose buffer and injecting it must not
leave the original queued behind, or the agent runs it twice."
  (let* ((agent-shell-header-style 'graphical)
         (captured (cons nil nil))
         (state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first" "second")))))
    (agent-shell-inject-tests--with-shell state t
      (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                 (lambda (&rest _) shell-buffer))
                ((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender "injected" captured))
                ((symbol-function 'agent-shell-viewport--position)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell-viewport--update-header)
                 (lambda (&rest _) nil)))
        (with-temp-buffer
          (agent-shell-viewport-edit-mode)
          (setq agent-shell-viewport--edit-pending-index 0)
          (setq agent-shell-viewport--edit-pending-prompt "first")
          (insert "first, but sharper")
          (agent-shell-viewport-compose-inject t)
          (should (equal (map-nested-elt (car captured) '(:params prompt))
                         (vector (list (cons 'type "text")
                                       (cons 'text "first, but sharper")))))
          (should (equal (map-elt state :pending-prompts) '("second"))))))))

(ert-deftest agent-shell-viewport-compose-inject-edited-idle-finishes-test ()
  "Test submitting an edited queued prompt while idle finishes composition."
  (let* ((agent-shell-header-style 'graphical)
         (state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first" "second"))))
         submitted)
    (agent-shell-inject-tests--with-shell state nil
      (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                 (lambda (&rest _) shell-buffer))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest args) (setq submitted (plist-get args :text))))
                ((symbol-function 'agent-shell-viewport--position)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell-viewport--update-header)
                 (lambda (&rest _) nil)))
        (with-temp-buffer
          (agent-shell-viewport-edit-mode)
          (setq agent-shell-viewport--edit-pending-index 0
                agent-shell-viewport--edit-pending-prompt "first")
          (insert "first, but sharper")
          (agent-shell-viewport-compose-inject t)
          (should (equal submitted "first, but sharper"))
          (should (equal (map-elt state :pending-prompts) '("second")))
          (should-not agent-shell-viewport--injection-pending)
          (should-not buffer-read-only)
          (should (string-empty-p (buffer-string))))))))

(ert-deftest agent-shell-viewport-compose-inject-edited-unsupported-preserves-test ()
  "Test unsupported injection preserves the edited draft and queued source."
  (let* ((agent-shell-header-style 'graphical)
         (state (agent-shell-inject-tests--state
                 '(:pending-prompts . ("first" "second"))
                 '(:supports-prompt-injection . nil))))
    (agent-shell-inject-tests--with-shell state t
      (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                 (lambda (&rest _) shell-buffer))
                ((symbol-function 'agent-shell-viewport--position)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell-viewport--update-header)
                 (lambda (&rest _) nil)))
        (with-temp-buffer
          (agent-shell-viewport-edit-mode)
          (setq agent-shell-viewport--edit-pending-index 0)
          (setq agent-shell-viewport--edit-pending-prompt "first")
          (insert "first, but sharper")
          (should-error (agent-shell-viewport-compose-inject t) :type 'user-error)
          (should (equal (map-elt state :pending-prompts)
                         '("first" "second")))
          (should (equal (buffer-string) "first, but sharper"))
          (should (= agent-shell-viewport--edit-pending-index 0))
          (should (equal agent-shell-viewport--edit-pending-prompt "first")))))))

(ert-deftest agent-shell-viewport-compose-inject-declined-preserves-draft-test ()
  "Test a declined explicit injection keeps the viewport draft editable."
  (let* ((agent-shell-header-style 'graphical)
         (state (agent-shell-inject-tests--state))
         enqueued)
    (agent-shell-inject-tests--with-shell state t
      (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                 (lambda (&rest _) shell-buffer))
                ((symbol-function 'agent-shell--send-request)
                 (agent-shell-inject-tests--outcome-sender
                  "promptRequired" (cons nil nil)))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt))))
                ((symbol-function 'agent-shell-viewport--position)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell-viewport--update-header)
                 (lambda (&rest _) nil)))
        (with-temp-buffer
          (agent-shell-viewport-edit-mode)
          (insert "steer me")
          (agent-shell-viewport-compose-inject t)
          (should (derived-mode-p 'agent-shell-viewport-edit-mode))
          (should (equal (buffer-string) "steer me"))
          (should-not buffer-read-only)
          (should-not enqueued))))))

(ert-deftest agent-shell-viewport-compose-send-queues-during-detached-turn-test ()
  "Test default compose submission waits for an untracked injected turn.

A `startedNewTurn' outcome leaves shell-maker idle because Agent Shell does
not own that turn.  `C-c C-c' must still queue the next prompt instead of
submitting it concurrently."
  (let* ((agent-shell-header-style 'graphical)
         (agent-shell-viewport-dismiss-on-send nil)
         (agent-shell-prefer-viewport-interaction nil)
         (state (agent-shell-inject-tests--state
                 '(:prompt-queue-paused . detached-turn)))
         submitted)
    (agent-shell-inject-tests--with-shell state nil
      (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                 (lambda (&rest _) shell-buffer))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest args)
                   (setq submitted (plist-get args :text))))
                ((symbol-function 'agent-shell-viewport--position)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell-viewport--update-header)
                 (lambda (&rest _) nil))
                ((symbol-function 'kill-buffer) #'ignore)
                ((symbol-function 'pop-to-buffer) #'ignore))
        (with-temp-buffer
          (agent-shell-viewport-edit-mode)
          (insert "after detached turn")
          (agent-shell-viewport-compose-send)
          (should-not submitted)
          (should (equal (map-elt state :pending-prompts)
                         '("after detached turn"))))))))

(defun agent-shell-inject-tests--render-prompt (prompt)
  "Render injected PROMPT in a live comint buffer and return its text and state."
  (let* ((buffer (generate-new-buffer " *agent-shell inject render test*"))
         (process (start-process "agent-shell-inject-test" buffer "cat")))
    (set-process-query-on-exit-flag process nil)
    ;; The default sentinel reports the exit into the buffer, whose comint
    ;; output is read-only, and a failing sentinel aborts the whole run.
    (set-process-sentinel process #'ignore)
    (unwind-protect
        (with-current-buffer buffer
          (comint-mode)
          (setq-local comint-prompt-regexp "^Claude> ")
          (let ((state (list (cons :agent-config '((:shell-prompt . "Claude> ")))
                             (cons :buffer buffer)
                             (cons :injected-prompt-count 0)
                             (cons :last-entry-type "agent_message_chunk"))))
            (cl-letf (((symbol-function 'shell-maker--process) (lambda () process))
                      ((symbol-function 'agent-shell--append-transcript) #'ignore)
                      ((symbol-function 'agent-shell--separate-transcript-after-agent-message)
                       #'ignore))
              (shell-maker--output-filter process "Claude> ")
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert "list files<shell-maker-end-of-prompt>\nListing "))
              (cl-letf (((symbol-function 'agent-shell--state) (lambda () state)))
                (agent-shell--inject-render :prompt prompt))
              (list (cons :text (buffer-substring-no-properties
                                 (point-min) (point-max)))
                    (cons :propertized-text (buffer-substring
                                             (point-min) (point-max)))
                    (cons :last-entry-type (map-elt state :last-entry-type))))))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(defmacro agent-shell-inject-tests--with-injected-turn (&rest body)
  "Evaluate BODY in a live shell whose running turn was injected into.

Renders a realistic turn: a submitted prompt closed by shell-maker's
end-of-prompt marker, streamed output, an injected prompt, then more
output.  Binds `shell-buffer' to that shell."
  (declare (indent 0))
  `(let* ((shell-buffer (generate-new-buffer " *agent-shell injected turn test*"))
          (process (start-process "agent-shell-inject-turn-test" shell-buffer "cat")))
     (set-process-query-on-exit-flag process nil)
     ;; The default sentinel reports the exit into the buffer, whose comint
     ;; output is read-only, and a failing sentinel aborts the whole run.
     (set-process-sentinel process #'ignore)
     (unwind-protect
         (with-current-buffer shell-buffer
           (comint-mode)
           ;; Claim the mode without running it, as `agent-shell-mode' would
           ;; start an agent this test neither needs nor can drive.
           (setq-local major-mode 'agent-shell-mode)
           (setq-local comint-prompt-regexp "^Claude> ")
           (setq-local shell-maker--config
                       (make-shell-maker-config :name "Claude"
                                                :prompt "Claude> "
                                                :prompt-regexp "^Claude> "))
           (let ((state (list (cons :agent-config '((:shell-prompt . "Claude> ")))
                              (cons :buffer shell-buffer)
                              (cons :injected-prompt-count 0)
                              (cons :request-count 1)
                              (cons :last-entry-type "agent_message_chunk"))))
             (setq-local agent-shell--state state)
             (cl-letf (((symbol-function 'shell-maker--process) (lambda () process))
                       ((symbol-function 'agent-shell--state) (lambda () state))
                       ((symbol-function 'agent-shell--append-transcript) #'ignore)
                       ((symbol-function 'agent-shell--separate-transcript-after-agent-message)
                        #'ignore))
               (shell-maker--output-filter process "Claude> ")
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (setq-local comint-last-input-start (copy-marker (point)))
                 (insert "list files"))
               (shell-maker-insert-end-of-prompt-marker)
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (insert "\nEarly work.\n"))
               (agent-shell--inject-render :prompt "just filenames")
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (insert "\nLate work.\n"))
               ,@body)))
       (when (process-live-p process)
         (delete-process process))
       (kill-buffer shell-buffer))))

(ert-deftest agent-shell--inject-render-inlines-labeled-prompt-test ()
  "Test an injected prompt renders as a labeled block inside the turn.

A prompt-shaped line (the shell prompt plus an end-of-prompt marker) would
read as an interaction boundary for `shell-maker-narrow-to-prompt', which
splits the running turn in two.  The label is what separates the prompt
from the agent output around it instead."
  (let ((rendered (agent-shell-inject-tests--render-prompt "just filenames")))
    (should (equal (map-elt rendered :text)
                   (concat "Claude> list files<shell-maker-end-of-prompt>\n"
                           "Listing \n\n"
                           "Injected prompt\n"
                           "just filenames")))
    (should-not (equal (map-elt rendered :last-entry-type)
                       "user_message_chunk"))))

(ert-deftest agent-shell--inject-render-labels-prompt-test ()
  "Test the injected prompt carries a heading label and input face."
  (let* ((rendered (agent-shell-inject-tests--render-prompt "just filenames"))
         (text (map-elt rendered :propertized-text))
         (label-start (string-match "Injected prompt" text))
         (prompt-start (string-match "just filenames" text)))
    (should label-start)
    (should prompt-start)
    (should (equal (get-text-property label-start 'font-lock-face text)
                   'agent-shell-section-heading))
    (should (equal (get-text-property prompt-start 'font-lock-face text)
                   'agent-shell-input))))

(ert-deftest agent-shell--inject-render-keeps-one-interaction-test ()
  "Test an injected prompt leaves the turn as a single interaction.

The viewport rebuilds from `agent-shell-interaction-at-point' every time
the user switches between a shell and its viewport, so an injected prompt
that split the turn would hide itself and every update after it."
  (agent-shell-inject-tests--with-injected-turn
    (let ((history (shell-maker--extract-history "^Claude> ")))
      (should (equal (length history) 1))
      (should (equal (substring-no-properties (car (car history))) "list files"))
      (should (string-match-p "Injected prompt\njust filenames"
                              (substring-no-properties (cdr (car history))))))
    ;; Point past the injected prompt must still resolve the whole turn.
    (goto-char (point-min))
    (search-forward "Late work")
    (cl-letf (((symbol-function 'agent-shell--shell-buffer)
               (lambda (&rest _) shell-buffer)))
      (let ((interaction (agent-shell-interaction-at-point)))
        (should (equal (string-trim (map-elt interaction :prompt)) "list files"))
        (should (string-match-p "Injected prompt\njust filenames"
                                (map-elt interaction :response)))
        (should (string-match-p "Late work" (map-elt interaction :response)))))))

(ert-deftest agent-shell-viewport-compose-inject-follows-send-disposition-test ()
  "Test injecting leaves the compose buffer as `C-c C-c' would.

`agent-shell-viewport-dismiss-on-send' dismisses the window,
`agent-shell-prefer-viewport-interaction' shows the interaction in view
mode, and by default the compose buffer is killed and the shell focused.
A prefix argument keeps the cleared buffer in edit mode."
  (let ((agent-shell-header-style 'graphical)
        (shell-buffer (generate-new-buffer " *test-shell*"))
        injected dismissed viewed killed popped)
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                   (lambda (&rest _) shell-buffer))
                  ((symbol-function 'agent-shell--inject-strict)
                   (lambda (&rest args)
                     (setq injected (plist-get args :prompt))
                     (when-let* ((delivered (plist-get args :on-delivered)))
                       (funcall delivered))))
                  ((symbol-function 'agent-shell-viewport--position)
                   (lambda (&rest _) nil))
                  ((symbol-function 'agent-shell-viewport--update-header)
                   (lambda (&rest _) nil))
                  ((symbol-function 'agent-shell-viewport--dismiss)
                   (lambda (buffer) (setq dismissed buffer)))
                  ((symbol-function 'agent-shell-viewport-view-last)
                   (lambda (&rest _) (setq viewed t)))
                  ((symbol-function 'kill-buffer)
                   (lambda (buffer) (setq killed buffer)))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq popped buffer))))
          ;; Default: kill the compose buffer and focus the shell.
          (with-temp-buffer
            (agent-shell-viewport-edit-mode)
            (insert "steer me")
            (let ((agent-shell-viewport-dismiss-on-send nil)
                  (agent-shell-prefer-viewport-interaction nil))
              (agent-shell-viewport-compose-inject))
            (should (equal injected "steer me"))
            (should (equal killed (current-buffer)))
            (should (equal popped shell-buffer))
            (should-not dismissed)
            (should-not viewed))
          ;; Dismiss on send: hide the window, keep the buffer.
          (setq killed nil popped nil)
          (with-temp-buffer
            (agent-shell-viewport-edit-mode)
            (insert "steer me")
            (let ((agent-shell-viewport-dismiss-on-send t)
                  (agent-shell-prefer-viewport-interaction nil))
              (agent-shell-viewport-compose-inject))
            (should (equal dismissed (current-buffer)))
            (should-not killed)
            (should-not viewed))
          ;; Prefer viewport interaction: watch the turn in view mode.
          (setq dismissed nil killed nil)
          (with-temp-buffer
            (agent-shell-viewport-edit-mode)
            (insert "steer me")
            (let ((agent-shell-viewport-dismiss-on-send nil)
                  (agent-shell-prefer-viewport-interaction t))
              (agent-shell-viewport-compose-inject))
            (should viewed)
            (should-not dismissed)
            (should-not killed))
          ;; Prefix argument: keep composing in the cleared buffer.
          (setq dismissed nil viewed nil killed nil popped nil)
          (with-temp-buffer
            (agent-shell-viewport-edit-mode)
            (insert "steer me")
            (let ((agent-shell-viewport-dismiss-on-send t)
                  (agent-shell-prefer-viewport-interaction t))
              (agent-shell-viewport-compose-inject t))
            (should (derived-mode-p 'agent-shell-viewport-edit-mode))
            (should (string-empty-p (string-trim (buffer-string))))
            (should-not dismissed)
            (should-not viewed)
            (should-not killed)
            (should-not popped)))
      (kill-buffer shell-buffer))))

(ert-deftest agent-shell-prompt-send-follows-while-busy-setting-test ()
  "Test `agent-shell-prompt-send' dispatches on `agent-shell-prompt-while-busy'.

`inject' hands the prompt to the running turn, `queue' holds it until the
turn ends.  Both keep the prompt, so neither setting can lose it."
  (let (injected enqueued)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--inject-or-queue)
                 (lambda (&rest args) (setq injected (plist-get args :prompt))))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (let ((agent-shell-prompt-while-busy 'inject))
          (agent-shell-prompt-send "steer me")
          (should (equal injected "steer me"))
          (should-not enqueued))
        (setq injected nil enqueued nil)
        (let ((agent-shell-prompt-while-busy 'queue))
          (agent-shell-prompt-send "wait your turn")
          (should (equal enqueued "wait your turn"))
          (should-not injected))))))

(ert-deftest agent-shell-prompt-send-submits-when-idle-test ()
  "Test `agent-shell-prompt-send' submits with no turn running.

Queueing an idle shell would leave the prompt pending until a manual
resume, so neither setting queues here."
  (dolist (setting '(inject queue))
    (let (submitted enqueued)
      (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) nil
        (cl-letf (((symbol-function 'agent-shell--insert-to-shell-buffer)
                   (lambda (&rest args) (setq submitted (plist-get args :text))))
                  ((symbol-function 'agent-shell--prompt-queue-enqueue)
                   (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
          (let ((agent-shell-prompt-while-busy setting))
            (agent-shell-prompt-send "just ask")
            (should (equal submitted "just ask"))
            (should-not enqueued)))))))

(ert-deftest agent-shell--prompt-send-honors-explicit-disposition-test ()
  "Test an explicit DISPOSITION overrides `agent-shell-prompt-while-busy'.

The compose buffer carries the disposition of whichever command opened
it, so a queue-flavored entry point stays queueing even when the setting
says inject, and the other way round."
  (let (injected enqueued)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--inject-strict)
                 (lambda (&rest args) (setq injected (plist-get args :prompt))))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (let ((agent-shell-prompt-while-busy 'inject))
          (with-current-buffer shell-buffer
            (agent-shell--prompt-send :prompt "hold it" :disposition 'queue))
          (should (equal enqueued "hold it"))
          (should-not injected))
        (setq injected nil enqueued nil)
        (let ((agent-shell-prompt-while-busy 'queue))
          (with-current-buffer shell-buffer
            (agent-shell--prompt-send :prompt "steer it" :disposition 'inject))
          (should (equal injected "steer it"))
          (should-not enqueued))))))

(cl-defun agent-shell-inject-tests--compose (&key command setting disposition
                                                  (busy t))
  "Run COMMAND in a compose buffer and report where the draft landed.

SETTING binds `agent-shell-prompt-while-busy' and DISPOSITION the compose
buffer's own `agent-shell-viewport--compose-disposition'.  BUSY controls
the shell state.  Returns `injected', `queued', or `submitted'."
  (let ((agent-shell-header-style 'graphical)
        landed)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) busy
      (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
                 (lambda (&rest _) shell-buffer))
                ((symbol-function 'agent-shell-viewport--busy-p)
                 (lambda (&rest _) t))
                ((symbol-function 'agent-shell--inject-or-queue)
                 (lambda (&rest _) (setq landed 'injected)))
                ((symbol-function 'agent-shell--inject-strict)
                 (lambda (&rest args)
                   (setq landed 'injected)
                   (when-let* ((delivered (plist-get args :on-delivered)))
                     (funcall delivered))))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest _) (setq landed 'queued)))
                ((symbol-function 'agent-shell--insert-to-shell-buffer)
                 (lambda (&rest _) (setq landed 'submitted)))
                ((symbol-function 'agent-shell-viewport--position)
                 (lambda (&rest _) nil))
                ((symbol-function 'agent-shell-viewport--update-header)
                 (lambda (&rest _) nil)))
        (with-temp-buffer
          (agent-shell-viewport-edit-mode)
          (setq agent-shell-viewport--compose-disposition disposition)
          (insert "steer me")
          (let ((agent-shell-prompt-while-busy setting))
            (funcall command t)))))
    landed))

(ert-deftest agent-shell-viewport-compose-send-follows-while-busy-setting-test ()
  "Test the compose send key follows `agent-shell-prompt-while-busy'."
  (should (eq (agent-shell-inject-tests--compose
               :command #'agent-shell-viewport-compose-send :setting 'inject)
              'injected))
  (should (eq (agent-shell-inject-tests--compose
               :command #'agent-shell-viewport-compose-send :setting 'queue)
              'queued)))

(ert-deftest agent-shell-viewport-compose-send-follows-buffer-disposition-test ()
  "Test a compose buffer's own disposition outranks the setting.

A draft started by a queue-flavored entry point keeps queueing even when
the setting says inject, so the command that opened the buffer keeps the
promise its name made."
  (should (eq (agent-shell-inject-tests--compose
               :command #'agent-shell-viewport-compose-send
               :setting 'inject :disposition 'queue)
              'queued))
  (should (eq (agent-shell-inject-tests--compose
               :command #'agent-shell-viewport-compose-send
               :setting 'queue :disposition 'inject)
              'injected)))

(ert-deftest agent-shell-viewport-compose-queue-selects-queue-delivery-test ()
  "Test the compose queue key selects queue delivery.

Its meaning does not move with `agent-shell-prompt-while-busy' or with
the buffer's disposition.  It waits while busy and submits when idle."
  (should (eq (agent-shell-inject-tests--compose
               :command #'agent-shell-viewport-compose-queue :setting 'inject)
              'queued))
  (should (eq (agent-shell-inject-tests--compose
               :command #'agent-shell-viewport-compose-queue
               :setting 'inject :disposition 'inject)
              'queued))
  (should (eq (agent-shell-inject-tests--compose
               :command #'agent-shell-viewport-compose-queue
               :setting 'inject :disposition 'inject :busy nil)
              'submitted)))

(ert-deftest agent-shell-viewport-compose-disposition-label-test ()
  "Test the edit-mode header names the disposition the send key will use.

The draft outlives the command that started it, so the header is where
the user reads which way the send key goes."
  (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
             (lambda (&rest _) (current-buffer)))
            ((symbol-function 'agent-shell--inject-available-p)
             (lambda (&rest _) t)))
    (let ((agent-shell-prompt-while-busy 'inject))
      (should (equal (agent-shell-viewport--compose-disposition-label nil) "inject"))
      (should (equal (agent-shell-viewport--compose-disposition-label 'queue) "queue")))
    (let ((agent-shell-prompt-while-busy 'queue))
      (should (equal (agent-shell-viewport--compose-disposition-label nil) "queue"))
      (should (equal (agent-shell-viewport--compose-disposition-label 'inject) "inject"))))
  ;; Generic send queues when preferred injection is unavailable.  An
  ;; explicit inject disposition remains visible as unavailable because it
  ;; will error instead of silently changing delivery methods.
  (cl-letf (((symbol-function 'agent-shell-viewport--shell-buffer)
             (lambda (&rest _) (current-buffer)))
            ((symbol-function 'agent-shell--inject-available-p)
             (lambda (&rest _) nil)))
    (let ((agent-shell-prompt-while-busy 'inject))
      (should (equal (agent-shell-viewport--compose-disposition-label nil) "queue"))
      (should (equal (agent-shell-viewport--compose-disposition-label 'inject)
                     "inject unavailable")))))

(cl-defun agent-shell-inject-tests--dwim-disposition (&key command prefer-viewport)
  "Run COMMAND and return the disposition it hands onward.

With PREFER-VIEWPORT the compose buffer is opened and the disposition
travels with it; otherwise the prompt is read from the minibuffer and sent
straight away.  Returns a cons of (WHERE . DISPOSITION), WHERE being
`viewport' or `sent'."
  (let (where disposition)
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) t
      (cl-letf (((symbol-function 'agent-shell--context) (lambda (&rest _) "context"))
                ((symbol-function 'agent-shell--display-viewport-when-ready)
                 (lambda (&rest args)
                   (setq where 'viewport
                         disposition (plist-get args :disposition))))
                ((symbol-function 'agent-shell--prompt-send)
                 (lambda (&rest args)
                   (setq where 'sent
                         disposition (plist-get args :disposition))))
                ((symbol-function 'agent-shell-completion--setup-minibuffer)
                 (lambda (&rest _) nil))
                ((symbol-function 'read-string) (lambda (&rest _) "steer me")))
        (let ((agent-shell-prefer-viewport-interaction prefer-viewport))
          (funcall command))))
    (cons where disposition)))

(ert-deftest agent-shell-prompt-dwim-commands-carry-their-disposition-test ()
  "Test each DWIM command hands on the disposition its name promises.

`agent-shell-prompt-send-dwim' defers to `agent-shell-prompt-while-busy'
by passing nil, while the queue and inject variants name one outright."
  (dolist (case '((agent-shell-prompt-send-dwim . nil)
                  (agent-shell-prompt-queue-dwim . queue)
                  (agent-shell-prompt-inject-dwim . inject)))
    (let ((sent (agent-shell-inject-tests--dwim-disposition
                 :command (car case) :prefer-viewport nil)))
      (should (eq (car sent) 'sent))
      (should (eq (cdr sent) (cdr case))))
    ;; The compose buffer outlives the command, so the disposition has to
    ;; travel with it rather than being decided when the command ran.
    (let ((viewport (agent-shell-inject-tests--dwim-disposition
                     :command (car case) :prefer-viewport t)))
      (should (eq (car viewport) 'viewport))
      (should (eq (cdr viewport) (cdr case))))))

(ert-deftest agent-shell-viewport-show-buffer-sets-disposition-test ()
  "Test the compose buffer records the disposition it was opened with.

Passing nil resets it rather than leaving a previous command's choice in
place, or a draft would silently inherit a disposition nobody asked for."
  (let ((agent-shell-header-style 'graphical))
    (agent-shell-inject-tests--with-shell (agent-shell-inject-tests--state) nil
      (cl-letf (((symbol-function 'agent-shell--context) (lambda (&rest _) ""))
                ((symbol-function 'agent-shell--display-buffer)
                 (lambda (buffer) (set-buffer buffer)))
                ((symbol-function 'agent-shell-viewport--busy-p) (lambda (&rest _) nil))
                ((symbol-function 'agent-shell-viewport--update-header)
                 (lambda (&rest _) nil)))
        (let ((viewport (map-elt (agent-shell-viewport--show-buffer
                                  :shell-buffer shell-buffer :edit t
                                  :disposition 'queue)
                                 :buffer)))
          (should (eq (buffer-local-value 'agent-shell-viewport--compose-disposition
                                          viewport)
                      'queue))
          (agent-shell-viewport--show-buffer
           :shell-buffer shell-buffer :edit t :disposition nil)
          (should-not (buffer-local-value 'agent-shell-viewport--compose-disposition
                                          viewport))
          (kill-buffer viewport))))))

(ert-deftest agent-shell--prompt-send-queues-while-paused-test ()
  "Test a paused queue is not bypassed by a send into an idle-looking shell.

A paused queue means an untracked turn is still running even though
`shell-maker-busy' reports idle, which is what a detached turn from
injection leaves behind.  Submitting then would fire a prompt at a
working agent, so generic send and explicit queue both wait instead."
  (dolist (disposition '(nil queue))
    (let (submitted enqueued)
      (agent-shell-inject-tests--with-shell
          (agent-shell-inject-tests--state '(:prompt-queue-paused . detached-turn)) nil
        (cl-letf (((symbol-function 'agent-shell--insert-to-shell-buffer)
                   (lambda (&rest args) (setq submitted (plist-get args :text))))
                  ((symbol-function 'agent-shell--prompt-queue-enqueue)
                   (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
          ;; `agent-shell--prompt-queue-paused-p' reads the buffer-local
          ;; `agent-shell--state', and every caller of
          ;; `agent-shell--prompt-send' runs in the shell buffer.
          (let ((agent-shell-prompt-while-busy 'inject))
            (with-current-buffer shell-buffer
              (agent-shell--prompt-send :prompt "hold on"
                                        :disposition disposition)))
          (should (equal enqueued "hold on"))
          (should-not submitted))))))

(ert-deftest agent-shell--prompt-send-injects-while-paused-test ()
  "Test a paused queue still allows steering a turn that is genuinely running.

The pause holds back automatic delivery into an untracked turn.  It is not
a reason to refuse steering a tracked one, so an available injection wins
over the pause."
  (let (injected enqueued)
    (agent-shell-inject-tests--with-shell
        (agent-shell-inject-tests--state '(:prompt-queue-paused . detached-turn)) t
      (cl-letf (((symbol-function 'agent-shell--inject-strict)
                 (lambda (&rest args) (setq injected (plist-get args :prompt))))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (with-current-buffer shell-buffer
          (agent-shell--prompt-send :prompt "steer me" :disposition 'inject))
        (should (equal injected "steer me"))
        (should-not enqueued)))))

(ert-deftest agent-shell--prompt-send-queues-unavailable-preferred-injection-test ()
  "Test generic send falls back when preferred injection is unavailable.

The dispatch asks `agent-shell--inject-available-p' first, which is the
same predicate the compose header reads, so a shell that cannot be steered
queues rather than being handed to the injection path."
  (let (injected enqueued)
    (agent-shell-inject-tests--with-shell
        (agent-shell-inject-tests--state '(:supports-prompt-injection . nil)) t
      (cl-letf (((symbol-function 'agent-shell--inject-or-queue)
                 (lambda (&rest args) (setq injected (plist-get args :prompt))))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (let ((agent-shell-prompt-while-busy 'inject))
          (agent-shell--prompt-send :prompt "steer me"))
        (should (equal enqueued "steer me"))
        (should-not injected)))))

(ert-deftest agent-shell--prompt-send-errors-unavailable-explicit-injection-test ()
  "Test an explicit injection disposition does not fall back to the queue."
  (let (injected enqueued)
    (agent-shell-inject-tests--with-shell
        (agent-shell-inject-tests--state '(:supports-prompt-injection . nil)) t
      (cl-letf (((symbol-function 'agent-shell--inject-or-queue)
                 (lambda (&rest args) (setq injected (plist-get args :prompt))))
                ((symbol-function 'agent-shell--prompt-queue-enqueue)
                 (lambda (&rest args) (setq enqueued (plist-get args :prompt)))))
        (with-current-buffer shell-buffer
          (should-error
           (agent-shell--prompt-send :prompt "steer me" :disposition 'inject)
           :type 'user-error))
        (should-not enqueued)
        (should-not injected)))))

(provide 'agent-shell-inject-tests)

;;; agent-shell-inject-tests.el ends here
