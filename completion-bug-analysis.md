# Completion Bug Analysis: @path/abc triggering command completion

## Symptom
When typing a path prefixed with @ (e.g., @path/abc), triggering completion (e.g., via completion-at-point or TAB) incorrectly identifies /abc as a command completion. This interferes with the expected file completion for @path/abc.

## Root Cause
The function agent-shell--completion-bounds in agent-shell-completion.el identifies the completion range by looking for a trigger character (/ or @) immediately preceding the current word. However, it fails to verify that the trigger character itself is at a valid boundary.

In the case of @path/abc:
1. agent-shell--file-completion-at-point correctly finds @ followed by path/abc.
2. agent-shell--command-completion-at-point incorrectly finds / followed by abc because it does not check if the / is preceded by whitespace or the start of the line.

While the automatic trigger agent-shell--trigger-completion-at-point (hooked to post-self-insert-hook) does perform a boundary check, the underlying CAPF functions used for manual completion do not, as they rely on agent-shell--completion-bounds.

## Affected Code
**File:** agent-shell-completion.el
**Function:** agent-shell--completion-bounds

```elisp
(defun agent-shell--completion-bounds (char-class trigger-char)
  "Find completion bounds for CHAR-CLASS, if TRIGGER-CHAR precedes them.
Returns alist with :start and :end if TRIGGER-CHAR is found before
the word, nil otherwise."
  (save-excursion
    (when-let* ((end (progn (skip-chars-forward char-class) (point)))
                (start (progn (skip-chars-backward char-class) (point)))
                ((eq (char-before start) trigger-char))) ;; <--- Missing boundary check for trigger-char
      `((:start . ,start) (:end . ,end)))))
```

## Proposed Fix
Update agent-shell--completion-bounds to ensure that the trigger-char is either at the beginning of the line or preceded by whitespace.
