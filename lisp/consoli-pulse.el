;;; consoli-pulse.el --- Feedback for editing and navigation -*- lexical-binding: t; -*-

(require 'pulse)
(require 'seq)

(defvar consoli-pulse--companions nil
  "Overlays sharing the lifetime and face of the current Emacs pulse.")

(defun consoli-pulse--clear-companions (&rest _)
  (mapc #'delete-overlay consoli-pulse--companions)
  (setq consoli-pulse--companions nil))

(advice-add 'pulse-momentary-unhighlight :after #'consoli-pulse--clear-companions)

(defun consoli-pulse--ranges (ranges)
  "Pulse nonempty RANGES together, including disjoint spans."
  (let ((spans (seq-filter (lambda (span) (/= (car span) (cdr span))) ranges)))
    (when spans
      (pulse-momentary-highlight-region (caar spans) (cdar spans))
      (when-let* ((primary pulse-momentary-overlay)
                  (face (overlay-get primary 'face)))
        (dolist (span (cdr spans))
          (let ((overlay (make-overlay (car span) (cdr span))))
            (overlay-put overlay 'face face)
            (overlay-put overlay 'priority (overlay-get primary 'priority))
            (push overlay consoli-pulse--companions)))))))

(defun consoli-pulse--copy (beg end &optional region)
  "Pulse the bounds passed by both Emacs and Smartparens copy commands."
  (consoli-pulse--ranges (if region (region-bounds) (list (cons beg end)))))

(advice-add 'copy-region-as-kill :after #'consoli-pulse--copy)

(defun consoli-pulse--yank (&rest _)
  "Pulse the inserted span after a successful yank or replacement."
  (when (and (eq this-command 'yank) (mark t) (/= (point) (mark t)))
    (pulse-momentary-highlight-region (region-beginning) (region-end))))

(dolist (command '(yank yank-pop consult-yank-from-kill-ring))
  (advice-add command :after #'consoli-pulse--yank))

(defun consoli-pulse--transpose (transpose &rest args)
  "Pulse the entire transposed block once, including repeated swaps."
  (let* ((beg nil) (end nil)
         ;; Each swap moves text without changing total length.  Its first
         ;; removal reaches the old end, and its last insert starts at the front.
         (record (lambda (from to)
                   (setq beg (min (or beg from) from)
                         end (max (or end to) to)))))
    (add-hook 'before-change-functions record nil t)
    (unwind-protect
        (prog1 (apply transpose args)
          (when (and beg (< beg end))
            (pulse-momentary-highlight-region beg end)))
      (remove-hook 'before-change-functions record t))))

(advice-add 'sp-transpose-sexp :around #'consoli-pulse--transpose)

(defvar sp-pair-list)

(defun consoli-pulse--wrap (wrap open)
  "Track the delimiters inserted by WRAP through any reindentation."
  ;; Smartparens inserts the closer first, then the opener.  Tracking these
  ;; edits also handles selected text containing unmatched delimiters.
  (let* ((expected (list (cdr (assoc open sp-pair-list)) open))
         (spans nil)
         (record (lambda (beg end old-length)
                   (when (and expected (zerop old-length)
                              (equal (car expected) (buffer-substring-no-properties beg end)))
                     (pop expected)
                     (push (cons (copy-marker beg t) (copy-marker end)) spans)))))
    (add-hook 'after-change-functions record nil t)
    (unwind-protect
        (prog1 (funcall wrap open)
          (unless expected (consoli-pulse--ranges spans)))
      (remove-hook 'after-change-functions record t)
      (dolist (span spans)
        (set-marker (car span) nil)
        (set-marker (cdr span) nil)))))

(advice-add 'sp-wrap-with-pair :around #'consoli-pulse--wrap)

(defvar avy-action)
(defvar avy-action-oneshot)

(defun consoli-pulse--avy (jump &rest args)
  "Pulse the destination after a window change or a jump of ten lines."
  (let ((window (selected-window))
        (origin (line-beginning-position)))
    (prog1 (apply jump args)
      (when (and (memq (or avy-action avy-action-oneshot) '(nil identity avy-action-goto))
                 (or (not (eq window (selected-window)))
                     (>= (count-lines (min origin (line-beginning-position))
                                      (max origin (line-beginning-position))) 10)))
        (pulse-momentary-highlight-one-line (point))))))

(dolist (command '(avy-goto-char-timer avy-goto-line avy-goto-subword-0
                   avy-goto-word-0 avy-goto-symbol-1))
  (advice-add command :around #'consoli-pulse--avy))

(provide 'consoli-pulse)
;;; consoli-pulse.el ends here
