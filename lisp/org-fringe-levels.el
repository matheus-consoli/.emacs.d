;;; org-fringe-levels.el --- Colored fringe indicators for org headings -*- lexical-binding: t; -*-

;;; Commentary:
;; Adds thin vertical colored lines in the left fringe for org-mode headings.
;; Each heading level (1-8) gets a distinct color matching org-level-N faces.

;;; Code:

(require 'org)

(defgroup org-fringe-levels nil
  "Fringe indicators for org headings."
  :group 'org
  :prefix "org-fringe-levels-")

(defcustom org-fringe-levels-side 'left-fringe
  "Which fringe to display level indicators."
  :type '(choice (const :tag "Left fringe" left-fringe)
                 (const :tag "Right fringe" right-fringe))
  :group 'org-fringe-levels)

(defvar-local org-fringe-levels--overlays nil
  "List of overlays for fringe indicators.")

(define-fringe-bitmap 'org-fringe-level-line
  [#b11100000] nil nil '(center repeated))

(defun org-fringe-levels--face-for-level (level)
  "Return face symbol for heading LEVEL (1-8)."
  (intern (format "org-level-%d" (max 1 (min 8 level)))))

(defun org-fringe-levels--clear-overlays ()
  "Remove all fringe level overlays."
  (mapc #'delete-overlay org-fringe-levels--overlays)
  (setq org-fringe-levels--overlays nil))

(defun org-fringe-levels--add-indicator (pos level)
  "Add fringe indicator at POS for heading LEVEL."
  (let* ((face (org-fringe-levels--face-for-level level))
         (display-spec `(,org-fringe-levels-side org-fringe-level-line ,face))
         (ov (make-overlay pos (1+ pos) nil t nil)))
    (overlay-put ov 'before-string (propertize "x" 'display display-spec))
    (overlay-put ov 'org-fringe-level level)
    (push ov org-fringe-levels--overlays)
    ov))

(defun org-fringe-levels--refresh ()
  "Refresh all fringe indicators in current buffer."
  (when (and org-fringe-levels-mode (derived-mode-p 'org-mode))
    (org-fringe-levels--clear-overlays)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ " nil t)
        (let ((level (1- (- (match-end 0) (match-beginning 0)))))
          (org-fringe-levels--add-indicator (match-beginning 0) level))))))

(defun org-fringe-levels--after-change (beg _end _len)
  "Refresh indicators after buffer change at BEG."
  (when org-fringe-levels-mode
    (save-excursion
      (goto-char beg)
      (let ((line-beg (line-beginning-position))
            (line-end (line-end-position)))
        ;; Remove overlays on this line
        (dolist (ov org-fringe-levels--overlays)
          (when (and (overlay-buffer ov)
                     (<= line-beg (overlay-start ov))
                     (<= (overlay-start ov) line-end))
            (setq org-fringe-levels--overlays (delq ov org-fringe-levels--overlays))
            (delete-overlay ov)))
        ;; Add indicator if this is a heading
        (goto-char line-beg)
        (when (looking-at "^\\*+ ")
          (let ((level (1- (- (match-end 0) (match-beginning 0)))))
            (org-fringe-levels--add-indicator (match-beginning 0) level)))))))

(defun org-fringe-levels--enable ()
  "Enable fringe level indicators."
  (org-fringe-levels--refresh)
  (add-hook 'after-change-functions #'org-fringe-levels--after-change nil t))

(defun org-fringe-levels--disable ()
  "Disable fringe level indicators."
  (remove-hook 'after-change-functions #'org-fringe-levels--after-change t)
  (org-fringe-levels--clear-overlays))

;;;###autoload
(define-minor-mode org-fringe-levels-mode
  "Display colored fringe indicators for org heading levels."
  :lighter nil
  :group 'org-fringe-levels
  (if org-fringe-levels-mode
      (org-fringe-levels--enable)
    (org-fringe-levels--disable)))

(provide 'org-fringe-levels)
;;; org-fringe-levels.el ends here
