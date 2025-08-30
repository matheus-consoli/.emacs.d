;;; font-scaling.el --- Simplified font scaling system -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides simplified font scaling that integrates with Emacs frames
;; and face system.

;;; Code:

(require 'font-families)
(require 'font-contexts)

(defvar consoli-config/--last-frame-size nil
  "Last frame size for change detection.")

(defvar consoli-config/--font-update-timer nil
  "Timer for debounced font updates.")

(defun consoli-config/frame-size-changed-p (&optional frame)
  "Check if frame size changed significantly."
  (let* ((frame (or frame (selected-frame)))
         (current-width (frame-pixel-width frame))
         (current-height (frame-pixel-height frame))
         (current-area (* current-width current-height))
         (last-area (when consoli-config/--last-frame-size
                      (* (car consoli-config/--last-frame-size)
                         (cdr consoli-config/--last-frame-size)))))
    (or (null last-area)
        (> (abs (- current-area last-area))
           (* 0.15 last-area)))))

(defun consoli-config/update-frame-fonts (&optional frame)
  "Update all font faces for FRAME with dynamic sizing."
  (when (display-graphic-p frame)
    (let ((ui-size (consoli-config/font-height-ui frame))
          (prog-size (consoli-config/font-height-programming frame))
          (org-size (consoli-config/calculate-font-size 'org frame))
          (modeline-size (consoli-config/calculate-font-size 'modeline frame))
          (small-size (consoli-config/calculate-font-size 'small frame))
          (tab-bar-size (consoli-config/calculate-font-size 'tab-bar frame))
          (centaur-tabs-size (consoli-config/calculate-font-size 'centaur-tabs frame)))

      (when frame
        (set-frame-parameter frame 'font
                             (format "%s-%d" ui-font (/ ui-size 10))))

      (when (featurep 'org)
        (set-face-attribute 'variable-pitch nil
                            :family org-font
                            :height org-size)
        (set-face-attribute 'fixed-pitch nil
                            :family alternative-programming-font
                            :height modeline-size)
        (set-face-attribute 'font-lock-doc-face nil
                            :height prog-size))

      (when (featurep 'tab-bar)
        (set-face-attribute 'tab-bar nil
                            :family alternative-programming-font
                            :height tab-bar-size))

      (when (featurep 'centaur-tabs)
        (centaur-tabs-change-fonts alternative-programming-font centaur-tabs-size))

      (when (featurep 'vterm)
        (set-face-attribute 'vterm-color-default nil
                            :family programming-font
                            :height small-size))

      (setq consoli-config/--last-frame-size
            (cons (frame-pixel-width frame) (frame-pixel-height frame))))))

(defun consoli-config/on-frame-size-change (&optional frame)
  "Handle frame size changes with simple debouncing."
  (when (and consoli-config/font-scaling-enabled
             (consoli-config/frame-size-changed-p frame))

    (when (and consoli-config/--font-update-timer
               (timerp consoli-config/--font-update-timer))
      (condition-case nil
          (cancel-timer consoli-config/--font-update-timer)
        (error nil)))

    (setq consoli-config/--font-update-timer
          (run-with-timer 0.3 nil
                          (lambda ()
                            (consoli-config/update-frame-fonts frame)
                            (setq consoli-config/--font-update-timer nil))))))

(defun consoli-config/init-font-scaling ()
  "Initialize font scaling system."
  (when (display-graphic-p)
    (let ((initial-ui-size (consoli-config/font-height-ui)))
      (add-to-list 'default-frame-alist
                   `(font . ,(format "%s-%d" ui-font (/ initial-ui-size 10)))))

    (add-hook 'window-size-change-functions #'consoli-config/on-frame-size-change)

    (setq consoli-config/--last-frame-size
          (cons (frame-pixel-width (selected-frame))
                (frame-pixel-height (selected-frame))))))

(provide 'font-scaling)
;;; font-scaling.el ends here
