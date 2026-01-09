;;; font-config.el --- Unified font configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidated font system: families, sizing, and dynamic scaling.
;; Replaces font-families.el, font-contexts.el, and font-scaling.el.

;;; Code:

(defgroup consoli-fonts nil
  "Font configuration."
  :group 'faces
  :prefix "consoli-fonts/")

;;; Font Families

(defconst consoli-fonts/families
  '((ui          . ("Atkinson Hyperlegible Mono" "SF Mono"))
    (programming . ("Dank Mono" "Victor Mono" "Myna"))
    (org         . ("Margem Rounded Test" "Maple Mono"))
    (modeline    . ("Reddit Mono" "SF Mono"))
    (alt-mono    . ("Victor Mono" "Myna")))
  "Font families with fallback preferences.")

(defun consoli-fonts/--available-p (family)
  "Check if FAMILY is available."
  (and (display-graphic-p)
       family
       (member family (font-family-list))))

(defun consoli-fonts/get (category)
  "Get first available font for CATEGORY, or monospace."
  (or (seq-find #'consoli-fonts/--available-p
                (alist-get category consoli-fonts/families))
      "monospace"))

;;; Font Sizing

(defcustom consoli-fonts/base-size 110
  "Base font size in 1/10th points (110 = 11pt)."
  :type 'integer
  :group 'consoli-fonts)

(defcustom consoli-fonts/scaling-enabled t
  "Enable dynamic font scaling based on display characteristics."
  :type 'boolean
  :group 'consoli-fonts)

(defcustom consoli-fonts/user-scale 1.0
  "User scaling multiplier."
  :type 'float
  :group 'consoli-fonts)

(defconst consoli-fonts/context-ratios
  '((ui . 1.0)
    (programming . 1.0)
    (org . 1.1)
    (small . 0.7)
    (modeline . 0.8)
    (tab-bar . 0.8)
    (centaur-tabs . 0.8)
    (echo-area . 0.8)
    (comment . 0.9))
  "Font size ratios relative to base size.")

(defun consoli-fonts/--ppi (&optional frame)
  "Calculate diagonal PPI for FRAME. Falls back to 96."
  (let* ((frame (or frame (selected-frame)))
         (px-w (float (display-pixel-width frame)))
         (px-h (float (display-pixel-height frame)))
         (mm-w (display-mm-width frame))
         (mm-h (display-mm-height frame)))
    (if (and mm-w mm-h (> mm-w 0) (> mm-h 0))
        (let ((diag-px (sqrt (+ (* px-w px-w) (* px-h px-h))))
              (diag-mm (sqrt (+ (* (float mm-w) (float mm-w))
                                (* (float mm-h) (float mm-h))))))
          (/ diag-px (/ diag-mm 25.4)))
      96.0)))

(defun consoli-fonts/--ppi-scale (&optional frame)
  "Font scale based on PPI. High PPI needs larger fonts."
  (let ((ppi (consoli-fonts/--ppi frame)))
    (cond
     ((< ppi 100) 1.0)
     ((< ppi 140) (+ 1.0 (* 0.001 (- ppi 100))))
     ((< ppi 200) (+ 1.04 (* 0.001 (- ppi 140))))
     (t (min 1.2 (+ 1.1 (* 0.001 (- ppi 200))))))))

(defun consoli-fonts/--frame-scale (&optional frame)
  "Adjustment for frame coverage of display (for tiling WMs)."
  (let* ((frame (or frame (selected-frame)))
         (f-area (* (float (frame-pixel-width frame))
                    (float (frame-pixel-height frame))))
         (d-area (* (float (display-pixel-width frame))
                    (float (display-pixel-height frame))))
         (coverage (if (> d-area 0) (/ f-area d-area) 1.0)))
    (cond
     ((< coverage 0.2) 0.95)
     ((< coverage 0.35) 0.98)
     (t 1.0))))

(defun consoli-fonts/size (context &optional frame)
  "Calculate font size for CONTEXT and FRAME."
  (if (not consoli-fonts/scaling-enabled)
      (max 60 (round (* consoli-fonts/base-size
                        (alist-get context consoli-fonts/context-ratios 1.0))))
    (let* ((base consoli-fonts/base-size)
           (ctx-ratio (alist-get context consoli-fonts/context-ratios 1.0))
           (ppi-s (consoli-fonts/--ppi-scale frame))
           (frm-s (consoli-fonts/--frame-scale frame))
           (usr-s consoli-fonts/user-scale)
           (result (round (* base ctx-ratio ppi-s frm-s usr-s))))
      (max 70 (min 200 result)))))

;;; Font Application

(defvar consoli-fonts/--last-frame-size nil
  "Last frame size for change detection.")

(defvar consoli-fonts/--update-timer nil
  "Timer for debounced font updates.")

(defun consoli-fonts/--frame-size-changed-p (&optional frame)
  "Check if frame size changed significantly (>15%)."
  (let* ((frame (or frame (selected-frame)))
         (current-area (* (frame-pixel-width frame) (frame-pixel-height frame)))
         (last-area (when consoli-fonts/--last-frame-size
                      (* (car consoli-fonts/--last-frame-size)
                         (cdr consoli-fonts/--last-frame-size)))))
    (or (null last-area)
        (> (abs (- current-area last-area))
           (* 0.15 last-area)))))

(defun consoli-fonts/apply-to-frame (&optional frame)
  "Apply fonts to FRAME and update all faces."
  (when (display-graphic-p frame)
    (let ((ui-size (consoli-fonts/size 'ui frame))
          (prog-size (consoli-fonts/size 'programming frame))
          (org-size (consoli-fonts/size 'org frame))
          (modeline-size (consoli-fonts/size 'modeline frame))
          (tab-bar-size (consoli-fonts/size 'tab-bar frame))
          (centaur-tabs-size (consoli-fonts/size 'centaur-tabs frame))
          (small-size (consoli-fonts/size 'small frame))
          (comment-size (consoli-fonts/size 'comment frame)))

      (when frame
        (set-frame-parameter frame 'font
                             (format "%s-%d" (consoli-fonts/get 'ui) (/ ui-size 10))))

      (when (featurep 'org)
        (set-face-attribute 'variable-pitch nil
                            :family (consoli-fonts/get 'org)
                            :height org-size)
        (set-face-attribute 'fixed-pitch nil
                            :family (consoli-fonts/get 'alt-mono)
                            :height modeline-size)
        (set-face-attribute 'font-lock-doc-face nil
                            :height prog-size))

      (when (featurep 'tab-bar)
        (set-face-attribute 'tab-bar nil
                            :family (consoli-fonts/get 'alt-mono)
                            :height tab-bar-size))

      (when (featurep 'centaur-tabs)
        (centaur-tabs-change-fonts (consoli-fonts/get 'alt-mono) centaur-tabs-size))

      (when (featurep 'vterm)
        (set-face-attribute 'vterm-color-default nil
                            :family (consoli-fonts/get 'programming)
                            :height small-size))

      (set-face-attribute 'font-lock-comment-face nil :height comment-size)
      (set-face-attribute 'font-lock-comment-delimiter-face nil :height comment-size)

      (setq consoli-fonts/--last-frame-size
            (cons (frame-pixel-width frame) (frame-pixel-height frame))))))

(defun consoli-fonts/--on-resize (&optional frame)
  "Handle frame resize with debouncing."
  (when (and consoli-fonts/scaling-enabled
             (consoli-fonts/--frame-size-changed-p frame))
    (when (timerp consoli-fonts/--update-timer)
      (cancel-timer consoli-fonts/--update-timer))
    (setq consoli-fonts/--update-timer
          (run-with-timer 0.3 nil
                          (lambda ()
                            (consoli-fonts/apply-to-frame frame)
                            (setq consoli-fonts/--update-timer nil))))))

(defun consoli-fonts/init ()
  "Initialize font system. Call from early-init.el."
  (when (display-graphic-p)
    (let ((ui-size (consoli-fonts/size 'ui)))
      (add-to-list 'default-frame-alist
                   `(font . ,(format "%s-%d" (consoli-fonts/get 'ui) (/ ui-size 10)))))
    (add-hook 'window-size-change-functions #'consoli-fonts/--on-resize)
    (setq consoli-fonts/--last-frame-size
          (cons (frame-pixel-width) (frame-pixel-height)))))

;;; Interactive

(defun consoli-fonts/adjust-scale (factor)
  "Set user scaling FACTOR."
  (interactive "nFont scaling factor (1.0 = normal): ")
  (setq consoli-fonts/user-scale factor)
  (consoli-fonts/apply-to-frame)
  (message "Font scale: %.2f" factor))

(defun consoli-fonts/toggle-scaling ()
  "Toggle dynamic font scaling."
  (interactive)
  (setq consoli-fonts/scaling-enabled (not consoli-fonts/scaling-enabled))
  (message "Font scaling %s" (if consoli-fonts/scaling-enabled "enabled" "disabled")))

(defun consoli-fonts/info ()
  "Show font configuration summary."
  (interactive)
  (let* ((f (selected-frame))
         (ppi (consoli-fonts/--ppi f))
         (ppi-s (consoli-fonts/--ppi-scale f))
         (frm-s (consoli-fonts/--frame-scale f)))
    (message "PPI: %.0f (×%.2f) | Frame: ×%.2f | User: ×%.1f | UI: %dpt | Prog: %dpt | Org: %dpt"
             ppi ppi-s frm-s consoli-fonts/user-scale
             (/ (consoli-fonts/size 'ui f) 10)
             (/ (consoli-fonts/size 'programming f) 10)
             (/ (consoli-fonts/size 'org f) 10))))

(provide 'font-config)
;;; font-config.el ends here
