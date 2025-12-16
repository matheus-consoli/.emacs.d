;;; font-contexts.el --- Context-aware font sizing -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides context-aware font sizing with simplified scaling logic.

;;; Code:

(require 'font-families)

(defcustom consoli-config/font-scaling-enabled t
  "Enable dynamic font scaling based on frame size."
  :type 'boolean
  :group 'consoli-config)

(defcustom consoli-config/base-font-size 90
  "Base font size in 1/10th of a point (7 point default)."
  :type '(integer :min 60 :max 200)
  :group 'consoli-config)

(defcustom consoli-config/font-scaling-factor 1.0
  "Global font scaling multiplier (user preference)."
  :type '(float :min 0.5 :max 2.0)
  :group 'consoli-config)

(defconst consoli-config/context-ratios
  '((ui . 1.0)
    (programming . 0.9)
    (org . 1.1)
    (small . 0.7)
    (modeline . 0.8)
    (tab-bar . 0.8)
    (centaur-tabs . 0.8)
    (echo-area . 0.8))
  "Font size ratios relative to base size.")

(defconst consoli-config/display-categories
  '((small . 0.9)   ; < 1500x1000
    (normal . 1.0)  ; 1500x1000 to 2500x1600
    (large . 1.1))  ; > 2500x1600
  "Display size multipliers.")

(defun consoli-config/get-display-category (&optional frame)
  "Get display category (small/normal/large) for FRAME."
  (let* ((frame (or frame (selected-frame)))
         (width (display-pixel-width frame))
         (height (display-pixel-height frame))
         (total-pixels (* width height)))
    (cond
     ((< total-pixels 1500000) 'small)
     ((> total-pixels 4000000) 'large)
     (t 'normal))))

(defun consoli-config/get-frame-category (&optional frame)
  "Get frame size category for FRAME."
  (let* ((frame (or frame (selected-frame)))
         (width (frame-pixel-width frame))
         (height (frame-pixel-height frame))
         (aspect-ratio (/ (float width) height)))
    (cond
     ((< width 800) 'small)
     ((and (> width 2000) (> aspect-ratio 1.5)) 'wide)
     ((< aspect-ratio 0.8) 'tall)
     (t 'normal))))

(defun consoli-config/calculate-font-size (context &optional frame)
  "Calculate font size for CONTEXT and optional FRAME."
  (if (not consoli-config/font-scaling-enabled)
      (let ((static-size (round (* consoli-config/base-font-size
                                   (alist-get context consoli-config/context-ratios 1.0)))))
        (max 60 static-size))
    
    (let* ((base-size consoli-config/base-font-size)
           (context-ratio (alist-get context consoli-config/context-ratios 1.0))
           (display-category (consoli-config/get-display-category frame))
           (display-multiplier (alist-get display-category consoli-config/display-categories 1.0))
           (frame-category (consoli-config/get-frame-category frame))
           (frame-adjustment (pcase frame-category
                               ('small 0.95)
                               ('wide 0.95)
                               ('tall 1.05)
                               (_ 1.0)))
           (user-scale consoli-config/font-scaling-factor))
      
      (max 60 (round (* base-size context-ratio display-multiplier 
                        frame-adjustment user-scale))))))

(defun consoli-config/font-height-ui (&optional frame)
  "Get UI font height for FRAME."
  (consoli-config/calculate-font-size 'ui frame))

(defun consoli-config/font-height-programming (&optional frame)
  "Get programming font height for FRAME."
  (consoli-config/calculate-font-size 'programming frame))

(defun consoli-config/font-height-org (&optional frame)
  "Get org font height for FRAME."
  (consoli-config/calculate-font-size 'org frame))

(defun consoli-config/font-height-modeline (&optional frame)
  "Get modeline font height for FRAME."
  (consoli-config/calculate-font-size 'modeline frame))

(defun consoli-config/font-height-small (&optional frame)
  "Get small font height for FRAME."
  (consoli-config/calculate-font-size 'small frame))

(defun consoli-config/font-height-tab-bar (&optional frame)
  "Get tab-bar font height for FRAME."
  (consoli-config/calculate-font-size 'tab-bar frame))

(defun consoli-config/font-height-centaur-tabs (&optional frame)
  "Get centaur-tabs font height for FRAME."
  (consoli-config/calculate-font-size 'centaur-tabs frame))

(defun consoli-config/font-height-echo-area (&optional frame)
  "Get echo-area font height for FRAME."
  (consoli-config/calculate-font-size 'echo-area frame))

(defun consoli-config/adjust-font-scale (factor)
  "Adjust global font scaling FACTOR interactively."
  (interactive "nFont scaling factor (1.0 = normal): ")
  (setq consoli-config/font-scaling-factor factor)
  (message "Font scaling set to %.2f" factor))

(defun consoli-config/toggle-font-scaling ()
  "Toggle dynamic font scaling on/off."
  (interactive)
  (setq consoli-config/font-scaling-enabled
        (not consoli-config/font-scaling-enabled))
  (message "Dynamic font scaling %s"
           (if consoli-config/font-scaling-enabled "enabled" "disabled")))

(defun consoli-config/show-font-info ()
  "Display current font configuration and display metrics."
  (interactive)
  (let* ((frame (selected-frame))
         (width (frame-pixel-width frame))
         (height (frame-pixel-height frame))
         (display-cat (consoli-config/get-display-category frame))
         (frame-cat (consoli-config/get-frame-category frame))
         (ui-size (consoli-config/font-height-ui frame))
         (prog-size (consoli-config/font-height-programming frame)))
    (message "Frame: %dx%d | Display: %s | Frame: %s | UI: %d | Prog: %d"
             width height display-cat frame-cat ui-size prog-size)))

(provide 'font-contexts)
;;; font-contexts.el ends here
