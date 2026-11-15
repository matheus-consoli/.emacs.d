;;; font-config.el --- Unified font configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Consolidated font system: families, sizing, and dynamic scaling.
;; Replaces font-families.el, font-contexts.el, and font-scaling.el.

;;; Code:

(require 'face-remap)
(require 'seq)

(defgroup consoli-fonts nil
  "Font configuration."
  :group 'faces
  :prefix "consoli-fonts/")

;;; Font Families

(defconst consoli-fonts/families
  '((ui          . ("SF Mono" "Atkinson Hyperlegible Mono"))
    (programming . ("Dank Mono" "Victor Mono" "Myna"))
    (org         . ("Monaspace Radon Frozen" "Margem Rounded Test" "Maple Mono"))
    (modeline    . ("Reddit Mono" "SF Mono"))
    (alt-mono    . ("Victor Mono" "Myna")))
  "Font families with fallback preferences.")

(defun consoli-fonts/--family (category available)
  "Choose the preferred font for CATEGORY from AVAILABLE families."
  (or (seq-find (lambda (family) (member family available))
                (alist-get category consoli-fonts/families))
      "monospace"))

(defun consoli-fonts/get (category &optional frame)
  "Get the preferred family for CATEGORY on FRAME."
  (consoli-fonts/--family
   category (when (display-graphic-p frame) (font-family-list frame))))

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

(defun consoli-fonts/--ppi (frame)
  "Return the dominant monitor's PPI for FRAME, or 96 when unknown."
  (let* ((monitor (and (display-graphic-p frame)
                       (frame-monitor-attributes frame)))
         (geometry (alist-get 'geometry monitor))
         (width (nth 2 geometry))
         (height (nth 3 geometry))
         (mm-size (alist-get 'mm-size monitor))
         (mm-width (car mm-size))
         (mm-height (cadr mm-size)))
    (if (and width height mm-width mm-height
             (> width 0) (> height 0) (> mm-width 0) (> mm-height 0))
        (* 25.4 (sqrt (/ (+ (* width width) (* height height))
                         (float (+ (* mm-width mm-width) (* mm-height mm-height))))))
      96.0)))

(defun consoli-fonts/--scale (frame)
  "Return the automatic and manual scaling multiplier for FRAME."
  (* consoli-fonts/user-scale
     (if consoli-fonts/scaling-enabled
         (min 1.2 (+ 1.0 (* 0.001 (max 0 (- (consoli-fonts/--ppi frame) 100)))))
       1.0)))

(defun consoli-fonts/--height (context scaled-base)
  "Return the face height for CONTEXT given SCALED-BASE in tenths of a point."
  (max 70 (min 200 (round (* scaled-base
                            (alist-get context consoli-fonts/context-ratios 1.0))))))

(defun consoli-fonts/size (context &optional frame)
  "Calculate the face height for CONTEXT on FRAME."
  (consoli-fonts/--height context (* consoli-fonts/base-size (consoli-fonts/--scale frame))))

;;; Font Application

(defface consoli-fonts/programming '((t nil))
  "Font for source buffers and Vterm."
  :group 'consoli-fonts)

(defface consoli-fonts/transient '((t nil))
  "Font for transient menus."
  :group 'consoli-fonts)

(defface consoli-fonts/echo-area '((t (:inherit mode-line :box nil)))
  "Font for the echo area."
  :group 'consoli-fonts)

(defconst consoli-fonts/--faces
  '((default ui ui)
    (consoli-fonts/programming programming programming)
    (consoli-fonts/transient ui small)
    (consoli-fonts/echo-area modeline echo-area)
    (mode-line modeline modeline)
    (mode-line-active modeline modeline)
    (mode-line-inactive modeline modeline)
    (variable-pitch org org)
    (fixed-pitch alt-mono programming)
    (font-lock-doc-face nil programming)
    (font-lock-comment-face programming comment)
    (font-lock-comment-delimiter-face nil comment)
    (tab-bar alt-mono tab-bar)
    (centaur-tabs-selected alt-mono centaur-tabs)
    (centaur-tabs-selected-modified alt-mono centaur-tabs)
    (centaur-tabs-unselected alt-mono centaur-tabs)
    (centaur-tabs-unselected-modified alt-mono centaur-tabs))
  "Faces with their family and size categories.")

(defun consoli-fonts/apply-to-frame (&optional frame force)
  "Apply fonts to graphical FRAME, defaulting to the selected frame.
With FORCE, recompute font settings even at the same scale."
  (let ((frame (or frame (selected-frame))))
    (when (and (frame-live-p frame) (display-graphic-p frame))
      (let ((scaled-base (* consoli-fonts/base-size (consoli-fonts/--scale frame))))
        (when (or force (not (equal scaled-base (frame-parameter frame 'consoli-font-scaled-base))))
          (let ((available (font-family-list frame)))
            (pcase-dolist (`(,face ,family ,context) consoli-fonts/--faces)
              (when (facep face)
                (let ((height (consoli-fonts/--height context scaled-base)))
                  (if family
                      (set-face-attribute face frame
                                          :family (consoli-fonts/--family family available)
                                          :height height)
                    (set-face-attribute face frame :height height)))))
            (set-frame-parameter frame 'consoli-font-scaled-base scaled-base)))))))

(defun consoli-fonts/--refresh-frames (&optional _theme)
  "Apply the current font preferences to all live graphical frames."
  (dolist (frame (frame-list))
    (consoli-fonts/apply-to-frame frame t)))

(defun consoli-fonts/--remap-default (face)
  "Add FACE to the current buffer's default face remaps once."
  (unless (memq face (alist-get 'default face-remapping-alist))
    (face-remap-add-relative 'default face)))

(defun consoli-fonts/use-programming-font ()
  "Use the programming font in the current buffer."
  (consoli-fonts/--remap-default 'consoli-fonts/programming))

(defun consoli-fonts/--use-transient-font ()
  "Use the transient font in the current menu buffer."
  (consoli-fonts/--remap-default 'consoli-fonts/transient))

(defun consoli-fonts/init ()
  "Register font updates for startup, new frames and monitor changes."
  (add-hook 'after-init-hook #'consoli-fonts/--refresh-frames t)
  (add-hook 'after-make-frame-functions #'consoli-fonts/apply-to-frame t)
  (add-hook 'move-frame-functions #'consoli-fonts/apply-to-frame)
  (add-hook 'window-size-change-functions #'consoli-fonts/apply-to-frame)
  ;; Theme changes recalculate faces and can clear frame-local attributes.
  (add-hook 'enable-theme-functions #'consoli-fonts/--refresh-frames t)
  (add-hook 'disable-theme-functions #'consoli-fonts/--refresh-frames t)
  (add-hook 'prog-mode-hook #'consoli-fonts/use-programming-font)
  (add-hook 'vterm-mode-hook #'consoli-fonts/use-programming-font)
  (add-hook 'transient-setup-buffer-hook #'consoli-fonts/--use-transient-font)
  (with-eval-after-load 'centaur-tabs
    (consoli-fonts/--refresh-frames)))

;;; Interactive commands

(defun consoli-fonts/adjust-scale (factor)
  "Set the manual font scaling multiplier to FACTOR."
  (interactive "nFont scaling factor (1.0 = normal): ")
  (unless (> factor 0)
    (user-error "Font scaling factor must be positive"))
  (setq consoli-fonts/user-scale factor)
  (consoli-fonts/--refresh-frames)
  (message "Font scale %.2f" factor))

(defun consoli-fonts/toggle-scaling ()
  "Toggle automatic monitor scaling and update the current frames."
  (interactive)
  (setq consoli-fonts/scaling-enabled (not consoli-fonts/scaling-enabled))
  (consoli-fonts/--refresh-frames)
  (message "Automatic font scaling %s" (if consoli-fonts/scaling-enabled "enabled" "disabled")))

(defun consoli-fonts/info ()
  "Show the applied fonts on the selected frame."
  (interactive)
  (if (display-graphic-p)
      (message "PPI %.0f | Scale ×%.2f | UI %.1f pt | Programming %.1f pt | Org %.1f pt"
               (consoli-fonts/--ppi (selected-frame))
               (consoli-fonts/--scale (selected-frame))
               (/ (face-attribute 'default :height) 10.0)
               (/ (face-attribute 'consoli-fonts/programming :height nil 'default) 10.0)
               (/ (face-attribute 'variable-pitch :height nil 'default) 10.0))
    (message "The terminal controls its font size and family")))

(provide 'font-config)
;;; font-config.el ends here
