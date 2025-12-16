;;; font-families.el --- Font family selection and fallbacks -*- lexical-binding: t; -*-

;;; Commentary:
;; This module handles font family selection with intelligent fallbacks.

;;; Code:

(defgroup consoli-config nil
  "Personal Emacs configuration settings."
  :group 'environment
  :prefix "consoli-config/")

(defconst consoli-config/font-families
  '((ui-font
     ("Atkinson Hyperlegible Mono" "SF Mono"))
    (programming-font
     ("Myna" "Victor Mono"))
    (org-font
     ("Margem Rounded Test" "Maple Mono"))
    (modeline-font
     ("Reddit Mono"))
    (alternative-font
     ("Victor Mono")))
  "Font families with fallback preferences.")

(defun consoli-config/font-available-p (font-family)
  "Check if FONT-FAMILY is available on the system."
  (when (and (display-graphic-p) font-family)
    (condition-case nil
        (member font-family (font-family-list))
      (error nil))))

(defun consoli-config/select-font (font-list)
  "Select first available font from FONT-LIST, or fallback to monospace."
  (or (seq-find #'consoli-config/font-available-p font-list)
      "monospace"))

(defconst ui-font
  (consoli-config/select-font (alist-get 'ui-font consoli-config/font-families))
  "Selected UI font with fallback.")

(defconst programming-font
  (consoli-config/select-font (alist-get 'programming-font consoli-config/font-families))
  "Selected programming font with fallback.")

(defconst org-font
  (consoli-config/select-font (alist-get 'org-font consoli-config/font-families))
  "Selected org-mode font with fallback.")

(defconst modeline-font
  (consoli-config/select-font (alist-get 'modeline-font consoli-config/font-families))
  "Selected modeline font with fallback.")

(defconst alternative-programming-font
  (consoli-config/select-font (alist-get 'alternative-font consoli-config/font-families))
  "Selected alternative programming font with fallback.")

(provide 'font-families)
;;; font-families.el ends here
