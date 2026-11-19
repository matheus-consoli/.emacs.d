;;; bright-tale-theme.el --- A sophisticated light theme with modern aesthetics -*- lexical-binding: t; -*-

(require 'tale-themes-common)

(deftheme bright-tale "A sophisticated light theme with modern aesthetics.")

(let ((bright-palette
       '((bg-main . "#fefefe")
         (bg-alt . "#f5f5f5")
         (bg-popup . "#f8f8f8")
         (bg-contrast . "#eeeeee")
         (bg-code . "#f2f2f2")
         (bg-dim . "#e8e8e8")
         (bg-selection . "#d8d8d8")
         (bg-special . "#f0f0f0")

         (fg-main . "#2e2e2e")
         (fg-bright . "#1a1a1a")
         (fg-popup . "#2a2a2a")

         (grey-subtle . "#6e6e6e")
         (grey-border . "#c0c0c0")
         (grey-docstring . "#555555")
         (grey-comment-blue . "#5a6b8c")

         (purple . "#7c3aed")
         (lavender . "#8553f6")
         (magenta . "#db2777")
         (rose . "#e11d48")
         (green . "#04855d")
         (teal . "#077f9c")
         (yellow . "#a25904")
         (blue . "#2563eb")

         (focus-bg . "#e5e7eb")
         (hover-bg . "#f3f4f6")
         (primary-selection . "#f3f4f6")
         (secondary-selection . "#ddd6fe")
         (visual-selection . "#e0e7ff")

         (success-fg . "#166534")
         (warning-fg . "#8a4b0e")
         (error-fg . "#9f1d32")

         (diff-added-bg . "#dcfce7")
         (diff-added-fg . "#166534")
         (diff-removed-bg . "#fecaca")
         (diff-removed-fg . "#dc2626")
         (diff-changed-bg . "#fef3c7")
         (diff-changed-fg . "#d97706")

         (hint-fg . "#9ca3af")
         (completion-bg . "#f9fafb"))))

  (tale-themes--create-theme 'bright-tale bright-palette)
  (tale-themes--set-variables 'bright-tale bright-palette))

;;;###autoload
(defun bright-tale-theme ()
  "Apply the bright-tale-theme."
  (interactive)
  (load-theme 'bright-tale t))

(provide-theme 'bright-tale)

;;; bright-tale-theme.el ends here
