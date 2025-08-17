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
         (bg-inactive-sel . "#e0e0e0")
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
         (lavender . "#8b5cf6")
         (magenta . "#db2777")
         (rose . "#e11d48")
         (green . "#059669")
         (teal . "#0891b2")
         (yellow . "#d97706")
         (blue . "#2563eb")

         (focus-bg . "#e5e7eb")
         (hover-bg . "#f3f4f6")
         (primary-selection . "#f3f4f6")
         (secondary-selection . "#ddd6fe")
         (visual-selection . "#e0e7ff")
         (inactive-selection . "#e5e5e5")

         (success-muted . "#059669")
         (warning-muted . "#d97706")
         (error-muted . "#dc2626")
         (info-muted . "#2563eb")

         (diff-added-bg . "#dcfce7")
         (diff-added-fg . "#166534")
         (diff-removed-bg . "#fecaca")
         (diff-removed-fg . "#dc2626")
         (diff-changed-bg . "#fef3c7")
         (diff-changed-fg . "#d97706")

         (hint-fg . "#9ca3af")
         (completion-bg . "#f9fafb"))))

  (tale-themes--create-theme 'bright-tale bright-palette)
  (tale-themes--set-variables 'bright-tale bright-palette)
  (tale-themes--setup-hooks 'bright-tale bright-palette))

;;;###autoload
(defun bright-tale-theme ()
  "Apply the bright-tale-theme."
  (interactive)
  (load-theme 'bright-tale t))

(provide-theme 'bright-tale)

;;; bright-tale-theme.el ends here
