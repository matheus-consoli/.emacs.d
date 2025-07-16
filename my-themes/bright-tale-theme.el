;;; bright-tale-theme.el --- A sophisticated light theme with modern aesthetics -*- lexical-binding: t; -*-

(require 'tale-themes-common)

(deftheme bright-tale "A sophisticated light theme with modern aesthetics.")

(let ((bright-palette
       '((bg-main . "#FAFAFA")
         (bg-alt . "#F5F5F5")
         (bg-popup . "#F8F8F8")
         (bg-contrast . "#EEEEEE")
         (bg-code . "#F2F2F2")
         (bg-dim . "#EFEFEF")
         (bg-inactive-sel . "#E5E5E5")
         (bg-selection . "#E0E0E0")
         (bg-special . "#C7C7C7")
         (fg-main . "#2A2A2A")
         (fg-bright . "#000000")
         (fg-popup . "#1A1A1A")
         (grey-subtle . "#6B6B6B")
         (grey-border . "#BDBDBD")
         (grey-docstring . "#555555")
         (grey-comment-blue . "#5A6B8C")
         (purple . "#8B4CB8")
         (lavender . "#7A5BA3")
         (magenta . "#C44569")
         (rose . "#D1477A")
         (green . "#6B9B47")
         (teal . "#1F9C5F")
         (yellow . "#B8934A")
         (blue . "#5A7BC0")
         (focus-bg . "#D8D8D8")
         (hover-bg . "#CCCCCC")
         (primary-selection . "#EEEEEE")
         (secondary-selection . "#C8D4D4")
         (visual-selection . "#D6C8E5")
         (inactive-selection . "#E5E5E5")
         (success-muted . "#8DB373")
         (warning-muted . "#D4A574")
         (error-muted . "#D16B6B")
         (info-muted . "#7A8FB5")
         (diff-added-bg . "#E8F5EC")
         (diff-added-fg . "#4A8A5C")
         (diff-removed-bg . "#F5E8EA")
         (diff-removed-fg . "#C75A6B")
         (diff-changed-bg . "#F5F5E8")
         (diff-changed-fg . "#A68B52")
         (hint-fg . "#909090")
         (completion-bg . "#F0F0F0"))))

  (tale-themes--create-theme 'bright-tale bright-palette)
  (tale-themes--set-variables 'bright-tale bright-palette)
  (tale-themes--setup-hooks bright-palette))

;;;###autoload
(defun bright-tale-theme ()
  "Apply the bright-tale-theme."
  (interactive)
  (load-theme 'bright-tale t))

(provide-theme 'bright-tale)

;;; bright-tale-theme.el ends here
