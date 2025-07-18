;;; witch-tale-theme.el --- A mystical dark theme inspired by Witch aesthetics -*- lexical-binding: t; -*-

(require 'tale-themes-common)

(deftheme witch-tale "A mystical dark theme inspired by Witch aesthetics.")

(let ((witch-palette
       '((bg-main . "#0A0F0A")
         (bg-alt . "#121A12")
         (bg-popup . "#161F16")
         (bg-contrast . "#1E281E")
         (bg-code . "#222C22")
         (bg-dim . "#282F28")
         (bg-inactive-sel . "#303A30")
         (bg-selection . "#3A453A")
         (bg-special . "#080C08")

         (fg-main . "#D9E6D9")
         (fg-bright . "#E8F5E8")
         (fg-popup . "#E0EDE0")

         (grey-subtle . "#8FA38F")
         (grey-border . "#4A5E4A")
         (grey-docstring . "#A3B8A3")
         (grey-comment-blue . "#5E7A5E")

         (purple . "#A85FC3")
         (lavender . "#9B7EB5")
         (magenta . "#C35E7A")
         (rose . "#D96E8F")
         (green . "#7FB57F")
         (teal . "#4AB58A")
         (yellow . "#C3A35E")
         (blue . "#5E8FC3")

         (focus-bg . "#2A352A")
         (hover-bg . "#354035")
         (primary-selection . "#101810")
         (secondary-selection . "#284848")
         (visual-selection . "#3A2A55")
         (inactive-selection . "#2A352A")

         (success-muted . "#4A7A4A")
         (warning-muted . "#8F6A3A")
         (error-muted . "#7A3A3A")
         (info-muted . "#3A5E8F")

         (diff-added-bg . "#0F3A1F")
         (diff-added-fg . "#6EB58E")
         (diff-removed-bg . "#3A1A1F")
         (diff-removed-fg . "#E57A93")
         (diff-changed-bg . "#3A3A1A")
         (diff-changed-fg . "#C3B56E")

         (hint-fg . "#6E876E")
         (completion-bg . "#1F281F"))))

  (tale-themes--create-theme 'witch-tale witch-palette)
  (tale-themes--set-variables 'witch-tale witch-palette)
  (tale-themes--setup-hooks witch-palette))

;;;###autoload
(defun witch-tale-theme ()
  "Apply the witch-tale-theme."
  (interactive)
  (load-theme 'witch-tale t))

(provide-theme 'witch-tale)

;;; witch-tale-theme.el ends here
