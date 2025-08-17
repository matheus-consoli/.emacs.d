;;; vampire-tale-theme.el --- A gothic theme inspired by vampire aesthetics -*- lexical-binding: t; -*-

(require 'tale-themes-common)

(deftheme vampire-tale "A gothic theme inspired by candlelit castles, ancient blood paintings, and Nosferatu's elegant chambers.")

(let ((vampire-palette
       '((bg-main . "#0f0a0d")
         (bg-alt . "#1a1014")
         (bg-popup . "#241a1f")
         (bg-contrast . "#2f2429")
         (bg-code . "#1d1318")
         (bg-dim . "#0c0709")
         (bg-inactive-sel . "#3d2832")
         (bg-selection . "#4d3342")
         (bg-special . "#221721")

         (fg-main . "#e8d5c4")
         (fg-bright . "#f5e6d3")
         (fg-popup . "#ded1c0")

         (grey-subtle . "#c4a484")
         (grey-border . "#5d4a3d")
         (grey-docstring . "#b49080")
         (grey-comment-blue . "#8a7065")

         (purple . "#d4af37")
         (lavender . "#dcd0c4")
         (magenta . "#8b2635")
         (rose . "#cd853f")
         (green . "#8fbc8f")
         (teal . "#8a9a8a")
         (yellow . "#d4af37")
         (blue . "#9370db")

         (focus-bg . "#5d4342")
         (hover-bg . "#4a3732")
         (primary-selection . "#722f37")
         (secondary-selection . "#5d3d2a")
         (visual-selection . "#8b2635")
         (inactive-selection . "#3d2832")

         (success-muted . "#4a5a4a")
         (warning-muted . "#8b6914")
         (error-muted . "#5d1a1a")
         (info-muted . "#4a3d5a")

         (diff-added-bg . "#2d4a2d")
         (diff-added-fg . "#8fbc8f")
         (diff-removed-bg . "#5d1a1a")
         (diff-removed-fg . "#8b2635")
         (diff-changed-bg . "#5d4a00")
         (diff-changed-fg . "#d4af37")

         (hint-fg . "#8a7065")
         (completion-bg . "#2d2327"))))

  (tale-themes--create-theme 'vampire-tale vampire-palette)
  (tale-themes--set-variables 'vampire-tale vampire-palette)
  (tale-themes--setup-hooks 'vampire-tale vampire-palette))

;;;###autoload
(defun vampire-tale-theme ()
  "Apply the vampire-tale-theme."
  (interactive)
  (load-theme 'vampire-tale t))

(provide-theme 'vampire-tale)

;;; vampire-tale-theme.el ends here
