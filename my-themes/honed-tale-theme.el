;;; honed-tale-theme.el --- Honed Digital Dark theme with mineral aesthetics -*- lexical-binding: t; -*-

(require 'tale-themes-common)

(deftheme honed-tale "A dark theme inspired by Honed Digital and Deep Basalt aesthetics.")

(let ((honed-palette
       '((bg-main . "#1C1C1E")
         (bg-alt . "#232325")
         (bg-popup . "#2A2A2D")
         (bg-contrast . "#303033")
         (bg-code . "#252527")
         (bg-dim . "#28282A")
         (bg-inactive-sel . "#353538")
         (bg-selection . "#3A3A3D")
         (bg-special . "#202022")

         (fg-main . "#EAE8E2")
         (fg-bright . "#F7F5F1")
         (fg-popup . "#E5E3DE")

         (grey-subtle . "#9A9895")
         (grey-border . "#4A4A4D")
         (grey-docstring . "#C5C3BE")
         (grey-comment-blue . "#8C8A85")

         (purple . "#9A7C9D")
         (lavender . "#AA8CAD")
         (magenta . "#BA8090")
         (rose . "#CA7080")
         (green . "#7A9A8A")
         (teal . "#6A9A9A")
         (yellow . "#D0B488")
         (blue . "#7A8AAA")

         (focus-bg . "#3A3A3D")
         (hover-bg . "#353538")
         (primary-selection . "#2A2A2D")
         (secondary-selection . "#3A3A40")
         (visual-selection . "#4A4A50")
         (inactive-selection . "#303033")

         (success-muted . "#6A9A7A")
         (warning-muted . "#C0A478")
         (error-muted . "#BA7070")
         (info-muted . "#7A8A9A")

         (diff-added-bg . "#1A2A20")
         (diff-added-fg . "#8ABA9A")
         (diff-removed-bg . "#2A1A1A")
         (diff-removed-fg . "#CA8A8A")
         (diff-changed-bg . "#2A2A1A")
         (diff-changed-fg . "#D0B088")

         (hint-fg . "#707070")
         (completion-bg . "#252527"))))

  (tale-themes--create-theme 'honed-tale honed-palette)
  (tale-themes--set-variables 'honed-tale honed-palette)
  (tale-themes--setup-hooks 'honed-tale honed-palette))

;;;###autoload
(defun honed-tale-theme ()
  "Apply the honed-tale-theme."
  (interactive)
  (load-theme 'honed-tale t))

(provide-theme 'honed-tale)

;;; honed-tale-theme.el ends here
