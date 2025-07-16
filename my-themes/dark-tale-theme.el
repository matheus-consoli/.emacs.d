;;; dark-tale-theme.el --- A sophisticated dark theme with modern aesthetics -*- lexical-binding: t; -*-

(require 'tale-themes-common)

(deftheme dark-tale "A sophisticated dark theme with modern aesthetics.")

(let ((dark-palette
       '((bg-main . "#000000")
         (bg-alt . "#0C0C0C")
         (bg-popup . "#0f0f0f")
         (bg-contrast . "#121212")
         (bg-code . "#161616")
         (bg-dim . "#171717")
         (bg-inactive-sel . "#2B2B2B")
         (bg-selection . "#2d2d2d")
         (bg-special . "#0a0a0a")

         (fg-main . "#EDEDED")
         (fg-bright . "#ffffff")
         (fg-popup . "#fcfcfc")

         (grey-subtle . "#A3A3A3")
         (grey-border . "#585858")
         (grey-docstring . "#B8B8B8")
         (grey-comment-blue . "#707A9A")

         (purple . "#C372FC")
         (lavender . "#B084EB")
         (magenta . "#F7768E")
         (rose . "#FF6B8F")
         (green . "#9ECE6A")
         (teal . "#26D17F")
         (yellow . "#E0AF68")
         (blue . "#7AA2F7")

         (focus-bg . "#363636")
         (hover-bg . "#404040")
         (primary-selection . "#141414")
         (secondary-selection . "#2D4B4B")
         (visual-selection . "#4B3B66")
         (inactive-selection . "#2B2B2B")

         (success-muted . "#5C8C4A")
         (warning-muted . "#B0864B")
         (error-muted . "#AD3E3E")
         (info-muted . "#566C9E")

         (diff-added-bg . "#10341C")
         (diff-added-fg . "#7EC699")
         (diff-removed-bg . "#38181D")
         (diff-removed-fg . "#FF8FA3")
         (diff-changed-bg . "#2B2B16")
         (diff-changed-fg . "#E5C07B")

         (hint-fg . "#878787")
         (completion-bg . "#1A1A1A"))))

  (tale-themes--create-theme 'dark-tale dark-palette)
  (tale-themes--set-variables 'dark-tale dark-palette)
  (tale-themes--setup-hooks dark-palette))

;;;###autoload
(defun dark-tale-theme ()
  "Apply the dark-tale-theme."
  (interactive)
  (load-theme 'dark-tale t))

(provide-theme 'dark-tale)

;;; dark-tale-theme.el ends here
