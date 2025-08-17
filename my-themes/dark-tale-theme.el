;;; dark-tale-theme.el --- A sophisticated dark theme with modern aesthetics -*- lexical-binding: t; -*-

(require 'tale-themes-common)

(deftheme dark-tale "A sophisticated dark theme with modern aesthetics.")

(let ((dark-palette
       '((bg-main . "#000000")
         (bg-alt . "#0a0a0a")
         (bg-popup . "#0f0f0f")
         (bg-contrast . "#121212")
         (bg-code . "#161616")
         (bg-dim . "#1a1a1a")
         (bg-inactive-sel . "#2a2a2a")
         (bg-selection . "#2d2d2d")
         (bg-special . "#0a0a0a")

         (fg-main . "#f0f0f0")
         (fg-bright . "#ffffff")
         (fg-popup . "#fcfcfc")

         (grey-subtle . "#888888")
         (grey-border . "#404040")
         (grey-docstring . "#b8b8b8")
         (grey-comment-blue . "#707a9a")

         (purple . "#bb9af7")
         (lavender . "#c3a5f8")
         (magenta . "#f7768e")
         (rose . "#ff7a93")
         (green . "#9ece6a")
         (teal . "#73daca")
         (yellow . "#e0af68")
         (blue . "#7aa2f7")

         (focus-bg . "#3a3a3a")
         (hover-bg . "#353535")
         (primary-selection . "#1a1a1a")
         (secondary-selection . "#2a3a3a")
         (visual-selection . "#4a3a5a")
         (inactive-selection . "#2a2a2a")

         (success-muted . "#6b9b47")
         (warning-muted . "#d4a574")
         (error-muted . "#e74c3c")
         (info-muted . "#7a8fb5")

         (diff-added-bg . "#0d3a1f")
         (diff-added-fg . "#7ec699")
         (diff-removed-bg . "#3a151a")
         (diff-removed-fg . "#ff8fa3")
         (diff-changed-bg . "#2a2a15")
         (diff-changed-fg . "#e5c07b")

         (hint-fg . "#707070")
         (completion-bg . "#1a1a1a"))))

  (tale-themes--create-theme 'dark-tale dark-palette)
  (tale-themes--set-variables 'dark-tale dark-palette)
  (tale-themes--setup-hooks 'dark-tale dark-palette))

;;;###autoload
(defun dark-tale-theme ()
  "Apply the dark-tale-theme."
  (interactive)
  (load-theme 'dark-tale t))

(provide-theme 'dark-tale)

;;; dark-tale-theme.el ends here
