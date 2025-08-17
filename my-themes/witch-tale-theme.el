;;; witch-tale-theme.el --- A mystical dark theme inspired by Witch aesthetics -*- lexical-binding: t; -*-

(require 'tale-themes-common)

(deftheme witch-tale "A mystical dark theme inspired by Witch aesthetics.")

(let ((witch-palette
       '((bg-main . "#0d1117")
         (bg-alt . "#161b22")
         (bg-popup . "#1c2128")
         (bg-contrast . "#21262d")
         (bg-code . "#242a30")
         (bg-dim . "#2d333a")
         (bg-inactive-sel . "#343a41")
         (bg-selection . "#3a4147")
         (bg-special . "#161b22")

         (fg-main . "#e6edf3")
         (fg-bright . "#f0f6fc")
         (fg-popup . "#e1e7ed")

         (grey-subtle . "#7d8590")
         (grey-border . "#484f58")
         (grey-docstring . "#8b949e")
         (grey-comment-blue . "#6e7681")

         (purple . "#a5d6a7")
         (lavender . "#b39ddb")
         (magenta . "#f48fb1")
         (rose . "#ffab91")
         (green . "#81c784")
         (teal . "#4dd0e1")
         (yellow . "#fff176")
         (blue . "#64b5f6")

         (focus-bg . "#2f3d2f")
         (hover-bg . "#3a4a3a")
         (primary-selection . "#1f2d1f")
         (secondary-selection . "#2a3f3a")
         (visual-selection . "#3a2f4a")
         (inactive-selection . "#2f3a2f")

         (success-muted . "#7fb069")
         (warning-muted . "#d4a574")
         (error-muted . "#f85149")
         (info-muted . "#58a6ff")

         (diff-added-bg . "#0f3a1f")
         (diff-added-fg . "#7fb069")
         (diff-removed-bg . "#3a1f1f")
         (diff-removed-fg . "#f85149")
         (diff-changed-bg . "#3a3a1f")
         (diff-changed-fg . "#d4a574")

         (hint-fg . "#6e7681")
         (completion-bg . "#21262d"))))

  (tale-themes--create-theme 'witch-tale witch-palette)
  (tale-themes--set-variables 'witch-tale witch-palette)
  (tale-themes--setup-hooks 'witch-tale witch-palette))

;;;###autoload
(defun witch-tale-theme ()
  "Apply the witch-tale-theme."
  (interactive)
  (load-theme 'witch-tale t))

(provide-theme 'witch-tale)

;;; witch-tale-theme.el ends here
