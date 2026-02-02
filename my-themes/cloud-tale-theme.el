;;; cloud-tale-theme.el --- Honed Digital Light theme inspired by Cloud Dancer 2026 -*- lexical-binding: t; -*-

(require 'tale-themes-common)

(deftheme cloud-tale "A light theme inspired by Cloud Dancer and Honed Digital aesthetics.")

(let ((cloud-palette
       '((bg-main . "#F7F5F1")
         (bg-alt . "#EBE9E4")
         (bg-popup . "#F2F0EB")
         (bg-contrast . "#E5E3DE")
         (bg-code . "#EFEDE8")
         (bg-dim . "#E8E6E1")
         (bg-inactive-sel . "#E0DED9")
         (bg-selection . "#D8D6D1")
         (bg-special . "#F0EEE9")

         (fg-main . "#2E2E30")
         (fg-bright . "#1C1C1E")
         (fg-popup . "#2A2A2C")

         (grey-subtle . "#6E6E70")
         (grey-border . "#C8C6C1")
         (grey-docstring . "#5A5A5C")
         (grey-comment-blue . "#8C746F")

         (purple . "#7A5C7D")
         (lavender . "#8A6C8D")
         (magenta . "#9A6070")
         (rose . "#A05060")
         (green . "#547A6A")
         (teal . "#4A7A7A")
         (yellow . "#B09468")
         (blue . "#5A6A8A")

         (focus-bg . "#E5E3DE")
         (hover-bg . "#EFEDE8")
         (primary-selection . "#E8E6E1")
         (secondary-selection . "#DDD8D0")
         (visual-selection . "#D5D0C8")
         (inactive-selection . "#E0DED9")

         (success-muted . "#4A7A5A")
         (warning-muted . "#A08050")
         (error-muted . "#A05050")
         (info-muted . "#5A6A8A")

         (diff-added-bg . "#E8F0E8")
         (diff-added-fg . "#3A6A4A")
         (diff-removed-bg . "#F0E8E8")
         (diff-removed-fg . "#8A4A4A")
         (diff-changed-bg . "#F0EDE0")
         (diff-changed-fg . "#8A7040")

         (hint-fg . "#9A9895")
         (completion-bg . "#F2F0EB"))))

  (tale-themes--create-theme 'cloud-tale cloud-palette)
  (tale-themes--set-variables 'cloud-tale cloud-palette)
  (tale-themes--setup-hooks 'cloud-tale cloud-palette))

;;;###autoload
(defun cloud-tale-theme ()
  "Apply the cloud-tale-theme."
  (interactive)
  (load-theme 'cloud-tale t))

(provide-theme 'cloud-tale)

;;; cloud-tale-theme.el ends here
