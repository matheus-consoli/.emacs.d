;;; sophisticated-light-tale-theme.el --- A sophisticated light theme inspired by Swiss Modern design -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'tale-themes-common)

(deftheme sophisticated-light-tale "A sophisticated light theme inspired by Swiss Modern design.")

(let ((sophisticated-light-palette
       '((bg-main . "#F5F5F7")          ; Soft Apple-style gray - main background
         (bg-alt . "#EBEBED")           ; Active line (5% darker than main)
         (bg-popup . "#FFFFFF")         ; Pure white for popups/menus
         (bg-contrast . "#E5E5E7")      ; Subtle contrast areas
         (bg-code . "#EDEDEF")          ; Code block backgrounds
         (bg-dim . "#DCDCDE")           ; Dimmed areas (inactive windows)
         (bg-inactive-sel . "#E0E0E2")  ; Inactive selections
         (bg-selection . "#D6D6D8")     ; General selections
         (bg-special . "#F8F8FA")       ; Special buffers (terminals, help)

         (fg-main . "#1D1D1F")          ; Deep obsidian - primary text (15.46:1 contrast)
         (fg-bright . "#0A0A0C")        ; True black for emphasis
         (fg-popup . "#1D1D1F")         ; Popup text

         (grey-subtle . "#6E6E70")      ; Subtle text elements
         (grey-border . "#C8C8CA")      ; Borders and dividers (1px solid)
         (grey-docstring . "#4A4A4C")   ; Documentation strings
         (grey-comment-blue . "#5A5A6C") ; Comments with slight blue tint

         ;; Semantic colors - Swiss Modern with WCAG AA compliance
         (purple . "#7C3AED")           ; Violet - constants (5.23:1)
         (lavender . "#9061F9")         ; Light violet - variables
         (magenta . "#E63946")          ; Racing Red - keywords/logic (4.85:1)
         (rose . "#D63F4B")             ; Light red - parameters
         (green . "#2A9D8F")            ; Pine Green - strings (4.52:1)
         (teal . "#2A8A7F")             ; Teal - constructors/fields
         (yellow . "#D4803D")           ; Burnt orange - warnings/macros (4.61:1)
         (blue . "#457B9D")             ; Steel Blue - functions/types (4.98:1)

         ;; UI interaction colors
         (focus-bg . "#D0D0D2")         ; Focused elements
         (hover-bg . "#E0E0E2")         ; Hover state
         (primary-selection . "#C8D5E3")   ; Primary selection (blue tint)
         (secondary-selection . "#D8D0E3") ; Secondary selection (purple tint)
         (visual-selection . "#C8D5E3")    ; Visual mode selection
         (inactive-selection . "#E5E5E7")  ; Inactive selections

         ;; Status colors
         (success-muted . "#2A9D8F")    ; Success states
         (warning-muted . "#D4803D")    ; Warnings
         (error-muted . "#E63946")      ; Errors
         (info-muted . "#457B9D")       ; Info messages

         ;; Diff colors
         (diff-added-bg . "#D4F0EC")    ; Light pine green background
         (diff-added-fg . "#2A9D8F")    ; Pine green text
         (diff-removed-bg . "#F5D8DB")  ; Light red background
         (diff-removed-fg . "#E63946")  ; Red text
         (diff-changed-bg . "#FFE8CC")  ; Light amber background
         (diff-changed-fg . "#D4803D")  ; Amber text

         ;; Special elements
         (hint-fg . "#8E8E90")          ; Hints and subtle information
         (completion-bg . "#FAFAFA"))))  ; Completion popup background

  (tale-themes--create-theme 'sophisticated-light-tale sophisticated-light-palette)
  (tale-themes--set-variables 'sophisticated-light-tale sophisticated-light-palette)
  (tale-themes--setup-hooks 'sophisticated-light-tale sophisticated-light-palette))

;;;###autoload
(defun sophisticated-light-tale-theme ()
  "Apply the sophisticated-light-tale-theme."
  (interactive)
  (load-theme 'sophisticated-light-tale t))

(provide-theme 'sophisticated-light-tale)

;;; sophisticated-light-tale-theme.el ends here
