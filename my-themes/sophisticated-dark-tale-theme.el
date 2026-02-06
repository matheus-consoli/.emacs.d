;;; sophisticated-dark-tale-theme.el --- A sophisticated dark theme inspired by Swiss Modern design -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'tale-themes-common)

(deftheme sophisticated-dark-tale "A sophisticated dark theme inspired by Swiss Modern design.")

(let ((sophisticated-dark-palette
       '((bg-main . "#0D0D0F")          ; Deep charcoal (not true black - prevents halation)
         (bg-alt . "#131315")           ; Active line highlight
         (bg-popup . "#1A1A1C")         ; Popup backgrounds
         (bg-contrast . "#1F1F21")      ; Contrast areas
         (bg-code . "#161618")          ; Code blocks
         (bg-dim . "#0A0A0C")           ; Dimmed areas
         (bg-inactive-sel . "#252527")  ; Inactive selections
         (bg-selection . "#2A2A2C")     ; General selections
         (bg-special . "#101012")       ; Special buffers

         (fg-main . "#E8E8EA")          ; Cool off-white (15.87:1 contrast)
         (fg-bright . "#F5F5F7")        ; Bright white for emphasis
         (fg-popup . "#EFEFF1")         ; Popup text

         (grey-subtle . "#8E8E90")      ; Subtle text
         (grey-border . "#3A3A3C")      ; Borders (subtle, not harsh)
         (grey-docstring . "#B8B8BA")   ; Documentation
         (grey-comment-blue . "#7A7A8C") ; Comments with blue tint

         ;; Semantic colors - desaturated bases with vibrant accents
         (purple . "#A78BFA")           ; Soft violet - constants (7.13:1)
         (lavender . "#C4B5FD")         ; Light violet - variables
         (magenta . "#F56476")          ; Warm red - keywords (6.44:1)
         (rose . "#FF8FA0")             ; Salmon - parameters
         (green . "#5FC0AC")            ; Aqua green - strings (8.90:1)
         (teal . "#6BD0BC")             ; Bright teal - constructors
         (yellow . "#E89B5F")           ; Warm amber - warnings (8.58:1)
         (blue . "#6B9BD4")             ; Sky blue - functions (6.72:1)

         ;; UI interaction
         (focus-bg . "#2F2F31")         ; Focus state
         (hover-bg . "#252527")         ; Hover
         (primary-selection . "#2A3A4A")   ; Blue-tinted selection
         (secondary-selection . "#3A2A4A") ; Purple-tinted selection
         (visual-selection . "#2A3A4A")    ; Visual mode
         (inactive-selection . "#252527")  ; Inactive

         ;; Status colors
         (success-muted . "#5FC0AC")    ; Success
         (warning-muted . "#E89B5F")    ; Warning
         (error-muted . "#F56476")      ; Error
         (info-muted . "#6B9BD4")       ; Info

         ;; Diff colors
         (diff-added-bg . "#1A3A34")    ; Dark pine background
         (diff-added-fg . "#5FC0AC")    ; Bright pine
         (diff-removed-bg . "#3A1A1E")  ; Dark red background
         (diff-removed-fg . "#F56476")  ; Bright red
         (diff-changed-bg . "#3A2A14")  ; Dark amber background
         (diff-changed-fg . "#E89B5F")  ; Bright amber

         ;; Special
         (hint-fg . "#6E6E70")          ; Hints
         (completion-bg . "#1A1A1C"))))  ; Completions

  (tale-themes--create-theme 'sophisticated-dark-tale sophisticated-dark-palette)
  (tale-themes--set-variables 'sophisticated-dark-tale sophisticated-dark-palette)
  (tale-themes--setup-hooks 'sophisticated-dark-tale sophisticated-dark-palette))

;;;###autoload
(defun sophisticated-dark-tale-theme ()
  "Apply the sophisticated-dark-tale-theme."
  (interactive)
  (load-theme 'sophisticated-dark-tale t))

(provide-theme 'sophisticated-dark-tale)

;;; sophisticated-dark-tale-theme.el ends here
