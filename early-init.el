;;; -*- lexical-binding: t; -*-
;; tell emacs to use plists for lsp
(setenv "LSP_USE_PLISTS" "true")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-parameter nil 'ns-appearance 'dark)
(set-frame-parameter nil 'ns-transparent-titlebar nil)

(make-directory (concat user-emacs-directory "backup/auto-saves") t)

;; Disable package.el in favor of straight.el
(setq-default package-enable-at-startup nil)

;; Disable garbage collection during startup
(setq-default gc-cons-threshold most-positive-fixnum)
(setq-default gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.8)
            (garbage-collect)))

;; Optimizations for improving IO performance. Increase max bytes read from a sub-process in a single op
;; default value: 3145728 (3M)
(setq-default read-process-output-max (* 8 1024 1024 ))

;; keep the eln cache clean
(setq-default load-prefer-newer t)
;; Don't ping things that look like domain names.
(setq-default ffap-machine-p-known 'reject)

(setq-default native-comp-deferred-compilation t
              native-comp-jit-compilation t
              native-comp-prune-cache t
              native-comp-async-jobs-number (- (num-processors) 1)
              native-comp-async-query-on-exit t
              native-comp-async-report-warnings-errors nil
              confirm-kill-processes t
              native-comp-always-compile t
              package-native-compile t)

(defconst znver3-march '("-march=znver3" "-mtune=znver3"))
(defconst arm-march '("-march=armv8-a+crc+lse+rcpc+rdma+dotprod+aes+sha3+sm4+fp16fml+rng+sb+ssbs+i8mm+bf16+flagm"))

(defconst system-arch (shell-command-to-string "uname -m"))

(defconst arch-specific-options
  (cond
   ((string-match-p "aarch64\\|arm64" system-arch)
    arm-march)
   ((string-match-p "x86_64" system-arch)
    znver3-march)
   (t '())))

(defconst options (append '("-O3"
                            "-fno-finite-math-only"
                            "-funroll-loops"
                            "-finline-functions"
                            "-fomit-frame-pointer"
                            "-floop-nest-optimize"
                            "-fipa-pta"
                            "-fno-semantic-interposition")
                          arch-specific-options))

(setq-default native-comp-speed 2
              native-comp-compiler-options options
              native-comp-driver-options options)

;; DEBUG
;; (setq debug-on-error t)
;;       debug-on-message "")

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq-default inhibit-compacting-font-caches t)

;;; Font Configuration System

(defun consoli-config/font-available-p (font-family)
  "Check if FONT-FAMILY is available on the system."
  (when (display-graphic-p)
    (member font-family (font-family-list))))

(defun consoli-config/select-font (font-list)
  "Select first available font from FONT-LIST, or fallback to monospace."
  (or (seq-find #'consoli-config/font-available-p font-list)
      "monospace"))

;; Font family definitions with intelligent fallbacks
(defconst consoli-config/font-families
  '((ui-font
     ("Atkinson Hyperlegible Mono" "SF Mono" "Menlo" "Monaco" "Consolas" "monospace"))
    (programming-font
     ("Dank Mono" "Fira Code" "JetBrains Mono" "Source Code Pro" "SF Mono" "monospace"))
    (org-font
     ("Margem Rounded Test" "ETBembo" "Libre Baskerville" "Georgia" "serif"))
    (modeline-font
     ("Reddit Mono" "SF Mono" "Menlo" "Monaco" "Consolas" "monospace"))
    (alternative-font
     ("Reddit Mono" "SF Mono" "Menlo" "Monaco" "Consolas" "monospace")))
  "Font families with fallback preferences.")

;; Dynamic font sizing configuration
(defgroup consoli-config nil
  "Personal Emacs configuration settings."
  :group 'environment
  :prefix "consoli-config/")

(defcustom consoli-config/font-scaling-enabled t
  "Enable dynamic font scaling based on frame size."
  :type 'boolean
  :group 'consoli-config)

(defcustom consoli-config/font-base-sizes
  '((ui . 120)
    (programming . 110)
    (org . 130)
    (small . 100)
    (modeline . 110))
  "Base font sizes before dynamic scaling."
  :type '(alist :key-type symbol :value-type (integer :min 8))
  :group 'consoli-config)

(defcustom consoli-config/font-scaling-factor 1.0
  "Global font scaling multiplier (user preference)."
  :type '(float :min 0.1 :max 3.0)
  :group 'consoli-config)

(defcustom consoli-config/vertical-screen-boost 1.15
  "Font size boost for vertical/portrait screens."
  :type '(float :min 1.0 :max 2.0)
  :group 'consoli-config)

;; Font scaling system
(defvar consoli-config/--font-cache (make-hash-table :test 'equal)
  "Cache for calculated font sizes.")

(defvar consoli-config/--last-frame-size nil
  "Last calculated frame size to detect changes.")

(defvar consoli-config/--font-update-timer nil
  "Timer for debounced font updates.")

(defun consoli-config/get-screen-resolution (&optional frame)
  "Get the native screen resolution for FRAME's display."
  (let ((frame (or frame (selected-frame))))
    (if (display-graphic-p frame)
        (let* ((attrs (frame-monitor-attributes frame))
               (geometry (alist-get 'geometry attrs))
               (width (nth 2 geometry))
               (height (nth 3 geometry)))
          (cons width height))
      ;; Fallback for terminal
      (cons 1920 1080))))

(defun consoli-config/get-reference-area (&optional frame)
  "Get reference area based on current screen resolution."
  (let ((resolution (consoli-config/get-screen-resolution frame)))
    (* (car resolution) (cdr resolution))))

(defun consoli-config/get-frame-metrics (&optional frame)
  "Get frame size metrics for scaling calculations."
  (let* ((frame (or frame (selected-frame)))
         (width (frame-pixel-width frame))
         (height (frame-pixel-height frame))
         (area (* width height))
         (aspect-ratio (/ (float width) height)))
    `((width . ,width)
      (height . ,height)
      (area . ,area)
      (aspect-ratio . ,aspect-ratio)
      (is-vertical . ,(< aspect-ratio 0.8)))))

(defun consoli-config/calculate-font-scale (&optional frame)
  "Calculate optimal font scaling factor for FRAME."
  (let* ((metrics (consoli-config/get-frame-metrics frame))
         (area (alist-get 'area metrics))
         (is-vertical (alist-get 'is-vertical metrics))
         (reference-area (consoli-config/get-reference-area frame))
         (base-scale (sqrt (/ (float area) reference-area)))
         (aspect-boost (if is-vertical consoli-config/vertical-screen-boost 1.0))
         (final-scale (* base-scale aspect-boost consoli-config/font-scaling-factor)))
    ;; Clamp scaling between reasonable bounds
    (max 0.7 (min 1.8 final-scale))))

(defun consoli-config/get-scaled-font-size (context &optional frame)
  "Get dynamically scaled font size for CONTEXT."
  (if (not consoli-config/font-scaling-enabled)
      (max 8 (alist-get context consoli-config/font-base-sizes))
    (let* ((cache-key (cons context (consoli-config/get-frame-metrics frame)))
           (cached-size (gethash cache-key consoli-config/--font-cache)))
      (or cached-size
          (let* ((base-size (max 8 (alist-get context consoli-config/font-base-sizes)))
                 (scale-factor (consoli-config/calculate-font-scale frame))
                 ;; Context-specific multipliers
                 (context-multiplier
                  (pcase context
                    ('programming 0.95)  ; Slightly smaller for code density
                    ('org 1.05)          ; Larger for reading comfort
                    ('modeline 0.9)      ; Proportional but not dominant
                    ('small 0.85)        ; Smaller UI elements
                    (_ 1.0)))            ; UI and others
                 (final-size (max 8 (round (* base-size scale-factor context-multiplier)))))
            (puthash cache-key final-size consoli-config/--font-cache)
            final-size)))))

(defun consoli-config/clear-font-cache ()
  "Clear font size cache."
  (clrhash consoli-config/--font-cache))

(defun consoli-config/frame-size-changed-p (&optional frame)
  "Check if frame size changed significantly."
  (let* ((current-metrics (consoli-config/get-frame-metrics frame))
         (current-area (alist-get 'area current-metrics))
         (last-area (when consoli-config/--last-frame-size
                      (alist-get 'area consoli-config/--last-frame-size))))
    (or (null last-area)
        (> (abs (- current-area last-area))
           (* 0.1 last-area)))))  ; 10% change threshold

;; Resolve fonts early for performance
(defconst ui-font
  (consoli-config/select-font (alist-get 'ui-font consoli-config/font-families))
  "Selected UI font with fallback.")

(defconst programming-font
  (consoli-config/select-font (alist-get 'programming-font consoli-config/font-families))
  "Selected programming font with fallback.")

(defconst org-font
  (consoli-config/select-font (alist-get 'org-font consoli-config/font-families))
  "Selected org-mode font with fallback.")

(defconst modeline-font
  (consoli-config/select-font (alist-get 'modeline-font consoli-config/font-families))
  "Selected modeline font with fallback.")

(defconst alternative-programming-font
  (consoli-config/select-font (alist-get 'alternative-font consoli-config/font-families))
  "Selected alternative programming font with fallback.")

;; Dynamic font height accessors
(defun consoli-config/font-height-ui (&optional frame)
  "Get dynamically scaled UI font height."
  (consoli-config/get-scaled-font-size 'ui frame))

(defun consoli-config/font-height-programming (&optional frame)
  "Get dynamically scaled programming font height."
  (consoli-config/get-scaled-font-size 'programming frame))

(defun consoli-config/update-frame-fonts (&optional frame)
  "Update all font faces for FRAME with dynamic sizing."
  (when (display-graphic-p frame)
    (let ((ui-size (consoli-config/font-height-ui frame))
          (prog-size (consoli-config/font-height-programming frame))
          (org-size (consoli-config/get-scaled-font-size 'org frame))
          (modeline-size (consoli-config/get-scaled-font-size 'modeline frame))
          (small-size (consoli-config/get-scaled-font-size 'small frame)))

      ;; Update frame font parameters
      (when frame
        (set-frame-parameter frame 'font
                             (format "%s-%d" ui-font (/ ui-size 10))))

      ;; Update dynamic faces that can't use function calls in :custom-face
      (when (featurep 'org)
        (set-face-attribute 'variable-pitch nil
                            :family org-font
                            :height org-size)
        (set-face-attribute 'fixed-pitch nil
                            :family alternative-programming-font
                            :height modeline-size)
        (set-face-attribute 'font-lock-doc-face nil
                            :height modeline-size))

      ;; Store current frame size for change detection
      (setq consoli-config/--last-frame-size
            (consoli-config/get-frame-metrics frame)))))

(defun consoli-config/on-frame-size-change (&optional frame)
  "Handle frame size changes with debouncing."
  (when (and consoli-config/font-scaling-enabled
             (consoli-config/frame-size-changed-p frame))
    ;; Cancel previous timer
    (when consoli-config/--font-update-timer
      (cancel-timer consoli-config/--font-update-timer))

    ;; Clear cache since frame size changed
    (consoli-config/clear-font-cache)

    ;; Set debounced update timer
    (setq consoli-config/--font-update-timer
          (run-with-timer 0.5 nil
                          (lambda ()
                            (consoli-config/update-frame-fonts frame)
                            (setq consoli-config/--font-update-timer nil))))))

;; Convenience accessors for compatibility
(defun consoli-config/font-height-org (&optional frame)
  "Get dynamically scaled org font height."
  (consoli-config/get-scaled-font-size 'org frame))

(defun consoli-config/font-height-modeline (&optional frame)
  "Get dynamically scaled modeline font height."
  (consoli-config/get-scaled-font-size 'modeline frame))

(defun consoli-config/font-height-small (&optional frame)
  "Get dynamically scaled small font height."
  (consoli-config/get-scaled-font-size 'small frame))

;; Early frame configuration to prevent startup flicker
(when (display-graphic-p)
  (let ((initial-ui-size (consoli-config/font-height-ui)))
    (add-to-list 'default-frame-alist
                 `(font . ,(format "%s-%d" ui-font (/ initial-ui-size 10)))))

  ;; Set up frame size change detection
  (add-hook 'window-size-change-functions #'consoli-config/on-frame-size-change)

  ;; Initialize frame size tracking
  (setq consoli-config/--last-frame-size (consoli-config/get-frame-metrics)))


;; Interactive font scaling functions
(defun consoli-config/toggle-font-scaling ()
  "Toggle dynamic font scaling on/off."
  (interactive)
  (setq consoli-config/font-scaling-enabled
        (not consoli-config/font-scaling-enabled))
  (consoli-config/clear-font-cache)
  (consoli-config/update-frame-fonts)
  (message "Dynamic font scaling %s"
           (if consoli-config/font-scaling-enabled "enabled" "disabled")))

(defun consoli-config/adjust-font-scale (factor)
  "Adjust global font scaling FACTOR interactively."
  (interactive "nFont scaling factor (1.0 = normal): ")
  (setq consoli-config/font-scaling-factor factor)
  (consoli-config/clear-font-cache)
  (consoli-config/update-frame-fonts)
  (message "Font scaling set to %.2f" factor))

(defun consoli-config/show-font-info ()
  "Display current font configuration and frame metrics."
  (interactive)
  (let* ((metrics (consoli-config/get-frame-metrics))
         (scale (consoli-config/calculate-font-scale))
         (ui-size (consoli-config/font-height-ui))
         (prog-size (consoli-config/font-height-programming)))
    (message "Frame: %dx%d (ratio %.2f) | Scale: %.2f | UI: %d | Prog: %d"
             (alist-get 'width metrics)
             (alist-get 'height metrics)
             (alist-get 'aspect-ratio metrics)
             scale ui-size prog-size)))

;; from minimal-emacs.d
(when (and (not (daemonp)) (not noninteractive))
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (setq frame-resize-pixelwise t)

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setopt auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setopt inhibit-startup-screen t)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  ;; The initial buffer is created during startup even in non-interactive
  ;; sessions, and its major mode is fully initialized. Modes like `text-mode',
  ;; `org-mode', or even the default `lisp-interaction-mode' load extra packages
  ;; and run hooks, which can slow down startup.
  ;;
  ;; Using `fundamental-mode' for the initial buffer to avoid unnecessary
  ;; startup overhead.
  (setopt initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

  ;; Unset command line options irrelevant to the current OS. These options
  ;; are still processed by `command-line-1` but have no effect.
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

(when (and (not (daemonp))
           (not noninteractive))
  (put 'mode-line-format
       'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format nil))))

(defun minimal-emacs--startup-load-user-init-file (fn &rest args)
  "Advice to reset `mode-line-format'. FN and ARGS are the function and args."
  (unwind-protect
      ;; Start up as normal
      (apply fn args)
    ;; If we don't undo inhibit-{message, redisplay} and there's an error, we'll
    ;; see nothing but a blank Emacs frame.
    (setq-default inhibit-message nil)
    (setq-default inhibit-redisplay nil)
    ;; Restore the mode-line
    (unless (default-toplevel-value 'mode-line-format)
      (setq-default mode-line-format (get 'mode-line-format
                                          'initial-value)))))

(advice-add 'startup--load-user-init-file :around
            #'minimal-emacs--startup-load-user-init-file)

;; end of code taken from minimal-emacs.d

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(provide 'early-init)
