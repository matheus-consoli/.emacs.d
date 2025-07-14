;;; -*- lexical-binding: t; -*-
;; tell emacs to use plists for lsp
(setenv "LSP_USE_PLISTS" "true")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(set-frame-parameter nil 'ns-appearance 'dark)
(set-frame-parameter nil 'ns-transparent-titlebar nil)

(make-directory (concat user-emacs-directory "backup" "auto-save") t)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Disable garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
                  gc-cons-percentage 0.8)
            (garbage-collect)))

;; Optimizations for improving IO performance. Increase max bytes read from a sub-process in a single op
;; default value: 3145728 (3M)
(setq read-process-output-max (* 8 1024 1024 ))

;; keep the eln cache clean
(setq load-prefer-newer t)
;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(setq native-comp-deferred-compilation t)
(setq native-comp-jit-compilation t)
(setq native-comp-prune-cache t)
(setq native-comp-async-jobs-number (- (num-processors) 1))
(setq native-comp-async-query-on-exit t)
(setq native-comp-async-report-warnings-errors nil)
(setq confirm-kill-processes t)

(setq native-comp-always-compile t)
(setq package-native-compile t)

(defconst options '("-march=znver3"
                    "-mtune=znver3"
                    "-O3"
                    "-fno-finite-math-only"
                    "-funroll-loops"
                    "-finline-functions"
                    "-fomit-frame-pointer"
                    "-floop-nest-optimize"
                    "-fipa-pta"
                    "-fno-semantic-interposition"))

(setq native-comp-speed 2)
(setq native-comp-compiler-options options)
(setq native-comp-driver-options options)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq inhibit-compacting-font-caches t)

;; from minimal-emacs.d
(when (and (not (daemonp)) (not noninteractive))
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (setq frame-resize-pixelwise t)

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
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
  (setq initial-major-mode 'fundamental-mode
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

(provide 'early-init)
