;;; -*- lexical-binding: t; -*-
;; tell emacs to use plists for lsp
(setenv "LSP_USE_PLISTS" "true")
(setq-default lsp-use-plists t)

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
(setq-default read-process-output-max (* 1024 1024)) ;; cat /proc/sys/fs/pipe-max-size

;; keep the eln cache clean
(setq-default load-prefer-newer t)
;; Don't ping things that look like domain names.
(setq-default ffap-machine-p-known 'reject)

(setq-default native-comp-deferred-compilation t
              package-quickstart t
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

(defconst options (append '("-Ofast"
                            "-g0"
                            "-fno-finite-math-only"
                            "-funroll-loops"
                            "-finline-functions"
                            "-fomit-frame-pointer"
                            "-floop-nest-optimize"
                            "-fipa-pta"
                            "-fno-finite-math-only"
                            "-fno-semantic-interposition"
                            "-flto=auto")
                          arch-specific-options))

(setq-default native-comp-speed 3
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

;; Load font scaling system
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'font-families)
(require 'font-contexts)
(require 'font-scaling)

;; Initialize font scaling early to prevent startup flicker
(consoli-config/init-font-scaling)

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
