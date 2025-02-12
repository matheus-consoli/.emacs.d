;; tell emacs to use plists for lsp
(setenv "LSP_USE_PLISTS" "true")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; The default is 800 kilobytes.  Measured in bytes.
;; emacs-lsp performance improvements
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold (* 400 1024 1024))
(setq gc-cons-percentage 0.6)
;; Optimizations for improving IO performance. Increase max bytes read from a sub-process in a single op
;; default value: 3145728 (3M)
(setq read-process-output-max (* 8 1024 1024 ))

;; keep the eln cache clean
(setq load-prefer-newer t)

(setq native-comp-deferred-compilation t)
(setq native-comp-jit-compilation t)
(setq native-comp-prune-cache t)
(setq native-comp-async-jobs-number (- (num-processors) 1))

(setq native-comp-always-compile t)

(setq native-comp-speed 2)
(setq native-comp-compiler-options '("-O2"
                                     "-mtune=native"
                                     "-fno-semantic-interposition"
                                     "-fipa-pta"
                                     "--param=max-inline-insns-auto=100"))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))


(setq inhibit-compacting-font-caches t)

(provide 'early-init)
