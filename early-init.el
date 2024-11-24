(setenv "LSP_USE_PLISTS" "true")
;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; The default is 800 kilobytes.  Measured in bytes.
;; emacs-lsp performance improvements
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold (* 200 1024 1024))
(setq gc-cons-percentage 0.4)
;; Optimizations for improving IO performance. Increase max bytes read from a sub-process in a single op
;; default value: 3145728 (3M)
(setq read-process-output-max (* 8 1024 1024 )) ;; 8mb

;; keep the eln cache clean
(setq load-prefer-newer t)

(setq native-comp-deferred-compilation t)
(setq native-comp-jit-compilation t)
(setq native-comp-prune-cache t)

(when 'native-comp-compiler-options
  (setq native-comp-speed 2
        native-comp-compiler-options '("-O2" "-mtune=native")))

(provide 'early-init)
