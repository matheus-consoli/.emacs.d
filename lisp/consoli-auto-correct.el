;;; consoli-auto-correct.el --- Correct the last word on space with Jinx -*- lexical-binding: t; -*-

(require 'dash)
(require 'jinx)

(declare-function jinx--mod-suggest "ext:jinx-mod.c")
(declare-function jinx--mod-describe "ext:jinx-mod.c")
(declare-function org-in-src-block-p "org")
(declare-function org-element-at-point "org-element")
(declare-function org-element-context "org-element")
(declare-function org-element-type "org-element")
(declare-function org-element-property "org-element")
(declare-function org-element-inline-src-block-parser "org-element")
(defvar org-element-inline-src-block-regexp)

(defun consoli-auto-correct--org-source-p ()
  "Recognize Org source, including blocks still being typed."
  (save-match-data
    (or (org-in-src-block-p)
        (eq (org-element-type (org-element-context)) 'inline-src-block)
        (save-excursion
          ;; An unclosed block parses as a paragraph.  Its scope ends at
          ;; the next heading, as for a complete Org block.
          (let ((case-fold-search t))
            (and (re-search-backward
                  "^\\(?:\\*+[ \t]+\\|[ \t]*#\\+\\(begin\\|end\\)_src\\(?:[ \t]\\|$\\)\\)"
                  nil t)
                 (equal (downcase (or (match-string-no-properties 1) "")) "begin")
                 (eq (org-element-type (org-element-at-point)) 'paragraph))))
        (save-excursion
          (let ((pos (point)) (case-fold-search nil))
            (when (re-search-backward org-element-inline-src-block-regexp
                                      (line-beginning-position) t)
              (let ((node (org-element-inline-src-block-parser)))
                (or (null node)
                    (< pos (- (org-element-property :end node)
                              (org-element-property :post-blank node)))))))))))

(defun consoli-auto-correct-context-p ()
  "Allow spelling corrections in prose and program comments."
  (and (not (bound-and-true-p org-src-mode))
       (cond ((derived-mode-p 'org-mode) (not (consoli-auto-correct--org-source-p)))
             ((derived-mode-p 'prog-mode) (nth 4 (syntax-ppss)))
             (t (derived-mode-p 'text-mode)))))

(defvar-local consoli-auto-correct--pending nil
  "Start position, original spelling and expected text for one rejection.")

(defun consoli-auto-correct--reject-binding (command)
  "Expose COMMAND only immediately after the expected corrected text."
  (pcase consoli-auto-correct--pending
    (`(,start ,_ ,expected)
     (and (<= (point-min) start)
          (= (point) (+ start (length expected)))
          (equal expected (buffer-substring-no-properties start (point)))
          command))))

(defun consoli-auto-correct-reject ()
  "Restore the typed spelling and remove its trailing space."
  (interactive)
  (unless (consoli-auto-correct--reject-binding t)
    (user-error "No correction to reject at point"))
  (pcase-let ((`(,start ,original ,_) consoli-auto-correct--pending))
    (atomic-change-group
      (delete-region start (point))
      (insert original))
    (setq consoli-auto-correct--pending nil)))

(defvar-keymap consoli-auto-correct--reject-map
  "DEL" '(menu-item "" consoli-auto-correct-reject
                    :filter consoli-auto-correct--reject-binding)
  "<backspace>" '(menu-item "" consoli-auto-correct-reject
                            :filter consoli-auto-correct--reject-binding))

(defun consoli-auto-correct--suggestion (word)
  "Return the first spelling candidate Jinx offers for WORD."
  ;; Jinx lists dictionaries in order, then session words, then save actions.
  (or (-some (lambda (dict) (car (jinx--mod-suggest dict word))) jinx--dicts)
      (car (jinx--session-suggestions word))))

(defun consoli-auto-correct--replace (overlay original suggestion space)
  "Apply SUGGESTION and offer to restore ORIGINAL on the next Backspace."
  (let ((buffer (current-buffer))
        (beg (overlay-start overlay))
        (suffix (buffer-substring-no-properties (overlay-end overlay) space)))
    (save-excursion (jinx--correct-replace overlay suggestion))
    (setq consoli-auto-correct--pending
          (list beg (concat original suffix) (concat suggestion suffix " ")))
    (set-transient-map
     consoli-auto-correct--reject-map nil
     (lambda ()
       (when (and (buffer-live-p buffer)
                  (not (eq this-command 'consoli-auto-correct-reject)))
         (with-current-buffer buffer (setq consoli-auto-correct--pending nil)))))))

(defun consoli-auto-correct--at-space ()
  "Check the last word synchronously, before Jinx's idle check."
  (when (and jinx-mode jinx--dicts (eq last-command-event ?\s)
             (not (eq last-command 'consoli-auto-correct-reject))
             (eq (char-before) ?\s)
             (not (bound-and-true-p org-src-mode))
             (or (not (derived-mode-p 'prog-mode)) (nth 4 (syntax-ppss))))
    (let* ((space (1- (point)))
           (end (save-excursion (goto-char space) (skip-chars-backward "[:punct:]") (point)))
           (start (save-excursion (goto-char end) (skip-chars-backward "^ \t\n\r") (point))))
      (when (< start end)
        ;; Check the whole token so Jinx can recognize URLs and other exclusions.
        (font-lock-ensure start end)
        (jinx--check-region start end)
        (when-let* ((overlay (-first
                             (lambda (ov) (= (overlay-end ov) end))
                             (jinx--get-overlays start end t)))
                    (beg (overlay-start overlay))
                    (allowed (save-excursion (goto-char beg) (consoli-auto-correct-context-p)))
                    (original (buffer-substring-no-properties beg end))
                    (suggestion (consoli-auto-correct--suggestion original)))
          (consoli-auto-correct--replace overlay original suggestion space))))))

(defun consoli-auto-correct-setup (&rest _)
  "Enable correction only when Jinx loaded every requested language exactly."
  (if (and jinx-mode (derived-mode-p 'prog-mode 'text-mode)
           jinx--dicts
           (let ((loaded (-map (lambda (dict) (car (jinx--mod-describe dict)))
                               jinx--dicts)))
             (-all? (lambda (lang) (member lang loaded))
                    (split-string jinx-languages))))
      (add-hook 'post-self-insert-hook #'consoli-auto-correct--at-space nil t)
    (remove-hook 'post-self-insert-hook #'consoli-auto-correct--at-space t))
  (setq consoli-auto-correct--pending nil))

;; Changing a buffer's languages reloads dictionaries without toggling Jinx.
(advice-add 'jinx-languages :after #'consoli-auto-correct-setup)

(provide 'consoli-auto-correct)
;;; consoli-auto-correct.el ends here
