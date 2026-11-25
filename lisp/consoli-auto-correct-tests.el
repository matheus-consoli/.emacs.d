;;; consoli-auto-correct-tests.el --- Jinx keyboard regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(require 'consoli-auto-correct)

(defmacro consoli-auto-correct-test--buffer (mode languages &rest body)
  (declare (indent 2))
  `(save-window-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (funcall ,mode)
       (setq-local jinx-languages ,languages)
       (let ((jinx-mode-hook '(consoli-auto-correct-setup)))
         (jinx-mode 1)
         ,@body))))

(ert-deftest consoli-auto-correct-test-punctuation ()
  (consoli-auto-correct-test--buffer #'text-mode "en_US"
    (dolist (suffix '("." ")" "”" "…)" "?!"))
      (erase-buffer)
      (insert "(spellingg" suffix)
      (execute-kbd-macro (kbd "SPC"))
      (should (equal (buffer-string) (concat "(spelling" suffix " ")))
      (execute-kbd-macro (kbd "DEL SPC"))
      (should (equal (buffer-string) (concat "(spellingg" suffix " "))))))

(ert-deftest consoli-auto-correct-test-org-source ()
  (consoli-auto-correct-test--buffer #'org-mode "en_US"
    (dolist (case '(("#+BEGIN_SRC emacs-lisp\n(first\n  " "" nil)
                    ("#+begin_src emacs-lisp\n(" "\n#+end_src" nil)
                    ("An inline src_elisp{" "}" nil)
                    ("An inline src_elisp[:results raw]{" "" nil)
                    ("#+begin_src elisp\n(foo)\n#+end_src\n" "" t)
                    ("#+begin_src elisp\n(foo\n* New heading\n" "" t)
                    ("Inline src_elisp{foo} then " "" t)))
      (erase-buffer)
      (insert (car case) "spellingg")
      (save-excursion (insert (cadr case)))
      (execute-kbd-macro (kbd "SPC"))
      (should (equal (buffer-string)
                     (concat (car case) (if (nth 2 case) "spelling " "spellingg ")
                             (cadr case)))))))

(ert-deftest consoli-auto-correct-test-language-change ()
  (consoli-auto-correct-test--buffer #'text-mode "en_US zz_ZZ"
    (should-not (memq #'consoli-auto-correct--at-space post-self-insert-hook))
    (insert "obrigado")
    (execute-kbd-macro (kbd "SPC"))
    (should (equal (buffer-string) "obrigado "))
    (jinx-languages "en_US")
    (insert "spellingg")
    (execute-kbd-macro (kbd "SPC"))
    (should (equal (buffer-string) "obrigado spelling "))
    ;; An unavailable regional variant may resolve to generic English.
    (jinx-languages "en_UK")
    (should-not (memq #'consoli-auto-correct--at-space post-self-insert-hook))))

(ert-deftest consoli-auto-correct-test-multilingual ()
  (consoli-auto-correct-test--buffer #'text-mode "en_GB en_US pt_BR"
    (dolist (word '("colour" "obrigado" "amanhã"))
      (erase-buffer)
      (insert word)
      (execute-kbd-macro (kbd "SPC"))
      (should (equal (buffer-string) (concat word " "))))
    (when (assoc "pt_BR" (jinx--mod-langs))
      (should (memq #'consoli-auto-correct--at-space post-self-insert-hook))
      (erase-buffer)
      (insert "spellingg")
      (execute-kbd-macro (kbd "SPC"))
      (should (equal (buffer-string) "spelling ")))))

;;; consoli-auto-correct-tests.el ends here
