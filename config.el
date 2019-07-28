(defun load-config (file)
  (org-babel-load-file (expand-file-name
			(concat user-emacs-directory file))))

(load-config "general.org")

(load-config "appearence.org")

(load-config "prog.org")

(load-config "rust.org")

(load-config "haskell.org")
