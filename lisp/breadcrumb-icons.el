;;; breadcrumb-icons.el --- Enhanced breadcrumb navigation with icons -*- lexical-binding: t; -*-

(require 'nerd-icons)

(defgroup breadcrumb-icons nil
  "Icon customization for breadcrumb navigation."
  :group 'breadcrumb
  :prefix "breadcrumb-icons-")

(defcustom breadcrumb-icons-project-root-icon "nf-fa-rocket" ;; 
  "Icon for project root node."
  :type 'string
  :group 'breadcrumb-icons)

(defcustom breadcrumb-icons-folder-icon "nf-fa-folder_open" ;; 
  "Icon for folder nodes."
  :type 'string
  :group 'breadcrumb-icons)

(defcustom breadcrumb-icons-default-leaf-icon "nf-cod-symbol_field" ;; 
  "Default icon for leaf nodes in imenu."
  :type 'string
  :group 'breadcrumb-icons)

(defcustom breadcrumb-icons-enable-caching t
  "Whether to cache icon strings for better performance."
  :type 'boolean
  :group 'breadcrumb-icons)

;;; Icon mappings

(defconst breadcrumb-icons-ipath-alist
  '(("Packages"   . ("nf-cod-package" codicon))            ;; 
    ("Requires"   . ("nf-cod-file_submodule" codicon))     ;; 
    ("Variable"   . ("nf-cod-symbol_variable" codicon))    ;; 
    ("Variables"  . ("nf-cod-symbol_variable" codicon))    ;; 
    ("Function"   . ("nf-md-function_variant" mdicon))     ;; 󰡱
    ("Functions"  . ("nf-md-function_variant" mdicon))     ;; 󰡱
    ("Class"      . ("nf-cod-symbol_class" codicon))       ;; 
    ("Classes"    . ("nf-cod-symbol_class" codicon))       ;; 
    ("Method"     . ("nf-cod-symbol_method" codicon))      ;; 
    ("Methods"    . ("nf-cod-symbol_method" codicon))      ;; 
    ("Interface"  . ("nf-cod-symbol_interface" codicon))   ;; 
    ("Interfaces" . ("nf-cod-symbol_interface" codicon))   ;; 
    ("Type"       . ("nf-cod-symbol_parameter" codicon))   ;; 
    ("Types"      . ("nf-cod-symbol_parameter" codicon)))  ;; 
  "Mapping of ipath node names to their icons and icon types.")

;;; Cache

(defvar breadcrumb-icons--cache (make-hash-table :test 'equal)
  "Cache for generated icon strings.")

;;; Helper functions

(defun breadcrumb-icons--clear-cache ()
  "Clear the icon cache."
  (interactive)
  (clrhash breadcrumb-icons--cache))

(defun breadcrumb-icons--safe-icon (icon-func icon-name &rest args)
  "Safely call ICON-FUNC with ICON-NAME and ARGS.
Return empty string if icon generation fails."
  (condition-case err
      (apply icon-func icon-name args)
    (error
     (message "breadcrumb-icons: Failed to generate icon %s: %s" icon-name err)
     "")))

(defun breadcrumb-icons--get-cached-icon (cache-key icon-func icon-name &rest args)
  "Get icon from cache or generate and cache it.
CACHE-KEY is used for caching, ICON-FUNC is the icon function,
ICON-NAME is the icon identifier, and ARGS are additional arguments."
  (if breadcrumb-icons-enable-caching
      (or (gethash cache-key breadcrumb-icons--cache)
          (puthash cache-key
                   (apply #'breadcrumb-icons--safe-icon icon-func icon-name args)
                   breadcrumb-icons--cache))
    (apply #'breadcrumb-icons--safe-icon icon-func icon-name args)))

(defun breadcrumb-icons--add-icon (icon string)
  "Prepend ICON to STRING with proper spacing."
  (if (and icon (not (string-empty-p icon)))
      (concat icon "" string)
    string))

(defun breadcrumb-icons--get-ipath-icon (node-name face)
  "Get icon for ipath NODE-NAME with FACE."
  (when-let ((icon-data (assoc node-name breadcrumb-icons-ipath-alist)))
    (let* ((icon-name (nth 1 icon-data))
           (icon-type (nth 2 icon-data))
           (icon-func (intern (concat "nerd-icons-" (symbol-name icon-type))))
           (cache-key (format "%s-%s-%s" node-name icon-name face)))
      (breadcrumb-icons--get-cached-icon cache-key icon-func icon-name :face face))))

;;; Advice functions

(defun breadcrumb-icons--format-project-node-advice (original-func p more &rest r)
  "Add icons to project nodes.
ORIGINAL-FUNC is the original function, P is the project,
MORE indicates if there are more nodes, R are additional arguments."
  (let ((string (apply original-func p more r)))
    (cond
     ((not more)
      ;; Leaf node - use file icon
      (let* ((cache-key (format "file-%s" string))
             (icon (breadcrumb-icons--get-cached-icon
                    cache-key #'nerd-icons-icon-for-file string)))
        (breadcrumb-icons--add-icon icon string)))
     (t
      ;; Parent node - use folder icon
      (let* ((cache-key (format "folder-%s" breadcrumb-icons-folder-icon))
             (icon (breadcrumb-icons--get-cached-icon
                    cache-key #'nerd-icons-faicon
                    breadcrumb-icons-folder-icon
                    :face 'breadcrumb-project-crumbs-face)))
        (breadcrumb-icons--add-icon icon string))))))

(defun breadcrumb-icons--project-crumbs-advice (return-value)
  "Add rocket icon to project root.
RETURN-VALUE is the original return value from the function."
  (when (listp return-value)
    (let* ((cache-key (format "root-%s" breadcrumb-icons-project-root-icon))
           (icon (breadcrumb-icons--get-cached-icon
                  cache-key #'nerd-icons-faicon
                  breadcrumb-icons-project-root-icon
                  :face 'breadcrumb-project-base-face)))
      (setf (car return-value)
            (concat " " (breadcrumb-icons--add-icon icon (car return-value))))))
  return-value)

(defun breadcrumb-icons--format-ipath-node-advice (original-func p more &rest r)
  "Add icons to ipath nodes.
ORIGINAL-FUNC is the original function, P is the path,
MORE indicates if there are more nodes, R are additional arguments."
  (let ((string (apply original-func p more r)))
    (cond
     ((not more)
      ;; Leaf node - use default leaf icon
      (let* ((cache-key (format "leaf-%s" breadcrumb-icons-default-leaf-icon))
             (icon (breadcrumb-icons--get-cached-icon
                    cache-key #'nerd-icons-codicon
                    breadcrumb-icons-default-leaf-icon
                    :face 'breadcrumb-imenu-leaf-face)))
        (breadcrumb-icons--add-icon icon string)))
     (t
      ;; Parent node - check for specific node types
      (let ((icon (breadcrumb-icons--get-ipath-icon string 'breadcrumb-imenu-crumbs-face)))
        (breadcrumb-icons--add-icon icon string))))))

;;; Minor mode

(defvar breadcrumb-icons--advices
  '((breadcrumb--format-project-node . (:around . breadcrumb-icons--format-project-node-advice))
    (breadcrumb--project-crumbs-1 . (:filter-return . breadcrumb-icons--project-crumbs-advice))
    (breadcrumb--format-ipath-node . (:around . breadcrumb-icons--format-ipath-node-advice)))
  "List of advice functions to apply.")

(defun breadcrumb-icons--apply-advices ()
  "Apply all breadcrumb icon advices."
  (dolist (advice breadcrumb-icons--advices)
    (let ((func (car advice))
          (advice-type (cadr advice))
          (advice-func (cddr advice)))
      (advice-add func advice-type advice-func))))

(defun breadcrumb-icons--remove-advices ()
  "Remove all breadcrumb icon advices."
  (dolist (advice breadcrumb-icons--advices)
    (let ((func (car advice))
          (advice-func (cddr advice)))
      (advice-remove func advice-func))))

;;;###autoload
(define-minor-mode breadcrumb-icons-mode
  "Toggle breadcrumb icons mode.
When enabled, breadcrumb navigation will display contextual icons
for projects, files, and code elements."
  :global t
  :group 'breadcrumb-icons
  :lighter " BCI"
  (if breadcrumb-icons-mode
      (progn
        (breadcrumb-icons--apply-advices)
        (message "Breadcrumb icons mode enabled"))
    (breadcrumb-icons--remove-advices)
    (breadcrumb-icons--clear-cache)
    (message "Breadcrumb icons mode disabled")))

;;; Interactive functions

;;;###autoload
(defun breadcrumb-icons-reload ()
  "Reload breadcrumb icons configuration."
  (interactive)
  (when breadcrumb-icons-mode
    (breadcrumb-icons--remove-advices)
    (breadcrumb-icons--clear-cache)
    (breadcrumb-icons--apply-advices)
    (message "Breadcrumb icons reloaded")))

(provide 'breadcrumb-icons)

;;; breadcrumb-icons.el ends here
