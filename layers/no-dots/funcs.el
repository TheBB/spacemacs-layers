(require 'cl-lib)

(defun no-dots/whitelistedp ()
  (member (with-helm-buffer (buffer-name)) no-dots-whitelist))

(defun no-dots/helm-ff-filter-candidate-one-by-one (fcn file)
  (when (or (no-dots/whitelistedp)
            (not (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)))
    (funcall fcn file)))

(defun no-dots/helm-file-completion-source-p (&rest args) t)

(defun no-dots/helm-attrset (fcn attribute-name value &optional src)
  (let ((src (or src (helm-get-current-source))))
    (when src
      (funcall fcn attribute-name value src))))

(defun no-dots/helm-find-files-up-one-level (fcn &rest args)
  (advice-add 'helm-file-completion-source-p
              :around 'no-dots/helm-file-completion-source-p)
  (advice-add 'helm-attrset
              :around 'no-dots/helm-attrset)
  (let ((res (apply fcn args)))
    (advice-remove 'helm-file-completion-source-p
                   'no-dots/helm-file-completion-source-p)
    (advice-remove 'helm-attrset
                   'no-dots/helm-attrset)
    res))
