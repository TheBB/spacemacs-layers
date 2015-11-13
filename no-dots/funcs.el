(require 'cl-lib)

(defun no-dots/helm-ff-filter-candidate-one-by-one (fcn file)
  (unless (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
    (funcall fcn file)))

(defun no-dots/helm-file-completion-source-p (&rest args) t)

(defun no-dots/helm-find-files-up-one-level (fcn &rest args)
  (prog2
      (advice-add 'helm-file-completion-source-p
                  :around 'no-dots/helm-file-completion-source-p)
      (apply fcn args)
    (advice-remove 'helm-file-completion-source-p
                   'no-dots/helm-file-completion-source-p)))
