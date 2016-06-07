(defconst bb-ag-skip-packages '(helm
                                helm-ag))

(defun bb-ag-skip/post-init-helm ()
  (with-eval-after-load 'helm-grep
    (advice-add 'helm-grep-save-results-1 :after 'bb/grep-skip-init)))

(defun bb-ag-skip/post-init-helm-ag ()
  (with-eval-after-load 'helm-ag
    (advice-add 'helm-ag--save-results :after 'bb/ag-skip-init)))
