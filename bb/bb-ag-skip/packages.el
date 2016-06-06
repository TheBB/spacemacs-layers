(defconst bb-ag-skip-packages '(helm-ag))

(defun bb-ag-skip/post-init-helm-ag ()
  (with-eval-after-load 'helm-ag
    (advice-add 'helm-ag--save-results
                :after 'bb/ag-skip-init)))
