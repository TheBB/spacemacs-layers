(defconst bb-ag-skip-packages '(helm-ag))

(defun bb-ag-skip/post-init-helm-ag ()
  (define-key evil-normal-state-map (kbd "] a") 'bb/ag-next)
  (define-key evil-normal-state-map (kbd "[ a") 'bb/ag-prev)
  (with-eval-after-load 'helm-ag
    (advice-add 'helm-ag--action-find-file
                :before 'bb/helm-ag--action-find-file-before)))
