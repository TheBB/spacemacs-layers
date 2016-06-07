(defconst bb-ag-skip-packages '(ivy
                                helm
                                helm-ag))

(defun bb-ag-skip/post-init-ivy ()
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map (kbd "<f3>") 'bb/ivy-save-to-buffer)))

(defun bb-ag-skip/post-init-helm ()
  (with-eval-after-load 'helm-grep
    (advice-add 'helm-grep-save-results-1 :after 'bb/grep-skip-init)))

(defun bb-ag-skip/post-init-helm-ag ()
  (with-eval-after-load 'helm-ag
    (advice-add 'helm-ag--save-results :after 'bb/ag-skip-init)))
