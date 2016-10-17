(setq bb-git-packages '(magit))

(defun bb-git/post-init-magit ()
  (setq magit-repository-directories '("~/repos"))
  (with-eval-after-load 'magit
    (define-key magit-mode-map "@" 'magit-branch-pull-request)))
