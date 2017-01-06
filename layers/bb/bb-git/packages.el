(setq bb-git-packages '(magit))

(defun bb-git/post-init-magit ()
  (with-eval-after-load 'magit-repos
    (push '("~/repos") magit-repository-directories))
  (with-eval-after-load 'magit
    (define-key magit-mode-map "@" 'magit-branch-pull-request)))
