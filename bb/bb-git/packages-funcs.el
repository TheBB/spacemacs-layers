(defun magit-branch-pull-request (number &optional branch checkout)
  "Create a new branch from a Github pull request and show its log.
Read \"NR[:BRANCH-NAME] from the user.  If BRANCH-NAME is not
provided use \"pr-NR\".  Set \"master\" as the upstream.
Assume all pull requests can be found on \"upstream\".  With a
prefix argument checkout branch instead of showing its log."
  (interactive
   (let ((input (magit-read-string "Branch pull request (NR[:BRANCH-NAME])")))
     (if (string-match "\\([1-9][0-9]*\\)\\(?::\\(.+\\)\\)?" input)
         (list (match-string 1 input)
               (match-string 2 input)
               current-prefix-arg)
       (user-error "Invalid input"))))
  (unless branch
    (setq branch (format "pr-%s" number)))
  (magit-call-git "fetch" "upstream" (format "pull/%s/head:%s" number branch))
  (magit-set-branch*merge/remote branch "master")
  (if checkout
      (magit-run-git "checkout" branch)
    (apply #'magit-log (list branch) (magit-log-arguments))))
