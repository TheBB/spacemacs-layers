;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; Thanks Jonas Bernoulli

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
  (if checkout
      (magit-run-git "checkout" branch)
    (apply #'magit-log (list branch) (magit-log-arguments))))
