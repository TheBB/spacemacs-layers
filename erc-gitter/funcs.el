(defmacro erc-gitter|filter (name regexp &optional replacement)
  (declare (indent 1))
  (let* ((func (intern (format "erc-gitter-filter-%S" name)))
         (start "[Github] ")
         (user '(group-n 1 (one-or-more (not whitespace))))
         (owner '(group-n 2 (one-or-more (not (any space "/")))))
         (owner-b '(backref 2))
         (repo '(group-n 3 (one-or-more (not whitespace))))
         (repo-b '(backref 3))
         (title '(group-n 4 (one-or-more anything)))
         (type '(group-n 5 (or "pull" "issues")))
         (number '(group-n 6 (one-or-more digit)))
         (url `(group-n 7 "http" (optional "s") "://github.com/"
                        ,owner-b "/" ,repo-b "/" ,type "/" ,number
                        (zero-or-more (not whitespace))))
         (commit-a `(group-n 8 (one-or-more (any hex-digit))))
         (commit-b `(group-n 9 (one-or-more (any hex-digit))))
         (url-comp `(group-n 10 "http" (optional "s") "://github.com/"
                             ,owner-b "/" ,repo-b "/compare/"
                             ,commit-a "..." ,commit-b))
         (repo-url `(group-n 11 "http" (optional "s") "://github.com/"
                             (one-or-more (not (any space "/"))) "/"
                             (one-or-more (not (any space "/")))))
         (num `(group-n 12 (one-or-more digit)))
         (regexp (eval `(rx ,@(mapcar 'eval regexp))))
         (replacement
          (when replacement
            `(let ((user (or (match-string 1) "[user]"))
                   (owner (or (match-string 2) "[owner]"))
                   (repo (or (match-string 3) "[repo]"))
                   (title (or (match-string 4) "[title]"))
                   (type (let ((raw-type (match-string 5)))
                           (cond
                            ((string= raw-type "issues") "issue")
                            ((string= raw-type "pull") "pull request")
                            (t "[type]"))))
                   (number (or (match-string 6) "[number]"))
                   (url (or (match-string 7) "[url]"))
                   (commit-a (or (match-string 8) "[commit-a]"))
                   (commit-b (or (match-string 9) "[commit-b]"))
                   (url-comp (or (match-string 10) "[url-comp]"))
                   (num (or (match-string 12) "[num]")))
               (concat ,@replacement)))))
    `(progn
       (defun ,func ()
         (interactive)
         (goto-char (point-min))
         (setq erc-insert-this t)
         (when (re-search-forward ,regexp)
           ,(if replacement
                `(let ((number (match-string 6))
                       (url (match-string 7))
                       (commit-a (match-string 8))
                       (commit-b (match-string 9))
                       (url-comp (match-string 10)))
                   (replace-match ,replacement)
                   (goto-char (point-min))
                   (when (and url number
                              (re-search-forward (concat "#" number)))
                     (erc-button-add-button (match-beginning 0)
                                            (match-end 0)
                                            'browse-url nil (list url)))
                   (goto-char (point-min))
                   (when (and url-comp commit-a commit-b
                              (re-search-forward (concat commit-a
                                                         "\\.\\.\\."
                                                         commit-b)))
                     (erc-button-add-button (match-beginning 0)
                                            (match-end 0)
                                            'browse-url nil (list url-comp))))
              `(setq erc-insert-this nil))))
       ,(if (not replacement)
            `(add-hook 'erc-insert-pre-hook ',func)
          ;; Must come before `erc-fill' in `erc-insert-modify-hook'
          `(setq erc-insert-modify-hook
                 (-insert-at (-elem-index 'erc-fill erc-insert-modify-hook)
                             ',func erc-insert-modify-hook)))
       )))
