(defun bb/web-post-command ()
  (cond
   ((and (looking-back "/")
         (looking-at "</"))
    (delete-char -1)
    (re-search-forward ">"))
   ((looking-back "\" >")
    (backward-char 2)
    (delete-char 1)
    (forward-char 1))))

(define-minor-mode bb-auto-tags-close-mode
  "Minor mode for closing tags properly in web-mode."
  :init-value nil
  :global nil
  (if bb-auto-tags-close-mode
      (add-hook 'post-command-hook 'bb/web-post-command nil 'local)
    (remove-hook 'post-command-hook 'bb/web-post-command 'local)))
