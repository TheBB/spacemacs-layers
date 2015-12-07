(defun bb/erc-github-filter ()
  "Shortens messages from gitter."
  (interactive)
  (when (and (< 18 (- (point-max) (point-min)))
             (string= (buffer-substring (point-min)
                                        (+ (point-min) 18))
                      "<gitter> [Github] "))
    (dolist (regexp '(" \\[Github\\]"
                      " \\(?:in\\|to\\) [^ /]+/[^ /:]+"))
      (goto-char (point-min))
      (when (re-search-forward regexp (point-max) t)
        (replace-match "")))
    (goto-char (point-min))
    (when (re-search-forward
           "https?://github\\.com/[^/]+/[^/]+/[^/]+/\\([[:digit:]]+\\)\\([^[:space:]]*\\)?"
           (point-max) t)
      (let* ((url (match-string 0))
             (number (match-string 1))
             (start (+ 1 (match-beginning 0)))
             (end (+ 1 (length number) start)))
        (replace-match (format "(#%s)" (match-string 1)))
        (erc-button-add-button start end 'browse-url nil (list url))))))

(defun bb/erc-foolish-filter (msg)
  "Ignores messages matching `erc-foolish-content'."
  (when (erc-list-match erc-foolish-content msg)
    (setq erc-insert-this nil)))

(defun bb/add-buffer-to-erc-persp ()
  (persp-add-buffer (current-buffer)
                    (persp-get-by-name "@ERC")
                    nil))
