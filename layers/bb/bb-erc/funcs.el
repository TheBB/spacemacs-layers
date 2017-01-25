(defun bb/erc-foolish-filter (msg)
  "Ignores messages matching `erc-foolish-content'."
  (when (erc-list-match erc-foolish-content msg)
    (setq erc-insert-this nil)))

(defun bb/add-buffer-to-erc-persp ()
  (persp-add-buffer (current-buffer)
                    (persp-get-by-name "@ERC")
                    nil))
