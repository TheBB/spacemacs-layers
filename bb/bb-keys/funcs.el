(defun bb/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))

(defun bb/maybe-quit ()
  (interactive)
  (if (cdr (visible-frame-list))
      (call-interactively 'spacemacs/frame-killer)
    (call-interactively 'spacemacs/prompt-kill-emacs)))

(defun bb/save-delete-quit ()
  (interactive)
  (spacemacs/write-file)
  (kill-this-buffer)
  (bb/maybe-quit))

(defun bb/spaces-before (n)
  (interactive "p")
  (dotimes (c n nil)
    (insert " ")))

(defun bb/spaces-after (n)
  (interactive "p")
  (forward-char)
  (dotimes (c n nil)
    (insert " "))
  (backward-char (1+ n)))

(defun bb/empty-commit ()
  (interactive)
  (insert ".")
  (call-interactively 'with-editor-finish))
