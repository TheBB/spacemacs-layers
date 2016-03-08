(setq bb-web-packages '(web-mode))

(defun bb-web/post-init-web-mode ()
  (add-hook 'web-mode-hook 'bb-auto-tags-close-mode))
