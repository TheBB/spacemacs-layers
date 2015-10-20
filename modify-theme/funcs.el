(defun modify-theme//in-or-all (key seq)
  (or (eq 'all seq) (memq key seq)))

(defun modify-theme/load-theme (theme &optional no-confirm no-enable)
  (unless no-enable

    ;; Remove existing modifications
    (dolist (face modify-theme--modified-faces)
      (custom-set-faces `(,face ((t ())))))
    (setq modify-theme--modified-faces nil)

    ;; Headings
    (let ((mods nil))
      (when (modify-theme//in-or-all theme modify-theme-headings-inherit-from-default)
        (setq mods (plist-put mods :inherit 'default)))
      (when (modify-theme//in-or-all theme modify-theme-headings-same-size)
        (setq mods (plist-put mods :height 1.0)))
      (when (modify-theme//in-or-all theme modify-theme-headings-bold)
        (setq mods (plist-put mods :weight 'bold)))
      (when mods
        (dolist (face modify-theme--header-faces)
          (custom-set-faces `(,face ((t ,mods))))
          (push face modify-theme--modified-faces))))

    ;; Add new modifications
    (dolist (spec (append (cdr (assq theme modify-theme-modifications))
                          (cdr (assq t modify-theme-modifications))))
      (custom-set-faces `(,(car spec) ((t ,(cdr spec)))))
      (push (car spec) modify-theme--modified-faces))
    ))
