(setq screaming-modeline-packages '(spaceline))

(defun screaming-modeline/post-init-spaceline ()
  (setq spaceline-face-func 'scml-face-func))
