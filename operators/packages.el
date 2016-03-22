(setq operators-packages '(evil))

(defun operators/post-init-evil ()
  (spacemacs/set-leader-keys "nn" 'evil-narrow-operator))
