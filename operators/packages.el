(setq operators-packages
      '(evil
        gist))

(defun operators/post-init-evil ()
  (spacemacs/set-leader-keys "nn" 'operators-narrow))

(defun operators/post-init-gist ()
  (spacemacs/set-leader-keys
    "gg" 'operators-gist-public
    "gG" 'operators-gist-private))
