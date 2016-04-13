(setq bb-keys-packages
      '(avy
        company
        evil
        evil-unimpaired
        helm
        hippie-exp
        magit
        (text-mode :location built-in)))

(defun bb-keys/post-init-avy ()
  (with-eval-after-load 'evil-integration
    (evil-define-avy-motion avy-goto-char-timer inclusive)
    (evil-define-avy-motion avy-isearch inclusive))
  (dolist (c '(goto-char-timer goto-char goto-char-2 isearch))
    (autoload (intern (format "evil-avy-%s" c))
      "evil-integration" nil 'interactive))
  (bb/define-key isearch-mode-map (kbd "C-'") 'evil-avy-isearch)
  (spacemacs/set-leader-keys
    "y" 'evil-avy-goto-char-timer
    "," 'evil-avy-goto-char
    "." 'evil-avy-goto-char-2))

(defun bb-keys/post-init-company ()
  (bb/define-key evil-insert-state-map (kbd "C-l") 'company-complete)
  (with-eval-after-load 'company
    (bb/define-key company-active-map
      (kbd "C-w") 'evil-delete-backward-word
      (kbd "C-s") 'company-filter-candidates)))

(defun bb-keys/post-init-evil ()
  (bb/define-key evil-normal-state-map
    "+" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
    "_" 'spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt)
  (bb/define-key evil-insert-state-map
    (kbd "C-e") 'move-end-of-line
    (kbd "C-a") 'back-to-indentation)
  (spacemacs/set-leader-keys
    "ee" 'evil-edit))

(defun bb-keys/post-init-evil-unimpaired ()
  (bb/define-key evil-normal-state-map
    "[s" 'bb/spaces-before
    "]s" 'bb/spaces-after))

(defun bb-keys/post-init-helm ()
  (spacemacs/set-leader-keys
    "ot" 'helm-top))

(defun bb-keys/post-init-hippie-exp ()
  (bb/define-key evil-insert-state-map
    (kbd "C-SPC") 'hippie-expand))

(defun bb-keys/post-init-magit ()
  (with-eval-after-load 'magit
    (evil-define-key 'normal magit-mode-map (kbd "ESC") nil)
    (evil-define-key 'normal magit-mode-map (kbd "M-j") 'magit-section-forward-sibling)
    (evil-define-key 'normal magit-mode-map (kbd "M-k") 'magit-section-backward-sibling)))

(defun bb-keys/init-text-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'text-mode
    "." 'bb/empty-commit))
