(with-eval-after-load 'evil
  (evil-define-operator operators-narrow (beg end)
    (narrow-to-region beg end))
  (evil-define-operator operators-gist-public (beg end)
    (gist-region beg end))
  (evil-define-operator operators-gist-private (beg end)
    (gist-region beg end 'private)))
