(c-add-style "bb"
             '((indent-tabs-mode . nil)
               (c-basic-offset . 4)
               (c-offsets-alist
                (substatement-open . 0)
                (inline-open . 0)
                (statement-cont . c-lineup-assignments)
                (inextern-lang . 0)
                (innamespace . 0))))

(c-add-style "sintef"
             '((indent-tabs-mode . nil)
               (c-basic-offset . 2)
               (c-offsets-alist
                (substatement-open . 0)
                (inline-open . 0)
                (statement-cont . c-lineup-assignments)
                (inextern-lang . 0)
                (innamespace . 0))))

(push '(other . "bb") c-default-style)

(setq-default c-macro-names-with-semicolon
              '("Q_OBJECT"
                "Q_PROPERTY"
                "Q_DECLARE"
                "Q_ENUMS"
                "Q_INTERFACES"))

(add-hook 'c-mode-common-hook 'c-make-macro-with-semi-re)

(dolist (mode '(c-mode c++-mode))
  (spacemacs/set-leader-keys-for-major-mode mode
    "os" 'c-set-style))
