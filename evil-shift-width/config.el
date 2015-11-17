(defun evil-shift-width/latex-mode ()
  (or (and (boundp 'LaTeX-indent-level) LaTeX-indent-level)
      (and (boundp 'tex-indent-basic) tex-indent-basic)))

;; Thanks to editorconfig-emacs for many of these
(defvar evil-shift-width-alist
  '(((awk-mode c-mode c++-mode java-mode groovy-mode
      idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    (latex-mode . (evil-shift-width/latex))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers or forms
which are evaluated upon entering a major mode.")

(add-hook
 'after-change-major-mode-hook
 (defun evil-shift-width/set-width (&rest args)
   (let ((shift-width
          (catch 'break
            (dolist (test evil-shift-width-alist)
              (let ((mode (car test))
                    (val (cdr test)))
                (when (or (and (symbolp mode) (derived-mode-p mode))
                          (and (listp mode) (apply 'derived-mode-p mode))
                          (eq 't mode))
                  (throw 'break (cond ((integerp val) val)
                                      (t (eval val)))))))
            (throw 'break evil-shift-width))))
     (if (and (integerp shift-width)
              (< 0 shift-width))
         (setq-local evil-shift-width shift-width)
       (message "Unable to appropriately set evil-shift-width."))))
 t)

(advice-add 'c-set-style
            :after 'evil-shift-width/set-width)
