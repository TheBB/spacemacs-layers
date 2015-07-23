(defvar evil-shift-width-alist
  '((python-mode . python-indent-offset)
    ((c-mode c++-mode java-mode) . c-basic-offset))
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
                  (throw 'break
                         (if (integerp val) val (eval val))))))
            (throw 'break evil-shift-width))))
     (if (and (integerp shift-width)
              (< 0 shift-width))
         (setq-local evil-shift-width shift-width)
       (message "Unable to appropriately set evil-shift-width."))))
 t)

(advice-add 'c-set-style
            :after 'evil-shift-width/set-width)
