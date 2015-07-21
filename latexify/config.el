(defvar latexify-prefix-sequence "\\"
  "Default prefix")

(defvar latexify--abbrevs
      '(("textexclamdown" "¡")
        ("sterling" "£")
        ("yen" "¥")))

(define-minor-mode latexify-mode
  "Provides LaTeX-like abbreviations."
  :init-value nil
  :lighter "TeX"
  (if latexify-mode (abbrev-mode t) (abbrev-mode nil)))

(setq latexify-abbrev-table nil)
(define-abbrev-table
  'latexify-abbrev-table nil
  "Abbreviations for latexify-mode")
(dolist (abb latexify--abbrevs)
  (define-abbrev latexify-abbrev-table
    (concat latexify-prefix-sequence (car abb))
    (cadr abb)
    nil
    :system t))

(setq abbrev-minor-mode-table-alist nil)
(push `(latexify-mode . ,latexify-abbrev-table)
      abbrev-minor-mode-table-alist)

(abbrev-table-put latexify-abbrev-table :regexp "\\(\\\\[a-z0-9@]+\\)")
