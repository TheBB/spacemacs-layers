(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (re-search-forward "[[:nonascii:]]"))
