(defvar-local bb--ag-tracker nil)

(defmacro bb|ag-skipper (name func)
  `(defun ,name ()
     (interactive)
     (let ((candidate
            (with-current-buffer "*helm-ag*"
              (when bb--ag-tracker (goto-char bb-ag-tracker))
              (,func)
              (let ((str (buffer-substring (point-at-bol) (point-at-eol))))
                (when (and str (< 0 (length str))
                           (< 1 (line-number-at-pos)))
                  (setq bb--ag-tracker (point))
                  (message
                   "Match %d/%d"
                   (- (line-number-at-pos) 1)
                   (- (save-excursion (goto-char (point-max)) (line-number-at-pos)) 2))
                  str)))))
       (when candidate
         (helm-ag--find-file-action candidate 'find-file nil)))))

(bb|ag-skipper bb/ag-next next-line)
(bb|ag-skipper bb/ag-prev previous-line)
