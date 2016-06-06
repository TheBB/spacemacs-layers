(defvar-local bb--ag-tracker nil)
(defvar-local bb--ag-max nil)

(defun bb/ag-skip-init (&rest args)
  (with-current-buffer "*helm ag results*"
    (setq bb--ag-tracker 5)
    (setq bb--ag-max (save-excursion
                       (goto-char (point-max))
                       (previous-line)
                       (line-number-at-pos)))
    (setq next-error-function 'bb/ag-skip-next)))

(defun bb/ag-skip-next (num reset)
  (let ((cand (with-current-buffer "*helm ag results*"
                (save-excursion
                  (when reset (setq bb--ag-tracker 5))
                  (setq bb--ag-tracker (min bb--ag-max (max 5 (+ bb--ag-tracker num))))
                  (goto-line bb--ag-tracker)
                  (helm-current-line-contents)))))
    (helm-ag--find-file-action cand 'find-file helm-ag--search-this-file-p)))
