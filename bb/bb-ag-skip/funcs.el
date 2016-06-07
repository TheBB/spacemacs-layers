(defvar-local bb--ag-tracker nil)
(defvar-local bb--ag-max nil)
(defvar bb--ag-buf nil)

(defun bb/ag-skip-init (&rest args)
  (with-current-buffer "*helm ag results*"
    (setq bb--ag-buf (current-buffer))
    (setq bb--ag-tracker 5)
    (setq bb--ag-max (save-excursion
                       (goto-char (point-max))
                       (previous-line)
                       (line-number-at-pos)))
    (setq next-error-function 'bb/ag-skip-next)))

(defun bb/ag-skip-next (num reset)
  (let ((cand (with-current-buffer bb--ag-buf
                (save-excursion
                  (when reset (setq bb--ag-tracker 5))
                  (setq bb--ag-tracker (min bb--ag-max (max 5 (+ bb--ag-tracker num))))
                  (goto-line bb--ag-tracker)
                  (buffer-substring (point-at-bol) (point-at-eol))))))
    (helm-ag--find-file-action cand 'find-file helm-ag--search-this-file-p)))

(defun bb/grep-skip-init (&rest args)
  (with-current-buffer "*hgrep*"
    (setq bb--ag-buf (current-buffer))
    (setq bb--ag-tracker 5)
    (setq bb--ag-max (save-excursion
                       (goto-char (point-max))
                       (previous-line)
                       (line-number-at-pos)))
    (setq next-error-function 'bb/grep-skip-next)))

(defun bb/grep-skip-next (num reset)
  (let ((cand (with-current-buffer bb--ag-buf
                (save-excursion
                  (when reset (setq bb--ag-tracker 5))
                  (setq bb--ag-tracker (min bb--ag-max (max 5 (+ bb--ag-tracker num))))
                  (goto-line bb--ag-tracker)
                  (buffer-substring (point-at-bol) (point-at-eol))))))
    (with-current-buffer bb--ag-buf
      (goto-line bb--ag-tracker)
      (helm-grep-action cand))))

(defun bb/ivy-skip-next (num reset)
  (let ((cand (with-current-buffer bb--ag-buf
                (save-excursion
                  (when reset (setq bb--ag-tracker 5))
                  (setq bb--ag-tracker (min bb--ag-max (max 1 (+ bb--ag-tracker num))))
                  (goto-line bb--ag-tracker)
                  (buffer-substring (point-at-bol) (point-at-eol))))))
    (with-current-buffer bb--ag-buf
      (let ((counsel--git-grep-dir default-directory))
        (goto-line bb--ag-tracker)
        (counsel-git-grep-action cand)))))

(defun bb/ivy-save-to-buffer ()
  (interactive)
  (ivy-quit-and-run
   (let ((buf "*ivy results*"))
     (with-current-buffer (get-buffer-create buf)
       (erase-buffer)
       (dolist (c ivy--all-candidates)
         (insert c "\n"))
       (setq bb--ag-buf (current-buffer))
       (setq bb--ag-tracker 1)
       (setq bb--ag-max (save-excursion
                          (goto-char (point-max))
                          (previous-line)
                          (line-number-at-pos)))
       (setq next-error-function 'bb/ivy-skip-next))
     (pop-to-buffer buf))))
