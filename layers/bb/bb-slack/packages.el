(setq bb-slack-packages '(slack))

(defun bb-slack/post-init-slack ()
  (require 'slack-team)
  (dolist (s bb/slacks)
    (apply 'slack-register-team :name s)))
