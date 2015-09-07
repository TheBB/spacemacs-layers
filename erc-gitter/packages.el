(setq erc-gitter-packages '(erc))

(defun erc-gitter/pre-init-erc ()
  (with-eval-after-load 'erc
    (message ">>> HI THERE!")
    ;; Ensure that `erc-button-add-buttons' comes before
    ;; `erc-fill' in `erc-insert-modify-hook'
    (when (and (memq 'erc-button-add-buttons erc-insert-modify-hook)
               (memq 'erc-fill erc-insert-modify-hook))
      (remove-hook 'erc-insert-modify-hook 'erc-button-add-buttons)
      (setq erc-insert-modify-hook
            (-insert-at (-elem-index 'erc-fill erc-insert-modify-hook)
                        'erc-button-add-buttons erc-insert-modify-hook)))

    (erc-gitter|filter comment
      (start user " commented in " owner "/" repo " on issue: " title " " url)
      (user " commented on " type " #" number ": " title))
    (erc-gitter|filter close-issue
      (start user " closed an issue in " owner "/" repo ": " title " " url)
      (user " closed issue #" number ": " title))
    (erc-gitter|filter open-issue
      (start user " opened an issue in " owner "/" repo ": " title " " url)
      (user " opened issue #" number ": " title))
    (erc-gitter|filter close-pull
      (start user " closed a Pull Request to " owner "/" repo ": " title " " url)
      (user " closed pull request #" number ": " title))
    (erc-gitter|filter open-pull
      (start user " opened a Pull Request to " owner "/" repo ": " title " " url)
      (user " opened pull request #" number ": " title))
    (erc-gitter|filter commits
      (start user " pushed " num " commit(s) to " owner "/" repo " " url-comp)
      (user " pushed " num " commits: " commit-a "..." commit-b))
    (erc-gitter|filter synchronize
      (start user " synchronize a Pull Request to " owner "/" repo ": " title " " url))
    (erc-gitter|filter star
      (start user " starred " repo-url))
    (erc-gitter|filter fork
      (start user " forked " owner "/" repo ": " repo-url))
    (erc-gitter|filter label
      (start user " labeled an issue in " owner "/" repo ": " title " " url))
    (erc-gitter|filter unlabel
      (start user " unlabeled an issue in " owner "/" repo ": " title " " url))))
