(setq bb-org-packages '(org))

(defun bb-org/post-init-org ()
  (setq-default
   org-tags-column -80
   org-startup-indented t
   org-clock-into-drawer "LOGBOOK"
   org-log-into-drawer "LOGBOOK"
   org-startup-align-all-tables t
   org-footnote-auto-adjust t
   org-footnote-auto-label 'confirm
   org-M-RET-may-split-line
   '((headline . nil) (item . nil) (table . nil))
   org-agenda-restore-windows-after-quit t
   org-agenda-window-setup 'other-window
   org-directory "~/org"
   org-default-notes-file "~/org/capture.org"
   org-agenda-files '("~/org/agenda.org")
   org-catch-invisible-edits 'show-and-error
   org-list-demote-modify-bullet '(("-" . "*") ("*" . "+") ("+" . "-"))
   org-list-allow-alphabetical t
   org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(D)")
     (type "SIMPLE(s)" "FAST-TRACK(f)" "CONFLICTING(c)" "WAITING(w)" "DUBIOUS(d)"
           "|" "MERGED(M)" "CLOSED(C)"))
   org-todo-keyword-faces
   '(("SIMPLE" . "khaki2")
     ("FAST-TRACK" . "OrangeRed1")
     ("WAITING" . "deepskyblue1"))
   org-capture-templates
   '(("t" "Tasks")
     ("tg" "General" entry (file+headline "" "Tasks")
      "* TODO %?\n%i\n%U"
      :empty-lines 1)
     ("tl" "Location" entry (file+headline "" "Tasks")
      "* TODO %?\n%i\n%U\n%a"
      :empty-lines 1)
     ("n" "Notes")
     ("ng" "General" entry (file+headline "" "Notes")
      "* %?\n%i\n%U"
      :empty-lines 1)
     ("nl" "Location" entry (file+headline "" "Notes")
      "* %?\n%i\n%U\n%a"
      :empty-lines 1))))
