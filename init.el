(defun dotspacemacs/layers ()
  (setq-default

   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/")
   dotspacemacs-delete-orphan-packages t
   dotspacemacs-distribution 'spacemacs

   dotspacemacs-configuration-layers
   `((auto-completion
      :variables
      auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior 'cycle
      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
      :disabled-for org erc)
     (c-c++ :variables c-c++-enable-clang-support t)
     clojure
     command-log
     csharp
     django
     elixir
     elfeed
     emacs-lisp
     emoji
     erc
     ess
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     extra-langs
     eyebrowse
     games
     git
     github
     haskell
     html
     (ibuffer :variables ibuffer-group-buffers-by nil)
     javascript
     latex
     markdown
     org
     python
     ranger
     ruby
     rust
     semantic
     (shell :variables shell-default-shell 'eshell)
     shell-scripts
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     (theming :variables
              theming-headings-inherit-from-default 'all
              theming-headings-same-size 'all
              theming-headings-bold 'all)
     typography
     (version-control :variables version-control-diff-tool 'diff-hl)
     vimscript
     yaml

     ,@(unless (string= system-type "windows-nt")
         '(dash
           fasd
           gtags
           (spell-checking :variables spell-checking-enable-by-default nil)
           spotify))

     ;; Non-contrib layers
     encoding
     evil-little-word
     no-dots

     ;; Personal config layers
     bb-c-styles
     bb-erc
     bb-ibuffer
     bb-latex)

   dotspacemacs-additional-packages
   `(helm-flycheck
     ;; (helm-fuzzier :location (recipe :fetcher github
     ;;                                 :repo "EphramPerdition/helm-fuzzier"))
     nameless
     nginx-mode

     ,@(unless (string= system-type "windows-nt")
         '(powerline
           (spaceline :location "~/repos/spaceline/")
           (unicode-fonts :excluded t))))

   dotspacemacs-excluded-packages
   `(clj-refactor
     elfeed-org
     julia-mode
     helm-flx
     vi-tilde-fringe
     ,@(when (string= system-type "windows-nt")
         '(evil-mc)))))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 10
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes
   '(monokai material spacemacs-dark spacemacs-light solarized-dark leuven zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font
   `("Source Code Pro"
     :size ,(if (string= system-type "windows-nt") 16 13)
     :weight normal :width normal :powerline-scale 1.15)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Home"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 10
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize t
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 1.0
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide nil
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed))

(defun dotspacemacs/user-init ()
  (setq-default

   ;; Miscellaneous
   vc-follow-symlinks t
   ring-bell-function 'ignore
   require-final-newline t
   indent-tabs-mode nil
   system-time-locale "C"
   paradox-github-token t
   open-junk-file-find-file-function 'find-file

   ;; Backups
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   make-backup-files nil

   ;; Evil
   evil-shift-round nil
   evil-want-C-i-jump t

   ;; Whitespace mode
   whitespace-style '(face tabs tab-mark newline-mark)
   whitespace-display-mappings
   '((newline-mark 10 [172 10])
     (tab-mark 9 [9655 9]))

   ;; Smartparens
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil

   ;; Magit
   magit-popup-show-common-commands nil

   ;; Flycheck
   flycheck-check-syntax-automatically '(save mode-enabled)

   ;; Avy
   avy-all-windows 'all-frames

   ;; Ranger
   ranger-override-dired t

   ;; Spaceline
   spaceline-buffer-encoding-abbrev-p nil
   spaceline-version-control-p nil

   ;; Matlab
   matlab-auto-fill nil
   matlab-fill-code nil
   matlab-functions-have-end t
   matlab-indent-function-body t

   ;; LaTeX
   font-latex-fontify-script nil
   TeX-newline-function 'reindent-then-newline-and-indent

   ;; Shell
   shell-default-term-shell "/bin/zsh"

   ;; Web
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2

   ;; Emacs Lisp
   nameless-global-aliases
   '(("sm" . "spacemacs")
     ("dsm" . "dotspacemacs")
     ("cfl" . "configuration-layer")
     ("sl" . "spaceline")
     ("eip" . "evil-indent-plus"))
   nameless-discover-current-name nil
   nameless-prefix ""
   nameless-separator nil

   ;; Rust
   rust-indent-method-chain t

   ;; Elfeed
   elfeed-feeds
   '("https://www.reddit.com/r/emacs/.rss"
     "http://xkcd.com/rss.xml")

   ;; Org
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
      :empty-lines 1))

   ;; IRC
   erc-autojoin-channels-alist
   '(("1\\.0\\.0" "#syl20bnr/spacemacs" "#syl20bnr/spacemacs-devel") ; Gitter
     ("irc.gitter.im" "#syl20bnr/spacemacs" "#syl20bnr/spacemacs-devel")
     ("freenode\\.net" "#emacs" "#emacs-beginners" "#spacemacs" "#evil-mode"))

   ;; Theme modifications
   theming-modifications
   '((monokai
      ;; Font locking
      (font-lock-comment-face :slant italic)
      (font-lock-string-face :slant italic)
      (font-lock-doc-face :slant italic)
      (font-lock-keyword-face :weight bold)
      (font-lock-builtin-face :foreground "#ff9eb8")
      (font-lock-warning-face :underline nil)
      (web-mode-html-attr-value-face
       :inherit font-lock-string-face :foreground nil)
      (web-mode-html-attr-name-face
       :inherit font-lock-variable-name-face :foreground nil)
      (web-mode-html-tag-face
       :inherit font-lock-builtin-face :foreground nil :weight bold)
      (web-mode-html-tag-bracket-face
       :inherit web-mode-html-tag-face :foreground nil)
      (web-mode-comment-face
       :inherit font-lock-comment-face :foreground nil)

      ;; Modeline
      (mode-line :box (:color "#999999" :line-width 1 :style released-button))
      (powerline-active1 :box (:color "#999999" :line-width 1 :style released-button)
                         :background "#5a5a5a")
      (powerline-active2 :box (:color "#999999" :line-width 1 :style released-button))
      (mode-line-inactive :box (:color "#666666" :line-width 1 :style released-button))
      (powerline-inactive1 :box (:color "#666666" :line-width 1 :style released-button))
      (powerline-inactive2 :box (:color "#666666" :line-width 1 :style released-button))
      (helm-prefarg :foreground "PaleGreen")

      ;; Flycheck
      (flycheck-fringe-error :background nil)
      (flycheck-fringe-warning :background nil)
      (flycheck-fringe-info :background nil)

      ;; Other
      (company-tooltip-annotation
       :foreground "#ff9eb8" :background "#49483e")
      (company-tooltip-annotation-selection :background "#66d9ef")
      (erc-timestamp-face
       :inherit font-lock-comment-face :foreground nil)
      (evil-search-highlight-persist-highlight-face
       :background "#fc5fef" :foreground "#000000")
      (region :background "#998f84")
      (spacemacs-transient-state-title-face :background nil :foreground nil :inherit font-lock-warning-face)
      (term :foreground nil :background nil)))))

(defun dotspacemacs/user-config ()

  ;; Utility functions
  (defun bb/define-key (keymap &rest bindings)
    (declare (indent 1))
    (while bindings
      (define-key keymap (pop bindings) (pop bindings))))

  ;; Settings
  (setq-default
   tab-width 8
   evil-move-beyond-eol nil
   helm-echo-input-in-header-line nil
   powerline-default-separator 'alternate)

  (dolist (e '(("xml" . web-mode)
               ("xinp" . web-mode)
               ("C" . c++-mode)
               ("h" . c++-mode)))
    (push (cons (concat "\\." (car e) "\\'") (cdr e)) auto-mode-alist))
  (push '("PKGBUILD" . shell-script-mode) auto-mode-alist)
  (with-eval-after-load 'projectile
    (push '("C" "h") projectile-other-file-alist))

  ;; Keybindings
  (bb/define-key evil-normal-state-map
    "+" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
    "_" 'spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt
    "\\" 'evil-repeat-find-char-reverse
    "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
    "]s" (lambda (n) (interactive "p")
           (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))
  (bb/define-key evil-insert-state-map
    (kbd "M-SPC") 'hippie-expand
    (kbd "C-e") 'move-end-of-line
    (kbd "C-a") 'back-to-indentation)
  (with-eval-after-load 'helm
    (bb/define-key helm-map
      (kbd "C-S-q") 'ace-jump-helm-line-execute-action))
  (spacemacs/set-leader-keys
    "ee" 'evil-edit
    "os" 'just-one-space
    "qw" (defun bb/maybe-quit ()
           (interactive)
           (if (cdr (visible-frame-list))
               (call-interactively 'spacemacs/frame-killer)
             (call-interactively 'spacemacs/prompt-kill-emacs)))
    "qf" (defun bb/save-delete-quit ()
           (interactive)
           (spacemacs/write-file)
           (kill-this-buffer)
           (bb/maybe-quit))
    "feh" 'helm-spacemacs-help
    "," 'avy-goto-char
    "." 'avy-goto-char-2)
  (spacemacs/set-leader-keys-for-major-mode 'text-mode
    "." (defun bb/empty-commit ()
          (interactive)
          (insert ".")
          (call-interactively 'with-editor-finish)))
  (bb/define-key company-active-map
    (kbd "C-w") 'evil-delete-backward-word)

  ;; Miscellaneous
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'makefile-mode-hook 'whitespace-mode)
  (remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)

  ;; Diminish
  (spacemacs|diminish hybrid-mode)
  (spacemacs|diminish which-key-mode)
  (with-eval-after-load 'emoji-cheat-sheet-plus
    (diminish 'emoji-cheat-sheet-plus-display-mode))
  (with-eval-after-load 'racer
    (diminish 'racer-mode))
  (with-eval-after-load 'command-log-mode
    (diminish 'command-log-mode))
  (unless (spacemacs/system-is-mswindows)
    (with-eval-after-load 'helm-gtags
      (diminish 'helm-gtags-mode)))

  ;; Disable smartparens highlighting
  (with-eval-after-load 'smartparens
    (show-smartparens-global-mode -1))

  ;; Semantic fucks up scrolling
  (with-eval-after-load 'semantic
    (setq semantic-submode-list (delq 'global-semantic-stickyfunc-mode semantic-submode-list)))

  ;; Some fixes for comint-style buffers
  (dolist (mode '(comint-mode
                  term-mode
                  eshell-mode
                  inferior-emacs-lisp-mode))
    (evil-set-initial-state mode 'normal))

  (let ((comint-hooks '(eshell-mode-hook
                        term-mode-hook
                        messages-buffer-mode-hook
                        comint-mode-hook)))
    (spacemacs/add-to-hooks (defun bb/no-hl-line-mode ()
                       (setq-local global-hl-line-mode nil))
                     comint-hooks))
  (add-hook 'inferior-emacs-lisp-mode-hook 'smartparens-mode)

  ;; IRC
  (spacemacs|define-custom-layout "@ERC"
    :binding "E"
    :body
    (erc-tls :server "irc.gitter.im" :port "6667" :nick "TheBB"
             :password bb/gitter-pwd :full-name bb/full-name)
    (erc :server "irc.freenode.net" :port "6667" :nick "TheBB" :full-name bb/full-name))

  ;; Evilification
  (with-eval-after-load 'magit
    (evil-define-key 'normal magit-mode-map (kbd "ESC") nil)
    (evil-define-key 'normal magit-mode-map (kbd "M-j") 'magit-section-forward-sibling)
    (evil-define-key 'normal magit-mode-map (kbd "M-k") 'magit-section-backward-sibling))
  (with-eval-after-load 'haskell-interactive-mode
    (evilified-state-evilify-map haskell-error-mode-map
      :mode haskell-error-mode))
  (with-eval-after-load 'proced
    (evilified-state-evilify-map proced-mode-map
      :mode proced-mode))

  ;; Safe local variables
  (put 'helm-make-build-dir 'safe-local-variable 'stringp)

  ;; Additional packages
  (use-package helm-flycheck
    :defer t
    :init
    (spacemacs/set-leader-keys "eh" 'helm-flycheck))
  ;; (use-package helm-fuzzier
  ;;   :defer t
  ;;   :commands helm-fuzzier-mode
  ;;   :init
  ;;   (with-eval-after-load 'helm
  ;;     (helm-fuzzier-mode 1)))
  (use-package nginx-mode
    :defer t
    :mode ("nginx\\.conf\\'" "/etc/nginx/.*\\'"))
  (use-package nameless
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'nameless-mode-from-hook)
      (spacemacs|add-toggle nameless
        :status nameless-mode
        :on (nameless-mode)
        :off (nameless-mode -1)
        :evil-leader-for-mode (emacs-lisp-mode . "o:"))))
  (when (configuration-layer/package-usedp 'unicode-fonts)
    (use-package unicode-fonts
      :config
      (unicode-fonts-setup)))
  (use-package warnings
    :defer t
    :config
    (push '(undo discard-info) warning-suppress-types))

  ;; Workarounds

  ;; https://github.com/EphramPerdition/helm-fuzzier/issues/9
  ;; (defun bb/helm-locate-library (orig-fn &rest args)
  ;;   (let ((fuzzy helm-fuzzier-mode))
  ;;     (prog2
  ;;         (when fuzzy (helm-fuzzier-mode -1))
  ;;         (apply orig-fn args)
  ;;       (when fuzzy (helm-fuzzier-mode 1)))))
  ;; (advice-add 'helm-locate-library :around 'bb/helm-locate-library)

  ;; Load local
  (when (file-exists-p "~/local.el")
    (load "~/local.el")))
