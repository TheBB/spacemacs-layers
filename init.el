;;; init.el --- Spacemacs configuration file
;;
;; Author: Eivind Fonn
;;
;; This file is not part of GNU Emacs.

(defun dotspacemacs/layers ()
  (setq-default

   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/")
   dotspacemacs-download-packages 'used

   dotspacemacs-configuration-layers
   '(asm
     (auto-completion
      :variables
      auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior 'cycle
      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
      :disabled-for org erc)
     bibtex
     (c-c++ :variables c-c++-enable-clang-support t)
     clojure
     command-log
     csharp
     dash
     django
     elfeed
     emacs-lisp
     emoji
     erc
     ess
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     extra-langs
     fasd
     games
     git
     github
     haskell
     html
     (ibuffer :variables ibuffer-group-buffers-by nil)
     ipython-notebook
     javascript
     latex
     lua
     markdown
     org
     (python :variables python-test-runner 'pytest)
     ranger
     react
     ruby
     rust
     semantic
     (shell :variables shell-default-shell 'eshell)
     shell-scripts
     (spell-checking :variables spell-checking-enable-by-default nil)
     spotify
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     typography
     (version-control :variables version-control-diff-tool 'diff-hl)
     vimscript
     yaml

     ;; Non-contrib layers
     encoding
     evil-little-word
     no-dots
     operators

     ;; Personal config layers
     bb-c
     bb-erc
     bb-ibuffer
     bb-git
     bb-keys
     bb-latex
     bb-org
     bb-theming
     bb-web)

   dotspacemacs-additional-packages
   '(cuda-mode
     defproject
     editorconfig
     helm-flycheck
     kivy-mode
     nameless
     nginx-mode
     powerline
     (spaceline :location "~/repos/spaceline/"))

   dotspacemacs-excluded-packages
   '(clj-refactor
     elfeed-org
     ido
     julia-mode
     vi-tilde-fringe)))

(defun dotspacemacs/layers/SINTEFPC6985 ()
  (bb/remove-elts-or-cars 'dotspacemacs-configuration-layers
    '(dash fasd spell-checking spotify))
  (bb/remove-elts-or-cars 'dotspacemacs-additional-packages
    '(powerline spaceline)))

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
     :size ,(if (spacemacs/system-is-mswindows) 16 13)
     :weight demibold :width normal :powerline-scale 1.15)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global t
   dotspacemacs-default-layout-name "Home"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 10
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
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
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
   magit-gh-pulls-pull-detail-limit 200

   ;; Flycheck
   flycheck-check-syntax-automatically '(save mode-enabled)

   ;; Avy
   avy-all-windows 'all-frames

   ;; Ranger
   ranger-override-dired t

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
     ("et" . "evil-targets")
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

   ;; IRC
   erc-server-list
   `(("irc.gitter.im" :port "6667" :nick "TheBB" :full-name ,bb/full-name
      :ssl t :password ,bb/gitter-pwd)
     ("irc.freenode.net" :port "6667" :nick "TheBB" :full-name ,bb/full-name))
   erc-autojoin-channels-alist
   '(("1\\.0\\.0" "#syl20bnr/spacemacs" "#syl20bnr/spacemacs-devel") ; Gitter
     ("irc.gitter.im" "#syl20bnr/spacemacs" "#syl20bnr/spacemacs-devel")
     ("freenode\\.net" "#emacs" "#emacs-beginners" "#spacemacs" "#evil-mode"))))

(defun dotspacemacs/user-config ()

  ;; Settings
  (setq-default
   tab-width 8
   evil-move-beyond-eol nil
   helm-echo-input-in-header-line nil)

  ;; Spaceline
  (setq powerline-default-separator 'arrow
        spaceline-buffer-encoding-abbrev-p nil
        spaceline-version-control-p nil)

  ;; Filenames
  (dolist (e '(("xml" . web-mode)
               ("xinp" . web-mode)
               ("C" . c++-mode)
               ("h" . c++-mode)
               ("service" . conf-mode)
               ("timer" . conf-mode)))
    (push (cons (concat "\\." (car e) "\\'") (cdr e)) auto-mode-alist))
  (dolist (e '(("PKGBUILD" . shell-script-mode)
               ("conky.conf" . lua-mode)))
    (push e auto-mode-alist))
  (with-eval-after-load 'projectile
    (push '("C" "h") projectile-other-file-alist))

  ;; Miscellaneous
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'text-mode-hook 'typo-mode)
  (add-hook 'makefile-mode-hook 'whitespace-mode)
  (add-hook 'prog-mode-hook 'page-break-lines-mode)
  (add-hook 'after-make-frame-functions
            (defun bb/delayed-redraw (frame)
              (run-with-timer 0.2 nil 'redraw-frame frame)))
  (remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)

  ;; Override diff-hl terminal settings in daemon mode
  (setq diff-hl-side 'right)
  (diff-hl-margin-mode -1)

  ;; Evil MC
  (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-mc-mode)
  (add-hook 'evil-mc-after-cursors-deleted
            (defun bb/clear-anzu () (interactive) (setq anzu--state nil)))

  ;; Semantic
  (with-eval-after-load 'semantic
    (setq semantic-default-submodes
          (remove 'global-semantic-stickyfunc-mode semantic-default-submodes)))

  ;; Diminish
  (spacemacs|diminish hybrid-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish evil-mc-mode)
  (with-eval-after-load 'emoji-cheat-sheet-plus
    (diminish 'emoji-cheat-sheet-plus-display-mode))
  (with-eval-after-load 'racer
    (diminish 'racer-mode))
  (with-eval-after-load 'command-log-mode
    (diminish 'command-log-mode))

  ;; Disable smartparens highlighting
  (with-eval-after-load 'smartparens
    (show-smartparens-global-mode -1))

  ;; Thanks StreakyCobra
  (evil-set-initial-state 'term-mode 'emacs)
  (push 'term-mode evil-escape-excluded-major-modes)
  (evil-define-key 'emacs term-raw-map (kbd "C-c") 'term-send-raw)

  (add-hook 'inferior-emacs-lisp-mode-hook 'smartparens-mode)

  ;; Evilification
  (with-eval-after-load 'haskell-interactive-mode
    (evilified-state-evilify-map haskell-error-mode-map
      :mode haskell-error-mode))
  (with-eval-after-load 'proced
    (evilified-state-evilify-map proced-mode-map
      :mode proced-mode))

  ;; Experimenting with transparency
  (let ((tp (cons dotspacemacs-active-transparency dotspacemacs-inactive-transparency)))
    (push `(alpha . ,tp) default-frame-alist)
    (set-frame-parameter (selected-frame) 'alpha tp))

  ;; Safe local variables
  (put 'helm-make-build-dir 'safe-local-variable 'stringp)

  ;; Additional packages
  (add-hook 'cuda-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  (use-package helm-flycheck
    :defer t
    :init
    (spacemacs/set-leader-keys "ee" 'helm-flycheck))
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
        :documentation "Nameless mode."
        :evil-leader-for-mode (emacs-lisp-mode . "o:"))))
  (use-package warnings
    :defer t
    :config
    (push '(undo discard-info) warning-suppress-types))
  (use-package editorconfig
    :config (editorconfig-mode 1))
  (use-package kivy-mode
    :defer t
    :init
    (push '(kivy-mode . kivy-indent-offset) spacemacs--indent-variable-alist))
  (use-package defproject
    :commands defproject))

(defun dotspacemacs/user-config/eivindf-sintef ()
  (defproject IFEM-PoroElasticity
    :path "~/work/IFEM/Apps/PoroElasticity"
    :nil
    ((helm-make-build-dir . "bld-sd"))))

(defun bb/remove-in-place (var pred)
  (set var (remove-if pred (symbol-value var))))

(defun bb/remove-elts-or-cars (var elts)
  (declare (indent 1))
  (bb/remove-in-place var (lambda (e)
                            (or (memq e elts)
                                (and (listp e) (memq (car e) elts))))))

(defmacro bb|wrap-func (func)
  (let ((advice-name (intern (format "%s--advice" func)))
        (target-name (intern (format "%s/%s" func system-name))))
    `(progn
       (defun ,advice-name (&rest args)
         (when (fboundp ',target-name)
           (apply ',target-name args)))
       (advice-add ',func :after ',advice-name))))

(bb|wrap-func dotspacemacs/layers)
(bb|wrap-func dotspacemacs/init)
(bb|wrap-func dotspacemacs/user-init)
(bb|wrap-func dotspacemacs/user-config)

(when (file-exists-p "~/local.el")
  (load "~/local.el"))
