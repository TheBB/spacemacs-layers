(setq-default
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
    (header-line :box (:color "#555555" :line-width 1))
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
    (company-tooltip-annotation :foreground "#ff9eb8")
    (company-tooltip-annotation-selection :background "#66d9ef")
    (erc-timestamp-face
     :inherit font-lock-comment-face :foreground nil)
    (evil-search-highlight-persist-highlight-face
     :background "#fc5fef" :foreground "#000000")
    (helm-ff-prefix :background nil :foreground "#666666" :weight bold)
    (org-done :foreground "MediumSpringGreen")
    (region :background "#998f84")
    (spacemacs-transient-state-title-face :background nil :foreground nil :inherit font-lock-warning-face)
    (term :foreground nil :background nil))))

(configuration-layer/declare-layer '(theming :variables
                            theming-headings-inherit-from-default 'all
                            theming-headings-same-size 'all
                            theming-headings-bold 'all))
