;; base16-theme.el -- base16 themes for emacs

;; Author: Kaleb Elwert <belak@coded.io>
;;         Neil Bhakta
;; Maintainer: Kaleb Elwert <belak@coded.io>
;; Version: 1.1
;; Homepage: https://github.com/belak/base16-emacs

;;; Commentary:
;; base16-theme is a collection of themes built around the base16
;; concept (https://github.com/chriskempson/base16).  All themes are
;; generated from the official set of color schemes and the templates
;; which are included in this repo.

;;; Code:

(defvar base16-shell-colors
  '(:base00 "black"
    :base01 "brightgreen"
    :base02 "brightyellow"
    :base03 "brightblack"
    :base04 "brightblue"
    :base05 "white"
    :base06 "brightmagenta"
    :base07 "brightwhite"
    :base08 "red"
    :base09 "brightred"
    :base0A "yellow"
    :base0B "green"
    :base0C "cyan"
    :base0D "blue"
    :base0E "magenta"
    :base0F "brightcyan")
  "Base16 colors used when in a terminal.

These mappings are based on the xresources themes.  If you're
using a different terminal color scheme, you may want to look for
an alternate theme for use in the terminal.")

(defun base16-transform-spec (spec colors)
  "Transform a theme `SPEC' into a face spec using `COLORS'."
  (let ((output))
    (while spec
      (let* ((key       (car  spec))
             (value     (cadr spec))
             (color-key (if (symbolp value) (intern (concat ":" (symbol-name value))) nil))
             (color     (plist-get colors color-key)))

        ;; Append the transformed element
        (cond
         ((and (memq key '(:box :underline)) (listp value))
          (setq output (append output (list key (base16-transform-spec value colors)))))
         (color
          (setq output (append output (list key color))))
         (t
          (setq output (append output (list key value))))))

      ;; Go to the next element in the list
      (setq spec (cddr spec)))

    ;; Return the transformed spec
    output))

(defun base16-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face       (car spec))
         (definition (cdr spec)))

    (list face `((((min-colors 256)) ,(base16-transform-spec definition colors))
                 (t                  ,(base16-transform-spec definition base16-shell-colors))))))

(defun base16-set-faces (theme-name colors faces)
  "Define the important part of `THEME-NAME' using `COLORS' to map the `FACES' to actual colors."
  (apply 'custom-theme-set-faces theme-name
         (mapcar #'(lambda (face)
                     (base16-transform-face face colors))
                 faces)))

(defun base16-theme-define (theme-name theme-colors)
  "Define the faces for a base16 colorscheme given a `THEME-NAME' and a plist of `THEME-COLORS'."
  (base16-set-faces
   theme-name
   theme-colors

   '(;; Built in stuff (Emacs 23)
     (border                                       :background base03)
     (border-glyph)
     (cursor                                       :background base08)
     (default                                      :foreground base05 :background base00)
     (fringe                                       :background base02)
     (gui-element                                  :background base01)
     (highlight                                    :background base01)
     (link                                         :foreground base0D)
     (link-visited                                 :foreground base0E)
     (minibuffer-prompt                            :foreground base0D)
     (mode-line                                    :foreground base04 :background base02 :box nil)
     (mode-line-buffer-id                          :foreground base0B :background nil)
     (mode-line-emphasis                           :foreground base06 :slant italic)
     (mode-line-highlight                          :foreground base0E :box nil :weight bold)
     (mode-line-inactive                           :foreground base03 :background base01 :box nil)
     (region                                       :background base02)
     (secondary-selection                          :background base03)
     (error                                        :foreground base08 :weight bold)
     (warning                                      :foreground base09 :weight bold)
     (success                                      :foreground base0B :weight bold)

     (header-line                                  :foreground base0E :background nil :inherit mode-line)

     ;; Font-lock stuff
     (font-lock-builtin-face                       :foreground base0C)
     (font-lock-comment-delimiter-face             :foreground base02)
     (font-lock-comment-face                       :foreground base03)
     (font-lock-constant-face                      :foreground base09)
     (font-lock-doc-face                           :foreground base04)
     (font-lock-doc-string-face                    :foreground base03)
     (font-lock-function-name-face                 :foreground base0D)
     (font-lock-keyword-face                       :foreground base0E)
     (font-lock-negation-char-face                 :foreground base0B)
     (font-lock-preprocessor-face                  :foreground base0D)
     (font-lock-regexp-grouping-backslash          :foreground base0A)
     (font-lock-regexp-grouping-construct          :foreground base0E)
     (font-lock-string-face                        :foreground base0B)
     (font-lock-type-face                          :foreground base0A)
     (font-lock-variable-name-face                 :foreground base08)
     (font-lock-warning-face                       :foreground base08)

     ;; linum-mode
     (linum                                        :foreground base03 :background base01)

     ;; Search
     (match                                        :foreground base0D :background base01 :inverse-video t)
     (isearch                                      :foreground base0A :background base01 :inverse-video t)
     (isearch-lazy-highlight-face                  :foreground base0C :background base01 :inverse-video t)
     (isearch-fail                                 :background base01 :inverse-video t :inherit font-lock-warning-face)
     (anzu-mode-line                               :foreground base0E)
     (evil-search-highlight-persist-highlight-face :background base01 :inverse-video t :inherit font-lock-warning-face)
     (go-guru-hl-identifier-face                   :background base02)

     ;; Popups
     (popup-face                                   :foreground base05 :background base02)
     (popup-isearch-match                          :foreground base00 :background base0B)
     (popup-scroll-bar-background-face             :background base03)
     (popup-scroll-bar-foreground-face             :background base05)
     (popup-summary-face                           :foreground base04)
     (popup-tip-face                               :foreground base00 :background base0A)
     (popup-menu-mouse-face                        :foreground base00 :background base0D)
     (popup-menu-selection-face                    :foreground base00 :background base0C)

     ;; company-mode
     (company-tooltip                              :background base01 :inherit default)
     (company-scrollbar-bg                         :background base07)
     (company-scrollbar-fg                         :background base04)
     (company-tooltip-annotation                   :foreground base08)
     (company-tooltip-common                       :inherit font-lock-constant-face)
     (company-tooltip-selection                    :background base02 :inherit font-lock-function-name-face)
     (company-preview-common                       :inherit secondary-selection)

     ;; eshell
     (eshell-ls-archive                            :foreground base08)
     (eshell-ls-backup                             :foreground base0F)
     (eshell-ls-clutter                            :foreground base09)
     (eshell-ls-directory                          :foreground base0D)
     (eshell-ls-executable                         :foreground base0B)
     (eshell-ls-missing                            :foreground base08)
     (eshell-ls-product                            :foreground base0F)
     (eshell-ls-readonly                           :foreground base06)
     (eshell-ls-special                            :foreground base0E)
     (eshell-ls-symlink                            :foreground base0C)
     (eshell-ls-unreadable                         :foreground base04)
     (eshell-prompt                                :foreground base05)

     ;; ivy-mode
     (ivy-current-match                            :foreground base09 :background base01)
     (ivy-minibuffer-match-face-1                  :foreground base0E)
     (ivy-minibuffer-match-face-2                  :foreground base0D)
     (ivy-minibuffer-match-face-3                  :foreground base0C)
     (ivy-minibuffer-match-face-4                  :foreground base0B)
     (ivy-confirm-face                             :foreground base0B)
     (ivy-match-required-face                      :foreground base08)
     (ivy-virtual                                  :foreground base04)
     (ivy-action                                   :foreground base0D)

     ;; Flycheck
     (flycheck-error                               :underline (:style wave :color base08))
     (flycheck-info                                :underline (:style wave :color base0B))
     (flycheck-warning                             :underline (:style wave :color base09))

     ;; Flymake
     (flymake-warnline                             :background base01 :underline base09)
     (flymake-errline                              :background base01 :underline base08)

     ;; Flyspell
     (flyspell-duplicate                           :underline (:style wave :color base09))
     (flyspell-incorrect                           :underline (:style wave :color base08))

     ;; Clojure errors
     (clojure-test-failure-face                    :background nil :inherit flymake-warnline)
     (clojure-test-error-face                      :background nil :inherit flymake-errline)
     (clojure-test-success-face                    :foreground nil :background nil :underline base0B)

     ;; clojure-mode
     (clojure-keyword                              :foreground base0A)
     (clojure-parens                               :foreground base06)
     (clojure-braces                               :foreground base0B)
     (clojure-brackets                             :foreground base0A)
     (clojure-double-quote                         :foreground base0C :background nil)
     (clojure-special                              :foreground base0D)
     (clojure-java-call                            :foreground base0E)

     ;; MMM-mode
     (mmm-code-submode-face                        :background base03)
     (mmm-comment-submode-face                     :inherit font-lock-comment-face)
     (mmm-output-submode-face                      :background base03)

     ;; rainbow-delimiters
     (rainbow-delimiters-depth-1-face              :foreground base0E)
     (rainbow-delimiters-depth-2-face              :foreground base0D)
     (rainbow-delimiters-depth-3-face              :foreground base0C)
     (rainbow-delimiters-depth-4-face              :foreground base0B)
     (rainbow-delimiters-depth-5-face              :foreground base0A)
     (rainbow-delimiters-depth-6-face              :foreground base09)
     (rainbow-delimiters-depth-7-face              :foreground base08)
     (rainbow-delimiters-depth-8-face              :foreground base03)
     (rainbow-delimiters-depth-9-face              :foreground base05)

     ;; spaceline
     (spaceline-evil-emacs                         :foreground base01 :background base0D)
     (spaceline-evil-insert                        :foreground base01 :background base0D)
     (spaceline-evil-motion                        :foreground base01 :background base0E)
     (spaceline-evil-normal                        :foreground base01 :background base0B)
     (spaceline-evil-replace                       :foreground base01 :background base08)
     (spaceline-evil-visual                        :foreground base01 :background base09)

     ;; IDO
     (ido-subdir                                   :foreground base04)
     (ido-first-match                              :foreground base09 :weight bold)
     (ido-only-match                               :foreground base08 :weight bold)
     (ido-indicator                                :foreground base08 :background base01)
     (ido-virtual                                  :foreground base04)

     ;; which-function
     (which-func                                   :foreground base0D :background nil :weight bold)

     (trailing-whitespace                          :foreground base0A :background base0C)
     (whitespace-empty                             :foreground base08 :background base0A)
     (whitespace-hspace                            :foreground base04 :background base04)
     (whitespace-indentation                       :foreground base08 :background base0A)
     (whitespace-line                              :foreground base0F :background base01)
     (whitespace-newline                           :foreground base04)
     (whitespace-space                             :foreground base04 :background base01)
     (whitespace-space-after-tab                   :foreground base08 :background base0A)
     (whitespace-space-before-tab                  :foreground base08 :background base09)
     (whitespace-tab                               :foreground base04 :background base04)
     (whitespace-trailing                          :foreground base0A :background base08)

     ;; Parenthesis matching (built-in)
     (show-paren-match                             :foreground base03 :background base0D)
     (show-paren-mismatch                          :foreground base03 :background base09)

     ;; Parenthesis matching (mic-paren)
     (paren-face-match                             :foreground nil :background nil :inherit show-paren-match)
     (paren-face-mismatch                          :foreground nil :background nil :inherit show-paren-mismatch)
     (paren-face-no-match                          :foreground nil :background nil :inherit show-paren-mismatch)

     ;; Parenthesis dimming (parenface)
     (paren-face                                   :foreground base04 :background nil)

     (sh-heredoc                                   :foreground nil :weight normal :inherit font-lock-string-face)
     (sh-quoted-exec                               :foreground nil :inherit font-lock-preprocessor-face)
     (slime-highlight-edits-face                   :weight bold)
     (slime-repl-input-face                        :weight normal :underline nil)
     (slime-repl-prompt-face                       :foreground base0E :underline nil :weight bold)
     (slime-repl-result-face                       :foreground base0B)
     (slime-repl-output-face                       :foreground base0D :background base01)

     (csv-separator-face                           :foreground base09)

     (diff-added                                   :foreground base0B)
     (diff-changed                                 :foreground base0E)
     (diff-removed                                 :foreground base08)
     (diff-header                                  :background base01)
     (diff-file-header                             :background base02)
     (diff-hunk-header                             :foreground base0E :background base01)

     (diff-hl-change                               :foreground base0E)
     (diff-hl-delete                               :foreground base08)
     (diff-hl-insert                               :foreground base0B)

     (ediff-even-diff-A                            :foreground nil :background nil :inverse-video t)
     (ediff-even-diff-B                            :foreground nil :background nil :inverse-video t)
     (ediff-odd-diff-A                             :foreground base04 :background nil :inverse-video t)
     (ediff-odd-diff-B                             :foreground base04 :background nil :inverse-video t)

     (eldoc-highlight-function-argument            :foreground base0B :weight bold)

     ;; undo-tree
     (undo-tree-visualizer-default-face            :foreground base06)
     (undo-tree-visualizer-current-face            :foreground base0B :weight bold)
     (undo-tree-visualizer-active-branch-face      :foreground base08)
     (undo-tree-visualizer-register-face           :foreground base0A)

     ;; auctex
     (font-latex-bold-face                         :foreground base0B)
     (font-latex-doctex-documentation-face         :background base03)
     (font-latex-italic-face                       :foreground base0B)
     (font-latex-math-face                         :foreground base09)
     (font-latex-sectioning-0-face                 :foreground base0A)
     (font-latex-sectioning-1-face                 :foreground base0A)
     (font-latex-sectioning-2-face                 :foreground base0A)
     (font-latex-sectioning-3-face                 :foreground base0A)
     (font-latex-sectioning-4-face                 :foreground base0A)
     (font-latex-sectioning-5-face                 :foreground base0A)
     (font-latex-sedate-face                       :foreground base0C)
     (font-latex-string-face                       :foreground base0A)
     (font-latex-verbatim-face                     :foreground base09)
     (font-latex-warning-face                      :foreground base08)

     ;; dired+
     (diredp-compressed-file-suffix                :foreground base0D)
     (diredp-dir-heading                           :foreground nil :background nil :inherit heading)
     (diredp-dir-priv                              :foreground base0C :background nil)
     (diredp-exec-priv                             :foreground base0D :background nil)
     (diredp-executable-tag                        :foreground base08 :background nil)
     (diredp-file-name                             :foreground base0A)
     (diredp-file-suffix                           :foreground base0B)
     (diredp-flag-mark-line                        :background nil :inherit highlight)
     (diredp-ignored-file-name                     :foreground base04)
     (diredp-link-priv                             :foreground base0E :background nil)
     (diredp-mode-line-flagged                     :foreground base08)
     (diredp-mode-line-marked                      :foreground base0B)
     (diredp-no-priv                               :background nil)
     (diredp-number                                :foreground base0A)
     (diredp-other-priv                            :foreground base0E :background nil)
     (diredp-rare-priv                             :foreground base08 :background nil)
     (diredp-read-priv                             :foreground base0B :background nil)
     (diredp-symlink                               :foreground base0E)
     (diredp-write-priv                            :foreground base0A :background nil)

     ;; term and ansi-term
     (term-color-black                             :foreground base02 :background base00)
     (term-color-white                             :foreground base05 :background base07)
     (term-color-red                               :foreground base08 :background base08)
     (term-color-yellow                            :foreground base0A :background base0A)
     (term-color-green                             :foreground base0B :background base0B)
     (term-color-cyan                              :foreground base0C :background base0C)
     (term-color-blue                              :foreground base0D :background base0D)
     (term-color-magenta                           :foreground base0E :background base0E)

     (link                                         :foreground nil :underline t)
     (widget-button                                :underline t)
     (widget-field                                 :background base03 :box (:line-width 1 :color base06))

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     (compilation-column-number                    :foreground base0A)
     (compilation-line-number                      :foreground base0A)
     (compilation-message-face                     :foreground base0D)
     (compilation-mode-line-exit                   :foreground base0B)
     (compilation-mode-line-fail                   :foreground base08)
     (compilation-mode-line-run                    :foreground base0D)

     ;; Grep
     (grep-context-face                            :foreground base04)
     (grep-error-face                              :foreground base08 :weight bold :underline t)
     (grep-hit-face                                :foreground base0D)
     (grep-match-face                              :foreground nil :background nil :inherit match)

     (regex-tool-matched-face                      :foreground nil :background nil :inherit match)

     ;; Cscope
     (cscope-file-face                             :foreground base0B)
     (cscope-function-face                         :foreground base0D)
     (cscope-line-number-face                      :foreground base0A)
     (cscope-mouse-face                            :foreground base04 :background base01)
     (cscope-separator-face                        :foreground base08 :overline t :underline t :weight bold)

     ;; mark-multiple
     (mm/master-face                               :foreground nil :background nil :inherit region)
     (mm/mirror-face                               :foreground nil :background nil :inherit region)

     ;; org-mode
     (org-agenda-structure                         :foreground base0E)
     (org-agenda-date                              :foreground base0D :underline nil)
     (org-agenda-done                              :foreground base0B)
     (org-agenda-dimmed-todo-face                  :foreground base04)
     (org-block                                    :foreground base09)
     (org-code                                     :foreground base0A)
     (org-column                                   :background base01)
     (org-column-title                             :weight bold :underline t :inherit org-column)
     (org-date                                     :foreground base0E :underline t)
     (org-document-info                            :foreground base0C)
     (org-document-info-keyword                    :foreground base0B)
     (org-document-title                           :foreground base09 :weight bold :height 1.44)
     (org-done                                     :foreground base0B)
     (org-ellipsis                                 :foreground base04)
     (org-footnote                                 :foreground base0C)
     (org-formula                                  :foreground base08)
     (org-hide                                     :foreground base03)
     (org-link                                     :foreground base0D)
     (org-scheduled                                :foreground base0B)
     (org-scheduled-previously                     :foreground base09)
     (org-scheduled-today                          :foreground base0B)
     (org-special-keyword                          :foreground base09)
     (org-table                                    :foreground base0E)
     (org-todo                                     :foreground base08)
     (org-upcoming-deadline                        :foreground base09)
     (org-warning                                  :foreground base08 :weight bold)

     (markdown-url-face                            :inherit link)
     (markdown-link-face                           :foreground base0D :underline t)

     (hl-sexp-face                                 :background base03)
     (highlight-80+                                :background base03)

     ;; Python-specific overrides
     (py-builtins-face                             :foreground base09 :weight normal)

     ;; js2-mode
     (js2-warning-face                             :underline base09)
     (js2-error-face                               :foreground nil :underline base08)
     (js2-external-variable-face                   :foreground base0E)
     (js2-function-param-face                      :foreground base0D)
     (js2-instance-member-face                     :foreground base0D)
     (js2-private-function-call-face               :foreground base08)

     ;; js3-mode
     (js3-warning-face                             :underline base09)
     (js3-error-face                               :foreground nil :underline base08)
     (js3-external-variable-face                   :foreground base0E)
     (js3-function-param-face                      :foreground base0D)
     (js3-jsdoc-tag-face                           :foreground base09)
     (js3-jsdoc-type-face                          :foreground base0C)
     (js3-jsdoc-value-face                         :foreground base0A)
     (js3-jsdoc-html-tag-name-face                 :foreground base0D)
     (js3-jsdoc-html-tag-delimiter-face            :foreground base0B)
     (js3-instance-member-face                     :foreground base0D)
     (js3-private-function-call-face               :foreground base08)

     ;; nxml
     (nxml-name-face                               :foreground unspecified :inherit font-lock-constant-face)
     (nxml-attribute-local-name-face               :foreground unspecified :inherit font-lock-variable-name-face)
     (nxml-ref-face                                :foreground unspecified :inherit font-lock-preprocessor-face)
     (nxml-delimiter-face                          :foreground unspecified :inherit font-lock-keyword-face)
     (nxml-delimited-data-face                     :foreground unspecified :inherit font-lock-string-face)
     (rng-error-face                               :underline base08)

     ;; RHTML
     (erb-delim-face                               :background base03)
     (erb-exec-face                                :background base03 :weight bold)
     (erb-exec-delim-face                          :background base03)
     (erb-out-face                                 :background base03 :weight bold)
     (erb-out-delim-face                           :background base03)
     (erb-comment-face                             :background base03 :weight bold :slant italic)
     (erb-comment-delim-face                       :background base03)

     ;; Message-mode
     (message-header-other                         :foreground nil :background nil :weight normal)
     (message-header-subject                       :foreground base0A :weight bold :inherit message-header-other)
     (message-header-to                            :foreground base09 :weight bold :inherit message-header-other)
     (message-header-cc                            :foreground nil :inherit message-header-to)
     (message-header-name                          :foreground base0D :background nil)
     (message-header-newsgroups                    :foreground base0C :background nil :slant normal)
     (message-separator                            :foreground base0E)

     ;; Jabber
     (jabber-chat-prompt-local                     :foreground base0A)
     (jabber-chat-prompt-foreign                   :foreground base09)
     (jabber-chat-prompt-system                    :foreground base0A :weight bold)
     (jabber-chat-text-local                       :foreground base0A)
     (jabber-chat-text-foreign                     :foreground base09)
     (jabber-chat-text-error                       :foreground base08)

     (jabber-roster-user-online                    :foreground base0B)
     (jabber-roster-user-xa                        :foreground base04)
     (jabber-roster-user-dnd                       :foreground base0A)
     (jabber-roster-user-away                      :foreground base09)
     (jabber-roster-user-chatty                    :foreground base0E)
     (jabber-roster-user-error                     :foreground base08)
     (jabber-roster-user-offline                   :foreground base04)

     (jabber-rare-time-face                        :foreground base04)
     (jabber-activity-face                         :foreground base0E)
     (jabber-activity-personal-face                :foreground base0C)

     ;; Gnus
     (gnus-cite-1                                  :foreground nil :inherit outline-1)
     (gnus-cite-2                                  :foreground nil :inherit outline-2)
     (gnus-cite-3                                  :foreground nil :inherit outline-3)
     (gnus-cite-4                                  :foreground nil :inherit outline-4)
     (gnus-cite-5                                  :foreground nil :inherit outline-5)
     (gnus-cite-6                                  :foreground nil :inherit outline-6)
     (gnus-cite-7                                  :foreground nil :inherit outline-7)
     (gnus-cite-8                                  :foreground nil :inherit outline-8)
     ;; there are several more -cite- faces...
     (gnus-header-content                          :inherit message-header-other)
     (gnus-header-subject                          :inherit message-header-subject)
     (gnus-header-from                             :foreground base09 :weight bold :inherit message-header-other-face)
     (gnus-header-name                             :inherit message-header-name)
     (gnus-button                                  :foreground nil :inherit link)
     (gnus-signature                               :inherit font-lock-comment-face)

     (gnus-summary-normal-unread                   :foreground base0D :weight normal)
     (gnus-summary-normal-read                     :foreground base06 :weight normal)
     (gnus-summary-normal-ancient                  :foreground base0C :weight normal)
     (gnus-summary-normal-ticked                   :foreground base09 :weight normal)
     (gnus-summary-low-unread                      :foreground base04 :weight normal)
     (gnus-summary-low-read                        :foreground base04 :weight normal)
     (gnus-summary-low-ancient                     :foreground base04 :weight normal)
     (gnus-summary-high-unread                     :foreground base0A :weight normal)
     (gnus-summary-high-read                       :foreground base0B :weight normal)
     (gnus-summary-high-ancient                    :foreground base0B :weight normal)
     (gnus-summary-high-ticked                     :foreground base09 :weight normal)
     (gnus-summary-cancelled                       :foreground base08 :background nil :weight normal)

     (gnus-group-mail-low                          :foreground base04)
     (gnus-group-mail-low-empty                    :foreground base04)
     (gnus-group-mail-1                            :foreground nil :weight normal :inherit outline-1)
     (gnus-group-mail-2                            :foreground nil :weight normal :inherit outline-2)
     (gnus-group-mail-3                            :foreground nil :weight normal :inherit outline-3)
     (gnus-group-mail-4                            :foreground nil :weight normal :inherit outline-4)
     (gnus-group-mail-5                            :foreground nil :weight normal :inherit outline-5)
     (gnus-group-mail-6                            :foreground nil :weight normal :inherit outline-6)
     (gnus-group-mail-1-empty                      :foreground base04 :inherit gnus-group-mail-1)
     (gnus-group-mail-2-empty                      :foreground base04 :inherit gnus-group-mail-2)
     (gnus-group-mail-3-empty                      :foreground base04 :inherit gnus-group-mail-3)
     (gnus-group-mail-4-empty                      :foreground base04 :inherit gnus-group-mail-4)
     (gnus-group-mail-5-empty                      :foreground base04 :inherit gnus-group-mail-5)
     (gnus-group-mail-6-empty                      :foreground base04 :inherit gnus-group-mail-6)
     (gnus-group-news-1                            :foreground nil :weight normal :inherit outline-5)
     (gnus-group-news-2                            :foreground nil :weight normal :inherit outline-6)
     (gnus-group-news-3                            :foreground nil :weight normal :inherit outline-7)
     (gnus-group-news-4                            :foreground nil :weight normal :inherit outline-8)
     (gnus-group-news-5                            :foreground nil :weight normal :inherit outline-1)
     (gnus-group-news-6                            :foreground nil :weight normal :inherit outline-2)
     (gnus-group-news-1-empty                      :foreground base04 :inherit gnus-group-news-1)
     (gnus-group-news-2-empty                      :foreground base04 :inherit gnus-group-news-2)
     (gnus-group-news-3-empty                      :foreground base04 :inherit gnus-group-news-3)
     (gnus-group-news-4-empty                      :foreground base04 :inherit gnus-group-news-4)
     (gnus-group-news-5-empty                      :foreground base04 :inherit gnus-group-news-5)
     (gnus-group-news-6-empty                      :foreground base04 :inherit gnus-group-news-6)

     (erc-direct-msg-face                          :foreground base09)
     (erc-error-face                               :foreground base08)
     (erc-header-face                              :foreground base06 :background base04)
     (erc-input-face                               :foreground base0B)
     (erc-keyword-face                             :foreground base0A)
     (erc-current-nick-face                        :foreground base0B)
     (erc-my-nick-face                             :foreground base0B)
     (erc-nick-default-face                        :foreground base0E :weight normal)
     (erc-nick-msg-face                            :foreground base0A :weight normal)
     (erc-notice-face                              :foreground base04)
     (erc-pal-face                                 :foreground base09)
     (erc-prompt-face                              :foreground base0D)
     (erc-timestamp-face                           :foreground base0C)

     ;; helm
     (helm-M-x-key                                 :foreground base0C)
     (helm-action                                  :foreground base05)
     (helm-buffer-directory                        :foreground base04 :background nil :weight bold)
     (helm-buffer-file                             :foreground base0C)
     (helm-buffer-not-saved                        :foreground base08)
     (helm-buffer-process                          :foreground base03)
     (helm-buffer-saved-out                        :foreground base0F)
     (helm-buffer-size                             :foreground base09)
     (helm-candidate-number                        :foreground base00 :background base09)
     (helm-ff-directory                            :foreground base04 :background nil :weight bold)
     (helm-ff-executable                           :foreground base0B)
     (helm-ff-file                                 :foreground base0C)
     (helm-ff-invalid-symlink                      :foreground base00 :background base08)
     (helm-ff-prefix                               :foreground nil :background nil)
     (helm-ff-symlink                              :foreground base00 :background base0C)
     (helm-grep-cmd-line                           :foreground base0B)
     (helm-grep-file                               :foreground base0C)
     (helm-grep-finish                             :foreground base00 :background base09)
     (helm-grep-lineno                             :foreground base03)
     (helm-grep-match                              :foreground base0A)
     (helm-grep-running                            :foreground base09)
     (helm-header                                  :foreground base0A :background base00 :underline nil)
     (helm-match                                   :foreground base0A)
     (helm-moccur-buffer                           :foreground base0C)
     (helm-selection                               :foreground nil :background base02 :underline nil)
     (helm-selection-line                          :foreground nil :background base02)
     (helm-separator                               :foreground base02)
     (helm-source-header                           :foreground base05 :background base01 :weight bold)
     (helm-visible-mark                            :foreground base00 :background base0B)

     ;; powerline
     (powerline-active1                            :foreground base09 :background base00)
     (powerline-active2                            :foreground base08 :background base01)
     (powerline-inactive1                          :foreground base06 :background base01)
     (powerline-inactive2                          :foreground base07 :background base02)

     (custom-variable-tag                          :foreground base0D)
     (custom-group-tag                             :foreground base0D)
     (custom-state                                 :foreground base0B)))

  ;; Anything leftover that doesn't fall neatly into a face goes here.
  (let ((base00 (plist-get theme-colors :base00))
        (base01 (plist-get theme-colors :base01))
        (base02 (plist-get theme-colors :base02))
        (base03 (plist-get theme-colors :base03))
        (base04 (plist-get theme-colors :base04))
        (base05 (plist-get theme-colors :base05))
        (base06 (plist-get theme-colors :base06))
        (base07 (plist-get theme-colors :base07))
        (base08 (plist-get theme-colors :base08))
        (base09 (plist-get theme-colors :base09))
        (base0A (plist-get theme-colors :base0A))
        (base0B (plist-get theme-colors :base0B))
        (base0C (plist-get theme-colors :base0C))
        (base0D (plist-get theme-colors :base0D))
        (base0E (plist-get theme-colors :base0E))
        (base0F (plist-get theme-colors :base0F)))
    (custom-theme-set-variables
     theme-name
     `(ansi-color-names-vector
       ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
       [,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05])
     `(ansi-term-color-vector
       ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
       [unspecified ,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05]))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'base16-theme)

;;; base16-theme.el ends here
