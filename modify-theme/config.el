(defvar modify-theme-modifications '()
  "An alist of theme modifications. Each element should
be on the form (THEME . SPEC), where THEME is a symbol
representing a theme, and SPEC is an alist mapping faces
to face specs (see `defface').")

(defvar modify-theme-headings-inherit-from-default '()
  "A list of themes where all headings should inherit
from the default face, or the symbol `all'.")

(defvar modify-theme-headings-same-size '()
  "A list of themes where all headings should have the
same size, or the symbol `all'.")

(defvar modify-theme-headings-bold '()
  "A list of themes where all headings should be bold,
or the symbol `all'.")

(defvar modify-theme--modified-faces '())

(defvar modify-theme--header-faces
  '(font-latex-sectioning-0-face
    font-latex-sectioning-1-face
    font-latex-sectioning-2-face
    font-latex-sectioning-3-face
    font-latex-sectioning-4-face
    font-latex-sectioning-5-face
    font-latex-slide-title-face
    info-title-1
    info-title-2
    info-title-3
    info-title-4
    markdown-header-face
    markdown-header-face-1
    markdown-header-face-2
    markdown-header-face-3
    markdown-header-face-4
    markdown-header-face-5
    markdown-header-face-6
    org-document-title
    org-level-1
    org-level-2
    org-level-3
    org-level-4
    org-level-5
    org-level-6
    org-level-7
    org-level-8))

(advice-add 'load-theme :after 'modify-theme/load-theme)
(modify-theme/load-theme spacemacs--cur-theme)
