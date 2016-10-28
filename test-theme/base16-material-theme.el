;; base16-material-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Authors:
;; Scheme: Jan T. Sott (http://github.com/idleberg)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-material-colors
  '(:base00 "#090300"
    :base01 "#3a3432"
    :base02 "#4a4543"
    :base03 "#5c5855"
    :base04 "#807d7c"
    :base05 "#a5a2a2"
    :base06 "#d6d5d4"
    :base07 "#f7f7f7"
    :base08 "#e65100"
    :base09 "#ff9800"
    :base0A "#ffeb3b"
    :base0B "#aeea00"
    :base0C "#00bcd4"
    :base0D "#2196f3"
    :base0E "#9c27b0"
    :base0F "#ff9100")
  "All colors for Base16 material are defined here.")

;; Define the theme
(deftheme base16-material)

;; Add all the faces to the theme
(base16-theme-define 'base16-material base16-material-colors)

;; Mark the theme as provided
(provide-theme 'base16-material)

(provide 'base16-material-theme)

;;; base16-material-theme.el ends here
