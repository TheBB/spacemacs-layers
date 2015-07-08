;;; evil-indent-textobject.el --- evil textobjects based on indentation

;; Original evil-indent-textobject.el Copyright (C) 2013 Michael Markert
;; Modifications Copyright (C) 2014-2015 Eivind Fonn
;; Author: Eivind Fonn <evfonn@gmail.com>
;; Created: 2014-08-31
;; Version: 1.0.0
;; Keywords: convenience evil
;; URL: http://github.com/TheBB/evil-indent-textobject
;; Package-Requires: ((evil "0"))
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Adds new textobjects:
;;;
;;; ii, ai: Block of text with same or higher indentation
;;; iI, aI: Block of text with same or higher indentation, including the first line
;;;         above with smaller indentation
;;; iJ, aJ: Block of text with same or higher indentation, including the first lines
;;;         above and below with smaller indentation

;;; Code:

(require 'cl-lib)
(require 'evil)

(defun evil-indent--chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun evil-indent--empty-line-p ()
  (string= "" (evil-indent--chomp (thing-at-point 'line))))

(defun evil-indent--not-empty-line-p ()
  (not (evil-indent--empty-line-p)))

(defun evil-indent--geq-p ()
  (>= (current-indentation) base))

(defun evil-indent--geq-or-empty-p ()
  (or (evil-indent--empty-line-p) (evil-indent--geq-p)))

(defun evil-indent--g-p ()
  (> (current-indentation) base))

(defun evil-indent--g-or-empty-p ()
  (or (evil-indent--empty-line-p) (evil-indent--g-p)))

(defun evil-indent--seek (start direction before skip predicate)
  "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
  (save-excursion
    (goto-char start)
    (goto-char (point-at-bol))
    (let ((bnd (if (> 0 direction)
                   (point-min)
                 (point-max)))
          (pt (point)))
      (when skip (forward-line direction))
      (loop while (and (/= (point) bnd) (funcall predicate))
            do (progn
                 (when before (setq pt (point-at-bol)))
                 (forward-line direction)
                 (unless before (setq pt (point-at-bol)))))
      pt)))

(defun evil-indent--same-indent-range (&optional point)
  "Return the point at the begin and end of the text block with the same (or greater) indentation.
If `point' is supplied and non-nil it will return the begin and end of the block surrounding point."
  (save-excursion
    (when point
      (goto-char point))
    (let ((base (current-indentation))
          (begin (point))
          (end (point)))
      (setq begin (evil-indent--seek begin -1 t nil 'evil-indent--geq-or-empty-p))
      (setq begin (evil-indent--seek begin 1 nil nil 'evil-indent--g-or-empty-p))
      (setq end (evil-indent--seek end 1 t nil 'evil-indent--geq-or-empty-p))
      (setq end (evil-indent--seek end -1 nil nil 'evil-indent--empty-line-p))
      (list begin end base))))

(defun evil-indent--up-indent-range (&optional point)
  (let* ((range (evil-indent--same-indent-range point))
         (base (third range))
         (begin (evil-indent--seek (first range) -1 nil nil 'evil-indent--geq-or-empty-p)))
    (list begin (second range) base)))

(defun evil-indent--up-down-indent-range (&optional point)
  (let* ((range (evil-indent--same-indent-range point))
         (base (third range))
         (begin (evil-indent--seek (first range) -1 nil nil 'evil-indent--geq-or-empty-p))
         (end (evil-indent--seek (second range) 1 nil nil 'evil-indent--geq-or-empty-p)))
    (list begin end base)))

(defun evil-indent--linify (range)
  (let ((nbeg (save-excursion (goto-char (first range)) (point-at-bol)))
        (nend (save-excursion (goto-char (second range)) (point-at-eol))))
    (evil-range nbeg nend 'line)))

(defun evil-indent--extend (range)
  (let ((begin (first range))
        (end (second range))
        nend)
    (setq nend (evil-indent--seek end 1 t t 'evil-indent--empty-line-p))
    (when (= nend end)
      (setq begin (evil-indent--seek begin -1 t t 'evil-indent--empty-line-p)))
    (list begin nend)))

(evil-define-text-object evil-indent-i-indent (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
skipping empty lines."
  :type line
  (evil-indent--linify (evil-indent--same-indent-range)))

(evil-define-text-object evil-indent-a-indent (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
skipping empty lines."
  :type line
  (evil-indent--linify (evil-indent--extend (evil-indent--same-indent-range))))

(evil-define-text-object evil-indent-i-indent-up (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above, skipping empty lines."
  :type line
  (evil-indent--linify (evil-indent--up-indent-range)))

(evil-define-text-object evil-indent-a-indent-up (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above, skipping empty lines."
  :type line
  (evil-indent--linify (evil-indent--extend (evil-indent--up-indent-range))))

(evil-define-text-object evil-indent-i-indent-up-down (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above and below, skipping empty lines."
  :type line
  (evil-indent--linify (evil-indent--up-down-indent-range)))

(evil-define-text-object evil-indent-a-indent-up-down (&optional count beg end type)
  "Text object describing the block with the same (or greater) indentation as the current line,
and the line above and below, skipping empty lines."
  :type line
  (evil-indent--linify (evil-indent--extend (evil-indent--up-down-indent-range))))

;;;###autoload
(with-eval-after-load 'evil
  (autoload 'evil-indent-i-indent "evil-indent-textobject" nil t)
  (autoload 'evil-indent-a-indent "evil-indent-textobject" nil t)
  (autoload 'evil-indent-i-indent-up "evil-indent-textobject" nil t)
  (autoload 'evil-indent-a-indent-up "evil-indent-textobject" nil t)
  (autoload 'evil-indent-i-indent-up-down "evil-indent-textobject" nil t)
  (autoload 'evil-indent-a-indent-up-down "evil-indent-textobject" nil t)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-a-indent-up-down)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-a-indent-up)
  (define-key evil-inner-text-objects-map "i" 'evil-indent-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-a-indent))

(provide 'evil-indent-textobject)
;;; evil-indent-textobject.el ends here
