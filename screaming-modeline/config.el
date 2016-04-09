(defface scml-normal-1
  `((t (:background "OliveDrab2"
        :foreground "#3E3D31"
        :inerit 'mode-line)))
  "")
(defface scml-normal-2
  `((t (:background "OliveDrab3"
        :foreground "#3E3D31"
        :inerit 'mode-line)))
  "")

(set-face-attribute 'mode-line-buffer-id nil :foreground "#3E3D31")
(set-face-attribute 'scml-normal-1 nil :background "OliveDrab4")
(set-face-attribute 'scml-normal-2 nil :background "OliveDrab3")

(defun scml-face-func (face active)
  (let ((state (if active evil-state 'inactive))
        (tface (if (memq face '(face1 highlight)) 'face1 face)))
    (pcase (list state tface)
      (`(normal face1) 'scml-normal-1)
      (`(normal face2) 'scml-normal-2)
      (_ (cond
          ((eq 'face1 face) (if active 'powerline-active1 'powerline-inactive1))
          ((eq 'face2 face) (if active 'mode-line 'mode-line-inactive))
          ((eq 'line face) (if active 'powerline-active2 'powerline-inactive2))
          ((eq 'highlight face) (if active
                                    (funcall spaceline-highlight-face-func)
                                  'powerline-inactive1))))
      )))

(powerline-reset)
