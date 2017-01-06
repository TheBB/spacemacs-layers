(defface scml-normal-1
  `((t (:background "chartreuse1"
        :foreground "#3E3D31"
        :inerit 'mode-line)))
  "")
(defface scml-normal-2
  `((t (:background "chartreuse4"
        :foreground "#3E3D31"
        :inerit 'mode-line)))
  "")
(defface scml-normal-l
  `((t (:background "#223200"
        :foreground "#3E3D31"
        :inerit 'mode-line)))
  "")

(defun scml-face-func (face active)
  (let ((state (if active evil-state 'inactive))
        (tface (if (memq face '(face1 highlight)) 'face1 face)))
    (pcase (list state tface)
      (`(normal face1) 'scml-normal-1)
      (`(normal face2) 'scml-normal-2)
      (`(normal line) 'scml-normal-l)
      (_ (cond
          ((eq 'face1 face) (if active 'powerline-active1 'powerline-inactive1))
          ((eq 'face2 face) (if active 'mode-line 'mode-line-inactive))
          ((eq 'line face) (if active 'powerline-active2 'powerline-inactive2))
          ((eq 'highlight face) (if active
                                    (funcall spaceline-highlight-face-func)
                                  'powerline-inactive1))))
      )))
