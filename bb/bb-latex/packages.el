(setq bb-latex-packages '(auctex))

(defun bb-latex/post-init-auctex ()
  (setq font-latex-match-function-keywords
        '(("address" "{")
          ("definecolor" "{{{")
          ("includegraphics" "[{")
          ("graphicspath" "{")
          ("makeatletter" "")
          ("makeatother" "")
          ("makesavenoteenv" "{")
          ("newacronym" "{{{")
          ("pgfplotsset" "{")
          ("reserveinserts" "{")
          ("setbeamercovered" "{")
          ("titlegraphic" "{")
          ("usecolortheme" "{")
          ("usepgfplotslibrary" "{")
          ("usetheme" "{")
          ("usetikzlibrary" "{"))
        font-latex-match-italic-command-keywords
        '(("url" "{"))
        font-latex-match-reference-keywords
        '(("autoref" "{")
          ("inst" "{"))
        font-latex-match-slide-title-keywords
        '(("frametitle" "{"))
        font-latex-match-textual-keywords
        '(("abstract" "{")
          ("and" "")
          ("challengeList" "{")
          ("doList" "{")
          ("hfill" "")
          ("institute" "[{")
          ("overview" "{")
          ("titlepage" ""))
        font-latex-match-variable-keywords
        '()
        font-latex-match-warning-keywords
        '()
        font-latex-user-keyword-classes
        '(("variables"
           (("baselineskip" "")
            ("textheight" "")
            ("textwidth" ""))
           font-lock-variable-name-face noarg)
          ("subst"
           (("copyright" "")
            ("o" "")
            ("ldots" ""))
           font-lock-builtin-face noarg)
          ("tikz"
           (("foreach" "")
            ("draw" "[")
            ("matrix" "[")
            ("node" "["))
           default command))))
