;;; init-org.el --- Configuration of org-mode


;;; Code:

;; (setq org-export-in-background t)

;;
;; LaTeX Export
;;
(require 'ox-latex)

(setq org-latex-default-class "bxjsarticle")
;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -e '$dvipdf=q/dvipdfmx -o %D %S/' -norc -gg -pdfdvi %f"))
;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$latex=q/uplatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -e '$dvips=q/dvips -Ppdf -z -f %S | convbkmk -u > %D/' -e '$ps2pdf=q/ps2pdf %S %D/' -norc -gg -pdfps %f"))
;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/platex-ng %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdf %f"))
;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/pdflatex %S/' -e '$bibtex=q/bibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/makeindex -o %D %S/' -norc -gg -pdf %f"))
(setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/lualatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdf %f"))
;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/luajitlatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdf %f"))
;; (setq org-latex-pdf-process '("/Library/TeX/texbin/latexmk -e '$pdflatex=q/xelatex %S/' -e '$bibtex=q/upbibtex %B/' -e '$biber=q/biber --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex -o %D %S/' -norc -gg -pdf %f"))
(setq org-file-apps
      '(("pdf" . "/usr/bin/open -a Preview.app %s")))
(add-to-list 'org-latex-classes
             '("bxjsarticle"
               "\\ifdefined\\kanjiskip
  \\documentclass[autodetect-engine,dvipdfmx,12pt,a4paper,ja=standard]{bxjsarticle}
\\else
  \\ifdefined\\pdfoutput
    \\ifnum\\pdfoutput=0
      \\documentclass[autodetect-engine,dvipdfmx,12pt,a4paper,ja=standard]{bxjsarticle}
    \\else
      \\documentclass[autodetect-engine,12pt,a4paper,ja=standard]{bxjsarticle}
    \\fi
  \\else
    \\documentclass[autodetect-engine,12pt,a4paper,ja=standard]{bxjsarticle}
  \\fi
\\fi
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\ifdefined\\kanjiskip
\\usepackage{pxjahyper}
\\hypersetup{colorlinks=true}
\\else
\\ifdefined\\XeTeXversion
\\hypersetup{colorlinks=true}
\\else
\\ifdefined\\directlua
\\hypersetup{pdfencoding=auto,colorlinks=true}
\\else
\\hypersetup{unicode,colorlinks=true}
\\fi
\\fi
\\fi"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("bxjsarticle-dvips"
               "\\documentclass[uplatex,dvips,12pt,a4paper,ja=standard]{bxjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\hypersetup{setpagesize=false,colorlinks=true}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("ltjsarticle"
               "\\documentclass[12pt,a4paper]{ltjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
\\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage{hyperref}
\\hypersetup{pdfencoding=auto,colorlinks=true}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(provide 'init-org)

;;; init-org.el ends here
