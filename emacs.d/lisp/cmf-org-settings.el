;;; cmf-org-settings.el --- Personal customizations for org-mode
;; ====================================================================
;;;
;; Copyright (c) 2016 Christopher M. Fuhrman
;; All rights reserved
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Wed Feb 17 21:20:03 2016 PST
;;
;; ====================================================================

;;; Commentary:
;;
;; Applies personal customizations for org-mode
;;

;;; Code:

;; Variables
(custom-set-variables
 '(org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODO's"
      ((agenda "")
       (alltodo)))
     ("g" . "GTD Task Lists")
     ("gh" "Home" tags-todo "HOME")
     ("go" "Office" tags-todo "OFFICE")
     ("gg" "G.G. Guards" tags-todo "GGG")
     ("gr" "Tasks to refile"
      ((todo "TODO"
             ((org-agenda-files
               (quote
                ("~/org/from-mobile.org" "~/org/refile.org")))))
       (todo "STARTED"
             ((org-agenda-files
               (quote
                ("~/org/from-mobile.org" "~/org/refile.org")))))
       (todo "WAITING"
             ((org-agenda-files
               (quote
                ("~/org/from-mobile.org" "~/org/refile.org")))))
       (todo "DELEGATED"
             ((org-agenda-files
               (quote
                ("~/org/from-mobile.org" "~/org/refile.org")))))))
     ("B" "GTD (B)lock Agenda"
      ((tags-todo "OFFICE")
       (tags-todo "GGG")
       (tags-todo "HOME"))
      nil)
     ("H" "Home Agenda"
      ((agenda "")
       (tags-todo "HOME"))
      ((org-agenda-tag-filter-preset
        (quote
         ("+HOME")))))
     ("O" "Office Agenda"
      ((agenda "")
       (tags-todo "OFFICE"))
      ((org-agenda-tag-filter-preset
        (quote
         ("+OFFICE")))))
     ("G" "G.G. Guards Agenda"
      ((agenda "")
       (tags-todo "GGG"))
      ((org-agenda-tag-filter-preset
        (quote
         ("+GGG"))))))))
 '(org-agenda-files
   (quote
    ("~/org/tasks.org" "~/org/refile.org" "~/org/notes.org")))
 '(org-archive-location "~/org/archive/%s_archive::file+olp+datetree/")
 '(org-ascii-charset (quote utf-8))
 '(org-capture-templates
   (quote
    (("t" "Task" entry
      (file+headline "~/org/refile.org" "New Tasks")
      "* TODO %^{Description}  %^G
  Added: %U
  Context: %a

  %?")
     ("i" "Idea" entry
      (file+headline "~/org/refile.org" "New Ideas")
      "* %^{Title} %^G
  Added: %U
  Context: %a

  %?
            ")
     ("a" "Appointment" entry
      (file+headline "~/org/tasks.org" "Appointments")
      "* TODO %^{Description}  :APPT:%^G
  Added: %U
  Context: %a

  %?")
     ("n" "Note" entry
      (file+datetree "~/org/notes.org")
      "* %^{Description}  %^G
  Added: %U
  Context: %a

  %?" :empty-lines 1)
     ("j" "Journal Entry" entry
      (file+datetree "~/org/journal.org")
      "** %^{Heading} :JOURNAL:%^G
  Added: %U
  Mood: %^{mood}
  
  %?" :empty-lines 1)
     ("B" "Blood Pressure Log Entry" table-line
      (file+headline "~/org/journal.org" "Blood Pressure Log")
      "| %^u | %^{Systolic} | %^{Diastolic} |")
     ("E" "Household Expense" table-line
      (file+headline "~/org/journal.org" "Household Expenses")
      "| %^u | %^{Vendor} | %^{Description} | %^{Cost} |")
     ("P" "Project" entry
      (file+headline "~/org/projects.org" "Projects")
      "* %^{Description} :%^G:
  Added: %U
  Context: %a

** Why?

   %?

** Outcome

** Lead

** Stakeholders

** Tasks

  " :prepend t :empty-lines 1))))
 '(org-clock-persist (quote history))
 '(org-clock-persist-file "~/.org-clock-save.el")
 '(org-complete-tags-always-offer-all-agenda-tags t)
 '(org-crypt-key "ABCDE12345") 		; Add your key here
 '(org-crypt-tag-matcher "CRYPT")
 '(org-default-notes-file "~/org/notes.org")
 '(org-directory "~/org")
 '(org-export-backends (quote (ascii html icalendar latex md texinfo)))
 '(org-export-latex-listings t)
 '(org-export-latex-packages-alist (quote (("" "listings") ("" "color"))))
 '(org-export-with-tags nil)
 '(org-fast-tag-selection-single-key t)
 '(org-latex-classes
   (quote
    (("koma-article" "\\documentclass{scrartcl}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(org-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t)
     ("T1" "fontenc" t)
     ("" "graphicx" t)
     ("" "longtable" nil)
     ("" "float" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "marvosym" t)
     ("" "wasysym" t)
     ("" "amssymb" t)
     ("" "hyperref" t)
     "\\tolerance=1000")))
 '(org-latex-packages-alist nil)
 '(org-latex-pdf-process
   (quote
    ("latexmk -pdflatex='lualatex -shell-excape -interaction nonstopmode' -pdf -f %f" "bibtex %b")))
 '(org-log-done (quote note))
 '(org-log-note-clock-out t)
 '(org-log-refile (quote time))
 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-files
   (quote
    (org-agenda-files "~/org/notes.org" "~/org/journal.org" "~/org/incubate.org" "~/org/ideas.org")))
 '(org-refile-targets
   (quote
    ((org-agenda-files :maxlevel . 2)
     ("~/org/incubate.org" :maxlevel . 1)
     ("~/org/ideas.org" :maxlevel . 1))))
 '(org-refile-use-outline-path nil)
 '(org-src-fontify-natively t)
 '(org-stuck-projects
   (quote
    ("+LEVEL=2/-DONE"
     ("TODO" "NEXT" "STARTED" "WAITING")
     nil "")))
 '(org-tag-persistent-alist
   (quote
    (("HOME" . 104)
     ("OFFICE" . 111)
     ("ERRAND" . 101)
     ("PHONE" . 112)
     ("EMAIL" . 109)
     ("GGG" . 103)
     ("APPT" . 97))))
 '(org-tags-exclude-from-inheritance (quote ("CRYPT")))
 '(org-todo-keywords
   (quote
    ((sequence "TODO" "NEXT(n!)" "STARTED(s!/@)" "DEFERRED(f!/@)" "DELEGATED(l@/@)" "WAITING(w@/@)" "|" "CANCELED(x@)" "DONE(d@)"))))
 )

(provide 'cmf-org-settings)

;;; cmf-org-settings.el ends here
