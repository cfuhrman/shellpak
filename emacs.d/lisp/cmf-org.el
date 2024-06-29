;;; cmf-org.el --- ORG-mode customization
;; ====================================================================
;;;
;; Copyright (c) 2024 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Fri Jun 14 08:35:30 2024 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Contains any customizations related to org-mode
;;

;;; Code:

;;
;; Variables
;;

(custom-set-variables
 '(org-adapt-indentation t)
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
 '(org-archive-location "~/org/archive/%s_archive::datetree/")
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
      (file+olp+datetree "~/org/notes.org")
      "* %^{Description}  %^G
  Added: %U
  Context: %a

  %?" :empty-lines 1)
     ("j" "Journal Entry" entry
      (file+olp+datetree "~/org/journal.org")
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
 '(org-crypt-key "943DD02859DB2579")
 '(org-crypt-tag-matcher "CRYPT")
 '(org-default-notes-file "~/org/notes.org")
 '(org-directory "~/org")
 '(org-ellipsis " ▾")
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
    (("T1" "fontenc" t)
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
    ("%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f" "%latex -interaction nonstopmode -output-directory %o %f")))
 '(org-list-allow-alphabetical t)
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

(add-to-list
 'safe-local-variable-values
 '(org-latex-pdf-process "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f %f" "bibtex %b"
                         ))

;;
;; Packages
;;

(use-package org
  ;; This is a built-in mode
  :bind (
         ("C-c C-o l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c [" . org-time-stamp-inactive)
         )

  :hook ((org-mode . auto-fill-mode)
         (org-mode .
                   (lambda ()
                     (footnote-mode -1))))

  :init
  (require 'epa-file)
  (require 'org-crypt)

  ;; Enable org-crypt as appropriate
  (org-crypt-use-before-save-magic)

  :config
  (use-package company-org-block
    :ensure t

    :hook ((org-mode . (lambda ()
                         (setq-local company-backends '(company-org-block))
                         (company-mode +1))))

    :custom
    (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
    )

  (use-package org-bullets
    :ensure t

    :hook (org-mode . org-bullets-mode)
    )

  (use-package org-fancy-priorities
    :ensure t
    :diminish org-fancy-priorities-mode

    :hook (org-mode . org-fancy-priorities-mode)

    :config
    ;; Customization for org-fancy-priorities
    (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
    (setq org-lowest-priority ?D)
    )

  (use-package org-present
    :ensure t

    :bind
    (:map org-present-mode-keymap
          ("C-c a"   . org-present-beginning)
          ("C-c e"   . org-present-end)
          ("C-c n"   . org-present-next)
          ("C-c p"   . org-present-prev)
          ("<right>" . right-char)
          ("<left>"  . left-char)
          ("<up>"    . previous-line)
          ("<down>"  . next-line))

    :config
    ;; Used with org-present
    (use-package visual-fill-column
      :ensure t

      :custom
      (visual-fill-column-width 155)
      (visual-fill-column-center-text t)
      )

    (defun cmf/org-present-prepare-slide (buffer-name heading)
      "Hook to run when each slide is presented."
      ;; Show only top-level headlines
      (org-overview)

      ;; Unfold the current entry
      (org-show-entry)

      ;; Show only direct subheadings of the slide but don't expand them
      (org-show-children)
      )

    (defun cmf/org-present-start ()
      "Hook to run upon starting org-present mode."
      (visual-fill-column-mode 1)
      (visual-line-mode 1)

      ;; Tweak font sizes
      (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                         (header-line (:height 4.0) variable-pitch)
                                         (org-document-title (:height 1.75) org-document-title)
                                         (org-code (:height 1.25) org-code)
                                         (org-table (:height 1.05) org-table)
                                         (org-verbatim (:height 1.05) org-verbatim)
                                         (org-block (:height 0.9) org-block)
                                         (org-block-begin-line (:height 0.7) org-block)))
      (setq header-line-format " ")

      ;; Display inline images automatically
      (org-display-inline-images)
      )

    (defun cmf/org-present-end ()
      "Hook to run upon exiting org-present mode."
      (visual-fill-column-mode 0)
      (visual-line-mode 0)

      ;; Reset font customization
      (setq-local face-remapping-alist '((default default)
                                         (org-code (:height 1.55 :inherit `(shadow fixed-pitch)) org-code)))

      ;; Clear the header line string so that it isn't displayed
      (setq header-line-format nil)

      ;; Stop displaying inline images
      (org-remove-inline-images)

      ;; To be safe
      (cmf/org-font-setup)
      )

    ;; Register hooks with org-present
    (add-hook 'org-present-mode-hook 'cmf/org-present-start)
    (add-hook 'org-present-mode-quit-hook 'cmf/org-present-end)
    (add-hook 'org-present-after-navigate-functions 'cmf/org-present-prepare-slide)
    )

  (unless (eq (executable-find "pandoc") nil)
    (use-package ox-pandoc
      :ensure t
      )
    )

  (use-package ox-twbs
    :ensure t
    )

  ;; Code borrowed from https://github.com/daviwil/emacs-from-scratch/blob/master/init.el
  (defun cmf/org-font-setup ()
    "Set up font settings for `org-mode' under Emacs."
    (dolist (face '((org-level-1 . 1.75)
                    (org-level-2 . 1.50)
                    (org-level-3 . 1.25)
                    (org-level-4 . 1.2)
                    (org-level-5 . 1.15)
                    (org-level-6 . 1.13)
                    (org-level-7 . 1.11)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Cantarell"
                          :weight 'regular :height (cdr face)))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch :height 1.1)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    ;; (set-face-attribute 'org-date         :height 1.1)
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch) :height 1.1)
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch) :height 1.1)
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit
                        'fixed-pitch)
    )

  (org-clock-persistence-insinuate)

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (if (member system-type '(gnu/linux))
      (setq org-file-apps
            '((auto-mode . emacs)
              ("\\.x?html?\\'" . "xdg-open %s")
              ("\\.pdf\\'" . "xdg-open \"%s\"")
              ("\\.pdf::\\([0-9]+\\)\\'" . "xdg-open \"%s\"")))
    )

  ;; Only set up fonts if this is a window-system (i.e., not CLI/terminal mode)
  (if (window-system)
      (cmf/org-font-setup)
    )
  )


(provide 'cmf-org)
;;; cmf-org.el ends here
