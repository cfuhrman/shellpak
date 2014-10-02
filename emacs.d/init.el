;; ====================================================================
;;
;; emacs.d/init.el
;;
;; Copyright (c) 2008 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Simplified BSD License (also known
;; as the "2-Clause License" or "FreeBSD License".)
;;
;; --------------------------------------------------------------------
;;
;; DESCRIPTION:
;;
;;   GNU Emacs-compatible initialization file defining a number of
;;   coding styles and preferences
;;
;; CAVEATS:
;;
;;   To install php-auto-yasnippets, php-mode must be loaded
;;   beforehand.  This gets around an issue whereby
;;   php-auto-yasnippets is loaded before php-mode, causing an error.
;;
;; TODO:
;;
;;    - The same thing we do every night, Pinky.  Try to take over
;;      *THE WORLD*!
;;
;; ====================================================================

;; Make sure all Emacs Lisp files are byte-compiled for speed
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Load the appropriate customization file based on if we are running
;; under a window system, such as "x" or "mac"
(if (equal window-system nil)
    (if (or (string-match "term-256" (getenv "TERM"))
            (string-match "screen-256" (getenv "TERM")))
        (setq custom-file "~/.emacs.d/custom-nox256.el")
      (setq custom-file "~/.emacs.d/custom-nox.el"))
  (setq custom-file "~/.emacs.d/custom.el"))

(load custom-file)

;; Variable definitions
(defvar ac-ispell-present nil)
(defvar auto-complete-present nil)
(defvar flymake-mode-present nil)
(defvar geben-present nil)
(defvar ggtags-mode-present nil)
(defvar indent-guide-present nil)
(defvar local-execpaths '("/usr/texbin" "/usr/pkg/bin" "/usr/local/bin"))
(defvar local-loadpaths '("/pkg/share/emacs/site-lisp" "/usr/local/share/emacs/site-lisp"))
(defvar multi-web-present nil)
(defvar my-package-list nil)
(defvar org-bullets-present nil)
(defvar payas-mode-present nil)
(defvar smart-mode-line-present nil)
(defvar solarized-theme-present nil)
(defvar vc-fossil-present nil)
(defvar xlicense-present nil)
(defvar yasnippet-present nil)

;; Sane defaults
(setq-default indent-tabs-mode nil)

;; Add personal paths
(add-to-list 'load-path (format "%s/.emacs.d" (getenv "HOME")))
(add-to-list 'load-path (format "%s/.emacs.d/thirdparty" (getenv "HOME")))

;; Load individual files
(load-file (format "%s/.emacs.d/thirdparty/phpdocumentor.el" (getenv "HOME")))

;; Define desired packages
(setq my-package-list '(ac-ispell
                        apache-mode
                        auctex
                        auto-complete
                        crontab-mode
                        csv-nav
                        flymake-php
                        flymake-shell
                        geben
                        git-commit-mode
                        git-rebase-mode
                        indent-guide
                        markdown-mode
                        multi-web-mode
                        org-bullets
                        php-eldoc
                        php-extras
                        php-mode
                        psvn
                        solarized-theme
                        vc-fossil
                        xkcd
                        xlicense
                        yaml-mode
                        yasnippet))


;; Use Emacs 'ls' emulation
(require 'ls-lisp)

;; Add Emacs-24-specific packages
(if (>= emacs-major-version 24)
    (setq my-package-list
          (append my-package-list '(ggtags
                                    smart-mode-line
                                    weather-metno))))

;; Customize font under X
(if (equal window-system 'x)
    (set-face-attribute 'default nil
                        :family  "DejaVu Sans Mono"
                        :foundry 'unknown
                        :slant   'normal
                        :weight  'normal
                        :height   100
                        :width   'normal))

;; Install Org-Mode, if present
(if (>= emacs-major-version 23)
    (require 'org-install)
  (message "Emacs version %d does not include org-mode, so not setting it up" emacs-major-version))

;; Auto-Complete mode present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^auto-complete" 'nosort))
    (setq auto-complete-present t))

;; AC-Ispell mode present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^ac-ispell" 'nosort))
    (setq ac-ispell-present t))

;; Flymake-mode present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^flymake" 'nosort))
    (setq ac-ispell-present t))

;; Geben present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^geben" 'nosort))
    (setq geben-present t))

;; ggtags-mode present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^ggtags" 'nosort))
    (setq ggtags-mode-present t))

;; Indent-Guide mode present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^indent-guide" 'nosort))
    (setq indent-guide-present t))

;; Multi-Web mode present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^multi-web-mode" 'nosort))
    (setq multi-web-present t))

;; Org-bullets present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^org-bullets" 'nosort))
    (setq org-bullets-present t))

;;  PHP Auto YASnippets present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^php-auto-yasnippets" 'nosort))
    (setq payas-mode-present t))

;; Smart Mode Line present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^smart-mode-line" 'nosort))
    (setq smart-mode-line-present t))

;; Solarized theme present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^solarized" 'nosort))
    (setq solarized-theme-present t))

;; Xlicense present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^xlicense" 'nosort))
    (setq xlicense-present t))

;; VC-Fossil present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^vc-fossil" 'nosort))
    (setq vc-fossil-present t))

;; YASnippet present?
(if (and (file-directory-p "~/.emacs.d/elpa/")
         (directory-files "~/.emacs.d/elpa/" (not 'absolute) "^yasnippet" 'nosort))
    (setq yasnippet-present t))

;; Set speller to aspell
(setq-default ispell-program-name "aspell")


;; Aliases
;; --------------------------------------------------------------------

(defalias 'perl-mode 'cperl-mode)


;; Macros
;; --------------------------------------------------------------------

(fset 'align-on-equal
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("xalign-regex=" 0 "%d")) arg)))

(fset 'align-on-hash-arrow
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217848 97 108 105 103 110 45 114 101 103 101 120 return 61 62 return] 0 "%d")) arg)))


;; Keyboard Bindings
;; --------------------------------------------------------------------

;; Use Option Key for Meta under Mac OS X
(setq mac-option-modifier 'meta)

;; Define some org-mode maps
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

;; Set keyboard shortcut for normal-erase-is-backspace
(global-set-key [?\C-x ?\C-k ?0] 'normal-erase-is-backspace-mode)
(global-set-key [?\C-x ?\C-k ?1] 'delete-trailing-whitespace)
(global-set-key [?\C-x ?\C-k ?2] 'align-on-equal)
(global-set-key [?\C-x ?\C-k ?3] 'align-on-hash-arrow)


;; Functions
;; --------------------------------------------------------------------

(defun cmf-autoinstall-packages ()
  "Automatically downloads and installs list of preferred packages"
  (interactive)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-package-list)
    (unless (package-installed-p package)
      (package-install package)))
  )

;; Debug a simple PHP script.
(defun cmf-php-debug ()
  "Run current PHP script for debugging with geben"
  (interactive)
  (call-interactively 'geben)
  (shell-command
    (concat "XDEBUG_CONFIG='idekey=emacs' /usr/bin/php "
    (buffer-file-name) " &"))
  )

;; Function for loading exec paths
(defun nice-exec-path (pathlist)
  "Add given list of paths to exec-path"
  (dolist (path pathlist)
    (add-to-list 'exec-path path)
    (setenv "PATH" (concat (getenv "PATH") ":" path)))
  )

;; Function for loading library paths
;; TODO: Simplify list iteration logic a la nice-exec-path
(defun nice-load-path (pathlist)
  "Add given list of paths to load-path"
  (while pathlist
    (if (and (file-directory-p (car pathlist))
             (not (member (car pathlist) load-path)))
        (let ((default-directory (car pathlist)))
          (normal-top-level-add-to-load-path '("."))
          (normal-top-level-add-subdirs-to-load-path)))
    (setq pathlist (cdr pathlist)))
  )

;; A nice function for email, commit messages, etc
(defun nice-text-mode ()
  "Setup a sane mode for editing english text"
  (interactive)
  (text-mode)
  )

;; Functions for perltidy
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t))
  )

(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
		  (perltidy-region))
  )


;; Programming styles
;; --------------------------------------------------------------------

;; OpenBSD Coding Style
(c-add-style "openbsd"
             '("bsd"
               (c-backspace-function . delete-backward-char)
               (c-syntactic-indentation-in-macros . nil)
               (c-tab-always-indent . nil)
               (c-hanging-braces-alist
                (block-close . c-snug-do-while))
               (c-offsets-alist
                (arglist-cont-nonempty . *)
                (statement-cont . *))
               (indent-tabs-mode . t)))

;; My own style
(c-add-style "cmf"
             '("bsd"
	       (indent-tabs-mode . nil)))


;; Hooks
;; --------------------------------------------------------------------

;; Enable code folding
(defun folding-code-hook ()
  "Enable code folding"
  (defvar outline-minor-mode-prefix)
  (setq outline-minor-mode-prefix "\C-co")
  (outline-minor-mode t)
  (hide-body)
  )

;; Define a basic hook for Emacs lisp files
(defun nice-elisp-hook ()
  "Hook for sane editing of lisp files"
  (setq indent-tabs-mode nil)
  )

;; Define a basic hook for enabling ac-ispell
(defun nice-ispell-hook ()
  "Hook for enabling ac-ispell"
  (if (equal ac-ispell-present t)
      (add-to-list 'ac-sources 'ac-source-ispell))
  )

;; Define a basic hook for sane editing of makefiles
(defun nice-makefile-hook ()
  "Hook for sane editing of Makefiles"
  (if (>= emacs-major-version 23)
      (linum-mode t))
  )

;; Define a basic hook for sane editing of org-mode documents
(defun nice-org-hook ()
  "Hook for sane editing of org-mode documents"
  (defvar org-mode-map)
  (org-defkey org-mode-map "\C-c[" 'org-time-stamp-inactive)
  (if (equal org-bullets-present t)
      (org-bullets-mode t))
  )

;; Define a basic hook for editing PHP files
(defun nice-php-hook ()
  ;; Hook for sane editing of PHP files
  (c-set-style "cmf")
  (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
  (define-key php-mode-map (kbd "C-x p") 'phpdoc)
  )

;; Define a nice programming hook
(defun nice-prog-hook ()
  "Enable some sanity for programming source files"
  (if (>= emacs-major-version 23)
      (linum-mode t))
  (if (equal ggtags-mode-present t)
      (ggtags-mode t))
  (if (equal indent-guide-present t)
      (indent-guide-mode t))
  (auto-fill-mode t)
  (eldoc-mode t)
  (electric-pair-mode t)
  )

;; Define a nice hook for editing SQL files
(defun nice-sql-hook ()
  "Hook for sane editing of SQL files"
  (if (>= emacs-major-version 23)
      (linum-mode t))
  (auto-fill-mode t)
  (electric-pair-mode t)
  )

;; Define a basic hook for sane editing of text documents
(defun nice-text-hook ()
  "Hook for sane editing of text (ASCII) documents"
  (auto-fill-mode t)
  (flyspell-mode t)
  (footnote-mode t)
  (if (equal auto-complete-present t)
      (auto-complete-mode t))
  )

;; Real lisp hackers use the lambda character
;; Source: https://github.com/edmore/dotemacs/blob/master/modules/defuns.el
(defun sm-lambda-mode-hook ()
  (if (equal custom-file "~/.emacs.d/custom.el")
      (font-lock-add-keywords
       nil `(("\\<lambda\\>"
              (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                        ,(make-char 'greek-iso8859-7 107))
                        nil))))))
  )

;;
;; Enable appropriate programming hooks
;;

;; For C files
(add-hook 'c-mode-common-hook 'nice-prog-hook)
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; For Emacs Lisp files
(add-hook 'emacs-lisp-mode-hook    'nice-prog-hook)
(add-hook 'emacs-lisp-mode-hook    'nice-elisp-hook)
(add-hook 'emacs-lisp-mode-hook    'sm-lambda-mode-hook)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; For Perl files
(add-hook 'cperl-mode-hook    'nice-prog-hook)
(add-hook 'cperl-mode-hook    'folding-code-hook)
(add-hook 'cperl-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; For HTML files
(add-hook 'html-mode-hook     'nice-prog-hook)

;; For javascript files
(add-hook 'js-mode-hook       'nice-prog-hook)

;; For Makefiles
(add-hook 'makefile-mode-hook 'nice-makefile-hook)

;; For PHP files
(add-hook 'php-mode-hook      'flymake-php-load)
(add-hook 'php-mode-hook      'nice-php-hook)

;; For Ruby files
(add-hook 'ruby-mode-hook     'nice-prog-hook)

;; For Shell Scripts
(add-hook 'sh-mode-hook       'nice-prog-hook)
(add-hook 'sh-set-shell-hook  'flymake-shell-load)
(add-hook 'sh-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;; For SQL files
(add-hook 'sql-mode-hook      'nice-sql-hook)

;; For XML files
(add-hook 'nxml-mode-hook     'nice-prog-hook)

;;
;; Enable appropriate hooks for text documents
;;

(add-hook 'log-edit-mode-hook 'nice-text-hook)
(add-hook 'muse-mode-hook     'nice-text-hook)
(add-hook 'org-mode-hook      'nice-org-hook)
(add-hook 'text-mode-hook     'nice-text-hook)

;; nice-ispell-hook() will automatically detect presence of ac-ispell
(add-hook 'LaTeX-mode-hook    'nice-ispell-hook)
(add-hook 'log-edit-mode-hook 'nice-ispell-hook)
(add-hook 'muse-mode-hook     'nice-ispell-hook)
(add-hook 'org-mode-hook      'nice-ispell-hook)
(add-hook 'text-mode-hook     'nice-ispell-hook)

;; Enable lambda character translation for appropriate Lisp documents
(add-hook 'lisp-interactive-mode-hook 'sm-lamba-mode-hook)
(add-hook 'scheme-mode-hook           'sm-lambda-mode-hook)


;; Filename associations
;; --------------------------------------------------------------------

;; Apache Mode
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"           . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'"         . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"          . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"            . apache-mode))

;; CPERL Mode
(add-to-list 'auto-mode-alist '("\\.cgi\\'"                . cperl-mode))

;; Crontab Mode
(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'"     . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."        . crontab-mode))

;; CSV files
(add-to-list 'auto-mode-alist '("\\.csv\\'"                . csv-nav-mode))

;; HTML Mode
(add-to-list 'auto-mode-alist '("\\.tmpl\\'"               . html-mode))

;; Makefile Mode
(add-to-list 'auto-mode-alist '("[Mm]akefile\\."           . makefile-mode))

;; Muse Mode
(add-to-list 'auto-mode-alist '("\\.muse\\'"               . muse-mode))

;; Org Mode
(add-to-list 'auto-mode-alist '("\\.org\\'"                . org-mode))

;; PHP Mode
(add-to-list 'auto-mode-alist '("\\.php\\'"                . php-mode))

;; Puppet Mode
(add-to-list 'auto-mode-alist '("\\.pp\\'"                 . puppet-mode))

;; Text Mode
(add-to-list 'auto-mode-alist '("COMMIT"                   . text-mode))
(add-to-list 'auto-mode-alist '("\\.tmp\\'"                . text-mode))
(add-to-list 'auto-mode-alist '("bzr_log\\."               . text-mode))
(add-to-list 'auto-mode-alist '("pico\\."                  . text-mode))

;; XML Mode
(add-to-list 'auto-mode-alist '("\\.xsd\\'"                . xml-mode))
(add-to-list 'auto-mode-alist '("\\.wsdl\\'"               . xml-mode))

;; YAML Mode
(add-to-list 'auto-mode-alist '("\\.sls\\'"                . yaml-mode))


;; Customized Variables
;; --------------------------------------------------------------------

(custom-set-variables
 '(ac-ispell-requires 4)
 '(all-christian-calendar-holidays t)
 '(c-default-style (quote ((c-mode . "cmf") (c++-mode . "cmf") (objc-mode . "cmf") (java-mode . "cmf") (awk-mode . "awk") (other . "cmf"))))
 '(c-echo-syntactic-information-p t)
 '(c-report-syntactic-errors t)
 '(c-require-final-newline (quote ((c-mode . t) (c++-mode . t) (objc-mode . t) (java-mode . t))))
 '(calendar-christian-all-holidays-flag t)
 '(calendar-daylight-time-zone-name "PDT")
 '(calendar-latitude 37.7822)
 '(calendar-location-name "San Francisco, CA")
 '(calendar-longitude -122.4167)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name "PST")
 '(calendar-time-zone -480)
 '(calendar-view-holidays-initially-flag nil)
 '(column-number-mode t)
 '(comment-auto-fill-only-comments t)
 '(comment-fill-column 80)
 '(comment-multi-line t)
 '(comment-style (quote indent))
 '(cperl-continued-brace-offset -8)
 '(cperl-continued-statement-offset 4)
 '(cperl-electric-keywords t)
 '(cperl-electric-linefeed t)
 '(cperl-electric-parens t)
 '(cperl-font-lock t)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-indent-level 8)
 '(delete-selection-mode nil)
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(display-time-mode t)
 '(emerge-combine-versions-template "
%b
%a
")
 '(global-hl-line-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(ido-mode t)
 '(js-indent-level 8)
 '(js-expr-indent-offset 4)
 '(linum-delay t)
 '(mark-even-if-inactive t)
 '(mark-holidays-in-calendar t)
 '(org-agenda-custom-commands (quote (("n" "Agenda and all TODO's" ((agenda "") (alltodo))) ("g" . "GTD Task Lists") ("gh" "Home" tags-todo "HOME") ("go" "Office" tags-todo "OFFICE") ("gr" "Tasks to refile" todo "TODO" ((org-agenda-files (quote ("~/org/from-mobile.org" "~/org/refile.org"))))) ("B" "GTD (B)lock Agenda" ((tags-todo "OFFICE") (tags-todo "HOME")) nil) ("H" "Home Agenda" ((agenda "") (tags-todo "HOME")) ((org-agenda-tag-filter-preset (quote ("+HOME"))))) ("O" "Office Agenda" ((agenda "") (tags-todo "OFFICE")) ((org-agenda-tag-filter-preset (quote ("+OFFICE"))))))))
 '(org-agenda-files (quote ("~/org/tasks.org")))
 '(org-archive-location "~/org/archive/%s_archive::datetree/")
 '(org-capture-templates (quote (("t" "Task" entry (file+headline "~/org/refile.org" "New Tasks") "* TODO %^{Description}  %^G
  Added: %U
  Context: %a

  %?") ("i" "Idea" entry (file+headline "~/org/refile.org" "New Ideas") "* %^{Title} %^G
  Added: %U
  Context: %a

  %?
            ") ("a" "Appointment" entry (file+headline "~/org/tasks.org" "Appointments") "* TODO %^{Description}  :APPT:%^G
  Added: %U
  Context: %a

  %?") ("n" "Note" entry (file+datetree "~/org/notes.org") "* %^{Description}  %^G
  Added: %U
  Context: %a

  %?" :empty-lines 1) ("j" "Journal Entry" entry (file+datetree "~/org/journal.org") "** %^{Heading}
  Added: %U

  %?" :empty-lines 1) ("K" "Kawasaki Riding Log Entry" table-line (file+headline "~/org/journal.org" "Kawasaki Riding Log") " | %^u | %^{Miles} |") ("B" "Blood Pressure Log Entry" table-line (file+headline "~/org/journal.org" "Blood Pressure Log") "| %^u | %^{Systolic} | %^{Diastolic} |") ("E" "Household Expense" table-line (file+headline "~/org/journal.org" "Household Expenses") "| %^u | %^{Vendor} | %^{Description} | %^{Cost} |") ("P" "Project" entry (file+headline "~/org/projects.org" "Projects") "* %^{Description} :%^G:
  Added: %U
  Context: %a

** Why?

   %?

** Outcome

** Lead

** Stakeholders

** Tasks

  " :prepend t :empty-lines 1))))
 '(org-complete-tags-always-offer-all-agenda-tags t)
 '(org-default-notes-file "~/org/notes.org")
 '(org-directory "~/org")
 '(org-export-latex-listings t)
 '(org-export-latex-packages-alist (quote (("" "listings") ("" "color"))))
 '(org-export-with-tags nil)
 '(org-fast-tag-selection-single-key t)
 '(org-log-done (quote note))
 '(org-log-refile (quote time))
 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-files (quote (org-agenda-files "~/org/notes.org" "~/org/journal.org" "~/org/incubate.org" "~/org/ideas.org")))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 2) ("~/org/incubate.org" :maxlevel . 1) ("~/org/ideas.org" :maxlevel . 1))))
 '(org-refile-use-outline-path t)
 '(org-src-fontify-natively t)
 '(org-stuck-projects (quote ("+LEVEL=2/-DONE" ("TODO" "NEXT" "STARTED" "WAITING") nil "")))
 '(org-tag-persistent-alist (quote (("HOME" . 104) ("OFFICE" . 111) ("ERRAND" . 101) ("PHONE" . 112) ("EMAIL" . 109) ("GGG" . 103) ("APPT" . 97))))
 '(org-todo-keywords (quote ((sequence "TODO" "NEXT(n!)" "STARTED(s!/@)" "DEFERRED(f!/@)" "DELEGATED(l@/@)" "WAITING(w@/@)" "|" "CANCELED(x@)" "DONE(d@)"))))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(recentf-mode t)
 '(scroll-bar-mode (quote right))
 '(sh-basic-offset 8)
 '(sh-indent-comment t)
 '(sh-indent-for-case-alt (quote +))
 '(sh-indent-for-case-label 0)
 '(sh-indent-for-continuation 4)
 '(sh-indentation 8)
 '(show-paren-mode 1)
 '(sr-speedbar-max-width 20)
 '(sr-speedbar-right-side nil)
 '(transient-mark-mode 1)
 '(user-full-name "Christopher M. Fuhrman")
 '(user-mail-address "cfuhrman@pobox.com")
 '(view-calendar-holidays-initially nil)
 '(weather-metno-unit-name (quote (("celcius" . "°C"))))
 '(which-function-mode t)
 )


;; Miscellany
;; --------------------------------------------------------------------

;; Additional paths to load
(nice-load-path local-loadpaths)
(nice-exec-path local-execpaths)


;; Library evaluations
;; --------------------------------------------------------------------

(eval-after-load "auto-complete"
  '(progn
     (require 'auto-complete-config)
     (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
     (ac-config-default)
     (ac-set-trigger-key "TAB")
     (ac-set-trigger-key "<tab>")
     (if (equal ac-ispell-present t)
         (ac-ispell-setup))))

(eval-after-load "dired"
  '(progn
     ;; Use Emacs ls(1) emulation
     (setq ls-lisp-use-insert-directory-program nil)
     (if (equal window-system 'ns)
         (setq default-directory (concat (getenv "HOME") "/")))
     ))

(eval-after-load "multi-web-mode"
  '(progn
     ;; Set up multi-web-mode
     (setq mweb-default-major-mode 'html-mode)
     (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                       (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                       (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
     (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
     (multi-web-global-mode 1)
     ))

;; php-auto-yasnippets attempts to use php-mode *before* it has had a
;; chance to compile, so only append php-auto-yasnippets to the
;; package list until after we know it's been loaded.
(eval-after-load "php-mode"
  '(progn
     (add-to-list 'my-package-list 'php-auto-yasnippets t)
     ))

(eval-after-load "smart-mode-line"
  '(progn
     (setq custom-safe-themes (quote ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" default)))
     (sml/apply-theme 'respectful)
     ))

(eval-after-load "solarized-theme"
  '(progn
     (if (window-system)
         (load-theme 'solarized-dark t))
     ))

(eval-after-load "vc-fossil"
  '(progn
     ;; Add Fossil support to VC
     (add-to-list 'vc-handled-backends 'Fossil)
     ))

(eval-after-load "yasnippet"
  '(progn
     (require 'setup-yasnippet)
     ))


;; Post-init processing
;; --------------------------------------------------------------------

;; Load post-init packages
(add-hook 'after-init-hook
          #'(lambda ()
	      (if (equal yasnippet-present t)
                  (require 'yasnippet))
              (if (equal auto-complete-present t)
                  (require 'auto-complete))
              (if (equal flymake-mode-present t)
                  ((require 'flymake)
                   (require 'flymake-php)
                   (require 'flymake-shell)))
              (if (equal geben-present t)
                  (autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t))
              (if (equal indent-guide-present t)
                  (require 'indent-guide))
              (if (equal multi-web-present t)
                  (require 'multi-web-mode))
              (if (equal org-bullets-present t)
                  (require 'org-bullets))
              (if (equal payas-mode-present t)
                  (require 'php-auto-yasnippets))
              (if (equal vc-fossil-present t)
                  (require 'vc-fossil))
              (if (equal xlicense-present t)
                  (require 'xlicense))
              (if (equal smart-mode-line-present t)
                  (sml/setup))
              (if (equal solarized-theme-present t)
                  (load-theme 'solarized-dark t))
              (require 'sr-speedbar)))

;; Ende
