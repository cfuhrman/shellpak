;;; init.el --- Personal customization
;; ====================================================================
;;;
;; Copyright (c) 2008, 2016, 2021 Christopher M. Fuhrman
;; All rights reserved
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Thu Feb 29 17:16:39 2008 PST
;;
;; ====================================================================

;;; Commentary:
;;
;;   GNU Emacs-compatible initialization file defining a number of
;;   coding styles and preferences
;;

;;; Code:

;; This will let Emacs load faster by reducing the number of times we
;; must garbage collect.  The default is 800 kilobytes.
;;
;; LATER: Check out https://gitlab.com/koral/gcmh to see if that
;;        further impacts performance
(setq gc-cons-threshold (* 50 1000 1000))

;; Update load paths
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/thirdparty")

;; Set exec path
(dolist (path '("~/.composer/vendor/bin"
                "~/bin"
                "~/go/bin"
                "~/perl5/bin"
                "~/vendor/bin"
                "/Library/TeX/texbin"
                "/usr/local/bin"
                "/usr/pkg/bin"))
  (if (file-directory-p path)
      (progn
        (add-to-list 'exec-path path)
        (setenv "PATH" (concat (getenv "PATH") ":" path))))
  )


;; VARIABLES
;;
;; Contains all global variables, including custom-set-variables.
;; --------------------------------------------------------------------

;; Variable definitions
(defvar cmf/full-name "Christopher M. Fuhrman")
(defvar cmf/mail-address "cfuhrman@example.com")
(defvar cmf/latitude ####)		; Determine your lat/long at maps.google.com
(defvar cmf/longitude ####)
(defvar cmf/location-name "San Jose, CA")
(defvar cmf/time-zone "America/Los Angeles")
(defvar cmf/time-zone-short-name "PST")
(defvar cmf/time-zone-short-name-daylight "PDT")

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((c-mode        . "cmf")
     (c++-mode        . "cmf")
     (objc-mode . "cmf")
     (java-mode . "cmf")
     (awk-mode        . "awk")
     (other        . "cmf"))))
 '(calendar-christian-all-holidays-flag t)
 '(calendar-daylight-time-zone-name cmf/time-zone-short-name-daylight)
 '(calendar-latitude cmf/latitude)
 '(calendar-location-name cmf/location-name)
 '(calendar-longitude cmf/longitude)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name cmf/time-zone-short-name)
 '(calendar-time-zone -480)
 '(calendar-view-holidays-initially-flag nil)
 '(comment-fill-column 80)
 '(column-number-mode t)
 '(comment-multi-line t)
 '(comment-style (quote indent))
 '(custom-safe-themes
   '("fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" default))
 '(display-time-mode t)
 '(emerge-combine-versions-template "
%b
%a
")
 '(global-prettify-symbols-mode t)
 '(log-edit-hook '(log-edit-show-files))
 ;; '(safe-local-variable-values '(buffer-auto-save-file-name))
 '(recentf-mode t)
 '(safe-local-variable-values
   '((org-latex-pdf-process "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f %f" "bibtex %b")))
 '(sh-indent-comment t)
 '(sh-indent-for-case-alt (quote +))
 '(sh-indent-for-case-label 0)
 '(sh-indent-for-continuation 4)
 '(sh-indentation 8)
 '(show-paren-mode t)
 '(transient-mark-mode t)
 '(which-function-mode t)
 '(user-full-name cmf/full-name)
 '(user-mail-address cmf/mail-address)
 )

(if (window-system)
    (tool-bar-mode -1)
  )


;; GENERAL CONFIGURATION
;;
;; Contains general emacs configuration, including set up of package.el
;; --------------------------------------------------------------------

;; Originally taken from
;; https://github.com/daviwil/emacs-from-scratch/blob/master/init.el
(defun cmf/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Uncomment to enable
;; (add-hook 'emacs-startup-hook #'cmf/display-startup-time)

;; package.el installation and configuration
(require 'package)

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/"))
      )
(setq package-check-signature nil)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Only enable for debugging
(setq use-package-verbose nil)


;; BEHAVIOR
;;
;; Section containing macros and packages that change default
;; behavior.
;; --------------------------------------------------------------------

;; Macros
(fset 'yes-or-no-p
      'y-or-n-p)

(fset 'align-on-equal
      [?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g tab return ?= return])

(fset 'align-on-hash-arrow
      [?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g tab return ?= ?> return])

;; Custom key bindings for commonly used commands
(global-set-key [?\C-x ?\C-k ?0] 'normal-erase-is-backspace-mode)
(global-set-key [?\C-x ?\C-k ?1] 'delete-trailing-whitespace)
(global-set-key [?\C-x ?\C-k ?2] 'align-on-equal)
(global-set-key [?\C-x ?\C-k ?3] 'align-on-hash-arrow)

;; Mode customization
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'footnote-mode)
(add-hook 'text-mode-hook
          (lambda ()
            (setq fill-column 80)))

;; Functions
(defun cmf/choose-line-number-mode-hook ()
  "Determine line number mode to use based on Emacs version."
  (if (version< emacs-version "26.1")
      (linum-mode t)
    (display-line-numbers-mode t))
  )

;; GNU/Linux systems typically have GNU coreutils installed, so pass
;; the --dired flag to ls since GNU ls supports that
(if (eq system-type 'gnu/linux)
    (setq dired-listing-switches "-alh --dired")
  (setq dired-listing-switches "-alh")
  )

;; Packages
(use-package ace-window
  :ensure t
  :pin melpa-stable

  :bind ("C-x o" . ace-window)

  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(use-package counsel
  :ensure t
  :pin melpa-stable
  :after swiper
  :diminish

  :bind (
         ("M-x"         . counsel-M-x)
         ("C-x C-f"     . counsel-find-file)
         ("M-y"         . counsel-yank-pop)
         ("<f1> f"      . counsel-describe-function)
         ("<f1> v"      . counsel-describe-variable)
         ("<f1> l"      . counsel-find-library)
         ("<f2> i"      . counsel-info-lookup-symbol)
         ("<f2> u"      . counsel-unicode-char)
         ("<f2> j"      . counsel-set-variable)
         ("C-x b"       . counsel-switch-buffer)
         ("M-i"         . counsel-imenu)

         ;; Ivy-based interface to shell and system tools
         ("C-c c"       . counsel-compile)
         ("C-c g"       . counsel-git)
         ("C-c j"       . counsel-git-grep)
         ("C-c L"       . counsel-git-log)
         ("C-c k"       . counsel-rg)
         ("C-c m"       . counsel-linux-app)
         ("C-x l"       . counsel-locate)
         ("C-c J"       . counsel-file-jump)
         ("C-S-o"       . counsel-rhythmbox)
         ("C-c w"       . counsel-wmctrl)

         ;; Other commands
         ("C-c b"       . counsel-bookmark)
         ("C-c d"       . counsel-descbinds)
         ("C-c o"       . counsel-outline)
         ("C-c t"       . counsel-load-theme)
         ("C-c F"       . counsel-org-file)
         )

  :custom
  (counsel-preselect-current-file t)

  :config
  (counsel-mode t)
  )

(use-package ivy
  :ensure t
  :pin melpa-stable
  :diminish

  :bind (
         ;; Ivy-based interface to standard commands
         ("C-c v"       . ivy-push-view)
         ("C-c V"       . ivy-pop-view)


         ;; Ivy-resume and other commands
         ("C-c C-r"     . ivy-resume)
         )

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-count-format "(%d/%d) ")

  :init
  (ivy-mode t)

  :config
  (use-package ivy-rich
    :ensure t

    :init
    (ivy-rich-mode t)
    )
  )

(use-package swiper
  :ensure t
  :pin melpa-stable
  :after ivy

  :bind (
         ("C-s"         . swiper)
         ("C-r"         . swiper-backward)
         )

  :init
  (ivy-mode t)
  )

;;
;; Operating system specific code
;;

(when (eq system-type 'gnu/linux)
  (setq trash-directory "~/.local/share/Trash/files/emacs"
        delete-by-moving-to-trash t
        )

  ;; Make sure backup directory exists
  (if (not (file-exists-p trash-directory))
      (make-directory trash-directory t))
  )

(when (eq system-type 'darwin)
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs"
        )

  (if (eq window-system nil)
      (normal-erase-is-backspace-mode 0)
    )

  (use-package osx-location
    :ensure t

    :hook
    (osx-location-changed .
                          (lambda ()
                            (setq calendar-latitude osx-location-latitude
                                  calendar-longitude osx-location-longitude
                                  calendar-location-name (format "%s, %s" osx-location-latitude osx-location-longitude)))
                          )

    :config
    (osx-location-watch)
    )
  )


;; APPEARANCE
;;
;; Section for controlling the appearance of Emacs, including theme
;; installation.
;; --------------------------------------------------------------------

;; Packages
(use-package all-the-icons
  :ensure t
  :pin melpa-stable
  :if window-system

  :config

  ;; Install the font files only if we have not already done so
  (defvar cmf/fonts-installed-file "~/SHELL/emacs.d/.icon-fonts-installed")
  (unless (file-exists-p cmf/fonts-installed-file)
    (progn
      (message "Installing icon fonts")
      (all-the-icons-install-fonts t)
      (with-temp-buffer (write-file cmf/fonts-installed-file)))
    )
  )

;; all-the-icons-dired
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :if window-system

  :init
  (require 'font-lock+)

  :hook (dired-mode . all-the-icons-dired-mode)

  :diminish all-the-icons-dired-mode
  )

(use-package all-the-icons-ivy
  :ensure t
  :if window-system
  :pin melpa-stable
  :after (ivy all-the-icons)

  :config
  (all-the-icons-ivy-setup)

  (use-package all-the-icons-ivy-rich
    :ensure t
    :if window-system

    :init
    (all-the-icons-ivy-rich-mode t)
    )
  )

(use-package doom-modeline
  :ensure t

  :hook (after-init . doom-modeline-mode)

  :custom
  (doom-modeline-vcs-max-length 14)
  )

(use-package doom-themes
  :ensure t

  :config
  (load-theme 'doom-nord)
  )

(use-package emojify
  :ensure t
  :pin melpa-stable
  :if window-system

  :custom
  (emojify-emoji-styles '(unicode))

  :config
  (global-emojify-mode)
  )


;; EXTENSIONS
;;
;; Section for adding functionality to Emacs, such as version-control
;; or spell-checking functionality.
;; --------------------------------------------------------------------

;; Packages
(use-package flyspell
  :ensure t

  :bind (:map flyspell-mode-map
              ("C-c ;" . flyspell-correct-wrapper))

  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))

  :config
  (setq-default ispell-program-name "aspell")
  (use-package flyspell-correct-ivy
    :ensure t
    :pin melpa
    )

  :custom
  (ispell-extra-args (quote ("--run-together")))
  )

(use-package helpful
  :ensure t
  :pin melpa-stable
  :commands (helpful-callable helpful helpful-command helpful-key)

  :bind
  ([remap describe-function]    . counsel-helpful-function)
  ([remap describe-symbol]      . helpful-symbol)
  ([remap describe-variable]    . counsel-helpful-variable)
  ([remap describe-command]     . helpful-command)
  ([remap describe-key]         . helpful-key)

  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  )

(use-package log-edit
  ;; This is a built-in mode
  :mode (
         ("COMMIT.*"   . log-edit-mode)
         ("ci-comment" . log-edit-mode)
         ("bzr_log\\." . log-edit-mode)
         ("cvs*"       . log-edit-mode)
         ("pico\\."    . log-edit-mode)
         )
  )

(unless (eq (executable-find "git") nil)
  (use-package magit
    :ensure t
    :commands magit-status

    :bind ("C-x g" . magit-status)
    )
  )

(use-package sudo-edit
  :ensure t
  :pin melpa-stable
  :no-require t

  :custom
  (sudo-edit-indicator-mode t)
  )

(use-package treemacs
  :ensure t
  :pin melpa-stable
  :defer t

  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))

  :init
  ;; HACK: Added here so that treemacs won't complain about
  ;;       hl-line-mode's background color for icons under terminal
  ;;       mode
  (defvar treemacs-no-load-time-warnings t)

  :custom
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  :config
  (use-package treemacs-projectile
    :ensure t
    :pin melpa-stable
    :after projectile
    )

  (use-package treemacs-magit
    :ensure t
    :pin melpa-stable
    :after magit
    )
  )

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode

  :hook ((prog-mode .
                    (lambda ()
                      (undo-tree-mode t)))
         (text-mode .
                    (lambda ()
                      (undo-tree-mode t)))
         (conf-mode .
                    (lambda ()
                      (undo-tree-mode t))))

  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  )

(use-package vc-fossil
  :ensure t

  :init
  (require 'vc-fossil)

  :config
  (add-to-list 'vc-handled-backends 'Fossil)
  )

(unless (eq (executable-find "w3m") nil)
  (use-package w3m
    :ensure t
    )
  )

(use-package which-key
  :ensure t
  :pin melpa-stable
  :diminish which-key-mode

  :init
  (which-key-mode)

  :custom
  (which-key-idle-delay 1)
  )


;; ORG-MODE
;;
;; Section for configuring org-mode.
;; --------------------------------------------------------------------

(use-package org
  ;; This is a built-in mode
  :bind (
         ("C-c l" . org-store-link)
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
  (require 'org-install)
  (require 'org-crypt)

  ;; Enable org-crypt as appropriate
  (org-crypt-use-before-save-magic)

  :config
  ;; org-mode specific variables are customized here
  (require 'cmf-org-settings)

  (org-clock-persistence-insinuate)

  ;; NOTE: Testing out this feature
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (if (member system-type '(gnu/linux))
      (setq org-file-apps
            '((auto-mode . emacs)
              ("\\.x?html?\\'" . "xdg-open %s")
              ("\\.pdf\\'" . emacs)
              ("\\.pdf::\\([0-9]+\\)\\'" . "xdg-open \"%s\" -p %1")
              ("\\.pdf.xoj" . "xournal %s")))
    )

  (use-package org-bullets
    :ensure t

    :hook (org-mode . org-bullets-mode)

    :init
    (require 'org-bullets)
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
  )


;; FILE-SPECIFIC MODES
;;
;; Configuration or installation of modes for dealing with specific
;; types of files that are not necessarily programming languages.
;; --------------------------------------------------------------------

(use-package adoc-mode
  :ensure t
  :pin melpa-stable
  :defer t
  )

(use-package apache-mode
  :ensure t
  :defer t
  :no-require t

  :hook (apache-mode . cmf/choose-line-nuber-mode)

  :mode (("\\.htaccess\\'"                   . apache-mode)
         ("access\\.conf\\'"                 . apache-mode)
         ("httpd\\.conf\\'"                  . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)
         ("srm\\.conf\\'"                    . apache-mode))
  )

(use-package auctex
  :ensure t
  :commands (latex-mode LaTeX-mode plain-tex-mode)

  :config
  (use-package company-auctex
    :ensure t
    :no-require t
    :after company

    :hook (auctex-mode .
                       (lambda ()
                         (add-to-list 'company-backends 'company-auctex)))

    :config
    (company-auctex-init)
    )

  (use-package company-bibtex
    :ensure t
    :no-require t
    :after company

    :hook (bibtex-mode .
                       (lambda ()
                         (add-to-list 'company-backends 'company-bibtex)))
    )
  )

(use-package crontab-mode
  :ensure t
  :no-require t

  :mode (("\\.cron\\(tab\\)?\\'" . crontab-mode)
         ("cron\\(tab\\)?\\."    . crontab-mode))
  )

(use-package dockerfile-mode
  :ensure t
  :pin melpa-stable

  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

(use-package json-mode
  :ensure t

  :hook ((json-mode . undo-tree-mode)
         (json-mode . lsp-deferred)
         (json-mode . cmf/choose-line-number-mode-hook))
  )

(use-package markdown-mode
  :ensure t
  :pin melpa-stable

  :commands (markdown-mode gfm-mode)

  :mode (("README\\.md\\'"         . gfm-mode)
         ("\\.md\\'"               . markdown-mode)
         ("\\.markdown\\'"         . markdown-mode))

  :init
  (setq markdown-command "multimarkdown")
  )

(use-package restclient
  :ensure t
  :mode ("\\.rtt\\'" . restclient-mode)

  ;; See https://github.com/kiwanami/emacs-calfw for setting up your
  ;; own calendars
  :config
  (use-package company-restclient
    :ensure t
    :pin melpa-stable
    :after company

    :hook ((restclient-mode . undo-tree-mode)
           (restclient-mode . cmf/choose-line-number-mode-hook)
           (restclient-mode .
                            (lambda ()
                              (add-to-list 'company-backends 'company-restclient))))
    )
  )

(use-package nxml-mode
  ;; This is a built-in mode
  :mode (("\\.xsd\\'"  . xml-mode)
         ("\\.wsdl\\'" . xml-mode))

  :hook (nxml-mode . cmf/choose-line-number-mode-hook)
  )

(use-package yaml-mode
  :ensure t
  :no-require t

  :mode ("\\.sls\\'" . yaml-mode)

  :hook ((yaml-mode . cmf/choose-line-number-mode-hook)
         (yaml-mode .
                    (lambda ()
                      (subword-mode t)
                      (auto-fill-mode -1)))
         )
  )


;; PROGRAMMING
;;
;; Section for configuring programming environment as well as adding
;; programming-specific packages that add functionality.
;; --------------------------------------------------------------------

;; Hook definitions
(add-hook 'prog-mode-hook 'cmf/choose-line-number-mode-hook)
(add-hook 'prog-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (eldoc-mode t)
            (electric-pair-mode t)
            (subword-mode t)
            (setq fill-column 120)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq fill-column 70)))

;; Packages
(use-package company
  :ensure t
  :pin melpa-stable

  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("TAB" . company-complete-selection)
              )

  :hook (emacs-lisp-mode .
                         (lambda ()
                           (add-to-list 'company-backends 'company-capf)))

  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-minimum-width 27)
  (company-idle-delay 0.3)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 3)
  (company-tooltip-flip-when-above t)

  :init
  (global-company-mode)
  )

(use-package company-box
  :ensure t
  :if window-system
  :unless (version< emacs-version "26.1")
  :after company

  :hook (company-mode . company-box-mode)

  :config
  (if (eq window-system 'ns)
      ;; DONT: Company-box and macOS do not behave properly when
      ;; running under full screen mode (at at least under Mojave).
      ;; The same is true regardless if ns-use-native-fullscreen is
      ;; set to nil or not.  For the time being, just notify the user.
      (message "Do not use full-screen mode when running under macOS!")
    )
  )

(use-package company-shell
  :ensure t
  :pin melpa-stable
  :commands shell-mode
  :after company

  :hook (sh-mode .
                 (lambda ()
                   (add-to-list 'company-backends 'company-shell)))

  :custom
  (company-shell-clean-manpage t)
  )

(use-package cperl-mode
  ;; This is a built-in mode
  :mode ("\\.cgi\\'" . cperl-mode)

  :custom
  (cperl-close-paren-offset -4)
  (cperl-continued-statement-offset 4)
  (cperl-electric-keywords t)
  (cperl-electric-linefeed t)
  (cperl-electric-parens nil)
  (cperl-font-lock t)
  (cperl-highlight-variables-indiscriminately t)
  (cperl-indent-level 4)
  (cperl-indent-parens-as-block t)
  (cperl-tab-always-indent t)

  :init
  (if (eq (length(getenv "PERL5LIB")) 0)
      (setenv "PERL5LIB" (concat (getenv "HOME") "/perl5/lib/perl5")))
  (defalias 'perl-mode 'cperl-mode)

  :config
  ;; Functions for perltidy
  (defun cmf/perltidy-region ()
    "Run perltidy on the current region."
    (interactive)
    (save-excursion
      (shell-command-on-region (point) (mark) "perltidy -q" nil t))
    )

  (defun cmf/perltidy-defun ()
    "Run perltidy on the current defun."
    (interactive)
    (save-excursion (mark-defun)
                    (perltidy-region))
    )

  (use-package company-plsense
    :ensure t
    :after company

    :hook (cperl-mode .
                      (lambda ()
                        (add-to-list 'company-backends 'company-plsense)))
    )
  )

(use-package diff-hl
  :ensure t
  :pin melpa-stable

  :config
  (global-diff-hl-mode)
  )

(use-package flycheck
  :ensure t

  :custom
  (flycheck-check-syntax-automatically (quote (idle-change)))
  (flycheck-disabled-checkers (quote (php-phpcs go-build python-flake8 python-pylint)))
  (flycheck-highlighting-mode 'lines)
  (flycheck-idle-change-delay 3)
  (flycheck-phpcs-standard "PSR2")

  :init
  (global-flycheck-mode)
  )

(use-package ggtags
  :ensure t
  :diminish ggtags-mode

  :hook (prog-mode . ggtags-mode)
  )

(use-package go-mode
  :ensure t
  :no-require t
  :pin melpa-stable

  :bind (:map go-mode-map
              ("C-c C-j" . go-direx-pop-to-buffer))

  :config
  (use-package company-go
    :ensure t
    :after company

    :hook (go-mode .
                   (lambda ()
                     (add-to-list 'company-backends 'company-go)))

    :init
    (add-hook 'before-save-hook #'gofmt-before-save)
    )

  (use-package go-complete
    :ensure t

    :hook (completion-at-point-functions . go-complete-at-point)
    )

  (use-package go-direx
    :ensure t
    )

  (use-package go-eldoc
    :ensure t
    :after eldoc

    :hook (go-mode . go-eldoc-setup)
    )

  (use-package go-snippets
    :ensure t
    :after yasnippet
    )
  )

(use-package hl-todo
  :ensure t
  :pin melpa-stable

  :bind (
         ("C-c h p" . hl-todo-previous)
         ("C-c h n" . hl-todo-next)
         ("C-c h o" . hl-todo-occur)
         ("C-c h i" . hl-todo-insert)
         )

  :hook (prog-mode . hl-todo-mode)

  :custom
  (hl-todo-keyword-faces
   (quote
    (("HOLD"            . "#d0bf8f")
     ("TODO"            . "#cc9393")
     ("NEXT"            . "#dca3a3")
     ("THEM"            . "#dc8cc3")
     ("PROG"            . "#7cb8bb")
     ("OKAY"            . "#7cb8bb")
     ("DONT"            . "#5f7f5f")
     ("FAIL"            . "#8c5353")
     ("DONE"            . "#afd8af")
     ("NOTE"            . "#d0bf8f")
     ("KLUDGE"          . "#d0bf8f")
     ("HACK"            . "#d0bf8f")
     ("TEMP"            . "#d0bf8f")
     ("FIXME"           . "#cc9393")
     ("XXX+"            . "#cc9393")
     ("\\?\\?\\?+"      . "#cc9393")
     ("BUG"             . "#8c5353")
     ("LATER"           . "#d0bf8f"))))
  )

(unless (eq (executable-find "npm") nil)
  (use-package lsp-ivy
    :ensure t
    :pin melpa-stable
    :after (ivy lsp)
    )
  )

(use-package jinja2-mode
  :ensure t
  :defer t

  :mode ("\\.j2\\'" . jinja2-mode)
  )

(unless (eq (executable-find "npm") nil)
  (use-package lsp-mode
    :ensure t
    :pin melpa-stable
    :commands (lsp lsp-deferred)

    :custom
    (lsp-prefer-flymake nil)
    (lsp-file-watch-threshold 40000)

    :init
    (setq lsp-keymap-prefix "C-c C-l")

    :config
    (lsp-enable-which-key-integration t)

    (if (window-system)
        (setq lsp-headerline-breadcrumb-icons-enable t)
      (setq lsp-headerline-breadcrumb-icons-enable nil)
      )

    ;; Stanza is required here to make sure lsp-mode is enabled when
    ;; editing Java files
    (unless (eq (executable-find "java") nil)
      (use-package lsp-java
        :ensure t
        :pin melpa-stable

        :config
        (add-hook 'java-mode-hook #'lsp)
        )
      )

    (use-package lsp-treemacs
      :ensure t
      :pin melpa-stable
      :after treemacs

      :bind (:map lsp-mode-map
                  ("C-x t s" . lsp-treemacs-symbols))
      )

    (use-package lsp-ui
      :ensure t

      :hook (lsp-mode . lsp-ui-mode)

      :custom
      (lsp-ui-doc-position 'top)
      )
    )
  )

(use-package make-mode
  ;; This is a built-in mode
  :mode ("[Mm]akefile\\." . makefile-mode)
  )

(use-package php-mode
  :ensure t
  :no-require t

  :bind (:map php-mode-map
              ("C-c -"                . php-current-class)
              ("C-c ="                . php-current-namespace)
              ("C-x p"                . php-insert-doc-block)
              )

  :hook (php-mode . lsp-deferred)

  :custom
  (php-insert-doc-access-tag nil)
  (php-enable-psr2-coding-style)
  (php-lineup-cascaded-calls t)
  (phpcbf-standard "PSR2")

  :config
  (require 'php-doc)
  (require 'phpcbf)

  (use-package company-php
    :ensure t
    :pin melpa-stable
    :after company

    :hook (php-mode .
                    (lambda ()
                      (add-to-list 'company-backends 'company-ac-php-backend
                                   )))

    :config
    (ac-php-core-eldoc-setup)
    )

  (use-package php-eldoc
    :ensure t
    :after eldoc

    :config
    (php-eldoc-enable)
    )

  (use-package phpunit
    :ensure t
    )
  )

(use-package projectile
  :ensure t
  :pin melpa-stable
  :diminish projectile-mode

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-tags-backend 'ggtags)

  :init
  (when (or (file-directory-p "~/dev") (file-directory-p "~/org"))
    (setq projectile-project-search-path '("~/dev" "~/org")))
  (setq projectile-switch-project-action #'projectile-dired)

  :config
  (projectile-mode +1)

  (use-package counsel-projectile
    :ensure t
    :pin melpa-stable
    :after counsel

    :config
    (counsel-projectile-mode)
    )
  )

;; TODO: Customize python development environment

(use-package rainbow-delimiters
  :ensure t

  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package sed-mode
  :ensure t
  :no-require t

  :mode (
         ("*.sed" . sed-mode)
         )
  )

(use-package sh-mode
  ;; This is a built-in mode
  :hook (sh-mode . lsp)
  )

(use-package sql
  ;; This is a built-in mode
  :mode (("/sql[^/]]*" . sql-mode))

  :config
  (use-package sql-indent
    :ensure t
    :diminish sqlind-minor-mode

    :hook (sql-mode .
                    (lambda ()
                      (sqlind-minor-mode t)))

    :custom
    (sql-indent-first-column-regexp
     "^\\s-*\\(create\\|d\\(?:elete\\|rop\\)\\|from\\|group\\|having\\|in\\(?:sert\\|t\\(?:ersect\\|o\\)\\)\\|order\\|se\\(?:\\(?:lec\\)?t\\)\\|truncate\\|commit\\|u\\(?:nion\\|pdate\\)\\|where\\)\\(\\b\\|\\s-\\)")
    )
  )

(use-package yasnippet
  :ensure t
  :pin melpa-stable

  :init
  (yas-global-mode t)

  :config
  (use-package yasnippet-snippets
    :ensure t
    )

  :custom
  (yas-wrap-around-region t)
  )

;;
;; Programming Styles
;;

;; My own coding style
(c-add-style "cmf"
             '("bsd"
               (indent-tabs-mode . nil)
               (c-offsets-alist
                (statement-cont . (first c-lineup-cascaded-calls +)))))


;; CLEAN UP
;; --------------------------------------------------------------------

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
