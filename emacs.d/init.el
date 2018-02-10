;;; init.el --- Personal customizations
;; ====================================================================
;;
;; Copyright (c) 2008, 2016 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Simplified BSD License (also known
;; as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Thu Feb 29 17:16:39 2008 PST
;;
;; ====================================================================

;;; Commentary:
;;
;;   GNU Emacs-compatible initialization file defining a number of
;;   coding styles and preferences
;;
;; TODO:
;;
;;    - The same thing we do every night, Pinky.  Try to take over
;;      *THE WORLD*!
;;

;;; Code:

;; Update load paths
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/thirdparty")


;; Variables
;; --------------------------------------------------------------------

;; Variable definitions
(defvar cmf-full-name "Christopher M. Fuhrman")
(defvar cmf-mail-address "cfuhrman@example.com")
(defvar cmf-latitude ####)              ; Determine your lat/long at maps.google.com
(defvar cmf-longitude ####)
(defvar cmf-location-name "San Jose, CA")
(defvar cmf-time-zone "America/Los Angeles")
(defvar cmf-time-zone-short-name "PST")
(defvar cmf-time-zone-short-name-daylight "PDT")
(defvar cmf-custom "~/.emacs.d/custom.el")
(defvar cmf-custom-16 "~/.emacs.d/custom-nox.el")
(defvar cmf-custom-256 "~/.emacs.d/custom-nox256.el")

;; List of paths to search
(defvar cmf-path-list '("/usr/texbin"
                        "/usr/pkg/bin"
                        "/usr/local/bin"
                        "~/.composer/vendor/bin"
                        "~/go/bin"
                        "/Library/TeX/texbin"))

;; Customize generic variables
(custom-set-variables
 '(c-default-style
   (quote
    ((c-mode . "cmf")
     (c++-mode . "cmf")
     (objc-mode . "cmf")
     (java-mode . "cmf")
     (awk-mode . "awk")
     (other . "cmf"))))
 '(calendar-christian-all-holidays-flag t)
 '(calendar-daylight-time-zone-name cmf-time-zone-short-name-daylight)
 '(calendar-latitude cmf-latitude)
 '(calendar-location-name cmf-location-name)
 '(calendar-longitude cmf-longitude)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name cmf-time-zone-short-name)
 '(calendar-time-zone -480)
 '(calendar-view-holidays-initially-flag nil)
 '(column-number-mode t)
 '(comment-auto-fill-only-comments t)
 '(comment-fill-column 80)
 '(comment-multi-line t)
 '(comment-style (quote indent))
 '(delete-selection-mode nil)
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(dired-use-ls-dired (quote unspecified))
 '(display-time-mode t)
 '(emerge-combine-versions-template "
%b
%a
")
 '(global-hl-line-mode t)
 '(linum-delay t)
 '(log-edit-hook (quote (log-edit-show-files)))
 '(mark-even-if-inactive t)
 '(mark-holidays-in-calendar t)
 '(recentf-mode t)
 '(scroll-bar-mode (quote right))
 '(sh-basic-offset 8)
 '(sh-indent-comment t)
 '(sh-indent-for-case-alt (quote +))
 '(sh-indent-for-case-label 0)
 '(sh-indent-for-continuation 4)
 '(sh-indentation 8)
 '(show-paren-mode 1)
 '(tramp-default-method "ssh")
 '(transient-mark-mode 1)
 '(use-package-concat t)
 '(user-full-name cmf-full-name)
 '(user-mail-address cmf-mail-address)
 )

;; Load the appropriate customization file based on if we are running
;; under a window system, such as "x" or "ns" (Mac OS X or GNUstep)
(if (equal window-system nil)
    (if (or (string-match "term-256" (getenv "TERM"))
            (string-match "screen-256" (getenv "TERM")))
        (setq custom-file cmf-custom-256)
      (setq custom-file cmf-custom-16))
  (setq custom-file cmf-custom)
  )

(load custom-file)


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
	       (indent-tabs-mode . nil)
               (c-offsets-alist
                (statement-cont . (first c-lineup-cascaded-calls +)))))


;; Functions
;; --------------------------------------------------------------------

;; Function for loading exec paths
(defun cmf/update-exec-path (pathlist)
  "Add given PATHLIST to 'exec-path'."
  (dolist (path pathlist)
    (add-to-list 'exec-path path)
    (setenv "PATH" (concat (getenv "PATH") ":" path)))
  )

;; Override stock use-fancy-splash-screens-p ()
(if (version< emacs-version "25.2.1")
    (defun use-fancy-splash-screens-p ()
      "Return t if fancy splash screens should be used."
      (when (and (display-graphic-p)
                 (or (and (display-color-p)
                          (image-type-available-p 'xpm))
                     (image-type-available-p 'pbm)))
        (let ((frame (fancy-splash-frame)))
          (when frame
            (let* ((img (create-image (fancy-splash-image-file)))
                   (image-height (and img (cdr (image-size img nil frame))))
                   ;; We test frame-height so that, if the frame is split
                   ;; by displaying a warning, that doesn't cause the normal
                   ;; splash screen to be used.
                   (frame-height (1- (frame-height frame))))
              ;; The original value added to the `image-height' for the
              ;; test was 19; however, that causes the test to fail on X11
              ;; by about 1.5 -- so use 17 instead.
              (> frame-height (+ image-height 17))))))
      ))

;; Fix for bug involving enriched text mode
(if (version< emacs-version "25.3")
    (eval-after-load "enriched"
      '(defun enriched-decode-display-prop (start end &optional param)
         (list start end))))


;; Hooks
;; --------------------------------------------------------------------

;; Define a nice programming hook
(defun nice-prog-hook ()
  "Enable some sanity for programming source files."
  (if (>= emacs-major-version 23)
      (linum-mode t))
  (auto-fill-mode t)
  (eldoc-mode t)
  (electric-pair-mode t)
  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|BUG\\|LATER\\):" 1 font-lock-warning-face t)))
  )

;; Define a basic hook for sane editing of text documents
(defun nice-text-hook ()
  "Hook for sane editing of text documents."
  (auto-fill-mode t)
  (footnote-mode t)
  )

;; Apply hooks as appropriate
(add-hook 'prog-mode-hook 'nice-prog-hook)
(add-hook 'text-mode-hook 'nice-text-hook)
(add-hook 'text-mode-hook
          (lambda ()
            (set-fill-column 80)))


;; Macros
;; --------------------------------------------------------------------

(fset 'yes-or-no-p
      'y-or-n-p)

(fset 'align-on-equal
   [?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g tab return ?= return])

(fset 'align-on-hash-arrow
   [?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g tab return ?= ?> return])


;; Keyboard Bindings
;; --------------------------------------------------------------------

;; Custom key bindings for commonly used commands
(global-set-key [?\C-x ?\C-k ?0] 'normal-erase-is-backspace-mode)
(global-set-key [?\C-x ?\C-k ?1] 'delete-trailing-whitespace)
(global-set-key [?\C-x ?\C-k ?2] 'align-on-equal)
(global-set-key [?\C-x ?\C-k ?3] 'align-on-hash-arrow)


;; Additional Initializations
;; --------------------------------------------------------------------

;; Update path list
(cmf/update-exec-path cmf-path-list)

;; Fix normal-erase-is-backspace under non-Linux OS
(if (and (equal window-system nil)
         (equal (equal system-type 'gnu/linux) nil))
    (normal-erase-is-backspace-mode 0))

;; Customize font under X
(if (equal window-system 'x)
    (set-face-attribute 'default nil
                        :family  "DejaVu Sans Mono"
                        :foundry 'unknown
                        :slant   'normal
                        :weight  'normal
                        :height   100
                        :width   'normal))

;; Enable Apple Color Emoji
(if (equal window-system 'ns)
    (set-fontset-font
     t 'symbol
     (font-spec :family "Apple Color Emoji") nil 'prepend))

;; Use Emacs ls(1) emulation
(require 'ls-lisp)


;; Package configuration
;; --------------------------------------------------------------------

;;
;; package.el installation and configuration
;;

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/"))
      )
(package-initialize)

;; Boostrap 'diminish' if necessary
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

;; Bootstrap 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;
;; OSX Modes (Mac OS X only)
;;

(when (eq system-type 'darwin)

  ;; Set up mac-specific parameters here
  (setq delete-by-moving-to-trash t
	mac-option-modifier 'meta
	trash-directory "~/.Trash/emacs"
	)

  ;; Make sure backup directory exists
  (if (not (file-exists-p trash-directory))
      (make-directory trash-directory))

  ;; osx-location
  (use-package osx-location
    :ensure t

    :init
    (add-hook 'osx-location-changed-hook
              (lambda ()
                (setq calendar-latitude osx-location-latitude
                      calendar-longitude osx-location-longitude
                      calendar-location-name (format "%s, %s" osx-location-latitude osx-location-longitude)
                      forecast-latitude osx-location-latitude
                      forecast-longitude osx-location-longitude
                      forecast-city (format "%s, %s" osx-location-latitude osx-location-longitude)
                      )
                )
              )

    :config
    (osx-location-watch)

    )
  )

;;
;; Generic Packages
;;
;; For use on all operating systems
;;

;; auto-complete
(use-package auto-complete
  :ensure t
  :ensure ac-ispell
  :ensure fuzzy

  :pin melpa-stable

  :demand

  :bind (:map ac-complete-mode-map
              ("\C-n" . ac-next)
              ("\C-p" . ac-previous)
	      )

  :init
  (add-hook 'text-mode-hook 'ac-ispell-ac-setup)
  (add-hook 'log-mode-hook  'ac-ispell-ac-setup)

  :config
  (ac-config-default)
  (ac-ispell-setup)
  (setq ac-fuzzy-enable 1 )

  :custom
  (ac-dwim t)
  )

;; yasnippet
(use-package yasnippet
  :ensure t
  :ensure yasnippet-snippets
  :pin melpa-stable

  :init
  (yas-global-mode 1)

  :custom
  (yas-wrap-around-region t)
  )

;; popwin
(use-package popwin
  :ensure t

  :config
  (setq display-buffer-function 'popwin:display-buffer)

  :custom
  (popwin-mode t)
  )

;; ace-window
(use-package ace-window
  :ensure t
  :pin melpa-stable

  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  :bind (
         ("C-x o" . ace-window))
  )

;; all-the-icons
(use-package all-the-icons
  :if window-system
  :ensure t
  :pin melpa-stable

  :config

  ;; Install the font files only if we have not already done so
  (defvar cmf-fonts-installed-file "~/SHELL/emacs.d/.icon-fonts-installed")
  (unless (file-exists-p cmf-fonts-installed-file)
    (progn
      (message "Installing icon fonts")
      (all-the-icons-install-fonts t)
      (with-temp-buffer (write-file cmf-fonts-installed-file)))
    )
  )

;; all-the-icons-dired
(use-package all-the-icons-dired
  :if window-system
  :ensure t
  :after all-the-icons

  :config
  (defun nice-all-the-icons-dired-hook ()
    "Hook for disabling font-lock-mode for dired buffers w/ all-the-icons"
    (font-lock-mode 0)
    )

  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (add-hook 'dired-mode-hook 'nice-all-the-icons-dired-hook)
  )

(use-package all-the-icons-ivy
  :if window-system
  :ensure t
  :after all-the-icons
  :pin melpa-stable

  :config
  (all-the-icons-ivy-setup)
  )

;; apache-mode
(use-package apache-mode
  :ensure t
  :defer t
  :no-require t
  :mode (("\\.htaccess\\'"                   . apache-mode)
         ("access\\.conf\\'"                 . apache-mode)
         ("httpd\\.conf\\'"                  . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)
         ("srm\\.conf\\'"                    . apache-mode))

  :init
  (add-hook 'apache-mode-hook 'nice-prog-hook)
  )

;; arjen-grey-theme
(use-package arjen-grey-theme
  :ensure t
  :if window-system

  :init
  (add-to-list 'custom-safe-themes
	       "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223")
  (load-theme 'arjen-grey t)

  :custom-face
  (highlight ((t (:distant-foreground "LightYellow3" :background "DarkOliveGreen"))))
  )

;; auctex
(use-package auctex
  :ensure t
  :no-require t
  )

;; beacon mode
(use-package beacon
  :ensure t
  :pin melpa-stable

  :diminish " 🄱"
  :init
  (require 'beacon)

  :config
  (beacon-mode 1)
  )

;; calfw
(use-package calfw
  :ensure t
  :ensure calfw-gcal
  :ensure calfw-ical
  :ensure calfw-org
  :pin melpa-stable

  :init
  (require 'calfw)
  (require 'calfw-ical)
  (require 'calfw-org)

  ;; See https://github.com/kiwanami/emacs-calfw for setting up your
  ;; own calendars
  :config
  (defun cmf-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer :contents-sources
			      (list
			       (cfw:org-create-source "Green")  ; orgmode source
			       (cfw:ical-create-source "Google Calendar"
						       "https://calendar.google.com/calendar/ical/jsmith%40example.com/private-set-me-up/basic.ics"
						       "Orange")
			       (cfw:ical-create-source "Trips via Kayak"
			       			       "https://www.kayak.com/trips/ical/tf/blahblah/blahblah/calendar.ics"
			       			       "SteelBlue")
    )
  )

;; [c]perl-mode
(use-package cperl-mode
  :mode ("\\.cgi\\'" . cperl-mode)

  :init
  (defalias 'perl-mode 'cperl-mode)
  (add-to-list 'exec-path
	       "~/perl5/bin")

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

  :custom
  (cperl-continued-brace-offset -8)
  (cperl-continued-statement-offset 4)
  (cperl-electric-keywords t)
  (cperl-electric-linefeed t)
  (cperl-electric-parens nil)
  (cperl-font-lock t)
  (cperl-highlight-variables-indiscriminately t)
  (cperl-indent-level 8)
  )

;; crontab-mode
(use-package crontab-mode
  :ensure t
  :no-require t

  :mode (("\\.cron\\(tab\\)?\\'" . crontab-mode)
         ("cron\\(tab\\)?\\."    . crontab-mode))
  )

;; diff-mode
(use-package diff
  :config
  (push '("[Dd]iff"
	  :regexp   t
	  :noselect t
	  :position right
	  :width    0.40)
	popwin:special-display-config)
  )

;; eldoc
(use-package eldoc
  :diminish eldoc-mode " Doc"
  )

;; emojify
(use-package emojify
  ;; While the melpa version of emojify supports an experimental
  ;; version of global-emojify-mode-line-mode, enabling this causes
  ;; issues with cutting & pasting text, so will revisit that option
  ;; when it becomes more stable.
  :ensure t
  :if window-system
  :pin melpa-stable

  :config
  (global-emojify-mode)
  )

;; flycheck
(use-package flycheck
  :ensure t

  :init
  (global-flycheck-mode)

  :custom
  (flycheck-disabled-checkers (quote (php-phpcs go-build)))
  (flycheck-highlighting-mode 'lines)
  (flycheck-phpcs-standard "PSR2")
  )

;; flyspell
(use-package flyspell
  :ensure t
  :ensure flyspell-correct-ivy

  :bind (:map flyspell-mode-map
	      ("C-c ;" . flyspell-correct-previous-word-generic))

  :init
  (require 'flyspell-correct-ivy)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)

  :config
  (setq-default ispell-program-name "aspell")
  (ac-flyspell-workaround)
  )

;; forecast
(if (version< emacs-version "24.4")
    (message "Forecast requires Emacs version 24.4 or later.  Unable to install")
  (use-package forecast
    :ensure t
    :no-require t

    :custom
    (forecast-latitude cmf-latitude)
    (forecast-longitude cmf-longitude)
    (forecast-city cmf-location-name)
    (forecast-country "United States")
    (forecast-units 'us)
    (forecast-api-key "f78b853ac0c972cccf06e75e21afbe9d")
    )
  )

;; geben
(use-package geben
  :ensure t
  :pin melpa-stable
  :no-require t
  )

;; gited - dired for git branches
(use-package gited
  :ensure t
  :no-require t

  :bind (
         ("C-x C-g" . gited-list-branches)
         )

  )

;; ggtags
(use-package ggtags
  :ensure t

  :diminish ggtags-mode " 🄶"

  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (ggtags-mode t)))
  )

;; go-mode
(use-package go-mode
  :ensure t
  :ensure go-autocomplete
  :ensure go-direx
  :ensure go-eldoc
  :ensure go-snippets

  :pin melpa-stable
  :no-require t

  :mode ("\\.go\\'" . go-mode)

  :bind (:map go-mode-map
	      ("C-c C-j" . go-direx-pop-to-buffer))

  :init
  (add-hook 'go-mode-hook 'nice-prog-hook)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook (lambda () (subword-mode 1)))

  :config
  (push '("^\*go-direx:"
	  :regexp    t
	  :position  left
	  :width     0.2
	  :dedicated t
	  :stick     t)
	popwin:special-display-config)
  )

;; highlight-parentheses
(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode

  :config
  (global-highlight-parentheses-mode)
  )

;; html-mode
(use-package html-mode
  :mode ("\\.tmpl\\'" . html-mode)
  )

;; ivy
(use-package ivy
  :ensure counsel
  :ensure swiper
  :diminish ivy-mode

  :bind (
         ("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-load-library)
         ("M-i"     . counsel-imenu)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-x l"   . counsel-locate)
         )

  :init
  (ivy-mode 1)

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-count-format "(%d/%d) ")
  )

;; jinja2-mode
(use-package jinja2-mode
  :ensure t
  :defer t
  :mode ("\\.j2\\'" . jinja2-mode)

  )

;; json-mode
(use-package json-mode
  :ensure t

  :init
  ;; Associated JSON files with nice-prog-hook
  (add-hook 'json-mode-hook 'nice-prog-hook)
  )

;; log-edit-mode
(use-package log-edit
  :mode (
         ("COMMIT.*"   . log-edit-mode)
         ("ci-comment" . log-edit-mode)
         ("bzr_log\\." . log-edit-mode)
         ("pico\\."    . log-edit-mode)
         )

  :init
  (add-hook 'log-edit-mode-hook 'nice-text-hook)
  (add-hook 'log-edit-mode-hook 'turn-on-orgstruct++)
  )

;; magit
(if (version< emacs-version "24.4")
    (message "Magit requires Emacs version 24.4 or later.  Unable to install")
  (use-package magit
    :ensure t

    :bind
    ("C-x g" . magit-status)
    )
  )

;; make-mode
(use-package make-mode
  :mode ("[Mm]akefile\\." . makefile-mode)
  )

;; nxml-mode
(use-package nxml-mode
  :ensure auto-complete-nxml

  :mode (("\\.xsd\\'"  . xml-mode)
         ("\\.wsdl\\'" . xml-mode))

  :config
  ;; Under normal circumstances, I would add nice-prog-hook to nxml
  ;; mode and be done with it.  The issue here is that nice-prog-hook
  ;; includes enabling auto-fill-mode, which wrecks havoc when editing
  ;; XML files at work.  So, create my own hook explicitly disabling
  ;; auto-fill-mode
  (defun nice-nxml-hook ()
    "Hook for sane editing of XML files."
    (if (>= emacs-major-version 23)
	(linum-mode t))
    (auto-fill-mode -1)
    )

  ;; Apply hook
  (add-hook 'nxml-mode-hook 'nice-nxml-hook)

  ;; Keystroke for popup help about something at point.
  (setq auto-complete-nxml-popup-help-key "C-:")

  ;; Keystroke for toggle on/off automatic completion.
  (setq auto-complete-nxml-toggle-automatic-key "C-c C-t")
  )

;; org-mode
(use-package org
  :ensure org-ac
  :ensure org-bullets
  :ensure ox-twbs

  :bind (
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)
         )

  :init
  (require 'org-install)
  (require 'org-bullets)

  :config

  ;; Org-mode settings are kept in a different file
  (require 'cmf-org-settings)
  ;; Define a basic hook for sane editing of org-mode documents
  (defun nice-org-hook ()
    "Hook for sane editing of 'org-mode' documents."
    (org-defkey org-mode-map "\C-c[" 'org-time-stamp-inactive)
    (org-ac/setup-current-buffer)
    (if (equal (car (split-string org-version ".")) 8)
        (require 'ox-md))
    )

  ;; Persist org-clock appropriately
  (org-clock-persistence-insinuate)

  ;; Org-mode hooks
  (add-hook 'org-mode-hook 'nice-text-hook)
  (add-hook 'org-mode-hook 'nice-org-hook)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; org-gcal
;;
;; LATER: When org-gcal is compliant with version 8.2.10 (Default
;; under Emacs 25.3)

;; php-mode
(use-package php-mode
  :ensure t
  :ensure php-auto-yasnippets
  :ensure php-eldoc
  :ensure php-extras
  :ensure phpcbf
  :ensure phpunit

  :no-require t

  :mode ("\\.php\\'" . php-mode)

  :bind (:map php-mode-map
	      ("C-c C--" . php-current-class)
	      ("C-c C-=" . php-current-namespace)
	      ("C-c C-y" . 'yas/create-php-snippet)
	      ("C-x p"   . php-insert-doc-block)
	      )

  :init
  (require 'php-doc)
  (require 'php-ext)
  (add-hook 'php-mode-hook 'nice-prog-hook)
  (add-hook 'php-mode-hook (lambda () (subword-mode 1)))

  :config
  ;; Load php documentor
  (load-file "~/.emacs.d/thirdparty/phpdocumentor.el")

  :custom
  (php-mode-coding-style (quote psr2))
  ;; (php-lineup-cascaded-calls t)
  (phpcbf-standard "PSR2")
  )

;; pretty-lambdada
(if (version< emacs-version "24.4")
    (use-package pretty-lambdada
      :ensure t

      :init
      (require 'pretty-lambdada)

      :config
      (pretty-lambda-for-modes)
      )
  ;; Otherwise use built-in prettify mode
  (global-prettify-symbols-mode t)
  )

;; ruby-mode
(use-package ruby-mode
  :init
  (add-hook 'ruby-mode-hook 'nice-prog-hook)
  )

;; sed-mode
(use-package sed-mode
  :ensure t
  :no-require t

  :mode (
         ("*.sed" . sed-mode)
         )
  )

;; smart-mode-line
(use-package smart-mode-line
  :ensure t

  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)

  :config
  (if (equal window-system nil)
      (progn
	(sml/apply-theme 'dark)
	(setq sml/read-only-char " 🔒"))
    (progn
      (sml/apply-theme 'dark)
      (setq sml/read-only-char " 🅛"))
    (sml/apply-theme 'respectful))

  :custom
  (sml/modified-char "★")
  )

;; sql
(use-package sql
  :mode (
         ("sql*" . sql-mode))

  :init
  (add-hook 'sql-mode-hook 'nice-prog-hook)
  )

;; sudo-edit
(use-package sudo-edit
  :ensure t
  :no-require t
  )

;; twilight-theme
(use-package twilight-theme
  :ensure t
  :if (equal custom-file cmf-custom-256)
  :init
  (load-theme 'twilight t)

  :custom-face
  (cfw:face-toolbar-button-off ((t (:foreground "grey65" :weight bold))))
  )

;; twittering-mode
(use-package twittering-mode
  :ensure t
  :no-require t

  :init
  (add-hook 'twittering-edit-mode-hook 'nice-text-hook)

  :config
  (unless (equal window-system nil)
    (setq twittering-icon-mode t)
    )

  :custom
  (twittering-display-remaining t)
  (twittering-timer-interval 900)
  (twittering-tinyurl-service (quote tinyurl))
  (twittering-use-icon-storage t)
  (twittering-use-master-password t)
  (twittering-username "wulflock")
  )

;; undo-tree
(use-package undo-tree
  :ensure t
  :diminish " 🆃"

  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (undo-tree-mode t)))
  (add-hook 'text-mode-hook
            (lambda ()
              (undo-tree-mode t)))

  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  )


;; vc-fossil
(use-package vc-fossil
  :ensure t
  :init
  (require 'vc-fossil)

  :config
  (add-to-list 'vc-handled-backends 'Fossil)
  )

;; which-func
(use-package which-func
  :no-require t

  :custom
  (which-function-mode t)
  )

;; which-key
(use-package which-key
  :ensure t
  :pin melpa-stable

  :config
  (which-key-mode)
  )

;; with-editor
(use-package with-editor
  :no-require t

  :init
  (add-hook 'with-editor-mode-hook 'nice-text-hook)
  (add-hook 'with-editor-mode-hook 'turn-on-orgstruct++)
  )

;; xkcd
(use-package xkcd
  :ensure t
  :if window-system
  :no-require t
  )

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :no-require t

  :mode ("\\.sls\\'" . yaml-mode)
  :init
  (add-hook 'yaml-mode-hook 'prog-mode-hook)
  )

;; Miscellany
;; --------------------------------------------------------------------

;; Diminished modes that we have to put here as :diminish doesn't seem
;; to want to work :/
(diminish 'yas-minor-mode " 🅈")

;; Do some byte recompilation
(byte-recompile-directory (expand-file-name "~/.emacs.d/thirdparty") 0)
(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)

;;; init.el ends here
