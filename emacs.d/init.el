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

;; List of paths to search
(defvar cmf-path-list '("/usr/texbin"
                        "/usr/pkg/bin"
                        "/usr/local/bin"
                        "~/.composer/vendor/bin"
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
 '(cperl-continued-brace-offset -8)
 '(cperl-continued-statement-offset 4)
 '(cperl-electric-keywords t)
 '(cperl-electric-linefeed t)
 '(cperl-electric-parens nil)
 '(cperl-font-lock t)
 '(cperl-highlight-variables-indiscriminately t)
 '(cperl-indent-level 8)
 '(custom-enabled-themes (quote (twilight)))
 '(delete-selection-mode nil)
 '(diff-switches "-u")
 '(dired-listing-switches "-alh")
 '(dired-use-ls-dired (quote unspecified))
 '(display-time-mode t)
 '(emerge-combine-versions-template "
%b
%a
")
 '(flycheck-disabled-checkers (quote (php-phpcs)))
 '(flycheck-highlighting-mode 'lines)
 '(flycheck-phpcs-standard "PSR2")
 '(forecast-latitude cmf-latitude)
 '(forecast-longitude cmf-longitude)
 '(forecast-city cmf-location-name)
 '(forecast-country "United States")
 '(forecast-units 'us)
 '(forecast-api-key "set-your-own-key") ; Get your own key from https://developer.forecast.io/
 '(global-hl-line-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-height 10)
 '(ivy-count-format "(%d/%d) ")
 '(linum-delay t)
 '(log-edit-hook (quote (log-edit-show-files)))
 '(mac-option-modifier 'meta)
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
 '(sml/modified-char "★")
 '(sml/read-only-char "🔒")
 '(tramp-default-method "ssh")
 '(transient-mark-mode 1)
 '(twittering-display-remaining t)
 '(twittering-timer-interval 900)
 '(twittering-tinyurl-service (quote tinyurl))
 '(twittering-use-icon-storage t)
 '(twittering-use-master-password t)
 ;; '(twittering-username "your_twitter_account_here")
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(use-package-concat t)
 '(user-full-name cmf-full-name)
 '(user-mail-address cmf-mail-address)
 '(which-function-mode t)
 '(yas-snippet-dirs '("~/.emacs.d/snippets"))
 )

;; Load the appropriate customization file based on if we are running
;; under a window system, such as "x" or "ns" (Mac OS X or GNUstep)
(if (equal window-system nil)
    (if (or (string-match "term-256" (getenv "TERM"))
            (string-match "screen-256" (getenv "TERM")))
        (setq custom-file "~/.emacs.d/custom-nox256.el")
      (setq custom-file "~/.emacs.d/custom-nox.el"))
  (setq custom-file "~/.emacs.d/custom.el"))

(load custom-file)


;; Programming styles
;; --------------------------------------------------------------------

;; Shipwire Coding Style
(c-add-style "psr2"
            '("psr2"
              (c-offsets-alist . (
                                  (statement-cont . (first c-lineup-cascaded-calls +))
                                  ))))

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

;; Function for loading exec paths
(defun cmf/update-exec-path (pathlist)
  "Add given PATHLIST to 'exec-path'."
  (dolist (path pathlist)
    (add-to-list 'exec-path path)
    (setenv "PATH" (concat (getenv "PATH") ":" path)))
  )


;; Defined modes
;; --------------------------------------------------------------------

(define-derived-mode nice-msg-mode
  text-mode "Nice"
  "Major mode for editing text documents."
  )

;; Apply hooks as appropriate
(add-hook 'nice-msg-mode-hook 'nice-text-hook)
(add-hook 'nice-msg-mode-hook 'turn-on-orgstruct++)
(add-hook 'nice-msg-mode-hook 'ac-emoji-setup)


;; Hooks
;; --------------------------------------------------------------------

;; Define a nice programming hook
(defun nice-prog-hook ()
  "Enable some sanity for programming source files."
  (if (>= emacs-major-version 23)
      (linum-mode t))
  (if (package-installed-p 'ggtags)
      (ggtags-mode t))
  (if (package-installed-p 'indent-guide)
      (indent-guide-mode t))
  (if (package-installed-p 'undo-tree)
      (undo-tree-mode t))
  (auto-fill-mode t)
  (eldoc-mode t)
  (electric-pair-mode t)
  )

;; Define a basic hook for sane editing of text documents
(defun nice-text-hook ()
  "Hook for sane editing of text (ASCII) documents."
  (auto-fill-mode t)
  (flyspell-mode t)
  (footnote-mode t)
  (if (package-installed-p 'undo-tree)
      (undo-tree-mode t))
  (if (package-installed-p 'auto-complete)
      (auto-complete-mode t))
  )

;; Aliases
;; --------------------------------------------------------------------

(defalias 'perl-mode 'cperl-mode)


;; Macros
;; --------------------------------------------------------------------

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

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Bootstrap 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Packages loaded through external files
(require 'cmf-org-settings)

;;
;; OSX Modes (Mac OS X only)
;;

(when (eq system-type 'darwin)

  ;; osx-location
  (use-package osx-location
    :ensure t

    :config
    (osx-location-watch)
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
    )

  ;; osx-lib
  (use-package osx-lib
    :ensure t
    )

  ;; osx-trash
  (use-package osx-trash
    :ensure t

    :init
    (setq delete-by-moving-to-trash t)

    :config
    (osx-trash-setup)
    )
  )

;;
;; Generic Packages
;;
;; For use on all operating systems
;;

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
  :config
  (add-hook 'apache-mode-hook 'nice-prog-hook)
  )

;; auto-complete
(use-package auto-complete
  :ensure auto-complete
  :ensure ac-emoji
  :ensure ac-ispell

  :init
  (require 'auto-complete)

  :config
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>")
  (if (package-installed-p 'ac-ispell)
      (ac-ispell-setup))
  )

;; auto-compile
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; beacon mode
(use-package beacon
  :ensure t

  :init
  (require 'beacon)

  :config
  (beacon-mode 1)
  )

;; cc-mode
(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook 'nice-prog-hook)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\|TODO\\|BUG\\|LATER\\):" 1 font-lock-warning-face t)))))
  )

;; [c]perl-mode
(use-package cperl-mode
  :config
  (add-hook 'cperl-mode-hook 'nice-prog-hook)
  (add-hook 'cperl-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\|TODO\\|BUG\\|LATER\\):" 1 font-lock-warning-face t)))))

  :mode ("\\.cgi\\'" . cperl-mode)
  )

;; crontab-mode
(use-package crontab-mode
  :ensure t
  :defer t
  :mode (("\\.cron\\(tab\\)?\\'" . crontab-mode)
         ("cron\\(tab\\)?\\."    . crontab-mode))
  )

;; csv-nav-mode
(use-package csv-nav-mode
  :ensure csv-nav
  :defer t
  :mode ("\\.csv\\'" . csv-nav-mode)
  )

;; darktooth-theme
(if (not(equal window-system nil))
    (use-package darktooth-theme
      :ensure t

      :init
      (load-theme 'darktooth t)
      )
  )

;; eldoc
(use-package eldoc
  :diminish eldoc-mode " Edoc"
  )

;; flycheck
(use-package flycheck
  :ensure t
  :defer t
  :config
  (add-hook 'after-init-hook  #'global-flycheck-mode)
  )

;; flyspell
(use-package flyspell
  :config
  (setq-default ispell-program-name "aspell")
  )

;; forecast
(if (version< emacs-version "24.4")
    (message "Forecast requires Emacs version 24.4 or later.  Unable to install")
  (use-package forecast
    :ensure t
    :defer t
    )
  )

;; ggtags
(use-package ggtags
  :ensure t
  :defer t
  :diminish ggtags-mode " G"
  )

;; go-mode
(use-package go-mode
  :no-require t

  :ensure go-mode
  :ensure go-autocomplete
  :ensure go-direx
  :ensure go-eldoc
  :ensure go-mode
  :ensure go-snippets

  :defer t

  :mode ("\\.go\\'" . go-mode)

  :config
  (add-hook 'go-mode-hook 'nice-prog-hook)
  )

;; html-mode
(use-package html-mode
  :mode ("\\.tmpl\\'" . html-mode)
  )

;; indent-guide
(use-package indent-guide
  :ensure t
  :defer t
  :diminish indent-guide-mode
  )

;; ivy-mode
(use-package ivy-mode
  :ensure counsel
  :ensure swiper

  :bind (
         ("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-load-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-x l"   . counsel-locate)
         )

  :init
  (ivy-mode 1)
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

  :config
  ;; Associated JSON files with nice-prog-hook
  (add-hook 'json-mode-hook 'nice-prog-hook)
  )

;; lisp-mode
(use-package lisp-mode
  :config
  ;; Define a basic hook for Emacs lisp files
  (defun nice-elisp-hook ()
    "Hook for sane editing of Lisp files."
    (setq indent-tabs-mode nil)
    )

  (add-hook 'emacs-lisp-mode-hook 'nice-prog-hook)
  (add-hook 'emacs-lisp-mode-hook 'nice-elisp-hook)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\|TODO\\|BUG\\|LATER\\):" 1 font-lock-warning-face t)))))
  )

;; log-edit-mode
(use-package log-edit
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

  :config
  ;; Define a basic hook for sane editing of makefiles
  (defun nice-makefile-hook ()
    "Hook for sane editing of Makefiles."
    (if (>= emacs-major-version 23)
        (linum-mode t))
    )

  (add-hook 'makefile-mode-hook 'nice-makefile-hook)
  )

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :defer t
  )

;; nxml-mode
(use-package nxml-mode
  :mode (("\\.xsd\\'"  . xml-mode)
         ("\\.wsdl\\'" . xml-mode))

  :config
  ;; Define a nice hook for editing nXML files
  (defun nice-nxml-hook ()
    "Hook for sane editing of nXML files."
    (if (>= emacs-major-version 23)
        (linum-mode t))
    (if (package-installed-p 'indent-guide)
        (indent-guide-mode t))
    (auto-fill-mode -1)
    )

  (add-hook 'nxml-mode-hook 'nice-nxml-hook)
  )

;; org-mode
(use-package org
  :ensure org-ac
  :ensure org-bullets
  :ensure ox-twbs

  :defer t

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
  ;; Define a basic hook for sane editing of org-mode documents
  (defun nice-org-hook ()
    "Hook for sane editing of 'org-mode' documents."
    (org-defkey org-mode-map "\C-c[" 'org-time-stamp-inactive)
    (org-ac/setup-current-buffer)
    (if (equal (car (split-string org-version ".")) 8)
        (require 'ox-md))
    (if (package-installed-p 'undo-tree)
        (undo-tree-mode t))
    )

  ;; Persist org-clock appropriately
  (org-clock-persistence-insinuate)

  ;; Org-mode hooks
  (add-hook 'org-mode-hook 'nice-text-hook)
  (add-hook 'org-mode-hook 'nice-org-hook)
  (add-hook 'org-mode-hook 'ac-emoji-setup)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

;; php-mode
(use-package php-mode
  :ensure ac-php
  :ensure php-mode
  :ensure php-auto-yasnippets
  :ensure php-eldoc
  :ensure php-extras
  :ensure phpcbf
  :ensure phpunit

  :defer t
  :mode ("\\.php\\'" . php-mode)

  :config
  ;; Define a nice hook for editing php files
  (defun nice-php-hook()
    "Hook for sane editing of PHP files."
    (c-set-style "psr2")
    (if (package-installed-p 'php-auto-yasnippets)
        (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet))
    (define-key php-mode-map (kbd "C-x p") 'phpdoc)
    )

  ;; For PHP files
  (add-hook 'php-mode-hook 'nice-php-hook)
  (add-hook 'php-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\|TODO\\|BUG\\|LATER\\)" 1 font-lock-warning-face t)))))

  ;; Load phpdocumentor
  (load-file "~/.emacs.d/thirdparty/phpdocumentor.el")

  )

;; pretty-lambdada
(if (version< emacs-version "24.4")
    (use-package pretty-lambdada
      :ensure t
      :defer t

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

;; smart-mode-line
(use-package smart-mode-line
  :ensure t

  :init
  (add-to-list 'custom-safe-themes
               "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa")
  (add-to-list 'custom-safe-themes
               "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e")
  (sml/setup)

  :config
  (sml/apply-theme 'dark)
  )

;; sh-script
(use-package sh-script
  :config
  (add-hook 'sh-mode-hook 'nice-prog-hook)
  (add-hook 'sh-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\|TODO\\|BUG\\|LATER\\):" 1 font-lock-warning-face t)))))
  )

;; sql
(use-package sql
  :mode (
         ("sql*" . sql-mode))

  :config
  ;; Define a nice hook for editing SQL files
  (defun nice-sql-hook ()
    "Hook for sane editing of SQL files."
    (if (>= emacs-major-version 23)
        (linum-mode t))
    (if (package-installed-p 'undo-tree)
        (undo-tree-mode t))
    (auto-fill-mode t)
    (electric-pair-mode t)
    )

  (add-hook 'sql-mode-hook 'nice-sql-hook)
  )

;; sudo-edit
(use-package sudo-edit
  :ensure t
  )

;; tex
(use-package tex
  :ensure auctex
  )

;; text-mode
(use-package text-mode
  :mode (
         ("COMMIT.*"   . nice-msg-mode)
         ("ci-comment" . nice-msg-mode)
         ("bzr_log\\." . nice-msg-mode)
         ("pico\\."    . nice-msg-mode)
         )

  :init
  (add-hook 'text-mode-hook     'nice-text-hook)
  )

;; twilight-theme
(if (equal window-system nil)
    (use-package twilight-theme
      :ensure t

      :init
      (load-theme 'twilight t)
      )
  )

;; twittering-mode
(use-package twittering-mode
  :ensure t
  :defer t

  :config
  (add-hook 'twittering-edit-mode-hook 'nice-text-hook)

  :if window-system
  :config
  (twittering-icon-mode t)
  )

;; undo-tree
(use-package undo-tree
  :ensure t
  )

;; vc-fossil
(if (< emacs-major-version 25)
    (use-package vc-fossil
      :ensure t
      :defer t

      :init
      (require 'vc-fossil)

      :config
      (add-to-list 'vc-handled-backends 'Fossil)
      )
  )

;; which-func
(use-package which-func
  :no-require t
  :config
  (custom-set-faces
   '(which-func ((t (:foreground "goldenrod")))))
  )

;; with-editor
(use-package with-editor
  :no-require t
  :config
  (add-hook 'with-editor-mode-hook 'nice-text-hook)
  (add-hook 'with-editor-mode-hook 'turn-on-orgstruct++)
  )

;; xkcd
(use-package xkcd
  :ensure t
  )

;; yaml-mode
(use-package yaml-mode
  :ensure t

  :mode ("\\.sls\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook 'nice-prog-hook)
  )

(use-package yasnippet
  :ensure t

  :config
  ;; Inter-field navigation
  (defun yas/goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line 1)
        (goto-char position))))

  (defun yas/goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line 1)
        (goto-char position))))

  ;; fix some org-mode + yasnippet conflicts:
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  ;; Turn on snippets
  (yas-global-mode t)

  ;; Keybindings
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
  (define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
  (define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

  ;; Update org-mode configuration
  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key [tab])
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))

  ;; Wrap around region
  (setq yas-wrap-around-region t)
  )


;; Miscellany
;; --------------------------------------------------------------------

;; Diminished functions
(diminish 'ivy-mode "")
(diminish 'yas-minor-mode " Y")
(diminish 'undo-tree-mode " UT")

;; Do some byte recompilation
(byte-recompile-directory (expand-file-name "~/.emacs.d/thirdparty") 0)
(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)

;;; init.el ends here
