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

;; Fix for bug involving enriched text mode
(if (version< emacs-version "25.3")
    (eval-after-load "enriched"
      '(defun enriched-decode-display-prop (start end &optional param)
         (list start end))))

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

;; Force fill column for nice-text-hook
(add-hook 'text-mode-hook
          (lambda ()
            (set-fill-column 80)))

;; Aliases
;; --------------------------------------------------------------------

(defalias 'perl-mode 'cperl-mode)


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

;; yasnippet
;;
;; Note this needs to be loaded first since we want to use the version
;; from melpa-stable.
(use-package yasnippet
  :ensure t
  :pin melpa-stable

  :config
  ;; fix some org-mode + yasnippet conflicts:
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  ;; Turn on snippets
  (yas-global-mode t)

  ;; Bind `C-c y' to `yas-expand' ONLY.
  (define-key yas-minor-mode-map (kbd "C-c y") #'yas-expand)

  ;; Update org-mode configuration
  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'yas/trigger-key)
              (setq yas/trigger-key "C-c y")
              (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
              (define-key yas/keymap [tab] 'yas/next-field)))

  ;; Wrap around region
  (setq yas-wrap-around-region t)

  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-wrap-around-region t)
  )

;; ace-window
(use-package ace-window
  :ensure t
  :pin melpa-stable

  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  :bind (
         ("M-p" . ace-window))
  )

;; all-the-icons
(use-package all-the-icons
  :if window-system
  :ensure t
  :ensure all-the-icons-dired
  :ensure all-the-icons-ivy

  :config

  ;; Install the font files only if we have not already done so
  (defvar cmf-fonts-installed-file "~/SHELL/emacs.d/.icon-fonts-installed")
  (unless (file-exists-p cmf-fonts-installed-file)
    (progn
      (message "Installing icon fonts")
      (all-the-icons-install-fonts t)
      (with-temp-buffer (write-file cmf-fonts-installed-file)))
    )

  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
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

  ;; resetting ac-sources
  (setq-default ac-sources '(
                             ac-source-yasnippet
                             ac-source-abbrev
                             ac-source-dictionary
                             ac-source-words-in-same-mode-buffers
                             ac-source-gtags
                             ))
  )

;; auto-compile
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; beacon mode
(use-package beacon
  :diminish " 💡"
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

;; calfw
(use-package calfw
  :ensure t
  :ensure calfw-org
  :pin melpa-stable

  :init
  (require 'calfw)
  (require 'calfw-org)
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

;; eldoc
(use-package eldoc
  :diminish eldoc-mode " 📔"
  )

;; emojify
(use-package emojify
  :if window-system
  :ensure t

  :config
  (global-emojify-mode)
  (global-emojify-mode-line-mode)
  )

;; flycheck
(use-package flycheck
  :ensure t
  :defer t

  :init
  (global-flycheck-mode)

  :custom
  (flycheck-disabled-checkers (quote (php-phpcs)))
  (flycheck-highlighting-mode 'lines)
  (flycheck-phpcs-standard "PSR2")

  )

;; flyspell
(use-package flyspell
  :config
  (setq-default ispell-program-name "aspell")

  :custom
  (flycheck-mode-line-prefix "🦋")
  )

;; forecast
(if (version< emacs-version "24.4")
    (message "Forecast requires Emacs version 24.4 or later.  Unable to install")
  (use-package forecast
    :ensure t
    :defer t

    :custom
    (forecast-latitude cmf-latitude)
    (forecast-longitude cmf-longitude)
    (forecast-city cmf-location-name)
    (forecast-country "United States")
    (forecast-units 'us)
    (forecast-api-key "f78b853ac0c972cccf06e75e21afbe9d")
    )
  )

;; ggtags
(use-package ggtags
  :ensure t
  :defer t
  :diminish ggtags-mode " 🄶"
  )

;; gited - dired for git branches
(use-package gited
  :ensure t
  :defer t

  :bind (
         ("C-x C-g" . gited-list-branches)
         )

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

;; highlight-parentheses
(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode

  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda()
              (highlight-parentheses-mode)
              ))

  (global-highlight-parentheses-mode)
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
  :ensure flyspell-correct-ivy
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
         ("M-i"     . counsel-imenu)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-x l"   . counsel-locate)
         ("C-c ;"   . flyspell-correct-previous-word-generic)
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

  (add-hook 'php-mode-hook
            '(lambda ()
               (require 'ac-php)
               (setq ac-sources (cons 'ac-source-php ac-sources))
               (ac-php-core-eldoc-setup ) ;; enable eldoc
               
               (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
               (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back)    ;go back
               ))
  
  ;; Load phpdocumentor
  (load-file "~/.emacs.d/thirdparty/phpdocumentor.el")

  :custom
  (phpcbf-standard "PSR2")

  )

;; planet-theme
(if (not(equal window-system nil))
    (use-package planet-theme
      :ensure t

      :init
      (load-theme 'planet t)
      )
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

;; sed-mode
(use-package sed-mode
  :ensure t

  :mode (
         ("*.sed" . sed-mode)
         )

  :init
  (add-hook 'sed-mode-hook 'nice-prog-hook)

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
  (if (equal custom-file cmf-custom-16)
      (sml/apply-theme 'light)
    (sml/apply-theme 'dark)
    )
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
(if (equal custom-file cmf-custom-256)
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
  :diminish " 🌲"
  :ensure t

  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  )

;; vc-fossil
(use-package vc-fossil
  :ensure t
  :defer t

  :init
  (require 'vc-fossil)

  :config
  (add-to-list 'vc-handled-backends 'Fossil)
  )

;; which-func
(use-package which-func
  :no-require t
  :config
  (custom-set-faces
   '(which-func ((t (:foreground "goldenrod")))))

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

;; Miscellany
;; --------------------------------------------------------------------

;; Diminished functions
(diminish 'ivy-mode "")
(diminish 'yas-minor-mode " 🅈")

;; Do some byte recompilation
(byte-recompile-directory (expand-file-name "~/.emacs.d/thirdparty") 0)
(byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0)

;;; init.el ends here
