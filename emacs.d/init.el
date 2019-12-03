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
                        "/opt/pkg/bin"
                        "/usr/local/bin"
                        "~/.composer/vendor/bin"
                        "~/bin"
                        "~/vendor/bin"
                        "~/go/bin"
                        "~/perl5/bin"
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
 '(epg-gpg-home-directory "~/.gnupg/")
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
(if (and (version< emacs-version "25.2.1") (not(version< emacs-version "24.5")))
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

;; Convenience function for disabling all minor modes
(defun disable-all-minor-modes ()
  "Disables all minor modes"
  (interactive)
  (mapc
   (lambda (mode-symbol)
     (when (functionp mode-symbol)
       ;; some symbols are functions which aren't normal mode functions
       (ignore-errors
         (funcall mode-symbol -1))))
   minor-mode-list))

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
  (hl-todo-mode t)
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
                        :height   100))

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
(setq package-check-signature nil)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/"))
      )
(setq package-unsigned-archives (quote ("melpa" "melpa-stable" "marmalade")))
(package-initialize)

;; Bootstrap 'gnu-elpa-keyring-update' if necessary
(unless (package-installed-p 'gnu-elpa-keyring-update)
  (package-refresh-contents)
  (package-install 'gnu-elpa-keyring-update)
  (require 'gnu-elpa-keyring-update))

;; Uncomment once *all* package in GNU elpa repository are signed
;; (setq package-check-signature t)

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

    :hook
    (osx-location-changed .
                          (lambda ()
                            (setq calendar-latitude osx-location-latitude
                                  calendar-longitude osx-location-longitude
                                  calendar-location-name (format "%s, %s" osx-location-latitude osx-location-longitude)
                                  forecast-latitude osx-location-latitude
                                  forecast-longitude osx-location-longitude
                                  forecast-city (format "%s, %s" osx-location-latitude osx-location-longitude)))
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

;; ace-window
(if (not(version< emacs-version "24.4"))
    (use-package ace-window
      :ensure t
      :pin melpa-stable

      :config
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

      :bind (
             ("C-x o" . ace-window))
      )
  )

;; all-the-icons
(if (version< emacs-version "24.4")
    (message "All-the-icons requires Emacs version 24.4 or later.  Unable to install")
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

    :init
    (require 'font-lock+)

    :hook (dired-mode . all-the-icons-dired-mode)

    :diminish all-the-icons-dired-mode " 🄸"
    )

  (use-package all-the-icons-ivy
    :if window-system
    :ensure t
    :after all-the-icons
    :pin melpa-stable

    :config
    (all-the-icons-ivy-setup)
    )
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

  :hook (apache-mode . nice-prog-hook)
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
;; TODO: Add company-bibtex, if necessary
(use-package auctex
  :ensure t
  :no-require t

  :config
  (use-package company-auctex
    :ensure t
    :after company

    :init
    (company-auctex-init)
    )
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
  :pin melpa-stable

  :init
  (require 'calfw)

  ;; See https://github.com/kiwanami/emacs-calfw for setting up your
  ;; own calendars
  :config
  ;; calfw-gcal
  (use-package calfw-gcal
    :ensure t

    :init
    (require 'calfw-gcal)
    )

  ;; calfw-ical
  (use-package calfw-ical
    :ensure t

    :init
    (require 'calfw-ical)
    )

  ;; calfw-org
  (use-package calfw-org
    :ensure t

    :init
    (require 'calfw-org)
    )

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

;; company-mode
(use-package company
  :ensure t
  :pin melpa-stable

  :diminish company-mode " 🄲"

  :init
  (global-company-mode)

  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("TAB" . company-complete-selection)
              )

  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-minimum-width 27)
  (company-idle-delay 0.3)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length 3)
  (company-tooltip-flip-when-above t)
  )

;; company-ansible
(use-package company-ansible
  :ensure t
  :pin melpa-stable

  :init
  (add-to-list 'company-backends 'company-ansible)
  )

;; [c]perl-mode
(use-package cperl-mode
  :mode ("\\.cgi\\'" . cperl-mode)

  :init
  (if (equal(length(getenv "PERL5LIB")) 0)
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

    :init
    (add-to-list 'company-backends 'company-plsense)
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

;; docker
;;
;; Note that I do not use Windows operating systems so do not know if
;; 'windows-nt' works as well.  Feel free to contact me if it does.
(if (member system-type '(darwin gnu/linux))
    (use-package docker
      :ensure t
      :bind ("C-c d" . docker)
      )
  (message "%s is not supported by Docker, so docker.el will not be installed"  (upcase-initials (prin1-to-string system-type)))
  )

;; dockerfile-mode
(use-package dockerfile-mode
  :ensure t
  :pin melpa-stable

  :mode ("Dockerfile\\'" . dockerfile-mode)
  )

;; emacs-lisp
(use-package emacs-lisp
  :no-require t
  :after company

  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-elisp))
            )
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

  :config
  (global-emojify-mode)
  )

;; flycheck
(use-package flycheck
  :ensure t

  :init
  (global-flycheck-mode)

  :custom
  (flycheck-check-syntax-automatically (quote (idle-change)))
  (flycheck-disabled-checkers (quote (php-phpcs go-build python-flake8 python-pylint)))
  (flycheck-highlighting-mode 'lines)
  (flycheck-idle-change-delay 1.5)
  (flycheck-phpcs-standard "PSR2")
  )

;; flyspell
(use-package flyspell
  :ensure t

  :bind (:map flyspell-mode-map
              ("C-c ;" . flyspell-correct-previous-word-generic))

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
    (forecast-api-key "")		; Get API key at https://developer.forecast.io/
    )
  )

;; geben
(use-package geben
  :ensure t
  :pin melpa-stable
  :no-require t
  )

;; gited - dired for git branches
(if (version< emacs-version "24.4")
    (message "Gited requires Emacs version 24.4 or greater.  Unable to install")
  (use-package gited
    :ensure t
    :no-require t

    :bind (
           ("C-c C-g" . gited-list-branches)
           )

    )
  )

;; ggtags
(if (version< emacs-version "25.1")
    (message "GGTags requires Emacs version 25.1 or greater.  Unable to install")
  (use-package ggtags
    :ensure t
    :diminish ggtags-mode " 🄶"
    :hook (prog-mode . ggtags-mode)
    )
  )

;; go-mode
(use-package go-mode
  :ensure t

  :pin melpa-stable
  :no-require t

  :mode ("\\.go\\'" . go-mode)

  :bind (:map go-mode-map
              ("C-c C-j" . go-direx-pop-to-buffer))

  :hook ((go-mode . nice-prog-hook)
         (go-mode . go-eldoc-setup))
  :init
  (add-hook 'go-mode-hook (lambda () (subword-mode 1)))

  :config
  ;; company-go
  (use-package company-go
    :ensure t

    :init
    (add-hook 'before-save-hook #'gofmt-before-save)
    (add-hook 'go-mode-hook
              (lambda ()
                (add-to-list 'company-backends 'company-go)))
    )

  ;; go-complete
  (use-package go-complete
    :ensure t

    :hook (completion-at-point-functions . go-complete-at-point)
    )

  ;; go-direx
  (use-package go-direx
    :ensure t
    )

  ;; go-eldoc
  (use-package go-eldoc
    :ensure t
    :after eldoc
    )

  ;; go-snippets
  (use-package go-snippets
    :ensure t
    :after yasnippet
    )
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

;; htmlize
(use-package htmlize
  :ensure t
  :pin melpa-stable
  )

;; hl-todo
(if (version< emacs-version "25.1")
    (message "hl-todo requires Emacs version 25.1 or greater.  Unable to install")
  (use-package hl-todo
    :ensure t
    :pin melpa-stable

    :bind (
	   ("C-c p" . hl-todo-previous)
	   ("C-c n" . hl-todo-next)
	   ("C-c o" . hl-todo-occur)
	   ("C-c i" . hl-todo-insert)
	   )

    :custom
    (hl-todo-keyword-faces
     (quote
      (("HOLD" . "#d0bf8f")
       ("TODO" . "#cc9393")
       ("NEXT" . "#dca3a3")
       ("THEM" . "#dc8cc3")
       ("PROG" . "#7cb8bb")
       ("OKAY" . "#7cb8bb")
       ("DONT" . "#5f7f5f")
       ("FAIL" . "#8c5353")
       ("DONE" . "#afd8af")
       ("NOTE" . "#d0bf8f")
       ("KLUDGE" . "#d0bf8f")
       ("HACK" . "#d0bf8f")
       ("TEMP" . "#d0bf8f")
       ("FIXME" . "#cc9393")
       ("XXX+" . "#cc9393")
       ("\\?\\?\\?+" . "#cc9393")
       ("BUG" . "#8c5353")
       ("LATER" . "#d0bf8f"))))
    )
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
  (swiper-goto-start-of-match nil)
  (swiper-include-line-number-in-search t)
  (swiper-stay-on-quit t)
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

  :hook (json-mode . nice-prog-hook)
  )

;; log-edit-mode
(use-package log-edit
  :mode (
         ("COMMIT.*"   . log-edit-mode)
         ("ci-comment" . log-edit-mode)
         ("bzr_log\\." . log-edit-mode)
         ("pico\\."    . log-edit-mode)
         )

  :hook ((log-edit-mode . nice-text-hook)
         (log-edit-mode . turn-on-orgstruct++))

  )

;; magit
(if (version< emacs-version "25.1")
    (message "Magit requires Emacs version 25.1 or later.  Unable to install")
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

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :pin melpa-stable

  :commands (markdown-mode gfm-mode)

  :mode (("README\\.md\\'"         . gfm-mode)
         ("\\.md\\'"               . markdown-mode)
         ("\\.markdown\\'"         . markdown-mode))

  :init (setq markdown-command "multimarkdown")
  )

;; nxml-mode
(use-package nxml-mode

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
  )

;; org-mode
(use-package org
  :bind (
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c b" . org-iswitchb)
         )

  :hook (org-mode . nice-text-hook)

  :init
  (require 'org-install)
  (require 'org-crypt)

  ;; Enable org-crypt as appropriate
  (org-crypt-use-before-save-magic)

  :config
  ;; org-bullets
  (use-package org-bullets
    :ensure t

    :init
    (require 'org-bullets)
    )

  ;; org-fancy-priorities
  (use-package org-fancy-priorities
    :ensure t

    :diminish " 🄵"

    :hook
    (org-mode . org-fancy-priorities-mode)

    :config
    ;; Customization for org-fancy-priorities
    (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))
    (setq org-lowest-priority ?D)
    )

  ;; ox-twbs
  (use-package ox-twbs
    :ensure t
    )

  ;; Org-mode settings are kept in a different file
  (require 'cmf-org-settings)
  ;; Define a basic hook for sane editing of org-mode documents
  (defun nice-org-hook ()
    "Hook for sane editing of 'org-mode' documents."
    (org-defkey org-mode-map "\C-c[" 'org-time-stamp-inactive)
    (if (equal (car (split-string org-version ".")) 8)
        (require 'ox-md))
    )

  ;; Persist org-clock appropriately
  (org-clock-persistence-insinuate)

  ;; Org-mode hooks
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
  :no-require t

  :mode ("\\.php\\'" . php-mode)

  :bind (:map php-mode-map
              ("C-c C--" . php-current-class)
              ("C-c C-=" . php-current-namespace)
              ("C-c C-y" . 'yas/create-php-snippet)
              ("C-x p"   . php-insert-doc-block)
              )

  :hook (php-mode . nice-prog-hook)

  :init
  (require 'php-doc)
  (add-hook 'php-mode-hook (lambda () (subword-mode 1)))

  :config
  ;; Load php documentor
  (load-file "~/.emacs.d/thirdparty/phpdocumentor.el")

  ;; company-php
  (use-package company-php
    :ensure t
    :pin melpa-stable
    :after company

    :init
    (add-hook 'php-mode-hook
              (lambda ()
                (add-to-list 'company-backends 'company-ac-php-backend )))
    )

  ;; php-auto-yasnippets
  (use-package php-auto-yasnippets
    :ensure t
    :after yasnippet
    )

  ;; php-eldoc
  (use-package php-eldoc
    :ensure t
    :after eldoc
    )

  ;; phpcbf
  (use-package phpcbf
    :ensure t
    )

  ;; phpunit
  (use-package phpunit
    :ensure t
    )

  :custom
  (php-insert-doc-access-tag nil)
  (php-enable-psr2-coding-style)
  (php-lineup-cascaded-calls t)
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

;; python
;;
;; Install the following packages beforehand via pip or package
;; manager, if available:
;;
;;  - autopep8
;;  - flake8
;;  - jedi
;;  - setuptools-black
;;  - virtualenv
;;  - yapf
;;
;; Then run `M-x elpy-config'
(use-package python

  :config
  ;; company-jedi
  (use-package company-jedi
    :ensure t
    :pin melpa-stable
    :after company

    :init
    (add-hook 'python-mode-hook
	      (lambda ()
		(add-to-list 'company-backends 'company-jedi)))

    :config
    (company-jedi t)
    )

  ;; elpy
  (use-package elpy
    :ensure t
    :pin melpa-stable
    :after flycheck

    :config
    (elpy-enable)
    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    )

  ;; sphinx-doc
  (use-package sphinx-doc
    :ensure t
    :pin melpa-stable
    :diminish " 🆂"

    :bind ("C-x p" . sphinx-doc)

    :init
    (add-hook 'python-mode-hook (lambda ()
                                  (require 'sphinx-doc)
                                  (sphinx-doc-mode t)))
    )

  :custom
  ;; Use python3 by default
  (elpy-rpc-python-command "python3")
  )

;; realgud
(if (version< emacs-version "25.1")
    (message "realgud requires Emacs version 25.1 or greater.  Unable to install")
  (use-package realgud
    :ensure t
    :pin melpa-stable
    )
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

;; sh-mode
(use-package shell
  :config
  (use-package company-shell
    :ensure t
    :pin melpa-stable

    :init
    (add-hook 'sh-mode-hook
              (lambda ()
                (add-to-list 'company-backends 'company-shell)))

    :custom
    (company-shell-clean-manpage t)
    )
  )

;; smart-mode-line
(use-package smart-mode-line
  :ensure t

  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)

  :config
  (pcase (window-system)
    ('ns
     (progn
       (sml/apply-theme 'respectful)
       (setq sml/read-only-char " 🅛")))
    ('nil
     (progn
       (sml/apply-theme 'dark)
       (setq sml/read-only-char " 🔒")))
    (window-system
     (progn
       (sml/apply-theme 'respectful)
       (setq sml/read-only-char " 🔒"))
     ))

  :custom
  (sml/modified-char "★")
  )

;; sql
(use-package sql
  :mode (("/sql[^/]]*" . sql-mode))

  :hook (sql-mode . nice-prog-hook)
  )

;; sudo-edit
(use-package sudo-edit
  :ensure t
  :no-require t

  :init
  (setq sudo-edit-indicator-mode t)
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

  :hook
  (twittering-edit-mode . nice-text-hook)
  ;; :init
  ;; (add-hook 'twittering-edit-mode-hook 'nice-text-hook)

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
  (twittering-username "#####")		; Add your twitter handle here
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
  (add-hook 'conf-mode-hook
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

  :custom
  (vc-fossil-extra-header-fields (quote (:remote-url :checkout :tags)))
  )

;; vue-js
(use-package vue-mode
  :ensure t
  :pin melpa-stable

  :config
  ;; 0, 1, or 2, representing (respectively) none, low, and high coloring
  (setq mmm-submode-decoration-level 2)
  )

;; which-func
(use-package which-func
  :no-require t

  :custom
  (which-function-mode t)
  )

;; which-key
(if (version< emacs-version "24.4")
    (message "Which-Key requires Emacs version 24.4 or greater.  Unable to install")
  (use-package which-key
    :ensure t
    :pin melpa-stable
    :diminish which-key-mode " 🅆"

    :config
    (which-key-mode)
    )
  )

;; with-editor
(use-package with-editor
  :no-require t

  :hook ((with-editor-mode . nice-text-hook)
         (with-editor-mode . turn-on-orgstruct++))
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
  (add-hook 'yaml-mode-hook (lambda () (subword-mode 1)))

  :hook (yaml-mode . nice-prog-hook)
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
