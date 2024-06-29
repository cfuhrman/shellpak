;;; init.el --- Personal customization
;; ====================================================================
;;;
;; Copyright (c) 2008, 2016, 2021, 2024 Christopher M. Fuhrman
;; All rights reserved.
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
(setq gc-cons-threshold (* 500 1000 1000))

;; Variables needed for initialization
(defvar cmf/min_emacs_version "27.1")
(defvar cmf/path)

;; Make sure we are running a recent version of Emacs
(when (version< emacs-version cmf/min_emacs_version)
  (message "This version of Emacs (%s) is not supported by this
configuration.  Use %s or greater.  Cowardly aborting!"
           emacs-version
           cmf/min_emacs_version)
  (sleep-for 10)
  (setq quit-flag t)
  )

;; Useful for debugging
(defun cmf/display-startup-time ()
  "Displays Emacs start up time in seconds.

Displays the number of seconds Emacs took to start up along with the
number of garbage collections that took place.  Useful for optimizing
your Emacs Configuration.

Originally taken from
https://github.com/daviwil/emacs-from-scratch/blob/master/init.el"

  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Uncomment to enable
;; (add-hook 'emacs-startup-hook #'cmf/display-startup-time)

;; Set load-path
(dolist (lpath '("./lisp"
                 "./thirdparty"
                 ))
  (progn
    (setq cmf/path (expand-file-name lpath user-emacs-directory))
    (if (file-directory-p cmf/path)
        (add-to-list 'load-path cmf/path)
      )
    )
  )

;; Set exec-path
(dolist (epath '("~/.composer/vendor/bin"
                 "~/.config/composer/vendor/bin"
                 "~/.dotnet/tools"
                 "~/bin"
                 "~/go/bin"
                 "~/perl5/bin"
                 "~/vendor/bin"
                 "/Library/Developer/CommandLineTools/usr/bin"
                 "/Library/TeX/texbin"
                 "/usr/local/bin"
                 "/opt/homebrew/bin"
                 "/usr/pkg/bin"))
  (progn
    (setq cmf/path (expand-file-name epath user-emacs-directory))
    (if (file-directory-p cmf/path)
        (add-to-list 'exec-path cmf/path)
      )
    )
  )


;; GENERAL CONFIGURATION
;;
;; Contains general emacs configuration, including set up of package.el
;; --------------------------------------------------------------------

;; Set trash-directory, creating it if we need to
(pcase system-type
  ('darwin
   (setq trash-directory "~/.Trash/emacs"))
  ('windows-nt
   ;; Let system-move-file-to-trash handle things
   )
  (_                                    ; Default
   (setq trash-directory "~/.local/share/Trash/files/emacs")
   (if (not (file-exists-p trash-directory))
       (make-directory trash-directory t)
     )
   ))

;; Set default coding system
(set-default-coding-systems 'utf-8)


;; PACKAGE CONFIGURATION
;;
;; Contains logic for initializing and configuring package management
;; --------------------------------------------------------------------

(require 'package)

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
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

;; Benchmark startup
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  )

;; Needed so :diminish works with use-package
(use-package diminish
  :ensure t
  )


;; MODULE LOADING
;;
;; To enable/disable modules, simply comment out what you don't need
;; below
;; --------------------------------------------------------------------

;; Customize UI
(require 'cmf-ui)

;; IDE Customization
(require 'cmf-ide)

;; Org-mode customization
(require 'cmf-org)

;; Programming environment customization
(require 'cmf-programming)

;; Publishing customization
(require 'cmf-publishing)

;; Add support for specific file types
(require 'cmf-file-support)

;; Add enhancements
(require 'cmf-enhancements)

;; Customize appearance
(require 'cmf-appearance)


;; CLEAN UP
;; --------------------------------------------------------------------

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 200 1000 1000))

;;; init.el ends here
