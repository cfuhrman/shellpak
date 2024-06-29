;;; cmf-enhancements.el --- Adds enhancements to Emacs
;; ====================================================================
;;;
;; Copyright (c) 2024 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Thu Jun 13 20:03:41 2024 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;;  Contains macros and customizations that change behavior or enhance
;;  functionality.
;;
;; WARN: cmf-ui *must* be loaded before this file
;;

;;; Code:

;;
;; Variables
;;

(require 'cmf-variables)


;;
;; Macros
;;

(fset 'yes-or-no-p
      'y-or-n-p)

(fset 'align-on-equal
      [?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g tab return ?= ?\[ ?^ ?> ?\] return])

(fset 'align-on-hash-arrow
      [?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g tab return ?= ?> return])

(fset 'align-on-dollar-sign
      [?\M-x ?a ?l ?i ?g ?n ?- ?r ?e ?g return ?\\ ?$ return])

;;
;; Keybindings
;;

(global-set-key [?\C-x ?\C-k ?0] 'normal-erase-is-backspace-mode)
(global-set-key [?\C-x ?\C-k ?1] 'delete-trailing-whitespace)
(global-set-key [?\C-x ?\C-k ?2] 'align-on-equal)
(global-set-key [?\C-x ?\C-k ?3] 'align-on-hash-arrow)
(global-set-key [?\C-x ?\C-k ?4] 'align-on-dollar-sign)

;;
;; Mode customization
;;

(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'footnote-mode)
(add-hook 'text-mode-hook
          (lambda ()
            (setq fill-column 80)))

;;
;; Behavior
;;

;; GNU/Linux systems typically have GNU coreutils installed, so pass
;; the --dired flag to ls since GNU ls supports that
(if (eq system-type 'gnu/linux)
    (setq dired-listing-switches "-alh --dired")
  (setq dired-listing-switches "-alh")
  )

;;
;; Packages
;;

(use-package accent
  :ensure t

  :bind ("C-c C-a" . accent-menu)
  )

(use-package dictionary
  ;; This is a built-in mode on GNU Emacs >= 28.1

  :custom
  (dictionary-server "dict.org")

  :init
  ;; dictionary is a built in as of Emacs 28.1, so only install if
  ;; necessary
  (if (and (version< emacs-version "28.1")
           (not (package-installed-p 'dictionary)))
      (package-install 'dictionary)
    )
  )

(use-package gptel
  :ensure t

  :custom
  ;; DONT: License key not appropriate for public!
  (gptel-api-key "*****")
  (gptel-model "gpt-4-turbo")
  )

(use-package mastodon
  :ensure t

  :hook ((mastodon-toot-mode . footnote-mode))

  :custom
  ;; DONT: Personal settings not appropriate for public!
  (mastodon-instance-url "https://mastodon.example.com")
  (mastodon-active-user "your-username-here")
  )

(if (eq system-type 'darwin)
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

(use-package restclient
  :ensure t
  :mode ("\\.rtt\\'" . restclient-mode)

  :hook (restclient-mode . auto-fill-mode);

  :config
  (use-package company-restclient
    :ensure t
    :after company

    :hook ((restclient-mode . undo-tree-mode)
           (restclient-mode . cmf/choose-line-number-mode-hook)
           (restclient-mode .
                            (lambda ()
                              (add-to-list 'company-backends 'company-restclient))))
    )
  )

(use-package sudo-edit
  :ensure t
  :no-require t

  :custom
  (sudo-edit-indicator-mode t)
  )

(unless (eq (executable-find "w3m") nil)
  (use-package w3m
    :ensure t
    :defer t
    )
  )


(provide 'cmf-enhancements)
;;; cmf-enhancements.el ends here
