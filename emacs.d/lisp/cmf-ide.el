;;; cmf-ide.el --- IDE customization -*- lexical-binding: t; -*-
;; ====================================================================
;;;
;; Copyright (c) 2024 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Fri Jun 14 08:15:14 2024 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Customize GNU Emacs into an integrated development environment.
;; This includes packages for working with code or projects in
;; general.
;;

;;; Code:

(require 'cmf-ui)

;;
;; Variables
;;

(custom-set-variables
 '(log-edit-hook '(log-edit-show-files))
 )

;;
;; Functions
;;

;; As suggested by Claude Sonnet 4.6
(defun cmf/lsp-mode-setup-completion ()
  ;; Use orderless (or a mix) for lsp-completion-at-point
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))
  )

;;
;; Packages
;;

(use-package dap-mode
  :ensure t
  :after lsp
  :commands (dap-debug dap-breakpoints-add)

  ;; MS Visual Studio Keybindings for debugging
  :bind (:map dap-mode-map
              ("<f5>"     . dap-continue)
              ("<f9>"     . dap-breakpoint-add)
              ("C-S-<f9>" . dap-breakpoint-clear-all)
              ("<f10>"    . dap-next)
              ("<f11>"    . dap-step-in)
              ("S-<f11>"  . dap-step-out))
  
  :init
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-auto-configure-mode)
  )

(use-package diff-hl
  :ensure t
  :if window-system

  :config
  (global-diff-hl-mode)
  )

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode

  :custom
  (flycheck-check-syntax-automatically (quote (idle-change)))
  (flycheck-disabled-checkers (quote (php-phpcs go-build python-flake8 python-pylint)))
  (flycheck-highlighting-mode 'lines)
  (flycheck-idle-change-delay 3)
  (flycheck-phpcs-standard "PSR2")

  :config
  (global-flycheck-mode)

  (use-package consult-flycheck
    :ensure t

    )
  )

(use-package flyspell
  ;; This is a built-in mode
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))

  :config
  ;; Enable flyspell-correct for better correction UI
  (use-package flyspell-correct
    :ensure t
    :after flyspell
    
    :bind (:map flyspell-mode-map
                ("C-c ;" . flyspell-correct-wrapper) ;; Trigger correction at point
                ("C-c $" . flyspell-correct-at-point))

    :init
    (setq flyspell-correct-interface #'flyspell-correct-completing-read)
    )
  
  (if (eq system-type 'windows-nt)
      (progn
        (setq ispell-dictionary "english")
        (add-to-list 'ispell-local-dictionary-alist '(("english"
                                                       "[[:alpha:]]"
                                                       "[^[:alpha:]]"
                                                       "[']"
                                                       t
                                                       ("-d" cmf/locale)
                                                       nil
                                                       utf-8)))
        (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
        )
    (setq-default ispell-program-name "aspell")
    )

  :custom
  (ispell-extra-args (quote ("--run-together")))
  )

(use-package hl-todo
  :ensure t

  :bind (
         ("C-c h p" . hl-todo-previous)
         ("C-c h n" . hl-todo-next)
         ("C-c h o" . hl-todo-occur)
         ("C-c h i" . hl-todo-insert)
         )

  :hook ((LaTeX-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))

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
     ("WARN"            . font-lock-warning-face)
     ("BUG"             . "#8c5353")
     ("LATER"           . "#d0bf8f"))))
  )

(use-package indent-bars
  :ensure t
  :after c-mode

  :hook (prog-mode . indent-bars-mode)

  :config
  (if (eq system-type 'windows-nt)
      (setq indent-bars-prefer-character t))
  )

(use-package log-edit
  ;; This is a built-in mode
  :mode (("COMMIT.*"   . log-edit-mode)
         ("ci-comment" . log-edit-mode)
         ("bzr_log\\." . log-edit-mode)
         ("cvs*"       . log-edit-mode)
         ("pico\\."    . log-edit-mode))
  )

(use-package lsp-mode
  :ensure t
  :after consult
  :commands (lsp lsp-deferred)

  :bind ("C-x C-k w" . consult-lsp-symbols)
  
  :hook ((lsp-mode . yas-minor-mode)
         (lsp-completion-mode . cmf/lsp-mode-setup-completion))

  :custom
  (lsp-completion-provider :none)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 40000)
  (lsp-response-timeout 30)

  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration t)

  (lsp-register-custom-settings
   `(("intelephense.phpdoc.functionTemplate"
      ,(list :summary "$1"
             :tags (vector ""
                           "@param ${1:$SYMBOL_TYPE} $SYMBOL_NAME $2"
                           ""
                           "@return ${1:$SYMBOL_TYPE} $2"
                           ""
                           "@throws ${1:$SYMBOL_TYPE} $2""" "$1")))))

  (if (window-system)
      (setq lsp-headerline-breadcrumb-icons-enable t)
    (setq lsp-headerline-breadcrumb-icons-enable nil)
    )

  (use-package consult-lsp
    :ensure t

    )

  (use-package lsp-treemacs
    :ensure t
    :after treemacs

    :bind (:map lsp-mode-map
                ("C-x t s" . lsp-treemacs-symbols))
    )

  (use-package lsp-ui
    :ensure t

    :hook (lsp-mode . lsp-ui-mode)

    :config
    (setq lsp-ui-doc-show-with-cursor t)

    :custom
    (lsp-ui-doc-alignment 'window)
    (lsp-ui-doc-position 'top)
    )
  )

(unless (eq (executable-find "git") nil)
  (use-package magit
    :ensure t
    :commands magit-status

    :bind ("C-x g" . magit-status)
    )
  )

(use-package projectile
  :ensure t
  :diminish projectile-mode

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :custom
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien)
  (projectile-switch-project-action #'projectile-find-file)

  :config
  ;; Install ripgrep package, which is needed by projectile
  (unless (eq (executable-find "rg") nil)
    (use-package ripgrep
      :ensure t)
    )

  (dolist (devpath '("~/dev/"
                     "~/org/"
                     "~/source/repos/"
                     ))
    (if (file-directory-p (expand-file-name devpath
                                            user-emacs-directory))
        (add-to-list 'projectile-project-search-path (expand-file-name
                                                      devpath user-emacs-directory))
      ))

  (projectile-mode +1)

  (use-package consult-projectile
    :ensure t
    :after consult

    )

  (use-package treemacs-projectile
    :ensure t
    :after treemacs
    )
  )

(use-package rainbow-delimiters
  :ensure t

  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package treemacs
  :ensure t
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
  (unless (eq (executable-find "git") nil)
    (use-package treemacs-magit
      :ensure t
      :after magit
      )
    )
  )

(unless (eq (executable-find "fossil") nil)
  (use-package vc-fossil
    :ensure t

    :config
    (add-to-list 'vc-handled-backends 'Fossil)
    )
  )

(use-package yasnippet
  :ensure t
  :diminish yasnippet-mode

  :config
  (yas-global-mode t)
  (use-package consult-yasnippet
    :ensure t
    :after consult
    
    )

  (use-package yasnippet-snippets
    :ensure t
    )

  :custom
  (yas-wrap-around-region t)
  )


(provide 'cmf-ide)
;;; cmf-ide.el ends here
