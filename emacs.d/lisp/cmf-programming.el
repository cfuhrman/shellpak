;;; cmf-programming.el --- Programming environment customization
;; ====================================================================
;;;
;; Copyright (c) 2024 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Fri Jun 14 08:56:59 2024 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Customize programming environment including the addition of any
;; packages related to programming language support.
;;

;;; Code:

;;
;; Variables
;;

(custom-set-variables
 '(c-default-style
   (quote
    ((c-mode    . "cmf")
     (c++-mode  . "cmf")
     (objc-mode . "cmf")
     (java-mode . "cmf")
     (awk-mode  . "awk")
     (other     . "cmf"))))
 '(emerge-combine-versions-template "
%b
%a
")
 '(show-paren-mode t)
 )

;;
;; Programming Styles
;;

(c-add-style "cmf"
             '("bsd"
               (c-offsets-alist
                (statement-cont . (first c-lineup-cascaded-calls +)))
               (indent-tabs-mode . nil)
               (fill-column . 120)))

;;
;; Hook definitions
;;

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(add-hook 'prog-mode-hook 'cmf/choose-line-number-mode-hook)
(add-hook 'prog-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (eldoc-mode t)
            (electric-pair-mode t)
            (subword-mode t)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq fill-column 70)
            (setq indent-tabs-mode nil)))

;;
;; Packages
;;

(use-package ansible
  :ensure t
  :defer t

  :hook (yaml-mode .
                   (lambda ()
                     (ansible 1)))

  :config
  (use-package company-ansible
    :ensure t
    :no-require t
    :after company

    :hook (ansible-mode .
                        (lambda ()
                          (add-to-list 'company-backends 'company-ansible)))
    )
  )

(unless (eq (executable-find "clangd") nil)
  (use-package c++-mode
    ;; This is a built-in mode
    :hook (c++-mode . lsp-deferred)
    )

  (use-package c-mode
    ;; This is a built-in mode
    :hook (c-mode . lsp-deferred)
    )
  )

(use-package cperl-mode
  ;; This is a built-in mode
  :mode ("\\.cgi\\'" . cperl-mode)

  :hook (cperl-mode . lsp-deferred)

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
  )

(unless (eq (executable-find "dotnet") nil)
  ;; NOTE: omnisharp C# language server appears to have issues
  ;;       installing under MS-Windows environment so csharp-ls is
  ;;       recommended
  (use-package csharp-mode
    ;; This is a built-in mode on Emacs >= 29.1

    :hook (csharp-mode . lsp-deferred)
    :mode ("\\.csproj\\'" . nxml-mode)

    :init
    ;; csharp-mode is built in as of Emacs 29.1, so only install if
    ;; necessary.
    (if (and (version< emacs-version "29.1")
             (not (package-installed-p 'csharp-mode)))
        (package-install 'csharp-mode)
      )

    :config
    )

  (use-package dotnet
    :ensure t

    :hook ((csharp-mode . dotnet-mode)
           (dired-mode . dotnet-mode))    ; Allows running dotnet in
                                          ; dired buffer
    )
  )

(unless (eq (executable-find "go") nil)
  (use-package go-mode
    :ensure t
    :no-require t

    :hook ((go-mode . lsp-deferred)
           (go-mode .
                    (lambda ()
                      (setq lsp-imenu-index-symbol-kinds '(Constant Variable Method Function Class)))))

    :mode ("\\.go\\'" . go-mode)

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
  )

(use-package java
  ;; This is a built-in mode

  :custom
  (lsp-java-code-generation-generate-comments t)
  (lsp-java-format-enabled nil)
  (lsp-java-format-on-type-enabled nil)

  :hook (java-mode . lsp)
  )

(unless (eq (executable-find "java") nil)
  (use-package lsp-java
    :ensure t
    :defer t
    :after (hydra lsp)

    :hook ((java-mode . lsp-java-boot-lens)
           (java-mode .
                      (lambda ()
                        (setq lsp-imenu-index-symbol-kinds
                              '(Property Constant Variable Constructor
                                         Method Function Class))))
           (lsp-mode . lsp-lens-mode))

    :config
    (require 'lsp-java-boot)
    (use-package dap-java
      :ensure nil
      )
    )
  )

(use-package js-mode
  ;; This is a built-in mode

  :hook (js-mode . lsp-deferred)
  )

(use-package jinja2-mode
  :ensure t
  :defer t

  :hook (jinja2-mode . cmf/choose-line-number-mode-hook)

  :mode ("\\.j2\\'" . jinja2-mode)
  )

(use-package make-mode
  ;; This is a built-in mode
  :mode (("[Mm]akefile\\'" . makefile-mode)
         ("\\.mk\\'"       . makefile-mode))
  )

(unless (eq (executable-find "npm") nil)
  (use-package php-mode
    :ensure t
    :no-require t

    :bind (:map php-mode-map
                ("C-c -"                . php-current-class)
                ("C-c ="                . php-current-namespace)
                ("C-x p"                . php-insert-doc-block)
                )

    :hook ((php-mode . lsp-deferred)
           (php-mode .
                     (lambda ()
                       (setq lsp-imenu-index-symbol-kinds '(Class Property Constuctor Method Function))
                       (setq lsp-imenu-sort-methods '(name)))))

    :mode (("\\.php\\'" . php-mode))

    :custom
    ;; DONT: License key settings not appropriate for public!
    ;; Purchase your intelephense license key at https://intelephense.com
    (lsp-intelephense-licence-key "*****")
    (php-insert-doc-access-tag nil)
    (php-enable-psr2-coding-style)
    (php-lineup-cascaded-calls t)
    (phpcbf-standard "PSR2")

    :config
    (require 'php-doc)
    (require 'phpcbf)

    (use-package company-php
      :ensure t
      :after company

      :hook (php-mode .
                      (lambda ()
                        (add-to-list 'company-backends 'company-ac-php-backend
                                     )))
      )

    (use-package phpunit
      :ensure t
      )
    )
  )

(if (eq system-type 'windows-nt)
    (use-package powershell
      :ensure t
      )
  )

(use-package python
  ;; This is a built-in mode

  :config
  (use-package lsp-pyright
    :ensure t
    :defer t

    :hook (python-mode .
                       (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
    )

  (use-package python-docstring
    :ensure t

    :hook (python-mode . python-docstring-mode)
    )

  (use-package sphinx-doc
    :ensure t

    :bind ("C-x p" . sphinx-doc)

    :hook (python-mode .
                       (lambda ()
                         (require 'sphinx-doc)
                         (sphinx-doc-mode t)))
    )

  :custom
  (python-shell-interpreter "python3")
  )

(use-package pyvenv
  :ensure t
  :defer t
  :after python

  :config
  (pyvenv-mode 1)
  )

(use-package sed-mode
  :ensure t
  :defer t
  :no-require t

  :mode (
         ("*.sed" . sed-mode)
         )
  )

(unless (eq (executable-find "npm") nil)
  (use-package sh-mode
    ;; This is a built-in mode

    :hook (sh-mode . lsp)

    :custom
    (sh-indent-comment t)
    (sh-indent-for-case-alt (quote +))
    (sh-indent-for-case-label 0)
    (sh-indent-for-continuation 4)
    (sh-indentation 8)
    )
  )

;; This *must* be loaded outside of sh-mode stanza
(use-package company-shell
  :ensure t
  :commands shell-mode

  :hook (sh-mode .
                 (lambda ()
                   (add-to-list 'company-backends 'company-shell)))

  :custom
  (company-shell-clean-manpage t)
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

;; Needed for editing ASP.NET Razor pages
(use-package web-mode
  :ensure t
  :no-require t

  ;; LATER: Consider using web-mode for *.html pages
  :mode (("\\.cshtml" . web-mode)
         ("\\.blade"  . web-mode)
         ("\\.svelte" . web-mode))

  :custom
  (web-mode-attr-indent-offset 4)
  (web-mode-attr-value-indent-offset 4)

  :config
  (setq web-mode-engines-alist
	'(("razor"  . "\\.cshtml\\'")
	  ("blade"  . "\\.blade\\.")
	  ("svelte" . "\\.svelte\\."))
        )
  )


(provide 'cmf-programming)
;;; cmf-programming.el ends here
