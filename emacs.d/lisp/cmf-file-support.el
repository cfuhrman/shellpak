;;; cmf-file-support.el --- File customization
;; ====================================================================
;;;
;; Copyright (c) 2024 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Fri Jun 14 09:42:22 2024 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Customize support for specific file types, such as Apache
;; configuration or JSON files.  These are files that are not related
;; to specific programming languages.
;;
;; WARN: cmf-ui *must* be loaded before this file
;;

;;; Code:

;;
;; Packages
;;

(use-package apache-mode
  :ensure t
  :defer t
  :no-require t

  :hook (apache-mode . cmf/choose-line-number-mode-hook)

  :mode (("\\.htaccess\\'"                   . apache-mode)
         ("access\\.conf\\'"                 . apache-mode)
         ("httpd\\.conf\\'"                  . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)
         ("srm\\.conf\\'"                    . apache-mode))
  )

(use-package crontab-mode
  :ensure t
  :no-require t

  :mode (("\\.cron\\(tab\\)?\\'" . crontab-mode)
         ("cron\\(tab\\)?\\."    . crontab-mode))

  :hook ((crontab-mode . cmf/choose-line-number-mode-hook)
         (crontab-mode . cmf/crontab-hook))

  :config
  (defun cmf/crontab-hook ()
    "Hook for sane editing of crontab files."
    (auto-fill-mode -1)
    )
  )

(use-package dockerfile-mode
  :ensure t
  :defer t

  :mode ("Dockerfile\\'" . dockerfile-mode)

  :hook (dockerfile-mode . lsp-deferred)
  )

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :no-require t
  )

(use-package json-mode
  :ensure t

  :hook ((json-mode . undo-tree-mode)
         (json-mode . lsp-deferred)
         (json-mode . cmf/choose-line-number-mode-hook))
  )

(use-package nxml-mode
  ;; This is a built-in mode
  :mode (("\\.xsd\\'"  . xml-mode)
         ("\\.wsdl\\'" . xml-mode))

  ;; NOTE: May need to download xmlls by hand in
  ;; ~/.emacs.d/.cache/lsp/xmlls
  :hook ((nxml-mode . cmf/choose-line-number-mode-hook)
         (nxml-mode . cmf/nxml-hook)
         (nxml-mode . lsp-deferred))

  :config
  (defun cmf/nxml-hook ()
    "Hook for sane editing of XML files."
    (auto-fill-mode -1)
    )

  :custom
  (nxml-child-indent 4)
  )

(unless (eq (executable-find "npm") nil)
  (use-package yaml-mode
    :ensure t
    :no-require t
    :after hl-todo

    :mode (("\\.sls\\'" . yaml-mode)
           ("\\.yml\\'" . yaml-mode))

    :hook ((yaml-mode . cmf/choose-line-number-mode-hook)
           (yaml-mode . lsp-deferred)
           (yaml-mode . hl-todo-mode)
           (yaml-mode .
                      (lambda ()
                        (subword-mode t)
                        (auto-fill-mode -1)))
           )
    )
  )


(provide 'cmf-file-support)
;;; cmf-file-support.el ends here
