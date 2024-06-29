;;; cmf-publishing.el --- Publishing customization
;; ====================================================================
;;;
;; Copyright (c) 2024 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Fri Jun 14 08:46:51 2024 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Contains any customization pertaining to customizing the authoring
;; and publishing of documents.  Note that for org-mode specific
;; settings, see cmf-org.el
;;

;;; Code:

;;
;; Packages
;;

(use-package adoc-mode
  :ensure t
  :defer t
  )

(use-package markdown-mode
  :ensure t

  :commands (markdown-mode gfm-mode)

  :mode (("README\\.md\\'"         . gfm-mode)
         ("\\.md\\'"               . markdown-mode)
         ("\\.markdown\\'"         . markdown-mode))

  :init
  (setq markdown-command "multimarkdown")
  )

(use-package tex
  :ensure auctex
  :no-require t
  :defer t

  :config
  (use-package company-auctex
    :ensure t
    :after (company latex)

    :config
    (company-auctex-init)
    )

  (use-package company-bibtex
    :ensure t
    :after (company)

    :hook (bibtex-mode .
                       (lambda ()
                         (add-to-list 'company-backends
                                      'company-bibtex)))
    )
  )


(provide 'cmf-publishing)
;;; cmf-publishing.el ends here
