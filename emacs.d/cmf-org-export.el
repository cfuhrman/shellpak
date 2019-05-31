;;; cmf-org-export.el --- Org-mode export wrapper functions
;; ====================================================================
;;
;; Copyright (c) 2014 Christopher M. Fuhrman
;; All rights reserved
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Thu Oct 30 08:54:31 2014 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Contains interactive wrapper functions for intelligently generating
;; documentation via org-mode
;;

;;; Code:

;; Grab Org-mode major version number
(defvar cmf-org-version (string-to-number (car (split-string (org-release) "[.]"))))

;; Force export to UTF-8 format
(custom-set-variables
 '(org-ascii-charset (quote utf-8)))

(defun cmf-export-to-txt ()
  "Wrapper function for exporting an 'org-mode' document to text format.

The default encoding for 'org-mode' versions >= 7 is UTF-8.  Older versions
will export to ASCII format."
  (interactive)
  (if (>= cmf-org-version 8)
      (org-ascii-export-to-ascii)
    (if (equal cmf-org-version 7)
        (org-export-as-utf8)
      (org-export-as-ascii)))
  )

(defun cmf-export-to-pdf ()
  "Wrapper function for exporting an 'org-mode' document to PDF format via LaTeX."
  (interactive)
  (if (>= cmf-org-version 8)
      (org-latex-export-to-pdf)
    (org-export-as-pdf))
  )

(defun cmf-export-to-html ()
  "Wrapper function for exporting an 'org-mode' document to HTML."
  (interactive)
  (if (>= cmf-org-version 8)
      (org-html-export-to-html)
    (org-export-as-html))
  )

(defun cmf-export-to-texinfo ()
  "Wrapper function for exporting an 'org-mode' document to TexInfo."
  (interactive)
  (if (>= cmf-org-version 8)
      (progn
        (require 'ox-texinfo)
        (org-texinfo-export-to-texinfo))
    (error "Org mode version %s does not support export to TexInfo" (org-release)))
  )

(defun cmf-export-to-docbook ()
  "Wrapper function for exporting an 'org-mode' document to DocBook."
  (interactive)
  (if (>= cmf-org-version 8)
      (error "Org mode version %s does not support export to DocBook" (org-release))
    (org-export-as-docbook))
  )

(defun cmf-export-to-markdown ()
  "Wrapper function for exporting an 'org-mode' document to Markdown."
  (interactive)
  (if (>= cmf-org-version 8)
      (org-md-export-to-markdown)
    (error "Org mode version %s does not support export to Markdown" (org-release)))
  )

;;; cmf-org-export.el ends here
