;;; setup-yasnippet.el --- Make autocomplete and yasnippet play nice 
;; ====================================================================
;;;
;; Copyright (c) 2014 Christopher M. Fuhrman
;; All rights reserved
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Thu Jul  3 13:31:46 2014 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Describe Lisp File here
;;

;;; Code:

;; (require 'yasnippet)

;; Use only own snippets, do not use bundled ones
;; (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; ;; Include snippets for stuff
;; (require 'buster-snippets)
;; (require 'angular-snippets)
;; (require 'datomic-snippets)

;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

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

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

;; ;; No dropdowns please, yas
;; (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; ;; No need to be so verbose
;; (setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

;; Update org-mode configuration
(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

(provide 'setup-yasnippet)

;;; setup-yasnippet.el ends here
