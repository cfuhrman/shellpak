;;; cmf-ui.el --- UI customization -*- lexical-binding: t; -*-
;; ====================================================================
;;;
;; Copyright (c) 2024, 2026 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Thu Jun 13 20:39:47 2024 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Customizes the user interface of GNU Emacs, not including overall
;; appearance.
;;

;;; Code:

;;
;; Variables
;;

(custom-set-variables
 '(auto-revert-verbose nil)
 '(comment-fill-column 80)
 '(comment-multi-line t)
 '(comment-style (quote indent))
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-prettify-symbols-mode t)
 '(recentf-mode t)
 `(tab-always-indent 'complete)
 '(transient-mark-mode t)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp)))
 )

;;
;; Functions
;;

(defun cmf/corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    (setq-local corfu-auto nil)
    (corfu-mode 1))
  )

;;
;; Behavior
;;

(if (and (eq system-type 'darwin)
         (eq window-system nil))
    (normal-erase-is-backspace-mode 0)
  )

;;
;; Packages
;;

(use-package ace-window
  :ensure t

  :bind ("C-x o" . ace-window)

  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

(use-package cape
  :ensure t

  :bind (("C-c e \\" . cape-tex)
         ("C-c e ^"  . cape-tex)
         ("C-c e _"  . cape-tex))

  :init
  ;; completion-at-point will evaluate these in order
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  )

(use-package consult
  :ensure t
  :pin gnu

  :bind (
         ("C-c b" . consult-bookmark)
         ("C-c j" . consult-git-grep)
         ("C-c g" . consult-grep)
         ("C-c k" . consult-ripgrep)
         ("C-c o" . consult-outline)
         ("C-c t" . consult-theme)
         ("M-g g" . consult-goto-line)
         ("C-x b" . consult-buffer)
         ("C-x l" . consult-locate)
         ("C-r"   . consult-line)
         ("C-s"   . consult-line)
         ("M-i"   . consult-imenu)
         ("M-y"   . consult-yank-pop)

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s"   . consult-history)                 ;; orig. next-matching-history-element
         ("M-r"   . consult-history)                 ;; orig. previous-matching-history-element
         )
  )

(use-package corfu
  :ensure t
  :pin gnu

  :bind
  (:map corfu-map
        ("TAB"     . corfu-next)
        ([tab]     . corfu-next)
        ("S-TAB"   . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET"     . corfu-insert))

  :hook ((prog-mode   . corfu-mode)
         (shell-mode  . corfu-mode)
         (eshell-mode . corfu-mode))

  :init
  (global-corfu-mode t)
  (corfu-history-mode t)

  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-auto-trigger ".")
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-preview-current t)
  )

(use-package corfu-popupinfo
  :ensure nil ;; part of corfu
  :after corfu

  :hook (corfu-mode . corfu-popupinfo-mode)

  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2))
  )

(add-hook 'minibuffer-setup-hook #'cmf/corfu-enable-in-minibuffer)

(if (version< emacs-version "31.1")
    (use-package corfu-terminal
      :ensure t
      :if (not (display-graphic-p))

      :config
      (corfu-terminal-mode 1)
      )
  )

(use-package embark
  :ensure t

  :bind (("C-c m ."      . embark-act)
         ("C-c m ,"      . embark-dwim)
         ("C-c m j"      . embark-dired-jump)
         ("C-h B"        . embark-bindings))

  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :pin gnu
  :after embark

  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  )

(use-package emojify
  :ensure t

  :config
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))

  ;; Mnemonic: C-c (i)nsert (e)moji
  (bind-key* (kbd "C-c i e") #'emojify-insert-emoji)
  )

(use-package goggles
  :ensure t
  
  :hook ((prog-mode text-mode) . goggles-mode)

  :config
  (setq-default goggles-pulse t) ;; set to nil to disable pulsing
  )

(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)

  :bind
  ([remap describe-symbol]      . helpful-symbol)
  ([remap describe-command]     . helpful-command)
  ([remap describe-key]         . helpful-key)
  )

(use-package marginalia
  :ensure t
  :pin gnu

  :init
  (marginalia-mode 1)
  )

(use-package orderless
  :ensure t
  :pin gnu

  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic
                                                 partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  )

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode

  :hook ((prog-mode .
                    (lambda ()
                      (undo-tree-mode t)))
         (text-mode .
                    (lambda ()
                      (undo-tree-mode t)))
         (conf-mode .
                    (lambda ()
                      (undo-tree-mode t))))

  :custom
  (undo-tree-history-directory-alist '(("" . "~/.emacs.d/undo")))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  )

(use-package vertico
  :ensure t
  :pin gnu

  :init
  (vertico-mode 1)

  :config
  ;; Configure directory extension.
  (use-package vertico-directory
    :after vertico
    :ensure nil

    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word)))
  )

(use-package which-key
  ;; This is a built-in mode on Emacs >= 30.1
  :diminish which-key-mode

  :init
  ;; which-key is built in as of Emacs 30.1, so only install if
  ;; necessary
  (if (and (version< emacs-version "30.1")
           (not (package-installed-p 'which-key)))
      (package-install 'which-key)
    )

  :config
  (which-key-mode t)

  :custom
  (which-key-idle-delay 1)
  )


(provide 'cmf-ui)
;;; cmf-ui.el ends here
