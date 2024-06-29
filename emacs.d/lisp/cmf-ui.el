;;; cmf-ui.el --- UI customization
;; ====================================================================
;;;
;; Copyright (c) 2024 Christopher M. Fuhrman
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
 '(transient-mark-mode t)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp)))
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

(use-package company
  :ensure t
  :diminish company-mode

  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("TAB" . company-complete-selection)
              )

  :hook (emacs-lisp-mode .
                         (lambda ()
                           (add-to-list 'company-backends 'company-capf)))

  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-idle-delay 0.3)
  (company-minimum-prefix-length 3)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 10)
  (company-tooltip-minimum-width 27)

  :init
  (global-company-mode)
  )

(use-package company-box
  :ensure t
  :diminish company-box-mode
  :if window-system
  :unless (version< emacs-version "26.1")
  :after company

  :hook (company-mode . company-box-mode)

  :config
  (if (eq window-system 'ns)
      ;; DONT: Company-box and macOS do not behave properly when
      ;; running under full screen mode (at least under Mojave).  The
      ;; same is true regardless if ns-use-native-fullscreen is set to
      ;; nil or not.  For the time being, just notify the user.
      (message "Do not use full-screen mode when running under macOS!")
    )
  )

(use-package helpful
  :ensure t
  :after counsel
  :commands (helpful-callable helpful helpful-command helpful-key)

  :bind
  ([remap describe-function]    . counsel-helpful-function)
  ([remap describe-symbol]      . helpful-symbol)
  ([remap describe-variable]    . counsel-helpful-variable)
  ([remap describe-command]     . helpful-command)
  ([remap describe-key]         . helpful-key)

  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  )

(use-package hydra
  :ensure t
  )

(use-package ivy
  :ensure t
  :diminish ivy-mode

  :bind (
         ;; Ivy-based interface to standard commands
         ("C-c v"   . ivy-push-view)
         ("C-c V"   . ivy-pop-view)

         ;; Ivy-resume and other commands
         ("C-c C-r" . ivy-resume)
         )

  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-count-format "(%d/%d) ")

  :init
  (ivy-mode t)

  :config
  (use-package ivy-hydra
    :ensure t
    :pin gnu
    :after hydra
    )
  )

(use-package ivy-emoji
  :ensure t
  :after ivy

  :bind ("C-c i e" . ivy-emoji) ;; mnemonics i e = insert emoji
  )

(use-package ivy-rich
  :ensure t
  :after (ivy counsel)

  :config
  (ivy-rich-mode t)
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  )

(use-package ivy-xref
  :ensure t
  :after ivy

  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))

  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  )

(use-package swiper
  :ensure t
  :after ivy

  :bind (
         ("C-s"         . swiper)
         ("C-r"         . swiper-backward)
         )

  :init
  (ivy-mode t)
  )

(use-package counsel
  :ensure t
  :diminish counsel-mode

  :bind (
         ("M-x"         . counsel-M-x)
         ("C-x C-f"     . counsel-find-file)
         ("C-x d"       . counsel-dired)
         ("M-y"         . counsel-yank-pop)
         ("<f1> f"      . counsel-describe-function)
         ("<f1> v"      . counsel-describe-variable)
         ("<f1> l"      . counsel-find-library)
         ("<f2> i"      . counsel-info-lookup-symbol)
         ("<f2> u"      . counsel-unicode-char)
         ("<f2> j"      . counsel-set-variable)
         ("C-x b"       . counsel-switch-buffer)
         ("M-i"         . counsel-imenu)

         ;; Ivy-based interface to shell and system tools
         ("C-c C"       . counsel-compile)
         ("C-c g"       . counsel-git)
         ("C-c j"       . counsel-git-grep)
         ("C-c L"       . counsel-git-log)
         ("C-c k"       . counsel-rg)
         ("C-c m"       . counsel-linux-app)
         ("C-x l"       . counsel-locate)
         ("C-c J"       . counsel-file-jump)
         ("C-S-o"       . counsel-rhythmbox)
         ("C-c w"       . counsel-wmctrl)

         ;; Other commands
         ("C-c b"       . counsel-bookmark)
         ("C-c d"       . counsel-descbinds)
         ("C-c o"       . counsel-outline)
         ("C-c t"       . counsel-load-theme)
         ("C-c F"       . counsel-org-file)
         )

  :custom
  (counsel-preselect-current-file t)

  :config
  (counsel-mode t)

  (use-package counsel-web
    :ensure t
    :defer t

    :config
    ;; Define "C-c w" as a prefix key.
    (defvar counsel-web-map
      (let ((map (make-sparse-keymap "counsel-web")))
        (define-key map (kbd "w") #'counsel-web-suggest)
        (define-key map (kbd "s") #'counsel-web-search)
        (define-key map (kbd ".") #'counsel-web-thing-at-point)
        map))
    (global-set-key (kbd "C-c w") counsel-web-map)
    )
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
  (undo-tree-history-directory-alist '(("" . "/tmp/cmf-undo-tree")))
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t)
  )

(use-package which-key
  :ensure t
  :diminish which-key-mode

  :init
  (which-key-mode)

  :custom
  (which-key-idle-delay 1)
  )


(provide 'cmf-ui)
;;; cmf-ui.el ends here
