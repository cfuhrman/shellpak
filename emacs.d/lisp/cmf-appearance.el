;;; cmf-appearance.el --- Appearance customization
;; ====================================================================
;;;
;; Copyright (c) 2024 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Thu Jun 13 18:50:25 2024 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;;  This file manages the appearance of GNU Emacs, including theme
;;  configuration and modeline configuration
;;

;;; Code:

;;
;; Variables
;;

(custom-set-variables
 '(column-number-mode t)
 '(display-time-mode t)
 '(scroll-bar-mode 'right)
 '(which-function-mode t)
 )

;;
;; Default Face Configuration
;;

(if (eq system-type 'windows-nt)
    (if (find-font (font-spec :name "Cascadia Code"))
        (custom-set-faces
         ;; custom-set-faces was added by Custom.
         ;; If you edit it by hand, you could mess it up, so be careful.
         ;; Your init file should contain only one such instance.
         ;; If there is more than one, they won't work right.
         ;;
         ;; Cascadia Code can be downloaded via
         ;; https://github.com/microsoft/cascadia-code
         '(default ((t (:inherit nil :slant normal :weight normal :height
                                 110 :width normal :foundary "outline" :family "Cascadia Code"))))
         '(fixed-pitch ((t (:family "Cascadia Code")))))
      )
  (if (find-font (font-spec :name "DejaVu Sans Mono"))
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(default ((t (:inherit nil :slant normal :weight regular :height
                               110 :width normal :family "DejaVu Sans Mono")))))
    )
  )

;; Remove tool-bar
(if (window-system)
    (tool-bar-mode -1)
  )

;;
;; Packages
;;

(use-package all-the-icons
  :ensure t
  :if window-system

  :config

  ;; Install the font files only if we have not already done so
  (unless (find-font (font-spec :name "all\-the\-icons"))
    (message "Installing all-the-icon fonts")
    (all-the-icons-install-fonts t)
    )

  (use-package all-the-icons-completion
    :ensure t
    :if window-system

    :config
    (all-the-icons-completion-mode)
    )

  (use-package all-the-icons-dired
    :ensure t

    :hook (dired-mode . all-the-icons-dired-mode)

    :custom
    (all-the-icons-dired-monochrome nil)
    )
  )

(use-package all-the-icons-ivy
  :ensure t
  :if window-system
  :after (ivy all-the-icons)

  :config
  (all-the-icons-ivy-setup)

  (use-package all-the-icons-ibuffer
    :ensure t

    :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
    )

  (use-package all-the-icons-ivy-rich
    :ensure t
    :if window-system
    :after ivy

    :init
    (all-the-icons-ivy-rich-mode t)
    )
  )

(use-package doom-modeline
  :ensure t

  :hook (after-init . doom-modeline-mode)

  :custom
  (doom-modeline-vcs-max-length 14)

  :config
  (use-package nerd-icons
    :ensure t
    :if window-system

    :config
    (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
      (message "Installing nerd icon fonts")
      (nerd-icons-install-fonts t)
      )
    )
  )

(use-package doom-themes
  :ensure t

  :config
  (load-theme 'doom-sourcerer t)
  )


(provide 'cmf-appearance)
;;; cmf-appearance.el ends here
