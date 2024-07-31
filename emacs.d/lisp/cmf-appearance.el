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

(if (find-font (font-spec :name "JetBrains Mono"))
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:foundry "JB" :family "JetBrains Mono" :height 110)))))
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

  :init
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

;; Enables ligature support for JetBrains Mono font
(use-package ligature
  :ensure t

  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all relevant ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t)
  )


(provide 'cmf-appearance)
;;; cmf-appearance.el ends here
