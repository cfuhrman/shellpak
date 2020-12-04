;;; custom.el --- Custom Emacs Configuration
;; ====================================================================
;;;
;; Copyright (c) 2008 Christopher M. Fuhrman
;; All rights reserved
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Sat Mar  1 00:13:53 2008 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Custom configuration for when running Emacs under a Window system
;; such as Mac OS X, X, or MS-Windows
;;

;;; Code:

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(emojify-emoji-styles (quote (unicode)))
 '(font-use-system-font t)
 '(linum-format "%-3d")
 '(sunshine-show-icons t)
 '(tool-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cfw:face-toolbar-button-off ((t (:foreground "light steel blue" :weight bold))))
 '(linum ((t (:foreground "LightSteelBlue3"))))
 '(which-func ((t (:foreground "goldenrod")))))

;;; custom.el ends here

