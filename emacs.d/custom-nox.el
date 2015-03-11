;;; custom-nox.el --- Console Emacs Configuration
;; ====================================================================
;;;
;; Copyright (c) 2009 Christopher M. Fuhrman
;; All rights reserved
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Fri Nov  6 22:23:29 2009 PST
;;
;; ====================================================================

;;; Commentary:
;;
;; Custom configuration for when running Emacs under non-256 color terminal
;;

;;; Code:

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(linum-format "%03d|"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(cperl-array-face ((nil (:background "darkblue" :foreground "yellow" :weight bold))))
 '(cperl-hash-face ((nil (:background "blue" :foreground "red" :slant italic :weight bold))))
 '(cursor ((t (:foreground "red"))))
 '(diff-added ((t (:foreground "green"))))
 '(diff-file-header ((nil (:background "blue" :foreground "yellow"))))
 '(diff-header ((nil (:foreground "white" :weight bold))))
 '(diff-hunk-header ((t (:inherit diff-header :foreground "cyan"))))
 '(diff-index ((t (:background "purple" :foreground "white" :weight bold))))
 '(diff-refine-change ((((class color) (background dark)) (:underline t))))
 '(diff-removed ((t (:foreground "red"))))
 '(dired-directory ((t (:foreground "lightblue" :weight bold))))
 '(dired-header ((t (:foreground "yellow" :weight bold))))
 '(font-lock-comment-face ((nil (:foreground "lightblue" :slant italic))))
 '(font-lock-function-name-face ((nil (:foreground "yellow" :weight bold))))
 '(font-lock-keyword-face ((nil (:foreground "darkgreen" :weight bold))))
 '(font-lock-string-face ((nil (:foreground "wheat"))))
 '(header-line ((default (:background "purple" :foreground "white" :underline t :weight bold)) (((type tty)) (:foreground "green" :inverse-video nil :box (:line-width 2 :color "grey75" :style released-button) :underline t :weight bold))))
 '(indent-guide-face ((t (:foreground "blue"))) t)
 '(link ((nil (:foreground "lightblue" :underline t))))
 '(linum ((t (:background "blue" :foreground "cyan" :weight light))))
 '(log-view-file ((nil (:background "purple" :foreground "white" :weight bold))))
 '(log-view-message ((nil (:foreground "cyan" :weight bold))))
 '(markup-complex-replacement-face ((t (:inherit markup-meta-face :foreground "yellow" :box (:line-width 2 :style released-button) :weight bold))))
 '(markup-gen-face ((t (:background "black" :foreground "red" :underline t :weight light))))
 '(markup-list-face ((t (:inherit markup-meta-face :background "blue" :foreground "yellow" :underline t))))
 '(markup-meta-face ((t (:stipple nil :foreground "cyan" :inverse-video nil :box nil :strike-through nil :overline nil :underline t :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "Monospace"))))
 '(markup-reference-face ((t (:foreground "cyan" :underline t))))
 '(menu ((((type tty)) (:background "blue" :foreground "white" :weight bold))))
 '(minibuffer-prompt ((t (:foreground "lightblue" :weight bold))))
 '(mode-line ((t (:background "blue" :foreground "white" :weight bold))))
 '(mode-line-inactive ((default (:inherit mode-line :background "blue" :foreground "cyan" :weight light)) (nil nil)))
 '(org-table ((t (:background "black" :foreground "cyan"))))
 '(org-tag ((t (:foreground "indianred3" :weight bold))))
 '(org-agenda-date ((t (:inherit org-agenda-structure :foreground "cyan" :underline nil))) t)
 '(org-agenda-structure ((t (:foreground "cyan" :weight bold))))
 '(smerge-markers ((t (:background "black" :foreground "white" :weight bold))))
 '(smerge-mine ((t (:foreground "green"))))
 '(smerge-other ((t (:foreground "red"))))
 '(smerge-refined-change ((t (:background "blue" :foreground "yellow"))))
 '(w3m-image-anchor ((((class color) (background dark)) (:background "blue" :foreground "yellow" :weight bold))))
 '(w3m-tab-selected ((((class color)) (:background "green" :foreground "white" :weight bold))))
 '(w3m-tab-selected-retrieving ((((class color)) (:background "red" :foreground "white" :weight bold))))
 '(w3m-tab-unselected ((((class color)) (:background "cyan" :foreground "black" :weight light))))
 '(w3m-tab-unselected-retrieving ((((class color)) (:background "red" :foreground "blue"))))
 '(w3m-tab-unselected-unseen ((((class color)) (:background "cyan" :foreground "white" :weight bold))))
 '(weather-metno-date ((t (:background "black" :foreground "white" :weight bold))))
 '(which-func ((nil (:background "blue" :foreground "cyan" :weight bold)))))

;;; custom-nox.el ends here

