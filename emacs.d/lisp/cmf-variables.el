;;; cmf-variables.el --- Variables
;; ====================================================================
;;;
;; Copyright (c) 2024 Christopher M. Fuhrman
;; All rights reserved.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under terms of the Simplified BSD License (also
;; known as the "2-Clause License" or "FreeBSD License".)
;;
;; Created Sun Jun 16 07:16:11 2024 PDT
;;
;; ====================================================================

;;; Commentary:
;;
;; Contains global variables to be used by this Emacs configuration
;;

;;; Code:


;; Variable definitions
;;
;; DONT: Some of the below variables are not suitable for public release!
(defvar cmf/full-name "Christopher M. Fuhrman")
(defvar cmf/mail-address "cfuhrman@example.com")
(defvar cmf/locale "en_US")
(defvar cmf/coding-system 'utf-8)
(defvar cmf/latitude 37.3306615)
(defvar cmf/longitude -212.9049502)
(defvar cmf/location-name "San Jose, CA")
(defvar cmf/time-zone "America/Los Angeles")
(defvar cmf/time-zone-short-name "PST")
(defvar cmf/time-zone-short-name-daylight "PDT")

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-christian-all-holidays-flag t)
 '(calendar-daylight-time-zone-name cmf/time-zone-short-name-daylight)
 '(calendar-latitude cmf/latitude)
 '(calendar-location-name cmf/location-name)
 '(calendar-longitude cmf/longitude)
 '(calendar-mark-holidays-flag t)
 '(calendar-standard-time-zone-name cmf/time-zone-short-name)
 '(calendar-time-zone -480)
 '(calendar-view-holidays-initially-flag nil)
 '(custom-file (locate-user-emacs-file "custom.el"))
 '(delete-by-moving-to-trash t)
 '(user-full-name cmf/full-name)
 '(user-mail-address cmf/mail-address)
 )


(provide 'cmf-variables)
;;; cmf-variables.el ends here
