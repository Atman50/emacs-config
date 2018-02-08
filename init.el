;;; init.el --- emacs initialization file
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Adam Taylor

;;; Commentary:
;;      This file is document in README.org
;;

;;; Code:

;; Your customizations are stored in the custom.el file
;;  *** DO THIS FIRST SO YOU CAN CUSTOMIZE PACKAGES BEFORE LOADING THEM ***
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Setup package system
(package-initialize)

;; This fixes the (package-refresh-contents) below which breaks on windows because of an issue with some packages in melpa.
(prefer-coding-system 'utf-8)
(package-refresh-contents)                      ;; Package paths are setup via custom.el
(unless (package-installed-p 'use-package)      ;; Make sure use-package is installed
  (package-install 'use-package))
(require 'use-package)

;; Literate emacs configuration requires org for org-babel-tangle-file
;; This nastiness is to make sure that we have the non-built in, and as it turns out, most recent version of org
(defvar my-org-desc (elt (cdr (assoc 'org package-alist)) 0))
(defvar my-org-version (car (package-desc-version my-org-desc)))
(unless (file-exists-p (concat user-emacs-directory "elpa/org-" (int-to-string my-org-version)))
  (package-install my-org-desc))
(require 'org)

;; Load up config
(defvar my-cfg (concat user-emacs-directory "README"))
(when (file-newer-than-file-p (concat my-cfg ".org") (concat my-cfg ".el"))
  (org-babel-tangle-file (concat my-cfg ".org")))
(load my-cfg)
;;; init.el ends here
