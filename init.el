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

(unless (assoc 'use-package package-archive-contents)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)      ;; Make sure use-package is installed
  (package-install 'use-package))
(require 'use-package)

;; Literate emacs configuration requires org for org-babel-tangle-file
;; Make sure that we have a non-built in org and if not, then install it using package-install. Sadly, use-package won't work for this.
(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))
(require 'org)

;; Load up config
(defvar my-cfg (concat user-emacs-directory "README"))
(when (file-newer-than-file-p (concat my-cfg ".org") (concat my-cfg ".el"))
  (org-babel-tangle-file (concat my-cfg ".org")))
(load my-cfg)
;;; init.el ends here
