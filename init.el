;;; init.el --- emacs initialization file
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Adam Taylor

;;; Commentary:
;;      This file is document in README.org
;;

;;; Code:

;; Your customizations are stored in the custom.el file
;;  *** DO THIS FIRST SO YOU CAN CUSTOMIZE PACKAGES BEFORE LOADING THEM ***
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; This fixes the (package-refresh-contents) below which breaks on windows because of an issue with some packages in melpa.
(prefer-coding-system 'utf-8)

;; Had to change the following logic so that it worked under both 26.0.91 and 27.0.50 (there is some difference to the initialization
;; process that got in the way of my old logic). First time in should package-install both org mode and use-package. Then only on
;; 26.0.91 package-initialize is called (27.0.50 calls package-initialize for you? - something to do with an early initialize capability...)
(unless (boundp 'package-user-dir)
  (unless (boundp 'package-archive-contents)
    (package-initialize))
  (unless (assoc 'use-package package-archive-contents)
    (package-refresh-contents)
    (package-install (elt (cdr (assoc 'org-plus-contrib package-archive-contents)) 0))
    (package-install (elt (cdr (assoc 'use-package package-archive-contents)) 0))))
(require 'use-package)
(require 'org)

;; Load up config
(defcustom my/cfg-file (concat user-emacs-directory "README")
  "The base name for the .org file to use for Emacs initialization."
  :group 'my-configuration
  :type 'string)
(when (file-newer-than-file-p (concat my/cfg-file ".org") (concat my/cfg-file ".el"))
  (org-babel-tangle-file (concat my/cfg-file ".org")))
(load my/cfg-file)
;;; init.el ends here
