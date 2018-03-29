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
    (package-install (elt (cdr (assoc 'org package-archive-contents)) 0))
    (package-install (elt (cdr (assoc 'use-package package-archive-contents)) 0))))
(require 'use-package)
(require 'org)

;; Load up config
(defvar my-cfg (concat user-emacs-directory "README"))
(when (file-newer-than-file-p (concat my-cfg ".org") (concat my-cfg ".el"))
  (org-babel-tangle-file (concat my-cfg ".org")))
(load my-cfg)
;;; init.el ends here
