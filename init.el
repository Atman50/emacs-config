;;; init.el --- emacs initialization file
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Adam Taylor

;;; Commentary:
;;      My emacs configuration. Here there is mostly glue code for literate initialization
;;          1. Initialize the package system
;;          2. Load a separate custom.el file (before any packages are actually loaded).
;;             This allows for variables to be overriden via custom-set-variables.
;;          3. Setup some optimizations for heavy package loading
;;          4. Fix up some broken windows package encoding issues (melpa)
;;          5. Load up use-package
;;          6. Load up org mode
;;          7. org-babel-load-file README.org
;;

;;; Code:

(package-initialize)

;; Your customizations are stored in the custom.el file
;;    *DO THIS FIRST SO YOU CAN CUSTOMIZE PACKAGES BEFORE LOADING THEM*
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Turn down the garbage collector during the loading of this file in case use-package
;; has do some compiling. Set it back when done with init.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Setup packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixes melpa package load issues with some unicode characters it doesn't know how to
;; deal with (incorrect or missing https://en.wikipedia.org/wiki/Byte_order_mark)
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(when mswindows-p
  (prefer-coding-system 'utf-8))

(package-refresh-contents)                      ;; Package paths are setup via custom.el
(unless (package-installed-p 'use-package)      ;; Make sure use-package is installed
  (package-install 'use-package))
(require 'use-package)

;; Literate emacs configuration requires org-babel-load-file
(use-package org
  :demand t
  :pin "org")
(org-babel-load-file "~/.emacs.d/README.org" t)

;;; init.el ends here
