;;; init.el --- emacs initialization file
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Adam Taylor

;;; Commentary:
;;      My Emacs configuration. Here there is mostly glue code for literate initialization
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

;; Your customizations are stored in the custom.el file
;;    *DO THIS FIRST SO YOU CAN CUSTOMIZE PACKAGES BEFORE LOADING THEM*
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Setup packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

;; This fixes the (package-refresh-contents) below which breaks on windows
;; because of an issue with some packages in melpa.
(prefer-coding-system 'utf-8)

(package-refresh-contents)                      ;; Package paths are setup via custom.el
(unless (package-installed-p 'use-package)      ;; Make sure use-package is installed
  (package-install 'use-package))
(require 'use-package)

;; Literate emacs configuration requires org-babel-load-file
;; This is a hack to get the latest and latest orgmode - need better way of getting version
(defvar my-org-version 20180205)
(unless (file-exists-p (concat user-emacs-directory "elpa/org-" (int-to-string my-org-version)))
  (let ((myorg (package-desc-create :name "org"
                                     :version (list my-org-version)
                                     :archive "org"
                                     :kind 'tar)))
    (package-install myorg)))

;; Load up config
(defvar my-cfg (concat user-emacs-directory "README"))
(when (file-newer-than-file-p (concat my-cfg ".org") (concat my-cfg ".el"))
  (org-babel-tangle-file (concat my-cfg ".org")))
(load my-cfg)
;;; init.el ends here
