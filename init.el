;;; init.el --- emacs initialization file -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Adam Taylor

;;; Commentary:
;;      This init file is used to load up the literate configuration that resides
;;      in the `my/cfg-file`.org file. This is babel'd into a .el file that is then
;;      loaded as the configuration.
;; 
;;      The code here simply:
;;          1. Disables the package system (using straight speeds up Emacs start quite a bit)
;;          2. Loads the custom file before anything else (to properly customize variables)
;;          3. Bootstraps the straight package system (and sets some variables)
;;          4. Loads the literate configuration
;; 

;;; Code:
;; Don't use package subsystem (1)
(setq package-enable-at-startup nil)
;; Load custom-file first (2)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)
;; Bootstrap straight (3)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-package as the package configuration tool of choice
(straight-use-package 'use-package)
;; Force use-package to use straight.el to automatically install missing packages
(setq straight-use-package-by-default t)
;; Make sure we have org for the org-babel-tangle-file below
(use-package org)
;; Load my config file (4)
(defcustom my/cfg-file (concat user-emacs-directory "README")
  "The base name for the .org file to use for Emacs initialization."
  :group 'my-configuration
  :type 'string)
(when (file-newer-than-file-p (concat my/cfg-file ".org") (concat my/cfg-file ".el"))
  (org-babel-tangle-file (concat my/cfg-file ".org")))
(load my/cfg-file)
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
;;; init.el ends here
