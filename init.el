;;; init.el --- emacs initialization file
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 Adam Taylor

;;; Commentary:
;;      This file is document in README.org
;;

;;; Code:
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)
(prefer-coding-system 'utf-8)
(package-initialize)
(unless (assoc 'use-package package-archive-contents)
  (package-refresh-contents)
  (package-install (elt (cdr (assoc 'org-plus-contrib package-archive-contents)) 0))
  (package-install (elt (cdr (assoc 'use-package package-archive-contents)) 0)))
(assoc-delete-all 'org package--builtins)
(setq use-package-enable-imenu-support t)
(require 'use-package)
(use-package org)
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
