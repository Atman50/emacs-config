;;; init-elpa.el --- Emacs configuration for package.el
;;
;; Copyright (c) 2016
;;
;; Author:
;; URL:
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Some configuration for installing packages.

;;; Code:

(require 'package)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))

;; Fire up package.el
(package-initialize)
(setq package-enable-at-startup nil)

;; list of installed packages
(defvar fx-packages
  '(ag
    anaconda-mode
    ansible
    ansible-doc
    async
    bind-key
    chef-mode
    company
    company-anaconda
    company-ansible
    dash
    dash-functional
    diminish
    epl
    exec-path-from-shell
    f
    flycheck
    flycheck-pyflakes
    function-args
    git-commit
    git-timemachine
    google-this
    ibuffer-projectile
    ido-vertical-mode
    ivy
    latex-preview-pane
    let-alist
    magit
    magit-filenotify
    magit-find-file
    magit-gh-pulls
    magit-popup
    markdown-mode
    ob-ipython
    org-autolist
    org-bullets
    org-projectile
    paredit
    php-mode
    pkg-info
    popup
    projectile
    protobuf-mode
    plantuml-mode
    pylint
    python-docstring
    pythonic
    rich-minority
    s
    seq
    sicp
    smart-mode-line
    smart-shift
    sphinx-doc
    swiper
    undo-tree
    use-package
    visual-regexp
    which-key
    with-editor
    xterm-color
    yaml-mode
    yasnippet
    yatemplate)

  "A list of packages to ensure are installed at launch.")

;; Check if all packages in `fx-packages' are installed
(defvar fx-packages-all-installed-p t
  "Non-nil means all packages in `fx-packages' are installed.")

(defun fx-packages-check-all-installed ()
  "Check if all packages needed installed."
  (dolist (pkg fx-packages)
    (unless (package-installed-p pkg)
     (setq fx-packages-all-installed-p nil))))

;; Install packages
(defun fx-install-packages ()
  "Install packages in `fx-packages'."
  (fx-packages-check-all-installed)
  (unless fx-packages-all-installed-p
    (package-refresh-contents)
    (dolist (pkg fx-packages)
      (unless (package-installed-p pkg)
        (package-install pkg)))
    (fx-packages-check-all-installed)
    (if fx-packages-all-installed-p
        (message "%s" "All packages in `fx-packages' are installed !")
      )))

;; Run package installation
(fx-install-packages)

(require 'use-package)

(provide 'init-elpa)
;;; init-elpa.el ends here
