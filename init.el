;;; init.el --- Generic init el for Carbonite Emacs users.

;; Copyright (C) 2017 Adam Taylor

;;; Commentary:
;;    My Emacs configuration

;;; Code:

(package-initialize)

;; Your customizations are stored in the custom.el file
;;    *DO THIS FIRST SO YOU CAN CUSTOMIZE PACKAGES BEFORE LOADING THEM*
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Setup the package stuff                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup package path
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(unless (assoc-default "gnu" package-archives)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t))
;; Setup pinned stuff for org
(setq package-pinned-packages '((org . "org")
                                (org-plus-contrib . "org")))

;; Re-read the packages
(package-refresh-contents)

;; Make sure the packages install if not there
(defun init/auto-install (pkg &rest args)
  "Advisor for use-package. Pass the same PKG and ARGS."
  (unless (package-installed-p pkg)
    (package-install pkg)))

(init/auto-install 'use-package)

(advice-add 'use-package :before #'init/auto-install)
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     PACKAGES                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; To use omnisharp follow directions below
;; Load up local omnisharp (roslyn flavor) - set the load-path to where you've put
;; the omnisharp-emacs repo: https://github.com/OmniSharp/omnisharp-emacs.git
;; this site also contains all of the directions for getting omnisharp running.
(defvar config/use-omnisharp (and (eq window-system 'w32) (file-exists-p (expand-file-name "~/repos/omnisharp-emacs"))))
(when config/use-omnisharp
  (add-to-list 'load-path (expand-file-name "~/repos/omnisharp-emacs"))

  ;; dependencies
  (use-package popup)
  (use-package flycheck)
  (use-package s)
  (use-package shut-up)
  (load-library "omnisharp")
  
  (use-package csharp-mode
    :config
    (progn
      (add-hook 'csharp-mode-hook 'company-mode)
      (add-hook 'csharp-mode-hook 'omnisharp-mode))))

;; other packages
(use-package terraform-mode)
(use-package bind-key)
(use-package git-commit)
(use-package magit)
(use-package magit-filenotify)
(use-package magit-find-file)
(use-package git-timemachine)

(use-package org
  :config (progn
            (add-hook 'org-mode-hook (lambda() (turn-on-auto-fill)))
            (add-hook 'org-mode-hook 'org-bullets-mode)))
(use-package org-plus-contrib)
(use-package org-autolist)

(use-package magit-popup)
(use-package protobuf-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get the python root based on the system we are running under.
;; This, of course, uses Python36 as the default version.

;; You'll need anaconda-mode in your python (via pip). Install ipython, anaconda-mode, pyflake8/flake8, pylint
(use-package anaconda-mode)
(use-package company
  :config
  (progn
   (add-to-list 'company-backends 'company-anaconda)
   (add-to-list 'company-backends 'company-ansible)
   (when config/use-omnisharp
     (add-to-list 'company-backends 'company-omnisharp))))
(use-package company-anaconda)
(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package flycheck-pyflakes) ;; flycheck uses flake8!
(use-package pylint)
(use-package python-docstring
  :config
  (setq-default python-indent-offset 4))

;; We rely on the python and flycheck package
(use-package python
  :config
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    (add-hook 'python-mode-hook 'company-mode)
    (defun python-config--disable-ac (orig-fun &rest args)
      "Don't allow for auto-complete mode in python mode, otherwise call ORIG-FUN with ARGS."
      (unless (eq major-mode 'python-mode)
        (apply orig-fun args)))
    (advice-add 'auto-complete-mode :around #'python-config--disable-ac)
    ;; This I found at: https://github.com/proofit404/anaconda-mode/issues/164, but it might be eldoc
    (remove-hook 'anaconda-mode-response-read-fail-hook 'anaconda-mode-show-unreadable-response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
 
  :config (progn
            (yas-global-mode t)
            (yas-reload-all)
            ;; Setup to allow for yasnippets to use code to expand
            (require 'warnings)
            (add-to-list 'warning-suppress-types '(yasnippet backquote-change))))
(use-package yatemplate)

;; Fix yas indent issues
(add-hook 'python-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

;; fix tab in term-mode
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :config (progn
            (setq projectile-completion-system 'ivy)))

(use-package org-projectile)
(use-package ibuffer-projectile)
(projectile-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some more packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package xterm-color)
(use-package which-key)
(which-key-mode 1)
(use-package sh-script)

;; To use this package you'll need silversearcher (ag) installed on your system.
(use-package ag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DESKTOP STICKINESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set the desktop-path to reflect where you started emacs. This allows
;; you to have various .emacs.desktop files scattered about: Each repo can
;; have it's own "state".
;; Turn this on by setting desktop-save-mode
(use-package desktop
 :config
 (set-variable 'desktop-path (cons default-directory desktop-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IVY CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 16)
  ;; no counting
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

;; We can use counsel with ivy
(use-package counsel)
(use-package counsel-projectile)
(use-package swiper
 
  :init (ivy-mode 1)
  :bind (("C-c s" . swiper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bits-o-configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-ansi-term (term-name cmd)
  "Create an ansi term with a name - other than *ansi-term* given TERM-NAME and CMD."
  (interactive "sName for terminal: \nsCommand to run [/bin/bash]: ")
  (if (= 0 (length cmd)) (setq cmd "/bin/bash"))
  (ansi-term cmd)
  (rename-buffer term-name))

;; Get bash/python file definitions. This fixes the problem where
;; a file is read in with a #!/.../[bash|python] that doesn't automatically
;; set its mode properly. This looks for the #! line and tries to make
;; sense of it...
;; Helps get the proper mode if the file lacks an extension or isn't an extension
;; that agrees with the #! at the beginning of the file.
(defun my-find-file-hook ()
  "If `fundamental-mode', look for script type so the mode gets properly set."
  (if (eq major-mode 'fundamental-mode)
      (condition-case nil
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^#!\s*/.*/\\(python\\|bash\\).*$")
            (if (string= (match-string 1) "python")
                (python-mode)
              (sh-mode)))
        (error nil))))
(add-hook 'find-file-hook 'my-find-file-hook)

;; Run elisp with eldoc-mode and paredit-mode (not sure about paredit these days)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; prefer ibuffer over list-buffers
(fset 'list-buffers 'ibuffer)

;; for lazy people use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; handy way of getting back to previous places
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(bind-key "C-h c" 'customize-group)

;; Handy for making things bigger and smaller
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

;; get rid of pesky "\C-z" and "\C-x\C-z" annoying minimize
(bind-key "C-z" 'nil)
(bind-key "C-z" 'nil ctl-x-map)

(bind-key "C-c r" 'revert-buffer)
(when config/use-omnisharp
  (bind-key "C-c o" 'omnisharp-start-omnisharp-server)
  (bind-key "C-c d" 'omnisharp-go-to-definition-other-window))
(bind-key "C-c t" 'toggle-truncate-lines)
(bind-key "C-c f" 'magit-find-file-other-window)
(bind-key "C-c g" 'magit-status)
(bind-key "C-c m" 'compile)
(bind-key "C-c c" 'comment-region)
(bind-key "C-c u" 'uncomment-region)

;; Some C-x map stuff
(bind-key "C-j" 'dired-jump ctl-x-map)
(bind-key "<up>" 'enlarge-window ctl-x-map)
(bind-key "<down>" 'shrink-window ctl-x-map)

;; Turn on nice mode line which-function-mode
(which-function-mode t)

;; For ediff mode
(setq-default ediff-ignore-similar-regions t)

;; Turn on some stuff that's normally set off
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Stolen from Sacha Chua's emacs setup
;;    http://pages.sachachua.com/.emacs.d/Sacha.html
;; Nice history
(require 'savehist)
(setq savehist-file (expand-file-name "savehist" user-emacs-directory))
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
	regexp-search-ring))

;; Stuff to do after custom macros
(use-package smart-mode-line)
(smart-mode-line-enable)

;;; init.el ends here