;;; init.el --- emacs initialization file
;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Adam Taylor

;;; Commentary:
;;    My Emacs configuration. Not trying to make everyone else like me but
;;    this might be a handy starting point for others.
;;
;;    Some benefits of this configuration:
;;       - Most settings are in the custom.el file so it's easy to change the
;;         look and feel without having to edit this file. Just customize group
;;         and face
;;       - Extensive use of use-package allows this init file to be portable: if
;;         you don't have the packages installed on the target system, they will
;;         automatically install
;;       - This makes it easier to keep init.el cleaner
;;
;;    Feel free to use all or part of this configuration

;;; Code:

(package-initialize)

;; Output load time to messages
(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fs"
                     (float-time (time-subtract after-init-time before-init-time)))))

;; Turn down the garbage collector during the loading of this file in case use-package
;; has do some compiling. Set it back when done with init.
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; Your customizations are stored in the custom.el file
;;    *DO THIS FIRST SO YOU CAN CUSTOMIZE PACKAGES BEFORE LOADING THEM*
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Setup the package stuff                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package paths are setup via custom.el

(defvar mswindows-p (string-match "windows" (symbol-name system-type)))

(when mswindows-p
  (prefer-coding-system 'utf-8))

(package-refresh-contents)

;; Make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     PACKAGES                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For use-package to diminish the mode name on the mode line
(use-package diminish)

(when (eq system-type 'gnu/linux)
  (use-package csharp-mode))

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(when mswindows-p
  (use-package powershell))

;; To use omnisharp follow directions below
;; Load up local omnisharp (roslyn flavor) - set the load-path to where you've put
;; the omnisharp-emacs repo: https://github.com/OmniSharp/omnisharp-emacs.git
;; this site also contains all of the directions for getting omnisharp running.

(defvar config/use-omnisharp nil)
(let ((omnisharp (car (get 'omnisharp-server-executable-path 'saved-value))))
  (unless (null omnisharp)
    (setq config/use-omnisharp (file-exists-p omnisharp))))

(when config/use-omnisharp
  (setq omnisharp-debug t)
  (use-package omnisharp
    :diminish "Omni#"
    :bind (:map omnisharp-mode-map
                ("C-c C-j" . imenu)))
  (use-package csharp-mode
    :config
    (progn
      (add-hook 'csharp-mode-hook 'company-mode)
      (add-hook 'csharp-mode-hook 'omnisharp-mode))))

;; Themes
(load-theme 'leuven t)
;;(load-theme 'zenburn t)
;;(load-theme 'sanityinc-tomorrow-blue t)

(use-package powerline
  :demand
  :config
  (powerline-default-theme))

(use-package bind-key)
(use-package git-commit)
(use-package magit
  :demand)
(use-package magit-filenotify)
(use-package magit-find-file)
(use-package git-timemachine)

(use-package org
  :pin "org"
  :demand t)

(use-package org-bullets
  :demand t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))


(use-package org-autolist)

(use-package terraform-mode)
(use-package protobuf-mode)

;; (use-package web-mode
;;   :config
;;   (progn
;;     (add-to-list 'auto-mode-alist '("\\.aspx\\'" . web-mode))
;;     (add-to-list 'auto-mode-alist '("\\.ascx\\'" . web-mode))))
(add-to-list 'auto-mode-alist
             '("\\.aspx\\'" . html-mode)
             '("\\.aspcx\\'" . html-mode))

;; Silver searcher support
(use-package ag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :diminish "Co"
  :config
  (when config/use-omnisharp
    (add-to-list 'company-backends 'company-omnisharp)))

(use-package company-jedi)
(use-package elpy :demand t
  :config
  (progn
    (elpy-enable)
    (add-hook 'elpy-mode-hook
              '(lambda ()
                 (progn
                   (setq-local flymake-start-syntax-check-on-newline t)
                   (setq-local flymake-no-changes-timeout 0.5))))))

(use-package flycheck
  :config
  (progn
    (global-flycheck-mode)))

(use-package flycheck-pyflakes) ;; flycheck uses flake8!
(use-package pylint)
(use-package python-docstring
  :config
  (setq-default python-indent-offset 4))

;; We rely on the python and flycheck package
(use-package python
  :config
  (progn
    (add-hook 'python-mode-hook 'flycheck-mode)
    (add-hook 'python-mode-hook 'company-mode)
    ;; (defun python-config--disable-ac (orig-fun &rest args)
    ;;   "Don't allow for auto-complete mode in python mode, otherwise call ORIG-FUN with ARGS."
    ;;   (unless (eq major-mode 'python-mode)
    ;;     (apply orig-fun args)))
    ;; (advice-add 'auto-complete-mode :around #'python-config--disable-ac)
    ))

(use-package realgud)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :demand
  :diminish (yas-minor-mode . "")
  :config (progn
            (yas-reload-all)
            ;; Setup to allow for yasnippets to use code to expand
            (require 'warnings)
            (add-to-list 'warning-suppress-types '(yasnippet backquote-change))))

;; Fix yas indent issues
(add-hook 'python-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

;; fix tab in term-mode
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :demand)
;; Somehow projectile mode being saved in the custom.el saves the setting
;; as '(projectile-mode t nil (projectile)) which causes issues when reloading
;; the elpa packages - fails because when custom.el is loading it doesn't know
;; about (projectile).
(projectile-mode t)

(use-package org-projectile)
(use-package ibuffer-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some more packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package xterm-color)
(use-package which-key
  :demand
  :diminish "")
(use-package sh-script)

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
  :diminish ""
  :demand
  :bind (:map ivy-minibuffer-map
              ("C-w" . ivy-yank-word)      ;; make work like isearch
              ("C-r" . ivy-previous-line))
  :config
  (progn
    ;; no regexp by default
    (setq ivy-initial-inputs-alist nil)
    ;; configure regexp engine.
    (setq ivy-re-builders-alist
          ;; allow input not in order
          '((t . ivy--regex-ignore-order)))))

;; We can use counsel with ivy
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x g" . counsel-git)
         ("C-x C-f" . counsel-find-file))
  :bind (:map help-map
              ("f" . counsel-describe-function)
              ("v" . counsel-describe-variable)
              ("b" . counsel-descbinds)))

(use-package counsel-projectile
  :demand
  :config
  (counsel-projectile-mode t))

(use-package counsel-etags)

(use-package ivy-hydra)

(use-package swiper
  :init (ivy-mode 1)
  :bind (("C-S-s" . isearch-forward)
         ("C-s" . swiper)
         ("C-S-r" . isearch-backward)
         ("C-r" . swiper)))

(use-package avy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix yasnippets with other company backends:
;;  https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Add in the company-yasnippet BACKEND."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bits-o-configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-ansi-term (term-name cmd)
  "Create an ansi term with a name - other than *ansi-term* given TERM-NAME and CMD."
  (interactive "sName for terminal: \nsCommand to run [/bin/bash]: ")
  (ansi-term (if (= 0 (length cmd)) "/bin/bash" cmd))
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

;; Run elisp with eldoc-mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; prefer ibuffer over list-buffers
(fset 'list-buffers 'ibuffer)

;; for lazy people use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Diminish some of the modes
(diminish 'eldoc-mode "Doc")

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
(bind-key "C-c l" 'magit-log-buffer-file)
(bind-key "C-c m" 'compile)
(bind-key "C-c c" 'comment-region)
(bind-key "C-c u" 'uncomment-region)

;; Some C-x map stuff
(bind-key "C-j" 'dired-jump ctl-x-map)
(bind-key "<up>" 'enlarge-window ctl-x-map)
(bind-key "<down>" 'shrink-window ctl-x-map)

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
;;  history settings are in history and history/savehist customized settings
(require 'savehist)

;;; init.el ends here
