;;; adam.init.el --- Adam's emacs config file

;; Copyright (C) 2016 by Adam Taylor

;;; Commentary:
;;    This is Adam's base Emacs configuration file.  See the comments
;;    throughout to change your Emacs configuration.

;;; Code:

;; Load-path

;; My own customized install. Everything is in here now, but should be broken
;; up using the same method as init-elpa.el and init-tramp.el. Just haven't
;; finished yet.
(package-initialize)

;; Add in the lisp directory stashed away in .emacs.d
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Debug on error
(setq debug-on-error t)

;; GC Optimization
(setq gc-cons-threshold (* 128 1024 1024))

;; Bootstrap config
(require 'init-elpa)       ;; Install required packages
(require 'init-tramp)      ;; Tramp setting

;; other packages
(use-package google-this :ensure t :defer t)
(use-package bind-key :ensure t :defer t)
(use-package git-commit :ensure t :defer t)
(use-package magit :ensure t :defer t)
(use-package magit-filenotify :ensure t :defer t)
(use-package magit-find-file :ensure t :defer t)
(use-package git-timemachine :ensure t :defer t)

(use-package org :ensure t :defer t)
(use-package org-autolist :ensure t :defer t)
(use-package org-bullets :ensure t :defer t)
(use-package ob-ipython :ensure t :defer t)

(use-package magit-popup :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package protobuf-mode :ensure t :defer t)
(use-package plantuml-mode :ensure t :defer t :if (window-system))

;; stuff for ansible
(use-package yaml-mode :ensure t :defer t)
(use-package ansible :ensure t :defer t)
(use-package ansible-doc :ensure t :defer t)
(add-hook 'yaml-mode-hook 'ansible-doc-mode)

;; Way cool undo-tree - learned from the emacs hangouts.
;; This with git-timemachine is useful
(use-package undo-tree
  :defer t
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package anaconda-mode :ensure t)
(use-package company :ensure t)
(use-package company-anaconda :ensure t)
(use-package company-ansible :ensure t)
(use-package flycheck :ensure t)
(use-package flycheck-pyflakes :ensure t)
(use-package pylint :ensure t)
(use-package python-docstring :ensure t
  :config
  (progn
    (setq-default python-indent-offset 4))
  )

;; We rely on the python and flycheck package
(use-package python :ensure t)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Try sphinx-doc??
;;(use-package sphinx-doc :ensure t)
;;(add-hook 'python-mode-hook 'sphinx-doc-mode)

;;prompting package for anaconda
(eval-after-load "company"
  '(progn
     (add-to-list 'company-backends 'company-anaconda)
     (add-to-list 'company-backends 'company-ansible)))
(use-package company :ensure t)
(add-hook 'python-mode-hook 'company-mode)

(defun python-config--disable-ac (orig-fun &rest args)
  "Don't allow for auto-complete mode in python mode, otherwise call ORIG-FUN with ARGS."
  (unless (eq major-mode 'python-mode)
    (apply orig-fun args)))
(advice-add 'auto-complete-mode :around #'python-config--disable-ac)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLYCHECK configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on global flycheck-mode
(use-package flycheck :ensure t)
(global-flycheck-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment following to enable yas-mode everywhere
(use-package yasnippet
  :ensure t
  :config (progn
            (yas-global-mode t)
            (yas-reload-all)
            ;; Setup to allow for yasnippets to use code to expand
            (require 'warnings)
            (add-to-list 'warning-suppress-types '(yasnippet backquote-change))))
(use-package yatemplate :ensure t)

;; Fix yas indent issues
(add-hook 'python-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

;; fix tab in term-mode
(add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile :ensure t)
(use-package org-projectile :ensure t)
(use-package ibuffer-projectile :ensure t)
(projectile-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cider :ensure t)
(use-package paredit :ensure t)
(use-package clojure-mode :ensure t)
(use-package clojure-snippets :ensure t)
(use-package clojure-mode-extra-font-locking :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load up cc-mode
(require 'cc-mode)
(require 'cc-vars)

(use-package function-args :ensure t)

;; Add c++ mode map
(define-key c++-mode-map "\C-c\C-m" 'compile)

;; Add the C files to the c++-mode
(setq auto-mode-alist
      (append '(("\\.c$"    . c++-mode)
                ("\\.h$"    . c++-mode)
                ("\\.idl$"  . c++-mode)
                ("[mM]ake\\.*"  . makefile-gmake-mode)
                ("\\.MAK$"  . makefile-gmake-mode)
                ("\\.jsp$"  . html-mode)
                ("\\.inc$"  . html-mode)
                ("\\.dwt$"  . html-mode)
                ("\\.prp$"  . text-mode)
                ("\\.xsd$"  . xml-mode)
                ("\\.lst$"  . text-mode)
                ("\\.def$"  . python-mode)
                ("\\.\\([pP]\\([Llm]\\|erl\\|od\\)\\|al\\)\\'" . cperl-mode))
              auto-mode-alist))

;; Turn on auto fill and column to fill to
(add-hook 'c-mode-common-hook
          (lambda ()
            (set-variable 'tab-width 4)
            (turn-on-auto-fill)
            (when c-tab-always-indent (setq c-tab-always-indent nil))
            (setq fill-column 80)))

(setq c-style-alist
      (append c-style-alist
              '(("adam" "gnu"
                 (c-basic-offset . 4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some more packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package xterm-color :ensure t)
(use-package which-key :ensure t)
;;(use-package focus :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bits-o-configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun grep-exp-at-point ()
  "Get the expression under the cursor."
  (interactive)
  (save-excursion
    ;; Default name is any word the cursor is on,
    ;; or if cursor not on a word, then nearest preceding
    ;; word.
    (and (not (looking-at "[a-zA-Z_\-]"))
         (skip-chars-backward "^a-zA-Z_\-"))
    (skip-chars-backward "a-zA-Z_0-9\-")
    (and (looking-at "(") (forward-char 1))
    (buffer-substring
     (point)
     (progn (skip-chars-forward "a-zA-Z_0-9\-") (point)))))

;; Grep for a value in the various files
(defun grep-at-point ()
  "Search for the word under the at cursor from all files in directory."
  (interactive)
  (grep (concat "grep -n " (grep-exp-at-point) " *")))

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
(require 'sh-script)
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

;; Makefiles should be tab indented and 8 space tab width.
(defun my-makefile-hook ()
  "Just turn on tab indentation and make it 8."
  (setq tab-width 8)
  (setq indent-tabs-mode t))
(add-hook 'makefile-gmake-mode-hook 'my-makefile-hook)

;; Handy tab-mode switcher
;; Toggle tab width from 4 to 8
(defun tab-toggle ()
  "Toggle the tab width between 4 and 8."
  (interactive)
  (if (= tab-width 4)
      (set-variable 'tab-width 8)
    (set-variable 'tab-width 4)))

;; Toggle wrapping mode
(defun wrap-toggle ()
  "Toggle the `truncate-lines' variable."
  (interactive)
  (if (and truncate-lines t)
      (set-variable 'truncate-lines nil)
    (set-variable 'truncate-lines t)))

;; Easier window movement
(require 'windmove)
(windmove-default-keybindings 'control) ; leave 'shift for selecting while moving

;; If I ever use find-dired - this is a speed up suggested
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

;; Add eldoc to anything elisp mode
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; prefer ibuffer over list-buffers
(fset 'list-buffers 'ibuffer)

;; for lazy people use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; handy way of getting back to previous places
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(global-set-key "\C-hc" 'customize-group)

;; Handy for making things bigger and smaller
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; get rid of pesky "\C-z" and "\C-x\C-z" annoying minimize
(define-key global-map "\C-z" 'nil)
(define-key ctl-x-map "\C-z" 'nil)

(define-key global-map "\C-cr" 'revert-buffer)
(define-key global-map "\C-ct" 'toggle-truncate-lines)
(define-key global-map "\C-cg" 'magit-status)
(define-key global-map "\C-cm" 'compile)
(define-key global-map "\C-cc" 'comment-region)
(define-key global-map "\C-cu" 'uncomment-region)
(define-key global-map "\C-x\C-j" 'dired-jump)

;; Turn on nice mode line which-function-mode
(which-function-mode t)

;; For ediff mode
(setq-default ediff-ignore-similar-regions t)

;; Sentences end with a single space please
(setq sentence-end-double-space nil)

;; Every five minutes save the session
(require 'timer)
(require 'desktop)
(set-variable 'desktop-path (cons (getenv "PWD") desktop-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IVY CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try swiper and counsel

(use-package swiper
  :ensure t
  :init (ivy-mode 1)
  :bind (("C-c s" . swiper)))

;; org mode gets auto-fill and bullets mode
(add-hook 'org-mode-hook (lambda() (turn-on-auto-fill)))
(add-hook 'org-mode-hook 'org-bullets-mode)

;;
;; Very COOL toggle-split-windows functionality - picked this one off twitter
;;
(defun my-iswitchb-close()
  "Open iswitchb or, if in minibuffer go to next match. Handy way
to cycle through the ring."
  (interactive)
  (if (window-minibuffer-p (selected-window))
      (keyboard-escape-quit)))

;; Toggle between split windows and a single window
(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of
windows we might have in the frame. The idea is to maximize the
current buffer, while being able to go back to the previous split
of windows in the frame simply by calling this command again."
  (interactive)
  (if (not (window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u))))
  (my-iswitchb-close))
(define-key global-map (kbd "C-|") 'toggle-windows-split)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C U S T O M I Z A T I O N  section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-overstrike-face (quote font-lock-builtin-face))
 '(Man-underline-face (quote font-lock-preprocessor-face))
 '(auto-save-default nil)
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-mozilla-program "/usr/bin/firefox")
 '(c-tab-always-indent nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-error-regexp-alist (quote (ant bash java gnu gcc-include lcc perl)))
 '(compilation-error-regexp-systems-list (quote (gnu perl)))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(cscope-do-not-update-database t)
 '(custom-file "~/repos/my-config/init.el")
 '(desktop-save-mode t)
 '(diff-switches "-bc")
 '(dired-listing-switches "-alh")
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ediff-split-window-function (quote split-window-vertically))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode t)
 '(eshell-history-size 1024)
 '(fill-column 78)
 '(flycheck-json-python-json-executable "/usr/local/bin/python2.7")
 '(flycheck-python-flake8-executable "/usr/local/bin/flake8")
 '(flycheck-python-pycompile-executable "/usr/local/bin/python2.7")
 '(flycheck-python-pyflakes-executable "/usr/local/bin/pyflakes")
 '(flycheck-python-pylint-executable "/usr/local/bin/pylint")
 '(font-lock-global-modes t)
 '(font-lock-maximum-size nil)
 '(font-lock-mode t t (font-lock))
 '(git-commit-style-convention-checks (quote (non-empty-second-line overlong-summary-line)))
 '(global-font-lock-mode t nil (font-lock))
 '(gud-gud-gdb-command-name "gdb -i=mi")
 '(gud-pdb-command-name "/usr/local/bin/python2.7 -m pdb")
 '(helm-candidate-number-limit 500)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(line-number-display-limit-width 40000)
 '(load-home-init-file t t)
 '(magit-pull-arguments nil)
 '(magit-repository-directories (quote (("~/repos" . 1))))
 '(make-backup-files nil)
 '(org-agenda-files (quote ("~/Org/work.org")))
 '(org-babel-load-languages (quote ((awk . t) (python . t))))
 '(org-babel-python-command "/usr/local/bin/python2.7")
 '(org-catch-invisible-edits (quote show))
 '(org-clock-into-drawer "CLOCKING")
 '(org-default-notes-file "~/Org/notes.org")
 '(org-directory "~/Org")
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-todo-keyword-faces
   (quote
    (("DONE" :foreground "forest green" :weight bold)
     ("PROG" :foreground "dark blue" :weight bold)
     ("STBY" :foreground "DarkOrange3" :weight bold))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t/!)" "WAIT(w/!)" "PROG(p/!)" "STBY(s/!)" "ASSIGNED(a/!)" "|" "DONE(d/!)" "COMPLETE(c/!)"))))
 '(package-selected-packages
   (quote
    (plantuml-mode clojure-mode-extra-font-locking paredit yatemplate yaml-mode xterm-color which-key visual-regexp use-package undo-tree sphinx-doc smart-shift smart-mode-line sicp rich-minority python-docstring pylint protobuf-mode php-mode org-projectile org-bullets org-autolist ob-ipython markdown-mode magit-find-file magit-filenotify latex-preview-pane ido-vertical-mode ibuffer-projectile google-this git-timemachine function-args flycheck-pyflakes exec-path-from-shell dockerfile-mode diminish company-ansible company-anaconda clojure-snippets chef-mode bind-key ansible-doc ansible)))
 '(python-indent-trigger-commands (quote (yas-expand yas/expand)))
 '(python-shell-completion-setup-code "from IPython.core.completerlib import module_completion")
 '(python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))
")
 '(python-shell-interpreter "/usr/local/bin/python2.7")
 '(python-shell-interpreter-args "-i")
 '(python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
 '(python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
 '(safe-local-variable-values
   (quote
    ((eval condition-case nil
           (setq cmake-ide-project-dir
                 (locate-dominating-file buffer-file-name ".dir-locals.el"))
           (error nil))
     (eval condition-case nil
           (setq cmake-ide-build-dir
                 (concat
                  (locate-dominating-file buffer-file-name ".dir-locals.el")
                  "cbuild"))
           (error nil)))))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t nil (paren))
 '(sml/no-confirm-load-theme t)
 '(sml/theme (quote light))
 '(split-width-threshold nil)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-dialog-box t)
 '(user-mail-address "adtaylor@carbonite.com")
 '(vc-diff-switches (quote ("--diff-cmd" "tkdiff")))
 '(vc-svn-diff-switches "-x -wb")
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal))))
 '(Info-quoted ((t (:foreground "red3"))))
 '(powerline-active1 ((t (:inherit mode-line :background "dark salmon"))))
 '(powerline-active2 ((t (:inherit mode-line :background "dark turquoise"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "dark gray"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "dim gray"))))
 '(region ((t (:background "sky blue" :distant-foreground "gtk_selection_fg_color")))))


;; Stuff to do after custom macros
(use-package smart-mode-line :ensure t :defer t)
(smart-mode-line-enable)


;; init time
(defun fx/time-subtract-millis (time-after time-before)
  "Time elapse between TIME-AFTER and TIME-BEFORE in millisecond."
  (* 1000.0 (float-time (time-subtract time-after time-before))))

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (fx/time-subtract-millis
                      after-init-time before-init-time))))

;;; adam.init.el ends here
