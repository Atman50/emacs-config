;;; custom.el --- Your emacs custom file.
;; -*- lexical-binding: t; -*-

;;; Commentary:
;;;    Where your settings are saved. This file is pointed
;;;    to by the variable `custom-file'

;;; Code:

;; Set this so we can customize below
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C U S T O M I Z A T I O N  section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-ignore-list
   '("\\\\*.dll" "\\\\*.png" "\\\\*.jpeg" "\\\\*.exe" "\\\\*.csproj"))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(auto-save-default nil)
 '(browse-url-browser-function 'browse-url-chrome)
 '(browse-url-chrome-program
   (if mswindows-p "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe" "/usr/bin/google-chrome"))
 '(c-tab-always-indent nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(command-log-mode-auto-show t)
 '(command-log-mode-is-global t)
 '(command-log-mode-open-log-turns-on-mode t)
 '(compilation-error-regexp-alist '(ant bash java gnu gcc-include lcc perl))
 '(compilation-error-regexp-systems-list '(gnu perl))
 '(confirm-kill-emacs 'yes-or-no-p)
 '(counsel-mode t)
 '(dabbrev-case-fold-search nil)
 '(desktop-save-mode t)
 '(diff-switches "-bc")
 '(dired-use-ls-dired nil)
 '(ediff-split-window-function 'split-window-vertically)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-mode t)
 '(eshell-history-size 1024)
 '(fill-column 132)
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(flymake-no-changes-timeout 2)
 '(font-lock-maximum-size nil)
 '(global-font-lock-mode t nil (font-lock))
 '(history-delete-duplicates t)
 '(ibuffer-formats
   '((mark modified read-only " " (name 36 36 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " "
           filename-and-process)
     (mark modified read-only " " (name 36 -1))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(kotlin-tab-width 4)
 '(line-move-visual nil)
 '(line-number-display-limit-width 40000)
 '(lsp-ui-doc-border "black")
 '(lsp-ui-sideline-ignore-duplicate t)
 '(make-backup-files nil)
 '(org-babel-load-languages '((shell . t) (plantuml . t) (python . t) (emacs-lisp . t)))
 '(org-catch-invisible-edits 'show)
 '(org-fold-catch-invisible-edits 'show)
 '(org-html-postamble t)
 '(org-html-postamble-format
   '(("en" "<p class=\"author\">Author: %a (%e)</p>\12<p class=\"date\">Date: %T</p>\12<p class=\"creator\">%c</p>")))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
 '(org-reveal-title-slide nil)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/") ("org" . "https://orgmode.org/elpa/")))
 '(projectile-completion-system 'ivy)
 '(projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
 '(recentf-mode t)
 '(rng-nxml-auto-validate-flag nil)
 '(safe-local-variable-values
   '((eval when (and (buffer-file-name) (not (file-directory-p (buffer-file-name))) (string-match-p "^[^.]" (buffer-file-name)))
           (unless (featurep 'package-build) (let ((load-path (cons "../package-build" load-path))) (require 'package-build)))
           (unless (derived-mode-p 'emacs-lisp-mode) (emacs-lisp-mode)) (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set (make-local-variable 'package-build-working-dir) (expand-file-name "../working/"))
           (set (make-local-variable 'package-build-archive-dir) (expand-file-name "../packages/"))
           (set (make-local-variable 'package-build-recipes-dir) default-directory))))
 '(savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
 '(savehist-file "~/.emacs.d/savehist")
 '(savehist-mode t)
 '(scroll-bar-mode 'right)
 '(sentence-end-double-space nil)
 '(show-paren-mode t nil (paren))
 '(show-paren-style 'expression)
 '(split-width-threshold nil)
 '(sql-mode-hook '((lambda nil (setq tab-width 4 indent-tabs-mode t))))
 '(straight-check-for-modifications '(check-on-save find-when-checking))
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows t)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(use-dialog-box t)
 '(use-package-always-ensure t)
 '(use-package-enable-imenu-support t)
 '(vc-diff-switches '("--diff-cmd" "tkdiff"))
 '(visible-bell t)
 '(warning-suppress-types
   '((comp) (initialization) (comp) (yasnippet backquote-change)))
 '(which-function-mode t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "DejaVu Sans Mono"))))
 '(flycheck-error ((t (:underline (:color "#FE251E" :style line :position nil) :weight bold)))))

;;; custom.el ends here
