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
   (quote
    ("\\\\*.dll" "\\\\*.png" "\\\\*.jpeg" "\\\\*.exe" "\\\\*.csproj")))
 '(auto-save-default nil)
 '(browse-url-browser-function (quote browse-url-chrome))
 '(browse-url-chrome-program
   (if mswindows-p "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe" "/usr/bin/google-chrome"))
 '(c-tab-always-indent nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(command-log-mode-auto-show t)
 '(command-log-mode-is-global t)
 '(command-log-mode-open-log-turns-on-mode t)
 '(compilation-error-regexp-alist (quote (ant bash java gnu gcc-include lcc perl)))
 '(compilation-error-regexp-systems-list (quote (gnu perl)))
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(counsel-mode t)
 '(dabbrev-case-fold-search nil)
 '(desktop-save-mode t)
 '(diff-switches "-bc")
 '(dired-listing-switches "-alh")
 '(ediff-split-window-function (quote split-window-vertically))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode t)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults))
 '(eshell-history-size 1024)
 '(fill-column 132)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flymake-no-changes-timeout 2)
 '(font-lock-maximum-size nil)
 '(git-commit-fill-column 78)
 '(git-commit-style-convention-checks (quote (non-empty-second-line overlong-summary-line)))
 '(global-font-lock-mode t nil (font-lock))
 '(history-delete-duplicates t)
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 36 36 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark modified read-only " "
           (name 36 -1)))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-mode t)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(line-move-visual nil)
 '(line-number-display-limit-width 40000)
 '(lsp-ui-doc-border "black")
 '(lsp-ui-sideline-ignore-duplicate t)
 '(magit-completing-read-function (quote ivy-completing-read))
 '(magit-repository-directories (quote (("~/repos" . 1))))
 '(magit-submodule-arguments (quote ("--recursive")))
 '(make-backup-files nil)
 '(my/use-elpy t)
 '(org-babel-load-languages
   (quote
    ((shell . t)
     (plantuml . t)
     (python . t)
     (emacs-lisp . t))))
 '(org-catch-invisible-edits (quote show))
 '(org-html-postamble t)
 '(org-html-postamble-format
   (quote
    (("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %T</p>
<p class=\"creator\">%c</p>"))))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
 '(org-reveal-title-slide nil)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/"))))
 '(package-selected-packages
   (quote
    (flycheck python-docstring pylint company-jedi elpy htmlize org-autolist org-bullets git-timemachine magit-find-file magit-filenotify magit git-commit omnisharp yasnippet-snippets yasnippet company-prescient ivy-prescient prescient ivy-posframe ivy-hydra counsel-codesearch counsel-projectile counsel company powershell lispy xterm-color ibuffer-projectile projectile realgud groovy-mode vlf which-key codesearch command-log-mode doom-modeline leuven-theme helpful diminish synosaurus use-package org-plus-contrib)))
 '(powerline-display-hud nil)
 '(projectile-completion-system (quote ivy))
 '(projectile-mode-line (quote (:eval (format " [%s]" (projectile-project-name)))))
 '(python-check-command
   (if mswindows-p "c:/Python36/Scripts/flake8.exe" "/usr/local/bin/flake8"))
 '(python-flymake-command (quote ("flake8" "-")))
 '(python-indent-trigger-commands (quote (yas-expand)))
 '(python-shell-interpreter "jupyter")
 '(python-shell-interpreter-args "console --simple-prompt")
 '(python-shell-prompt-detect-failure-warning nil)
 '(python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
 '(python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
 '(rng-nxml-auto-validate-flag nil)
 '(savehist-additional-variables (quote (kill-ring search-ring regexp-search-ring)))
 '(savehist-file "~/.emacs.d/savehist")
 '(savehist-mode t)
 '(scroll-bar-mode (quote none))
 '(sentence-end-double-space nil)
 '(show-paren-mode t nil (paren))
 '(show-paren-style (quote expression))
 '(split-width-threshold nil)
 '(sql-mode-hook
   (quote
    ((lambda nil
       (setq tab-width 4 indent-tabs-mode t)))))
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-dialog-box t)
 '(use-package-always-ensure t)
 '(use-package-enable-imenu-support t)
 '(vc-diff-switches (quote ("--diff-cmd" "tkdiff")))
 '(visible-bell t)
 '(which-function-mode t)
 '(which-key-mode t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "DejaVu Sans Mono"))))
 '(Info-quoted ((t (:foreground "red3"))))
 '(doom-modeline-buffer-modified ((t (:inherit (doom-modeline-warning bold)))))
 '(doom-modeline-project-dir ((t (:inherit doom-modeline-info))))
 '(ivy-posframe ((t (:inherit default :background "LightSkyBlue1" :foreground "#333333"))))
 '(ivy-subdir ((t (:inherit dired-directory :background "LightSkyBlue1"))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "royal blue" :box (:line-width -1 :color "royal blue") :weight ultra-bold :height 0.99))))
 '(lsp-ui-sideline-global ((t (:background "blanched almond"))))
 '(lsp-ui-sideline-symbol ((t (:foreground "sea green" :box (:line-width -1 :color "sea green") :weight ultra-bold :height 0.99))))
 '(region ((t (:background "sky blue" :distant-foreground "gtk_selection_fg_color")))))

;;; custom.el ends here
