;;; init-tramp.el --- Emacs configuration for tramp
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

;; Some configuration for tramp.

;;; Code:

(setq-default tramp-default-method "ssh")
(setq-default tramp-auto-save-directory (expand-file-name "~/.backups/tramp/"))
(setq-default tramp-chunksize 8192)

;; sudo save
(defun sudo-save ()
  "Save a file using sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x M-s") #'sudo-save)

(provide 'init-tramp)
;;; init-tramp.el ends here
