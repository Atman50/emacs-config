;;; .yas-setup.el --- fix the friggin' yasnippets python stuff
;;; -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'yasnippet)
(defvar yas-text)

(defvar my/python-split-arg-arg-regex
"[[:blank:]]*\\([[:alnum:]*_*]+\\)\\([[:blank:]]*:[[:blank:]]*[^=[:blank:]]*\\)?[[:blank:]]*\\(=[[:blank:]]*\\(.+\\)[[:blank:]]*\\)?"
"Regular expression matching an argument of a python function.
First group should give the argument name.")

(defvar my/python-split-arg-separator
"[[:space:]]*,[[:space:]]*"
"Regular expression matching the separator in a list of argument.")

(defun my/python-split-args (arg-string)
  "Split a python argument string ARG-STRING into a tuple of argument names."
  (mapcar (lambda (x)
            (when (string-match my/python-split-arg-arg-regex x)
              (list (concat (match-string-no-properties 1 x) (match-string-no-properties 2 x))
                    (match-string-no-properties 4 x))))
          (split-string arg-string my/python-split-arg-separator t)))

(defun my/python-args-to-docstring ()
  "Return docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (my/python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                          (lambda (x)
                            (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                                    (if (nth 1 x) (concat "\(default = " (nth 1 x) "\)"))))
                          args
                          indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent))))

;;; .yas-setup.el ends here
