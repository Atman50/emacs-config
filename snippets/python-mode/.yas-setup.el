;;; .yas-setup.el --- fix the friggin' yasnippets python stuff
;;; -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'yasnippet)
(defvar yas-text)

(defvar my/python-split-arg-arg-regex
  "[[:blank:]]*\\([[:alnum:]_]+\\)\\([[:blank:]]*:[[:blank:]]*[^=]*\\)?[[:blank:]]*\\(=[[:blank:]]*\\(.+\\)[[:blank:]]*\\)?"
  "Regular expression matching an argument of a python function.
First group should give the argument name.")

(defun my/count-chars (char str)
  "Count up the number of matching char CHAR in string STR."
  (if str
      (let ((s (char-to-string char))
            (count 0)
            (start-pos -1))
        (while (setq start-pos (string-search s str (+ 1 start-pos)))
          (setq count (+ 1 count)))
        count)
    0))

(defun my/python-separate-args (arg-string)
  "Separate comma delimited arguments in ARG-STRING to a list of arguments.
Try to fix overzealous separation by only commas.
No perfect in the least, but it's better than the alternative - completely
broken complex type hinting."
  (let* ((sep-args (split-string arg-string "," t))
         (final-sep-args nil)
         (held-arg nil))
    (dolist (sep-arg sep-args)
      (let* ((quotes (cl-oddp (+ (my/count-chars ?\' sep-arg) (my/count-chars ?\" sep-arg))))
             (quotes-held (cl-oddp (+ (my/count-chars ?\' held-arg) (my/count-chars ?\" held-arg))))
             (braces-count (- (my/count-chars ?\[ sep-arg) (my/count-chars ?\] sep-arg)))
             (braces-held-count (- (my/count-chars ?\[ held-arg) (my/count-chars ?\] held-arg))))
        ;; Strings take precedent
        (if (or quotes quotes-held (not (= braces-count 0)) (not (= braces-held-count 0)))
            (if (or (and quotes quotes-held)
                    (and (not (= braces-held-count 0))
                         (= braces-count braces-held-count)))
                ;; Completed either quotes or braces
                (setq final-sep-args (add-to-list 'final-sep-args (string-trim (concat held-arg "," sep-arg)) t)
                      held-arg nil)
              ;; save the argument for the next segment
              (if held-arg
                  (setq held-arg (concat held-arg "," sep-arg))
                (setq held-arg sep-arg)))
          ;; Just an argument
          (setq final-sep-args (add-to-list 'final-sep-args (string-trim sep-arg) t)))))
    (if held-arg
        (setq final-sep-args(add-to-list 'final-sep-args (string-trim held-arg) t)))
    final-sep-args))

(defun my/python-split-args (arg-string)
  "Split a python argument string ARG-STRING into a tuple of argument names."
  (mapcar (lambda (x)
            (when (string-match my/python-split-arg-arg-regex x)
              (list (concat (match-string-no-properties 1 x) (match-string-no-properties 2 x))
                    (match-string-no-properties 4 x))))
          (my/python-separate-args arg-string)))

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
