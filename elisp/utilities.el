;;; utilities.el --- Helper functions

;;; Commentary:
;; These are some helper functions that are not used interactively, but in
;; other functions.

;; -*- lexical-binding: t; -*-

;;; Code:

(defun lf:install-package (package)
  "Check if PACKAGE is installed and if not, install it.  Expect quoted argument."
  (unless (package-installed-p package)
    (package-install package))
  (require package))

;; The elisp function 'and' takes a list of arguments. It evaluates them
;; in order, until one of them is nil. Then it returns nil. If none of
;; them is nil, it returns the value of the last argument.

;; The elisp function 'delq' takes two arguments, a value and a list.
;; It deletes every element of the list that is equal to the value.
(defun lf:filter (predicate list)
  "A simple implementation of a filter function."
  (delq nil
	(mapcar (lambda (x) (and (funcall predicate x) x)) list)))

(defun lf:get-next-shell-name ()
  "Generate a new shell name by looking at the smallest number n for which
no shell-n buffer exists."
  (let* ((shell-numbers (lf:filter (lambda (x) (not (null x)))
				   (mapcar #'string-to-number
					   (mapcar (lambda (shell-name) (substring shell-name 5))
						   (lf:filter (lambda (bufname) (string-prefix-p "shell" bufname))
							      (mapcar #'buffer-name (buffer-list)))))))
	 (next-shell-number (lf:get-smallest-not-in-list shell-numbers)))
    (concat "shell" (number-to-string next-shell-number))))

(defun lf:get-smallest-not-in-list (my-list)
  "Return the smallest integer that is not in the list.
MY-LIST is assumed to be a list of integers."
  (letrec ((run (lambda (n)
		  (if (memq n my-list)
		      (funcall run (+ n 1))
		    n))))
    (funcall run 1)))

(defun lf:in-new-tab (name opener)
  "Use OPENER to open a buffer in a new tab called NAME."
  (tab-new)
  (tab-rename name)
  (funcall opener))

(defun lf:concat-with-newline (strings)
  "Concat the list of strings STRINGS as it contained the lines of a text."
  (apply #'concat
	 (mapcar (lambda (string) (concat string "\n")) strings)))

(defun lf:denote-filename-p (string)
  "A predicate that checks if STRING is a valid denote filename."
  (string-match-p (concat "\\`" denote-id-regexp) string))


(provide 'utilities)
;;; utilities.el ends here
