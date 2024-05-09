;;; openers.el --- Functions that open files.

;;; Commentary:
;; These are some functions to facilitate binding the opening of certain
;; files to keys.

;; -*- lexical-binding: t; -*-

;;; Code:

(require 'utilities)
(require 'lfp)

(defun lf:open-emacs-config-file ()
  (interactive)
  (let* ((elisp-files (mapcar (lambda (file-name) (concat "elisp/" file-name))
			       (directory-files (concat lf:emacs-repo "elisp") nil "^[^.#].*")))
	 (config-file (completing-read "Emacs config file to open: "
				       (cons "emacs.org"
					     elisp-files))))
    (find-file (concat lf:emacs-repo config-file))))

(defun lf:open-emacs-config-file-in-new-tab ()
  (interactive)
  (lf:in-new-tab "config" #'lf:open-emacs-config-file))

(defun lf:open-note ()
  "Choose a file from the list of denote files in 'notes-directory'."
  (interactive)
  (let ((note (completing-read "Note to open: "
			       (append (directory-files notes-directory t)
				       (directory-files (expand-file-name "~/code/scout") t)))))
    (find-file note)))

(defun lf:open-note-in-new-tab ()
  (interactive)
  (lf:in-new-tab "notes" #'lf:open-note))

(defun lf:open-book ()
  (interactive)
  (let* ((books-folder (expand-file-name "~/dox/books/"))
	 (book (completing-read "Book to open: "
				(directory-files books-folder)))
	 (book-file (concat books-folder book))
	 (book-file-extension (file-name-extension book)))
    (cond ((string-equal book-file-extension "epub")
	   (erpe:open-epub book-file))
	  (t (find-file book-file)))))

(defun lf:open-book-in-new-tab ()
  (interactive)
  (tab-new)
  (lf:open-book)
  (tab-rename (car
	       (parse-book-filename
		(file-name-nondirectory
		 (buffer-file-name))))))


(defun lf:open-nix-file ()
  (interactive)
  (let* ((nix-folder (expand-file-name "~/code/system/"))
	 (nix-file (completing-read "nix file to open: "
				    (lf:filter
				     (lambda (file) (string= "nix" (file-name-extension file)))
				     (directory-files nix-folder)))))
    (find-file (concat nix-folder nix-file))))

(defun lf:open-nix-file-in-new-tab ()
  (interactive)
  (lf:in-new-tab "nix" #'lf:open-nix-file))

(defun lf:open-emms-in-new-tab ()
  (interactive)
  (lf:in-new-tab "music" emms-smart-browse))

(defun lf:open-elfeed-in-new-tab ()
  (lf:in-new-tab "rss" elfeed)
  (elfeed-search-fetch nil))

(defun lf:open-new-shell ()
  "Open a new `shell-mode' buffer with an automatically generated name.
Usually shell-$number."
  (interactive)
  (let ((bufname (lf:get-next-shell-name)))
    (progn (shell)
	   (rename-buffer bufname))))

(defun lf:open-new-shell-in-new-tab ()
  (interactive)
  (lf:in-new-tab "cli" #'lf:open-new-shell))

(provide 'openers)
;;; openers.el ends here
