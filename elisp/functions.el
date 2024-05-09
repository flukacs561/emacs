;;; functions.el --- Custom function for my personal Emacs config.

;;; Commentary:
;; This file contains the helper functions that provide custom
;; functionality for my personal Emacs config.

;; -*- lexical-binding: t; -*-

;;; Code:
(require 'utilities)

(defun lf:display-line-numbers-off ()
  "Switch off line numbers."
  (interactive)
  (display-line-numbers-mode 0))

(defun lf:highlight-line-mode-off ()
  "Switch off line highlighting."
  (interactive)
  (hl-line-mode -1))

(defun lf:split-and-switch ()
  "Open a new buffer ot the right and switch to it."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun lf:close-window ()
  "Close current window and delete the window that takes its place."
  (interactive)
  (kill-current-buffer)
  (delete-window))

(defun lf:enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 10))

(defun lf:shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 10))

(defun lf:new-tab ()
  "Open a new tab and immediately ask for its name."
  (interactive)
  (let ((tab-name (read-string "Enter name for new tab: "
			       nil
			       nil
			       #'tab-bar-tab-name-function)))
    (tab-new)
    (tab-rename tab-name)))

(defun lf:tab-move-to-position ()
  (interactive)
  (let ((position (read-number "Enter new position for tab: "
			       1)))
    (tab-move-to position)))

(defun lf:close-current-buffer-and-tab ()
  (interactive)
  (kill-current-buffer)
  (tab-close))

(defun lf:dired-kill-unmarked-lines ()
  "Kill unmarked lines in a Dired filtering context."
  (interactive)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun lf:eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0))) (current-buffer))
    (error
     (message "Invalid expression.")
     (insert (current-kill 0)))))

(defun lf:pdf-zoom ()
  "Zoom in 5%."
  (interactive)
  (pdf-view-enlarge 1.05))

(defun lf:want-to-tangle ()
  "Prompt for a file to tangle the contents of the current (org-babel) buffer to."
  (interactive)
  (let ((file-name (read-file-name "Name of file to tangle to: ")))
    (insert (concat "#+property:   header-args :tangle "
		    (expand-file-name file-name)))))

(defun lf:want-to-export-latex ()
  "Prompt for a latex class to use for exporting.
If the `dokumentum' class is chosen, inserts some additional
latex headers."
  (interactive)
  (let ((class (completing-read "Name of latex class: "
				(mapcar #'car org-latex-classes))))
    (insert "#+latex_class: " class)
    (if (string= class "dokumentum")
	(let* ((starting-pos (point))
	       (fst-line-end (progn (forward-line 1)
				    (line-end-position)))
	       (title (buffer-substring 15 fst-line-end))
	       (start-of-author (progn (forward-line 2)
				       (forward-char 10)
				       (point)))
	       (snd-line-end (progn (forward-line 2)
				    (line-end-position)))
	       (author (buffer-substring start-of-author snd-line-end)))
	  (goto-char starting-pos)
	  (insert "\n#+latex_header: \\fancyhead[L]{" title "}")
	  (insert "\n#+latex_header: \\fancyhead[R]{" author "}"))
      nil)))

(defun lf:dired-open-file ()
  "In Dired, open the file at point."
  (interactive)
  (let* ((file (dired-get-filename nil t))
	 (extension (file-name-extension file)))
    (cond ((string-equal extension "html") (eww-open-file file))
	  (t (call-process "xdg-open" nil 0 nil file)))))

(defun lf:open-with-eww ()
  "In Dired, open file at point in eww."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (eww-open-file file)))

(defun lf:find-file-externally ()
  (interactive)
  (let ((file (read-file-name "Find file externally: ")))
    (call-process "xdg-open" nil 0 nil file)))

(defun lf:open-yt-link-with-mpv ()
  "Open provided link with mpv."
  (interactive)
  (let ((link (read-string "Link: ")))
    (call-process "mpv" nil 0 nil link)))

(defun lf:elfeed-open-with-mpv ()
  "Open the link of the current entry with mpv."
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (call-process "mpv" nil 0 nil link)))

(defun lf:insert-shell-command-output ()
  "Insert the output of the shell command."
  (interactive)
  (shell-command
   (read-shell-command "Shell command: ")
   t))

(defun lf:insert-backup-commit-message ()
  (interactive)
  (insert "backup ")
  (shell-command "date +\"%Y%m%dT%H%M%S\"" t))

(defun lf:insert-ddate ()
  (interactive)
  (shell-command "hxh ddate" t))

(provide 'functions)
;;; functions.el ends here
