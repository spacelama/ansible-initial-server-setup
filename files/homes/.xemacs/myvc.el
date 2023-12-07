;;; vc.el --- drive a version-control system from within Emacs

;; was /usr/share/xemacs/xemacs-packages/lisp/vc/vc.el on suse 8, 

;; Copyright (C) 1992, 93, 94, 95, 96 Free Software Foundation, Inc.

;; changes: make diff-mode, and fontify -- TimC 1/Oct/04


;;;###autoload
(defun vc-version-diff (file rel1 rel2)
  "For FILE, report diffs between two stored versions REL1 and REL2 of it.
If FILE is a directory, generate diffs between versions for all registered
files in or below it.

Interactively, `buffer-file-name' will be used for FILE, unless it's nil, or
unless a prefix arg is used. In the latter two cases, the user will be queried
for the name of FILE.
"
  (interactive
    (let ((file (if (and current-prefix-arg buffer-file-name)
					(read-file-name 
						(if buffer-file-name
							"File or dir to diff: (default visited file) "
						  "File or dir to diff: ")
						default-directory buffer-file-name t)
				  buffer-file-name))
		  (rel1-default nil) (rel2-default nil))
     ;; compute default versions based on the file state
     (cond
      ;; if it's a directory, don't supply any version defauolt
      ((file-directory-p file) 
       nil)
      ;; if the file is locked, use current version as older version
      ((vc-locking-user file)
       (setq rel1-default (vc-workfile-version file)))
      ;; if the file is not locked, use last and previous version as default
      (t
       (setq rel1-default (vc-previous-version (vc-workfile-version file)))
       (setq rel2-default (vc-workfile-version file))))
     ;; construct argument list
     (list file 
	   (let ((minibuffer-default rel1-default)
		 (rel1-choice
		  (read-string (if rel1-default
				   (concat "Older version: (default "
					   rel1-default ") ")
				 "Older version: "))))
	     (if (string-equal rel1-choice "")
		 rel1-default
	       rel1-choice))
	   (let ((minibuffer-default rel2-default)
		 (rel2-choice
		  (read-string (if rel2-default
				   (concat "Newer version: (default "
					   rel2-default ") ")
				 "Newer version (default: current source): ")
			       )))
	     (if (string-equal rel2-choice "")
		 rel2-default
	       rel2-choice)))))
  (if (string-equal rel1 "") (setq rel1 nil))
  (if (string-equal rel2 "") (setq rel2 nil))
  (if (file-directory-p file)
      (let ((camefrom (current-buffer)))
	(set-buffer (get-buffer-create "*vc-status*"))
	(set (make-local-variable 'vc-parent-buffer) camefrom)
	(set (make-local-variable 'vc-parent-buffer-name)
	     (concat " from " (buffer-name camefrom)))
	(erase-buffer)
	(insert "Diffs between "
		(or rel1 "last version checked in")
		" and "
		(or rel2 "current workfile(s)")
		":\n\n")
	(set-buffer (get-buffer-create "*vc-diff*"))
	(cd file)
	(vc-file-tree-walk
	 default-directory
	 (function (lambda (f)
		     (message "Looking at %s" f)
		     (and
		      (not (file-directory-p f))
		      (vc-registered f)
		      (vc-backend-diff f rel1 rel2)
		      (append-to-buffer "*vc-status*" (point-min) (point-max)))
		     )))
	(pop-to-buffer "*vc-status*")
	(insert "\nEnd of diffs.\n")
	(goto-char (point-min))
	(set-buffer-modified-p nil)
	)
    (if (zerop (vc-backend-diff file rel1 rel2))
	(message "No changes to %s between %s and %s." file rel1 rel2)
      (pop-to-buffer "*vc-diff*")))
  (diff-mode)
  (font-lock-mode)
  )
;;; vc.el ends here
