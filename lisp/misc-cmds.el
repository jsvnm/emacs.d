(defun spotlight ()
	(interactive)
	(let ((locate-command "mdfind"))
		(call-interactively 'locate nil)))

(defun current-file? ()
	(interactive)
	(let ((bfn (buffer-file-name)))
		(message (if bfn (kill-new bfn)
							 "No file in buffer!!!"))))

(defun describe-keymap (kmap)
	(interactive (list (completing-read "Keymap: " obarray
		 (lambda (sym) (or (keymapp sym)
											  (and (boundp sym)
														 (keymapp (eval sym)))
											 )))))
	(help-setup-xref (list #'describe-keymap kmap) (called-interactively-p 'interactive))
	(with-help-window (help-buffer)
		(princ (substitute-command-keys
						(format "description of variable %s\n\\{%s}" kmap kmap)))
		(help-make-xrefs (help-buffer))))


(defun ssh-status ()
	(interactive)
	(let ((st (call-process "ssh-add" nil nil nil "-l")))
		(when (called-interactively-p)
			(message "ssh-agent %s"
							 (cdr (assq st '((0 . "has your identity")
															 (1 . "is running with no identity")
															 (2 . "is hiding"))))))
		st))

(defun ssh-add (arg)
	(interactive "P")
	(let ((st (ssh-status)))
		(if (eq 2 st) (message "agent not found")
			(if (null arg) (if (eq 0 st) (message "no need to add")
											 (call-process "ssh-add" nil nil nil))
				(if (eq 1 st) (message "no need to remove")
					(call-process "ssh-add" nil nil nil "-d"))))))













