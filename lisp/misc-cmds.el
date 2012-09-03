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
