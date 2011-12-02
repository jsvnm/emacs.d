;;;###autoload
(defun semantic-tag-string (tag &optional no-properties)
  "Returns the string that TAG was made out of, optionally with NO-PROPERTIES."
  (with-current-buffer (semantic-tag-buffer tag)
    (if no-properties
        (buffer-substring-no-properties (semantic-tag-start tag) (semantic-tag-end tag))
      (buffer-substring (semantic-tag-start tag) (semantic-tag-end tag)))))

;;;###autoload
(defun semantic-tags-in-buffer (&optional clear)
  "Fetch tags from current buffer and pp them into another."
  (interactive "P")
  (and clear (semantic-clear-toplevel-cache))
  (setq tags (semantic-fetch-tags))
  (switch-to-buffer-other-window (get-buffer-create "tags"))
  (goto-char (point-max))
  (save-excursion
    (emacs-lisp-mode)
    (pp tags 'insert)
    (insert "\n\n")))

;;;###autoload
(defun semantic-lex-in-buffer (depth)
  "Lex region or whole buffer"
  (interactive "nDepth: ")
  (let (a b buf)
    (setq buf (current-buffer))
    (if (use-region-p)
        (setq a (region-beginning)
              b (region-end))
      (setq a (point-min)
            b (point-max)))
    (setq lex (funcall semantic-lex-analyzer a b depth))
    (switch-to-buffer (get-buffer-create "lex"))
    (goto-char (point-max))
    (save-excursion
      (emacs-lisp-mode)
      (dolist (token lex)
        (princ token 'insert)
        (insert "\n"
                (with-current-buffer buf (buffer-substring (cadr token) (cddr token)))
                "\n"))
      ;;(princ lex 'insert)
      ;;(insert "\n\n")
      )))

;;;###autoload
(defun dired-semanticdb-mk ()
  (interactive)
  (semanticdb-tags-from-files (dired-get-marked-files)))

;;;###autoload
(defun semanticdb-tags-from-files (&rest args)
  (when (and (eq 1 (length args))
             (listp (car args)))
    (setq args (car args)))
  ;; this from semanticdb-mk.el:
  (while args
    (princ (concat "Loading " (car args) "... "))
    (save-window-excursion
      ;; @TODO - RE-WRITE THIS with code from the idle-work process.
      (let* ((buffer (find-file-noselect (car args)))
             (tags nil))
        (set-buffer buffer)
        (setq tags (semantic-fetch-tags))
        (princ (length tags))
        (princ " tags found .\n"))
      (setq args (cdr args))))
  (semanticdb-save-all-db))

(provide 'jsvnm-semantic)
