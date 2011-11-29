

(defun dired-semanticdb-mk ()
  (interactive)
  (semanticdb-tags-from-files (dired-get-marked-files)))

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