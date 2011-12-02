


(defun jsvnm/add-faces-to-theme (regexp)
  (interactive (list (read-regexp "List faces matching regexp")))
  (let ((faces (delq nil (mapcar
                 (lambda (f) (when (string-match regexp (symbol-name f)) f))
                 (sort (face-list) #'string-lessp)))))
    (unless faces (error "No faces matching \"%s\"" regexp))
    (mapcar 'custom-theme-add-face faces)))

(defun jsvnm/custom-toggle-all-theme-faces ()
  (interactive)
  (dolist (face custom-theme-faces)
        (widget-apply-action (nth 1 (widget-get (nth 2 face) :buttons)))
        ))  


(defun jsvnm/find-url-contents (url &optional keep-headers)
  "Fetch contents of URL, no headers unless KEEP-HEADERS is t.
Use `set-auto-mode' on the buffer, with `buffer-file-name'
temporarily set to the original name. Show buffer when done."
  (interactive "sURL:\nP")
  (require 'url)
  (unless (url-p url) (setq url (url-generic-parse-url url)))
  (lexical-let ((file (url-filename url))
                (name (url-recreate-url url))
                (keep-headers keep-headers))
    (url-retrieve url
                  (lambda (&rest ignored)
                    (let ((buffer-file-name file)) (set-auto-mode))
                    (setq buffer-name name)
                    (unless keep-headers
                      (goto-char (point-min))
                      (delete-region (point-min) (search-forward "\n\n")))
                    (switch-to-buffer (current-buffer))))))

