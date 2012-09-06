(defvar org-redmine-conf-list)

(defun org-redmine-load-confs ()
	(let ((loaded (read (file-string "~/.redmine-confs.el"))))
		(setq org-redmine-conf-list loaded)))


(defun org-redmine-select-conf (conf)
	(interactive
	 (let ((key (intern (completing-read "Redmine server: " org-redmine-conf-list))))
		 (pp key)
		 (list (assoc key org-redmine-conf-list))))
	(let ((key (car conf))
				(conf (cdr conf)))
		(setq org-redmine-uri (plist-get conf :url))
		(setq org-redmine-auth-api-key (plist-get conf :key))
		(message "selected %s, at %s" key org-redmine-uri)
		))	

(org-redmine-load-confs)
(org-redmine-select-conf (car org-redmine-conf-list))

