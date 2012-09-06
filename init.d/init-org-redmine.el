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


(defun redmine-json-issue (project_id subject &rest args)
	"For creating a new issue"
	(let (o)
		(setq o (json-add-to-object o "project_id" project_id))
		(setq o (json-add-to-object o "subject" subject))
		(while args
			(let ((k (car  args))
						(v (cadr args)))
				(setq args (cddr args))
				(setq o (json-add-to-object o (format "%s" k) v))))
	  (json-add-to-object (json-new-object) "issue" o)))

;;------------------------------
;; org-redmine connection functions
;;------------------------------
(defun org-redmine-curl (uri method &optional data)
  "method should be GET PUT POST ..."
  (ignore-errors (kill-buffer org-redmine-curl-buffer))
  (unless (eq 0 (apply 'call-process "curl" nil `(,org-redmine-curl-buffer nil) nil
                       (org-redmine-curl-args-for uri method data)
                       ))
    (signal 'org-redmine-exception-not-retrieved "The requested URL returned error"))
  (save-current-buffer
    (set-buffer org-redmine-curl-buffer)
    (let ((json-object-type 'hash-table)
          (json-array-type 'list))
      (condition-case err
          (json-read-from-string (buffer-string))
        (json-readtable-error
         (message "%s: Non JSON data because of a server side exception. See %s"
                  (error-message-string err) org-redmine-curl-buffer))))))

(defun org-redmine-curl-args-for (uri method &optional data)
  (let ((args `("-X" ,method "-s" "-f")))
    (append
     args
     (cond (org-redmine-auth-api-key
						`("-H" ,(format "X-Redmine-API-Key: %s" org-redmine-auth-api-key)))
           (org-redmine-auth-username
            `("-u" ,(format "%s:%s" org-redmine-auth-username (or org-redmine-auth-password ""))))
           (org-redmine-auth-netrc-use '("--netrc"))
           (t ""))
		 (and data
					`("-H" "Content-Type: application/json"
						"-d" ,(format "%s" data)))
     `(,uri))))


(defun org-redmine-create-issue (project_id subject &rest args)
	"Create new issue in PROJECT_ID with SUBJECT, ARGS is KEY VALUE pairs for issue"
	(org-redmine-curl (format "%s/issues.json" org-redmine-uri) "POST"
									(json-encode
									 (apply 'redmine-json-issue project_id subject args))))


