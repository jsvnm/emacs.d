(require 'anything-config)
(require 'anything-match-plugin)
(require 'anything-complete)

(anything-lisp-complete-symbol-set-timer 150)

;;*directory
(defun make-anything-directory-source (source-name dir)
  "Returns an anything source for a particular directory"
  `((name . ,(concat source-name))
		(candidates . (lambda () (directory-files ,dir)))
		(action . find-file)
		(type . file)))

(setq anything-sources
			`(anything-c-source-emacs-function-at-point
				anything-c-source-emacs-variable-at-point
				anything-c-source-emacs-face-at-point
				;; anything-c-source-browse-code
				;; anything-c-source-org-keywords
				;; anything-c-source-org-headline
				;; anything-c-source-imenu
				anything-c-source-buffers-list
				anything-c-source-recentf
				anything-c-source-files-in-current-dir+
				))

(global-set-key (kbd "H-a") 'anything)
(define-key help-map "a" 'anything-apropos)

