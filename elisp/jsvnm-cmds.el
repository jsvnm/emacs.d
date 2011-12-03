;;disabled for now TODO: fix it (ido?)
(defsubst the-completing-read (prompt choices &optional predicate require-match
			       initial hist default inherit-input-method)
  "Use `ido-completing-read' if available, otherwise `completing-read'."
  (if nil ;;(fboundp 'ido-completing-read)
      (let ((list (mapfilter (lambda (x)
			      (and (funcall predicate x)
				   (stringify x))
			      ) choices)))
	(ido-completing-read prompt list nil require-match initial hist default))
    (completing-read prompt choices predicate
		     require-match initial hist default
		     inherit-input-method)))

;;;###autoload
(defun buffer-file-name-kill ()
  "Put buffer-file-name to kill-ring and display it."
  (interactive)
  (let ((n  (buffer-file-name)))
      (kill-new n)
      (message n)))

;;;###autoload
(defun fix-whitespace ()
  "Delete trailing whitespace and untabify whole buffer."
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

;; (@* "set-stuff")
;;;###autoload
(defun set-face (&optional face &rest attributes)
  "Set face interactively.

\(fn FACE [PROPERTYNAME VALUE]..."
  (interactive)
  (or-set face (symbolify (the-completing-read
               "Face: " obarray 'facep t (stringify (face-at-point)))))
  (or-set attributes (read (concat "("  (read-string
          "" (strip (apply 'concat (mapfilter
                                    (lambda (kv) (let ((k (car kv))
                                                  (v (cdr kv)))
                                              (and (not (eq 'unspecified v))
                                                   (format "%-15s %s\n" k v))))
                                    (face-all-attributes face))))) ")" )))
  (apply 'set-face-attribute face nil attributes))

;;;###autoload
(defun my-set-variable (var)
  "Set value to VAR interactively."
  (interactive "SVariable: ")
  (let ((sym (symbolify var)))
    (set sym (eval-minibuffer (format "Set %s:\n" var)
              (strip (pp-to-string (custom-quote (symbol-value sym))))))))

;;;###autoload
(defun set-variable-at-point ()
  "Set variable nearest to point, or ask which one to set if none."
  (interactive)
  (let ((var (symbolify (thing-at-point 'symbol))))
    (unless (and var (boundp var))
      (setq var (the-completing-read "Var: " obarray 'boundp)))
    (my-set-variable var)))


;; (@* "color-theme -related")
;;;###autoload
(defun fontify-theme-font ()
  "Fontify, in theme definition, the font definition at current line."
  (interactive)
  (let ((p (point)))
    (beginning-of-line)
    (down-list)
    (let* ((n    (symbol-at-point t))
           (na   (point))
           (nb   (progn (down-list) (backward-char) (point)))
           (nc   (progn (end-of-sexp) (point)))
           (sstr (buffer-substring-no-properties nb nc))
           (spec (read sstr))
           (ol   (make-overlay na nb)))
      (face-spec-set n spec)
      (overlay-put ol 'type 'theme-fontification)
      (overlay-put ol 'face n)
      (goto-char p))))

;;;###autoload
(defun defontify-theme-def (&optional beg end)
  "Remove fontifications.  Between BEG and END, if they're given."
  (interactive)
  (remove-overlays beg end 'type 'theme-fontification))

;;;###autoload
(defun fontify-theme-def ()
  "Fontify all font definitions in theme definition."
  (interactive)
  (save-excursion
    (or (re-search-forward  "(color-theme-install" nil t)
        (re-search-backward "(color-theme-install" nil t)
        (error "not found"))
    (let* ((a (progn (goto-char (match-end 0))
                     (down-list 1)
                     (forward-sexp 3)
                     (point)))
           (b (progn (goto-char (match-beginning 0))
                     (forward-sexp 1)
                     (down-list -2)
                     (point)))
           (vars (save-excursion
                   (beginning-of-defun)
                   (and (re-search-forward "(let\\*?")
                        (sexp-match-forward t)
                        (match-string-no-properties 1))))
           (defs (buffer-substring-no-properties a b))
           (faces (eval (read (format "(let %s `(%s))" vars defs))))
           )
      (defontify-theme-def a b)
      (dolist (f faces) (face-spec-set (car f) (cadr f)))
      (goto-char a)
      (while (sexp-match-forward
              "\\(\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-+\\)" b t)
        (let ((face (symbolify (match-string 2))))
          (match-put-overlay 1
                'type 'theme-fontification
                'face face))))))

;;;###autoload
(defun insert-dragged-color ()
  "if dragged on string or field, replace.  Otherwise just insert."
     (interactive)
     (with-point-at-mouse
      (let ((pr (get-text-property (point) 'face)))
        (case pr
          ((font-lock-string-face widget-field)
           (let ((a   (previous-property-change (point)))
                 (b   (next-property-change     (point)))
                 (fmt (case pr ('widget-field          "%s")
                               ('font-lock-string-face "%S"))))
                (delete-region a b)
                (goto-char a)
                (insert (format fmt ns-input-color))))
          (t (insert ns-input-color))))))
