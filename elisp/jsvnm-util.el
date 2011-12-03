;;;###autoload
(defun strip (str &optional each-line)
 "Strip leading and trailing whitespace from STR. if EACH-LINE is non-nil, do that by line"
  (let ((s  (stringify str))
        (re (if each-line "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)"
                "\\(\\`[[:space:]\n]*\\|[[:space:]\n]*\\'\\)")))
    (replace-regexp-in-string re "" s)))

;;;###autoload
(defsubst stringify (it)
  "Get string of IT."
  (cond ((stringp it) it)
        ((symbolp it) (symbol-name it))
        (t (pp-to-string it))))

;;;###autoload
(defsubst symbolify (it)
  "Get symbol of IT."
  (if (symbolp it) it (intern it)))


;; (@* "or-set VAR FORMS...")
;; (or-set x 30) in elisp is kinda lke x|=30 in ruby
;;;###autoload
(defmacro or-set (var &rest forms)
  "Return VAR if it's not nil. Otherwise set VAR to the result of evaluating FORMS."
  (or (and (boundp var) (eval var) var)
      `(setq ,var (progn ,@forms))))


;;; borrowed from swank-fuzzy.el which in turn borrowed from slime.el
;;;###autoload
(defun rcurry (fun &rest args)
  "Curry FUN with ARGS from the right."
  `(lambda (&rest more) (apply ',fun (append more ',args))))

;; and this seems logical, given above
;;;###autoload
(defun lcurry (fun &rest args)
  "Curry FUN with ARGS from the left."
  `(lambda (&rest more) (apply ',fun (append ',args more))))



;;;###autoload
(defalias 'uniq 'remove-duplicates)

;;;###autoload
(defmacro uniq! (seq &rest args)
  "Does (setq SEQ (uniq SEQ [KEYWORD VALUE])).

\(fn SEQ [KEYWORD VALUE]...)"
  `(setq ,seq (uniq ,seq ,@args)))

;;;###autoload
(defun filter-list (condp lst)
  "Return list of those elements in LST that CONDP element returns true."
  (delq nil (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

;;;###autoload
(defmacro filter (spec &rest pred)
  "Return those elements from LIST that,
when VAR is set to each element in turn,
PRED returns non-nil

\(fn (VAR LIST) PRED...)"
  (let ((tmp (make-symbol "'-js-filter-tmp--"))
        (ret (make-symbol "'-js-filter-ret--")))
    `(block nil
       (let ((,tmp ,(nth 1 spec)) ,ret ,(car spec))
         (while ,tmp
           (if (progn (setq ,(car spec) (car ,tmp)) ,@pred)
               (setq ,ret (append ,ret (list ,(car spec)))))
               (setq ,tmp (cdr ,tmp)))
         ,ret))))


;;;###autoload
(defun mapfilter (fn lst)
  "Applies FN to each element in LST, like `mapcar', but then
removes all nil elements from result"
  (delq nil (mapcar (lambda (elt) (funcall fn elt)) lst)))

;;;###autoload
(defun mapn (n fn the-list)
  "Apply FN to N elements from THE-LIST at a time, return list of results"
  (let ((len (length the-list)))
    (unless (eq 0 (% len n))
      (error "List has %d elements, that's not divisible by %d" len n))
    (let ((lst (reverse the-list))
          (ret ()))
      (dotimes (nn (/ len n) ret)
        (let ((args ()))
          (dotimes (i n) (push (pop lst) args))
          (setq ret `(,(apply fn args) ,@ret))))))) 

;;;###autoload
(defmacro takewhile (spec &rest pred )
  "Take elements from list until pred returns false. Does not modify the original.
Evaluate PRED with VAR bound to each `car' from LIST. If the result is not nil,
add the value to result list.

\(fn (VAR LIST) PRED...)"
  (let ((tmp (make-symbol "--takewhile-tmp--"))
        (ret (make-symbol "--takewhile-ret--"))
        )
    `(block nil
       (let ((,tmp ,(nth 1 spec))
             ,ret ,(car spec))
         (while (and ,tmp
                     (progn
                       (setq ,(car spec) (car ,tmp))
                       ,@pred))
                   (setq ,tmp (cdr ,tmp)
                         ,ret (append ,ret (list ,(car spec)))))
                 ,ret))))

;;(takewhile (x '(30 20 12030 2)) (> 100 x))
;;;###autoload
(defmacro takewhile! (spec &rest pred )
  "Take elements from list until pred returns false. Modifies the original list.
Evaluate PRED with VAR bound to each `car' from LIST. If the result is not nil,
remove element from LIST and add it to result.

\(fn (VAR LIST) PRED...)"
  (let ((ret (make-symbol "--takewhile-ret--")))
    (list 'block nil
          (list 'let
                 (list ret (car spec))
                 (list 'while
                        (list 'and
                              (nth 1 spec)
                              `(progn
                                 ,(list 'setq
                                        (car spec)
                                        (list 'car (nth 1 spec)))
                                 ,@pred))
                        (list 'setq
                              ret
                              (list 'append
                                    ret
                                    (list 'list (car spec)))
                              (nth 1 spec)
                              (list 'cdr (nth 1 spec))))
                 ret))))

;; (setq lst '(kala koira "kukko" 3 2 koira "hevonen"))
;; (takewhile! (x lst) (or (symbolp x) (stringp x)))
;; lst



;;;###autoload
(defun sexp-match-forward (&optional args bound noerror)
  "Match sexps forward according to ARGS.
ARGS is either an integer (to match that many sexps),
a regexp, t, nil or a list, where each arg is either
a regexp, t, or nil. Other than integer non-list ARGS get
treated like they were a list of one element.
If t, that sexp is added as a matched subexp. If nil, then not,
but sexp still goes to match item 0.
If regexp, then that regexp must match the sexp. Any subexps
it matches are added."
  (or-set args '(t))
  (and (integerp args)
       (let ((count args))
         (setq args ())
         (dotimes (not-used count) (push t args))))
  (or (listp args) (setq args (list args)))
  (let ((start (point))
        aa a b md)
    (condition-case err
        (progn
          (dolist (arg args)
            (forward-sexp 1)
            (setq b (point))
            (and bound
                 (> b bound)
                 (error "Out of bounds. (%s, limit was %s)" b bound))
            (save-excursion   (backward-sexp 1) (setq a (point)))
            (when (< a start) (error "Backward out of bounds! (started at %s, now at %s)" start a))
            (or-set aa a)
            (case arg
              ((t) (setq md (append md (list a b))))
              (t   (let* ((sexp (buffer-substring a b))
                          (remd (or (and (string-match arg sexp) (match-data))
                                    (error "regexp didn't match")))
                          (subs (mapcar (lambda (x) (+ a x)) (cddr remd))))
                     (setq md (append md subs))))
              ))
          (setq md (append (list aa b) md))
          (set-match-data md)
          aa)
      ((error)
       (goto-char start)
       (set-match-data nil)
       (or noerror (signal (car err) (cons "in sexp-match-forward-maybe" (cdr err))))
       nil))))

;;;###autoload
(defsubst match-properties (&optional subexp obj where)
  "Returns the properties of last match.
SUBEXP defaults to 0.
OBJ is just passed to `text-properties-at'
WHERE specifies the place within that SUBEXP.  May be nil or a number.
If nil, use (`match-beginning SUBEXP').  If positive, add that to beginning.
If negative, then the end."
  (or-set subexp 0)
  (setq where
        (if where (or (and (<= 0 where) (+ (match-beginning subexp) where))
                      (and (>  0 where) (+ (match-end subexp) where)))
          (match-beginning subexp)))
  (text-properties-at where obj))

;;;###autoload
(defsubst replace-match-keeping-properties (newtext &optional fixedcase
                                            literal obj subexp addprops)
  (or-set subexp 0)
  (let* ((oldps (match-properties subexp obj))
         (ps    (if addprops `(,@addprops ,@oldps) oldps))
         (new   (apply 'propertize newtext ps)))
    (replace-match new fixedcase literal obj subexp)))

;; (let ((str (propertize "kala" 'face 'highlight)))
;;   (string-match "kala" str)
;;   (replace-match-keeping-properties "foo" nil nil str))


;;;###autoload
(defsubst match-put-text-property (property value &optional object subexp)
  "Puts a text property to last match.
SUBEXP defaults to 0.
Does (`put-text-property' (`match-beginning' SUBEXP)
 (`match-end' SUBEXP) PROPERTY VALUE OBJECT)"
  (or-set subexp 0)
  (put-text-property (match-beginning subexp) (match-end subexp) property value object))

;;;###autoload
(defsubst match-put-overlay (subexp &rest args)
  "Put overlay to SUBEXP with given properties.

\(fn SUBEXP [PROPERTY VALUE]...)"
  (let ((ol (make-overlay (match-beginning subexp)
                          (match-end subexp))))
    (while args
      (overlay-put ol (pop args) (pop args)))
    ol))

;;;###autoload
(defun dropdown-list-pairs (display-real-pairs)
  "takes a list of (display real) pairs, like:
'((\"display\" 'foobar) (\"kala\" \"koira\").
displays a menu using the first item of each pair.
returns the corresponding second item."
  (let* ((ds (mapcar (lambda (x) (car x))  display-real-pairs))
         (rs (mapcar (lambda (x) (cadr x)) display-real-pairs))
         (n  (dropdown-list ds)))
    (when n  (nth n rs))))


;;;###autoload
(defun test-regexp (reg &optional str)
  (or-set str (buffer-string))
  (if (string-match reg str)
      (let* ((md   (match-data))
             (strs (mapn 2 (lambda (a b) (cons (cons a b)
                                          (if (and a b)
                                              (substring str a b)
                                            ""))) md)))
        (message (mapconcat (lambda (s) (format "'%s'" s)) strs "\n")))))

;;;###autoload
(defmacro with-point-at-mouse (&rest body)
  `(let* ((pos     (mouse-position))
          (frame   (car pos))
          (win     (window-at (cadr pos) (cddr pos) frame))
          (cwin    (coordinates-in-window-p (cdr pos) win))
          (buf     (window-buffer win)))
     (when (consp cwin)
       (save-excursion
         (with-current-buffer buf
           (goto-char (window-start win))
           (forward-line (if header-line-format
                             (1- (cdr cwin))
                           (cdr cwin)))
           (move-to-column (round (car cwin)))
           ,@body)))))

;;;###autoload
(defun marker-at-mouse ()
  "Return marker at `mouse-position'."
  (with-point-at-mouse (point-marker)))

;;;###autoload
(defun thing-at-mouse (&optional thing)
  (with-point-at-mouse (thing-at-point thing)))

;;;###autoload
(defun thing-at-mouse-with-bounds (&optional thing)
  (with-point-at-mouse (thing-at-point-with-bounds thing)))

;;(global-set-key (kbd "H-m") (lambda () (interactive) (message "%S" (thing-at-mouse 'symbol))))

;;;###autoload
(defun exec-path-search (regexp)
  (flatten (remove-if-not 'identity
             (mapcar (lambda (dir) (directory-files dir t regexp))
                     exec-path))))

;;;###autoload
(defun load-path-search (&optional regexp full-path)
  (unless regexp (set 'regexp "elc?$"))
  (uniq (sort (flatten
               (remove-if-not 'identity
                              (mapcar (lambda (dir) (directory-files dir full-path regexp)) load-path)))
              'string-lessp)))

;;;###autoload
(defun libraries-matching (regexp)
  (uniq (mapcar (lambda (f) (symbolify (file-name-sans-extension (file-name-nondirectory f))))
                (load-path-search (concat regexp ".*elc?$")))))

(provide 'jsvnm-util)
