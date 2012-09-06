
(defun describe-keymap (kmap)
	(interactive (list (completing-read "Keymap: " obarray
		 (lambda (sym) (or (keymapp sym)
											  (and (boundp sym)
														 (keymapp (eval sym)))
											 )))))
	(flet ((pp (o &optional stream)
						 (insert (substitute-command-keys (format "\\{%s}" kmap)))))
		(describe-variable (intern kmap))))



(defun windows-in-use-time-order (&optional old-to-new windows)
	"Windows sorted by their `window-use-time'.
Without arguments windows on current frame ordered from most to least recent.

Optional argument OLD-TO-NEW with non-nil value reverses the order.

Optional argument WINDOWS specifies which windows to sort by use time:

- a list is assumed to contain windows and sorted

- a function is called, should return list of windows to sort

- any other value is passed to `window-list-1' as ALL-FRAMES parameter"
	(sort* (cond ((and windows (listp windows)) windows)
							 ((functionp windows) (funcall windows))
							 ((window-list-1 nil 'nomini windows)))
				 (if old-to-new '< '>)
				 :key 'window-use-time))

(defvar swap-window-buffers-winring)
(defvar swap-window-buffers-tgt)
(defun swap-window-buffers (win1 &optional win2)
	"Swap buffers in WIN1 and WIN2.
when called interactively:
- On first call, swap buffers in current and previous window

- On next calls, revert previous swap, then swap buffers in
  current window and the next older window, or when no older
  windows exist, itself, resulting in original buffers in
  each windows. next call starts the cycle again."  
	(interactive
	 ;; setup
	 (if (not (equal last-command this-command))
			 (let* ((ws (windows-in-use-time-order))
							(w  (car ws)) 		(ws (cdr ws)))
				 (setq swap-window-buffers-tgt w
							 swap-window-buffers-winring (append ws (list w)))
				 (list (car ws) w))
	 ;; repeat
		 (let* ((ws swap-window-buffers-winring)
						(w (car ws))
						(ws (cdr ws))
						(tgt swap-window-buffers-tgt))
			 (setq swap-window-buffers-winring (append ws (list w)))
			 (swap-window-buffers w tgt)
			 (list (car ws) tgt))))
	(unless win2 (setq win2 (selected-window)))
	(let ((buf1 (window-buffer win1))
				(buf2 (window-buffer win2)))
		(set-window-buffer win1 buf2)
		(set-window-buffer win2 buf1)))

(defvar swb-r)
(defvar swb-s)
(defvar swb-t)
(defun swap-window-buffers (win1 &optional win2)
	"Swap buffers in WIN1 and WIN2."
	(interactive (progn
		 (if (not (equal last-command this-command))
				 (setq swb-t 0 swb-s 1 swb-r (ring-convert-sequence-to-ring (windows-in-use-time-order)))
			 (if current-prefix-arg ;; (let ((tmp swb-s)) (setq swb-s swb-t swb-t tmp))
					 (ring-remove swb-r 0))
			 (swap-window-buffers (ring-ref swb-r swb-t) (ring-ref swb-r swb-s))
			 (incf swb-s))
		 (list (ring-ref swb-r swb-s) (ring-ref swb-r swb-t))
	))
	(unless win2 (setq win2 (selected-window)))
	(message "%s <-> %s" win1 win2)
	(let ((buf1 (window-buffer win1))
				(buf2 (window-buffer win2)))
		(set-window-buffer win1 buf2)
		(set-window-buffer win2 buf1)))

(setq rr (ring-convert-sequence-to-ring (windows-in-use-time-order))
			w1 (ring-ref rr 0) 
			w2 (ring-ref rr 1))

(defun stepr () (setq w2 (ring-next rr w2)))
(stepr)
(setq it (ring-ref swb-r 0))
(ring-next swb-r it)


(setq lll
(lexical-let*
		((r (ring-convert-sequence-to-ring (windows-in-use-time-order)))
		 (a (ring-ref rr 0))
		 (b (ring-ref rr 1)))
	(lambda ()
		(setq b (ring-next r b))
		(list b a))))
(funcall lll)
		

(defun fileset-from-git-ls-files (name &optional path)
	(let* ((default-directory (or (and path
																		(file-name-directory path))
															 default-directory))
				 (files (split-string (shell-command-to-string "git ls-files"))))
		files))
