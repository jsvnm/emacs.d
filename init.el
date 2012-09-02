(defvar emacs.d (file-name-directory load-file-name) "the emacs.d dir")
(defsubst emacs.d (&rest pathcomponents)
  "Adds PATHCOMPONENTS to `emacs.d', strips leading /."
  (concat emacs.d
	  (mapconcat (lambda (p) (replace-regexp-in-string "^/" "" p)) pathcomponents "/")))

(setq-default
      tab-width                    2
      indent-tabs-mode             t
      dired-omit-mode              t)

(setq custom-file                  (emacs.d "custom.el")
      custom-theme-directory       (emacs.d "themes/")
      org-directory                "~/org"
      inhibit-startup-screen       t
      use-file-dialog              nil
      use-dialog-box               nil
      enable-recursive-minibuffers t
      eval-expression-print-length nil
      eval-expression-print-level  nil
      messages-buffer-max-lines    5000
      indicate-empty-lines         t
      transient-mark-mode          t
      visible-bell                 t
      auto-compression-mode        t
      buffers-menu-max-size        30
      case-fold-search             t
      compilation-scroll-output    t
      calendar-date-style         'iso
      regex-tool-backend          'perl
      autofit-frames-flag          nil
			mouse-wheel-scroll-amount   '(1)
			mouse-wheel-progressive-speed nil
      frame-title-format  '("%b")
			default-frame-alist '((background-mode . dark)
														(menu-bar-lines . 1)
														(tool-bar-lines . 0))
			)

(setq backup-directory-alist       	  `((".*" . ,(emacs.d "cache/backups/"))))
(setq auto-save-file-name-transforms  `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix  (emacs.d "cache/auto-save-list/.saves-"))
(setq tramp-persistency-file-name (emacs.d "cache/tramp-history"))
(setq ac-comphist-file       (emacs.d "cache/ac-comphist.dat"))
(setq org-clock-persist-file (emacs.d "cache/org-clock-save.el"))
(setq srecode-map-save-file  (emacs.d "cache/srecode-map.el"))
(setq semanticdb-default-save-directory  (emacs.d "cache/semanticdb"))
(setq ede-project-placeholder-cache-file (emacs.d "cache/ede-projects.el"))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region    'disabled   nil)
(put 'downcase-region  'disabled   nil)
(put 'narrow-to-region 'disabled   nil)

(load-file (emacs.d "cedet/cedet-devel-load.el")) ;; Must be early in init
(add-to-list 'load-path (emacs.d "cedet/contrib/"))

(tool-bar-mode        0)
(scroll-bar-mode      0)
(blink-cursor-mode    1)
(winner-mode          0)
(column-number-mode   1)
(electric-pair-mode   0)
(show-paren-mode      1)

(require 'dired-x)

(require 'uniquify)
(setq uniquify-separator           " • "
      uniquify-ignore-buffers-re   "^\\*"
      uniquify-buffer-name-style   'reverse
      uniquify-after-kill-buffer-p t)

(define-globalized-minor-mode global-goto-address-mode goto-address-mode goto-address-mode)
(global-goto-address-mode)

(cond ((eq system-type 'windows-nt) 
       (set-face-attribute 'default nil :family "consolas")) ;; "Courier New"
      ((eq system-type 'darwin)
       (add-to-list 'exec-path "/usr/local/bin")
       (add-to-list 'exec-path "/usr/local/bin/gems")))


;;(directory-files (concat emacs.d "eval-after") nil "^[^#]+\.el$")

;; See doc-string of `semantic-default-submodes' for other things you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)
(add-to-list 'semantic-new-buffer-setup-functions '(ruby-mode . wisent-ruby-default-setup))
(add-to-list 'semantic-new-buffer-setup-functions '(csharp-mode . wisent-csharp-default-setup))

(semantic-mode 1)
(global-ede-mode 1)

(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode '(both))

(add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)
(add-hook 'after-save-hook            'executable-make-buffer-file-executable-if-script-p)

(setq w32-pass-lwindow-to-system nil
      w32-lwindow-modifier       'hyper
      mac-command-modifier       'hyper)
(windmove-default-keybindings 'shift)

(define-key global-map                  (kbd "RET")        'newline-and-indent)
(define-key global-map                  (kbd "C-x u")      'revert-buffer)
(define-key global-map                  (kbd "H-z")        'undo)
(define-key global-map                  (kbd "H-Z")        'redo)
(define-key global-map                  (kbd "H-x")        'kill-region)
(define-key global-map                  (kbd "H-c")        'ns-copy-including-secondary)
(define-key global-map                  (kbd "H-v")        'yank)
(define-key global-map                  (kbd "H-V")        'ns-paste-secondary)
(define-key global-map                  (kbd "H-k")        'bury-buffer)
(define-key global-map                  (kbd "H-K")        'kill-this-buffer)
(define-key global-map                  (kbd "H-w")        'delete-window)
(define-key global-map                  (kbd "H-1")        'delete-other-windows)
(define-key global-map                  (kbd "H-M-f")      'ns-toggle-fullscreen)
(define-key global-map                  (kbd "H-/")        'hippie-expand)
(define-key global-map                  (kbd "C-c |")      'align-regexp)

(define-key read-expression-map         (kbd "TAB")        'lisp-complete-symbol)
(define-key emacs-lisp-mode-map         (kbd "H-h")        'jsvnm/popup-help-for-symbol)

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

(setq my-package-list
			'(el-get egg smex popup anything 
				auto-complete auto-complete-emacs-lisp
				;;auto-complete-ruby ; depends on rcodetools
				flymake-ruby emacs-pry
				cmake-mode csharp-mode
				haskell-mode haskell-mode-exts 
				org-mode org-link-minor-mode))

(setq el-get-user-package-directory (emacs.d "el-get-init"))
(add-to-list 'load-path (emacs.d "el-get/el-get"))
(eval-after-load 'el-get-recipes
	'(add-to-list 'el-get-recipe-path (emacs.d "el-get-recipes")))
(unless (require 'el-get nil t)
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s) (let (el-get-master-branch)
    (goto-char (point-max)) (eval-print-last-sexp)))))

(el-get 'sync my-package-list)

(load custom-file)
(server-start)
