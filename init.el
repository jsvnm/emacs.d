(defvar emacs.d (file-name-directory load-file-name) "the emacs.d dir")
(defsubst emacs.d (&rest pathcomponents)
  "Adds PATHCOMPONENTS to `emacs.d', strips leading /."
  (concat emacs.d
	  (mapconcat (lambda (p) (replace-regexp-in-string "^/" "" p)) pathcomponents "/")))

(setq my-package-list
			'(el-get egg smex popup anything 
				auto-complete auto-complete-emacs-lisp
				flymake-ruby emacs-pry
				cmake-mode csharp-mode
				haskell-mode haskell-mode-exts 
				org-mode org-link-minor-mode))

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

(tool-bar-mode        0)
(scroll-bar-mode      0)
(blink-cursor-mode    1)
(winner-mode          0)
(column-number-mode   1)
(electric-pair-mode   0)
(show-paren-mode      1)

(set-face-foreground 'default "white")
(set-face-background 'default "black")

(load-library (emacs.d "lisp/misc-cmds.el"))

;; el-get autoloads from here
(setq el-get-user-package-directory (emacs.d "init.d"))

;; load&init cedet if found in ~/.emacs.d/cedet, otherwise let el-get try
(let ((lib (emacs.d "cedet/cedet-devel-load.el")))
	(or (and (file-exists-p     lib)
					 (load-library lib)
					 (load-library (concat el-get-user-package-directory "/init-cedet.el")))
			(add-to-list 'my-package-list 'cedet)))

;; require or install el-get
(add-to-list 'load-path (emacs.d "el-get/el-get"))
(eval-after-load 'el-get-recipes
	'(add-to-list 'el-get-recipe-path (emacs.d "recipes")))
(unless (require 'el-get nil t)
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s) (let (el-get-master-branch)
    (goto-char (point-max)) (eval-print-last-sexp)))))

;; install/init everything 
(el-get 'sync my-package-list)


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

(find-function-setup-keys)

(define-key global-map                  (kbd "RET")        'newline-and-indent)
(define-key global-map                  (kbd "C-x u")      'revert-buffer)
(define-key global-map                  (kbd "H-k")        'bury-buffer)
(define-key global-map                  (kbd "H-K")        'kill-this-buffer)
(define-key global-map                  (kbd "H-w")        'delete-window)
(define-key global-map                  (kbd "H-1")        'delete-other-windows)
(define-key global-map                  (kbd "H-M-f")      'ns-toggle-fullscreen)
(define-key global-map                  (kbd "H-/")        'hippie-expand)
(define-key global-map                  (kbd "C-c |")      'align-regexp)

(define-key read-expression-map         (kbd "TAB")        'lisp-complete-symbol)
(define-key emacs-lisp-mode-map         (kbd "H-h")        'jsvnm/popup-help-for-symbol)

(cond ((eq window-system 'w32)
			     (global-set-key              (kbd "C-z")        'undo))
			((eq window-system 'ns)
			     (define-key global-map       (kbd "H-z")        'undo)
					 (define-key global-map       (kbd "H-Z")        'redo)
					 (define-key global-map       (kbd "H-x")        'kill-region)
					 (define-key global-map       (kbd "H-c")        'ns-copy-including-secondary)
					 (define-key global-map       (kbd "H-v")        'yank)
					 (define-key global-map       (kbd "H-V")        'ns-paste-secondary))
			(window-system
			     (global-set-key              (kbd "C-z")        nil)))

(load custom-file)
(server-start)
