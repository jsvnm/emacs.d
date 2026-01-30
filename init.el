;; init.el -- modernized setup, legacy preserved in init-legacy.el

(defvar emacs.d (file-name-directory load-file-name) "the emacs.d dir")
(defsubst emacs.d (&rest pathcomponents)
  "Adds PATHCOMPONENTS to `emacs.d', strips leading /."
  (concat emacs.d
          (mapconcat (lambda (p) (replace-regexp-in-string "^/" "" p)) pathcomponents "/")))

(setq custom-file (emacs.d "custom.el")
      custom-theme-directory (emacs.d "themes/")
      org-directory "~/org"
      inhibit-startup-screen t
      use-file-dialog nil
      use-dialog-box nil
      enable-recursive-minibuffers t
      eval-expression-print-length nil
      eval-expression-print-level nil
      messages-buffer-max-lines 5000
      indicate-empty-lines t
      transient-mark-mode t
      visible-bell t
      auto-compression-mode t
      buffers-menu-max-size 30
      case-fold-search t
      compilation-scroll-output t
      calendar-date-style 'iso
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      frame-title-format '("%b")
      default-frame-alist '((background-mode . dark)
                            (menu-bar-lines . 1)
                            (tool-bar-lines . 0)))

(setq-default
 tab-width 2
 indent-tabs-mode t
 dired-omit-mode t)

(setq backup-directory-alist `((".*" . ,(emacs.d "cache/backups/"))))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix (emacs.d "cache/auto-save-list/.saves-"))
(setq tramp-persistency-file-name (emacs.d "cache/tramp-history"))
(setq url-cookie-file (emacs.d "cache/url/cookies"))
(setq org-clock-persist-file (emacs.d "cache/org-clock-save.el"))
(setq srecode-map-save-file (emacs.d "cache/srecode-map.el"))
(setq semanticdb-default-save-directory (emacs.d "cache/semanticdb"))
(setq ede-project-placeholder-cache-file (emacs.d "cache/ede-projects.el"))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 1)
(winner-mode 0)
(column-number-mode 1)
(electric-pair-mode 0)
(show-paren-mode 1)

(set-face-foreground 'default "white")
(set-face-background 'default "black")
(set-face-attribute 'default nil :height 180)

(load-library (emacs.d "lisp/misc-fns.el"))
(load-library (emacs.d "lisp/misc-cmds.el"))

;; Elpaca bootstrap (official installer)
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Core quality-of-life
(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :custom (recentf-max-saved-items 200))

(use-package which-key
  :init (which-key-mode 1))

;; Import shell env for GUI Emacs (Spotlight launch)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom (exec-path-from-shell-check-startup-files nil)
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "OPENAI_API_KEY"))

;; Minibuffer completion stack
(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :custom (completion-styles '(orderless basic))
  :custom (completion-category-defaults nil)
  :custom (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult)

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

;; In-buffer completion
(use-package corfu
  :init (global-corfu-mode 1)
  :custom (corfu-auto t)
  :custom (corfu-cycle t))

(use-package cape)

;; Org (built-in)
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode))

(load-library (emacs.d "init.d/init-org-mode.el"))

;; System-specific paths
(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :family "consolas"))
      ((eq system-type 'darwin)
       (add-to-list 'exec-path "/usr/local/bin")
       (add-to-list 'exec-path "/usr/local/bin/gems")))

;; Keybindings (keep hyper keys close to old setup)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-x u") 'revert-buffer)
(define-key global-map (kbd "H-k") 'bury-buffer)
(define-key global-map (kbd "H-K") 'kill-this-buffer)
(define-key global-map (kbd "H-w") 'delete-window)
(define-key global-map (kbd "H-1") 'delete-other-windows)
(define-key global-map (kbd "H-M-f") 'toggle-frame-fullscreen)
(define-key global-map (kbd "H-/") 'hippie-expand)
(define-key global-map (kbd "C-c |") 'align-regexp)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(when (boundp 'emacs-lisp-mode-map)
  (when (fboundp 'jsvnm/popup-help-for-symbol)
    (define-key emacs-lisp-mode-map (kbd "H-h") 'jsvnm/popup-help-for-symbol)))

(cond ((eq window-system 'w32)
       (global-set-key (kbd "C-z") 'undo))
      ((eq window-system 'ns)
       (define-key global-map (kbd "H-z") 'undo)
       (define-key global-map (kbd "H-Z") 'redo)
       (define-key global-map (kbd "C-z") nil)
       (define-key global-map (kbd "H-x") 'kill-region)
       (define-key global-map (kbd "H-c") 'ns-copy-including-secondary)
       (define-key global-map (kbd "H-v") 'yank)
       (define-key global-map (kbd "H-V") 'ns-paste-secondary))
      (window-system
       (global-set-key (kbd "C-z") nil)))

;; Dired, uniq, links
(require 'dired-x)
(require 'uniquify)
(setq uniquify-separator " • "
      uniquify-ignore-buffers-re "^\\*"
      uniquify-buffer-name-style 'reverse
      uniquify-after-kill-buffer-p t)

(define-globalized-minor-mode global-goto-address-mode goto-address-mode goto-address-mode)
(global-goto-address-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq w32-pass-lwindow-to-system nil
      w32-lwindow-modifier 'hyper
      mac-command-modifier 'hyper)
(windmove-default-keybindings 'shift)

(find-function-setup-keys)

(when (file-exists-p custom-file)
  (load custom-file))

(server-start)

;; restore GC threshold after init
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold (* 16 1024 1024))))
