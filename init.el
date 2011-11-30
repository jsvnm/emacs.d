(defvar emacs.d (file-name-directory load-file-name) "the emacs.d dir")

(defsubst emacs.d (&rest pathcomponents)
  "Adds PATHCOMPONENTS to `emacs.d', strips leading /."
  (concat emacs.d
   (mapconcat (lambda (p) (replace-regexp-in-string "^/" "" p)) pathcomponents "/")))

;; Cedet must be loaded early. Not automagically installed. Must do:
;; (cd ~/.emacs.d ; bzr checkout bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk cedet)
(load-file (emacs.d "cedet/common/cedet.el"))
(require 'semantic-ia)
(require 'eassist)
(semantic-load-enable-gaudy-code-helpers)
(global-semantic-stickyfunc-mode -1)
(global-semantic-highlight-func-mode 1)
(global-semantic-idle-local-symbol-highlight-mode 1)
(semantic-toggle-decoration-style "semantic-decoration-on-private-members" t)
(semantic-toggle-decoration-style "semantic-decoration-on-protected-members" t)


;; Set some variables
(setq-default
      indent-tabs-mode             nil
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
      default-indicate-empty-lines t
      size-indication-mode         nil
      transient-mark-mode          t
      visible-bell                 t
      blink-cursor-delay           2
      blink-cursor-interval        0.5
      auto-compression-mode        t
      buffers-menu-max-size        30
      case-fold-search             t
      compilation-scroll-output    t
      european-calendar-style      t
      regex-tool-backend          'perl
      autofit-frames-flag          nil
      frame-title-format  '("%b")
      default-frame-alist '((foreground-color . "white")
                            (background-color . "black")
                            (background-mode . dark)
                            (border-color . "black")
                            (cursor-color . "#efe")
                            (menu-bar-lines . 1)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . right)
                            (cursor-type . (bar . 3))
                            ))

(set-face-attribute 'default       nil
                    :background "black" :foreground "white"
                    :height         100 :slant  'normal :weight    'normal :width 'normal
                    :inherit        nil :stipple    nil :inverse-video nil :box nil
                    :strike-through nil :overline   nil :underline     nil)

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region    'disabled   nil)
(put 'downcase-region  'disabled   nil)
(put 'narrow-to-region 'disabled   nil)

(blink-cursor-mode    1)
(winner-mode          1)
(column-number-mode   1)

(windmove-default-keybindings 'shift)

(defun kill-this-buffer ()
  "Kill the current buffer without asking anything."
  (interactive)
  (kill-buffer nil))

(define-key global-map                  (kbd "RET")        'newline-and-indent)
(define-key global-map                  (kbd "C-x u")      'revert-buffer)
(define-key global-map                  (kbd "H-/")        'hippie-expand)
(define-key global-map                  (kbd "H-k")        'kill-this-buffer)
(define-key read-expression-map         (kbd "TAB")        'lisp-complete-symbol)

(ido-mode       t)
(ido-everywhere t)
(setq ido-enable-flex-matching               t
      ido-use-filename-at-point              t
      ido-auto-merge-work-directories-length 0
      ido-cannot-complete-command 'ido-next-match
      ido-enable-tramp-completion          nil
      ido-enter-matching-directory         nil
      ido-ignore-directories       '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`.git/")
      ido-max-prospects                    123
      ido-max-window-height               0.25
      ido-max-work-file-list                30
      ido-use-filename-at-point         'guess
      ido-use-url-at-point                   t
      ido-decorations  
      '("\n" "" " " " |..." " " ""
        " [No match]" " [Matched]"
        " [Not readable]" " [Too big]" " [Confirm]"))

(require 'uniquify)
(setq uniquify-separator           " • "
      uniquify-ignore-buffers-re   "^\\*"
      uniquify-buffer-name-style   'reverse
      uniquify-after-kill-buffer-p t)

(recentf-mode)
(setq recentf-max-saved-items 100
      history-length          300)

(flymake-mode)
(show-paren-mode)

(add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)

(cond
 ((eq system-type 'windows-nt) 
    (set-face-attribute 'default nil :family "consolas") ;; "Courier New"
    (setq w32-pass-lwindow-to-system nil
          w32-lwindow-modifier       'hyper))
 ((eq system-type 'darwin)
    (setq mac-command-modifier       'hyper)))

(define-globalized-minor-mode global-goto-address-mode goto-address-mode goto-address-mode)
(global-goto-address-mode)

;; el-get:
(setq el-get-dir (emacs.d "el-get/"))
(add-to-list 'load-path (emacs.d "el-get/el-get/"))

;; This seems unreliable. Better to do:
;; (cd ~/.emacs.d/el-get ; git clone http://github.com/dimitri/el-get.git)
(unless (require 'el-get nil t)
  (url-retrieve "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s) (let (el-get-master-branch) (end-of-buffer) (eval-print-last-sexp)))))

(setq el-get-sources
      '((:name smex
               :after (lambda () (global-set-key (kbd "M-x") 'smex)))
        (:name frame-cmds :depends frame-fns)
        (:name goto-last-change
               :after (lambda () (global-set-key (kbd "C-?") 'goto-last-change)))
        (:name mic-paren :type emacswiki :features "mic-paren"
               :after (lambda () (paren-activate) (show-paren-mode -1)))
        (:name hide-comnt :type emacswiki)
        (:name thingatpt+ :type emacswiki)
        (:name thing-cmds :type emacswiki :depends (hide-comnt thingatpt+) :features (thing-cmds)
               :after (lambda () (global-set-key (kbd "C-M-SPC") 'cycle-thing-region)))
        (:name pretty-lambdada :type emacswiki :features pretty-lambdada
               :after (lambda () (pretty-lambda-for-modes)))
        (:name csharp-mode :features nil
               :post-init (lambda () (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)))
               :after (lambda () (require 'wisent-csharp)))
        (:name yasnippet-bundle :type elpa)
        ))

(setq my-el-get-pkgs
      `(el-get
        ruby-mode inf-ruby ruby-compilation rspec-mode flymake-ruby
        css-mode haml-mode sass-mode js2-mode
        auto-complete auto-complete-ruby
        egg smex full-ack gist
        frame-fns frame-cmds goto-last-change mic-paren
        ;;thing-cmds thingatpt+
        pretty-lambdada
        markdown-mode
        ,@(case system-type
            ('windows-nt '(csharp-mode yasnippet-bundle))
            ('darwin     '(yasnippet)))))

(el-get 'sync my-el-get-pkgs)


(setq locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(load custom-file)

(server-start)
