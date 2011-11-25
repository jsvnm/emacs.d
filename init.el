;; -*- mode: emacs-lisp; mode: linkd; coding: utf-8 -*-

(defvar emacs.d "~/emacs.d/" "Path to elisp root")

(defsubst emacs.d (&rest pathcomponents)
  "Adds PATHCOMPONENTS to `emacs.d', strips leading /."
  (concat emacs.d 
   (mapconcat (lambda (p) (replace-regexp-in-string "^/" "" p)) pathcomponents "/")))

(add-to-list 'exec-path"/usr/local/bin")
(add-to-list 'exec-path"/usr/local/sbin")
(add-to-list 'exec-path "/usr/local/bin/gems")

(autoload 'inversion-test "inversion" "Test that PACKAGE meets the MINIMUM version requirement.")
(autoload 'flymake-mode "flymake"  "Minor mode to do on-the-fly syntax checking.")

(setq-default
      indent-tabs-mode             nil
      dired-omit-mode              t)

(setq custom-file                  (emacs.d "custom.el")
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
      blink-cursor-delay           5
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
                            (cursor-color . "#fab")
                            (menu-bar-lines . 1)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . right)
                            (cursor-type . box)
                            ))

(set-face-attribute 'default       nil
                    :background "black" :foreground "white"
                    :height         100 :slant  'normal :weight    'normal :width 'normal
                    :inherit        nil :stipple    nil :inverse-video nil :box nil
                    :strike-through nil :overline   nil :underline     nil)

(fset 'yes-or-no-p 'y-or-n-p)           ;; Make yes-or-no answerable with 'y' or 'n'
(put 'upcase-region    'disabled   nil) ;; Don't disable case-change functions
(put 'downcase-region  'disabled   nil) ;;
(put 'narrow-to-region 'disabled   nil)

(blink-cursor-mode    1)
(winner-mode          1) ;; Navigate window layouts with "C-c <left>" and "C-c <right>"
(size-indication-mode 1) ;; Modeline tweaks
(column-number-mode   1)
(windmove-default-keybindings 'shift)

(global-set-key (kbd "RET") 'newline-and-indent)
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(require 'edebug)

;; (@* "recentf")
(require 'recentf)
(setq recentf-max-saved-items 100
      history-length          300)

;; (@* "uniquify")      Nicer naming of buffers for files with identical names
(require 'uniquify)
(setq uniquify-separator           " • "
      uniquify-ignore-buffers-re   "^\\*"
      uniquify-buffer-name-style   'reverse
      uniquify-after-kill-buffer-p t)

(require 'ido)
(load (emacs.d "vendor/ido-other-window/ido-other-window"))
(ido-mode       t)
(ido-everywhere t)
(setq ido-enable-flex-matching               t
      ido-use-filename-at-point              t
      ido-auto-merge-work-directories-length 0
      ido-cannot-complete-command 'ido-next-match
      ido-enable-tramp-completion          nil
      ido-enter-matching-directory         nil
      ido-ignore-directories       '("\\`CVS/"
                                  "\\`\\.\\./"
                                     "\\`\\./"
                                    "\\`.git/")
      ido-max-prospects                    123
      ido-max-window-height               0.25
      ido-max-work-file-list                30
      ido-use-filename-at-point         'guess
      ido-use-url-at-point                   t
      ido-decorations  
      '("\n" "" " " " |..." "|" ""
        " [No match]" " [Matched]"
        " [Not readable]" " [Too big]" " [Confirm]"))


(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(setq semanticdb-default-save-directory (emacs.d "cache/semanticdb/"))

(defvar flymake-mode nil)

(setq el-get-dir (emacs.d "el-get/"))
(add-to-list 'load-path (emacs.d "el-get/el-get/"))
(unless (require 'el-get nil t)
  (url-retrieve "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s) (let (el-get-master-branch) (end-of-buffer) (eval-print-last-sexp)))))

(el-get-emacswiki-refresh (emacs.d "el-get/recipes/emacswiki"))

(setq my:el-get-packages
 '(el-get auto-complete auto-complete-ruby auto-complete-yasnippet ac-dabbrev ac-slime ack
   anything autopair buffer-move cedet cmake-mode color-theme csharp-mode diminish dired+
   dired-details dired-details+ dired-single dired-view egg eol-conversion flymake-ruby
   frame-fns frame-cmds full-ack gist goto-last-change growl haml-mode highlight-parentheses
   htmlize inf-ruby js2-mode json levenshtein org-mode package paredit pos-tip psvn
   rainbow-delimiters regex-tool revive rspec-mode rinari ruby-mode ruby-block
   ruby-compilation ruby-end sass-mode scss-mode session smart-tab smex yari linkd ffap-
   cus-edit+ wid-edit+ display-buffer-for-wide-screen pp+ hide-comnt thingatpt+ thing-cmds
   help+ help-fns+ help-mode+ apropos+ apropos-fn+var misc-cmds color-moccur
   color-theme-ir-black mic-paren))



;;   yasnippet
;;   slime
;;   magit
;;   magithub
;;   ansi-color
;;   bookmark+
;;   haskell-mode haskell-mode-exts hs-mode


;;(el-get 'sync my:el-get-packages)
(el-get nil  my:el-get-packages)

(global-set-key (kbd "M-x") 'smex)

(require 'rainbow-delimiters)
(rainbow-delimiters-mode 1)
(paren-activate)

(require 'ffap-)
(require 'dired-view)
(require 'dired-details+)
;; (require 'wid-edit+)
;; (require 'cus-edit+)
(require 'linkd)
;; (require 'display-buffer-for-wide-screen)
;; (require 'pp+)
;; (require 'frame-cmds)
;; (require 'shell-command)
;; (require 'goto-last-change)
;; (require 'thing-cmds)
;; (require 'thingatpt+)
;; ;;(require 'mouse+)
;; ;;(require 'faces+)
;; (require 'imenu+)
;; (require 'ring+)
;; (require 'info+)
(require 'help+)
(require 'help-fns+)
(require 'help-mode+)
;; (require 'misc-cmds)
;; (require 'outline)
;; (require 'yaoddmuse)
;; (require 'doremi)
;; (require 'doremi-cmd)
;; (require 'doremi-frm)
;; (require 'color-moccur)
;; ;;(require 'less)
;; ;;(require 'dropdown-list)
;; ;;(require 'bookmark+)

;; (@* "Session")                Restore histories and registers after saving
(require 'session)
(setq session-save-file       (emacs.d "cache/session.el"))
;; (setq session-globals-include (uniq `(,@session-globals-include
;;                                       eproject-attributes-alist
;;                                        anything-magic-sources)))
(add-hook 'after-init-hook 'session-initialize)



;(unless (eq window-system 'w32) (require 'notify))
;; (require 'eproject)
;; (require 'eproject-extras)
;(require 'eol-conversion)
;(require 'all)


;; (@* "utf-8 everywhere")
(setq locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(server-start)
(require 'color-theme-ir-black)
(color-theme-ir-black)
(maximize-frame)
