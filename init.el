(defvar emacs.d (file-name-directory load-file-name) "the emacs.d dir")

(defsubst emacs.d (&rest pathcomponents)
  "Adds PATHCOMPONENTS to `emacs.d', strips leading /."
  (concat emacs.d
   (mapconcat (lambda (p) (replace-regexp-in-string "^/" "" p)) pathcomponents "/")))

(cond ((eq system-type 'windows-nt) 
       (set-face-attribute 'default nil :family "consolas") ;; "Courier New"
       (setq w32-pass-lwindow-to-system nil
             w32-lwindow-modifier       'hyper))
      ((eq system-type 'darwin)
       (add-to-list 'exec-path "/usr/local/bin")
       (add-to-list 'exec-path "/usr/local/bin/gems")
       (setq mac-command-modifier       'hyper)))

;; Cedet must be loaded early. Not automagically installed. Must do:
;; (cd ~/.emacs.d ; bzr checkout bzr://cedet.bzr.sourceforge.net/bzrroot/cedet/code/trunk cedet)
(load-file (emacs.d "cedet/common/cedet.el"))

(add-to-list 'load-path emacs.d)
(add-to-list 'load-path (emacs.d "rc"))    ;; for parts of init
(add-to-list 'load-path (emacs.d "elisp")) ;; for other elisp

(require 'rc-base)
(require 'rc-el-get)
(require 'rc-ido)
(require 'rc-cedet)


(defun kill-this-buffer ()
  "Kill the current buffer without asking anything."
  (interactive)
  (kill-buffer nil))

(defun jsvnm/popup-help-for-symbol (&optional symbol)
  (interactive)
  (unless symbol (setq symbol (symbol-at-point)))
  (popup-tip (ac-symbol-documentation symbol)))

(windmove-default-keybindings 'shift)
(define-key global-map                  (kbd "RET")        'newline-and-indent)
(define-key global-map                  (kbd "C-x u")      'revert-buffer)
(define-key global-map                  (kbd "H-/")        'hippie-expand)
(define-key global-map                  (kbd "H-k")        'kill-this-buffer)
(define-key global-map                  (kbd "C-c |")      'align-regexp)
(define-key read-expression-map         (kbd "TAB")        'lisp-complete-symbol)
(define-key emacs-lisp-mode-map         (kbd "H-h")        'jsvnm/popup-help-for-symbol)



(add-hook 'emacs-lisp-mode-hook       'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook             'turn-on-eldoc-mode)
(add-hook 'after-save-hook            'executable-make-buffer-file-executable-if-script-p)





(setq locale-coding-system   'utf-8)
(set-language-environment    'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)

(load custom-file)
(server-start)
