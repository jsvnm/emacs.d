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
      default-frame-alist '((background-mode . dark)
                            (menu-bar-lines . 1)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . right)
                            (cursor-type . (bar . 3))
                            ))

(fset 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region    'disabled   nil)
(put 'downcase-region  'disabled   nil)
(put 'narrow-to-region 'disabled   nil)

(blink-cursor-mode    1)
(winner-mode          1)
(column-number-mode   1)
(electric-pair-mode 1)
(show-paren-mode)

(require 'uniquify)
(setq uniquify-separator           " • "
      uniquify-ignore-buffers-re   "^\\*"
      uniquify-buffer-name-style   'reverse
      uniquify-after-kill-buffer-p t)

(recentf-mode)
(setq recentf-max-saved-items 100
      history-length          300)

(define-globalized-minor-mode global-goto-address-mode goto-address-mode goto-address-mode)
(global-goto-address-mode)

(provide 'rc-base)
