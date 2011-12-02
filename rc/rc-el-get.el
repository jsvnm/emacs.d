;; el-get:
(setq el-get-dir (emacs.d "el-get/"))
(add-to-list 'load-path (emacs.d "el-get/el-get/"))

;; This seems unreliable. Better to do:
;; (cd ~/.emacs.d/el-get ; git clone http://github.com/dimitri/el-get.git)
(unless (require 'el-get nil t)
  (url-retrieve "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s) (let (el-get-master-branch) (end-of-buffer) (eval-print-last-sexp)))))

(setq el-get-sources
      `((:name smex
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
        (:name color-theme-solarized :depends nil)
        (:name help-fns+ :type emacswiki :features help-fns+)
        (:name el-get    :branch "master" :url "https://github.com/jsvnm/el-get.git")
        (:name inf-ruby  :branch "master" :url "https://github.com/nonsequitur/inf-ruby.git" :type git)
        (:name ruby-mode :branch "master" :url "https://github.com/jacott/Enhanced-Ruby-Mode.git" :type git)
        ,@(case system-type
            ('windows-nt 
             '((:name Powershell :type emacswiki :features powershell)
               csharp-mode yasnippet-bundle)
             )
            ('darwin     
             '(yasnippet)
             ))
        el-get           full-ack            gist              
        rspec-mode       flymake-ruby        rinari            
        css-mode         haml-mode           sass-mode         
        auto-complete    auto-complete-ruby  smex              
        frame-fns        frame-cmds          goto-last-change  
        pretty-lambdada  mic-paren           egg               
        js2-mode         markdown-mode       
        ;;thing-cmds thingatpt+
        ))

(el-get 'sync)

(provide 'rc-el-get)
