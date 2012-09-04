;; See doc-string of `semantic-default-submodes' for other things you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

(semantic-mode   1)
(global-ede-mode 1)

(add-to-list 'load-path (concat
    (file-name-directory (find-library-name "cedet-devel-load"))
		"contrib") t)

(autoload 'wisent-ruby-default-setup    "wisent-ruby")
(autoload 'wisent-csharp-default-setup	"wisent-csharp")
	
(add-to-list 'semantic-new-buffer-setup-functions
						 '(ruby-mode . wisent-ruby-default-setup))
(add-to-list 'semantic-new-buffer-setup-functions
						 '(csharp-mode . wisent-csharp-default-setup))


