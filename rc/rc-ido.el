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

(provide 'rc-ido)
