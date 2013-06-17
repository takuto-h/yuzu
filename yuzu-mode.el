;;
;; usage:
;;
;; (autoload 'yuzu-mode "yuzu-mode")
;; (add-to-list 'auto-mode-alist '("\\.yz$" . yuzu-mode))
;;

(define-derived-mode yuzu-mode fundamental-mode "Yuzu"
  (make-local-variable 'font-lock-defaults)
  (let ((yuzu-mode-font-lock-keywords
         '(("\\<\\(and\\|begin\\|class\\|constraint\\|def\\|end\\|external\\|functor\\|in\\|include\\|inherit\\|initializer\\|let\\|method\\|module\\|object\\|open\\|rec\\|sig\\|struct\\|type\\|val\\|var\\|virtual\\)\\>" . (1 font-lock-function-name-face))
           ("\\<\\(as\\|assert\\|case\\|do\\|done\\|downto\\|else\\|exception\\|for\\|fun\\|function\\|if\\|lazy\\|match\\|mutable\\|new\\|private\\|then\\|to\\|try\\|when\\|while\\|with\\)\\>" . (1 font-lock-keyword-face))
           ("\\<\\(asr\\|land\\|lor\\|lsl\\|lsr\\|lxor\\|mod\\|of\\|or\\)\\>" . (1 font-lock-comment-face))
           ("\\<\\(false\\|true\\)\\>" . (1 font-lock-constant-face))
           ("def\\s +\\(\\sw+\\)" . (1 font-lock-variable-name-face))
           ("exception\\s +\\(\\sw+\\)" . (1 font-lock-variable-name-face))
           ("open\\s +\\(\\sw+\\)" . (1 font-lock-string-face))
           ("type\\s +\\(\\sw+\\)" . (1 font-lock-type-face))
           ("var\\s +\\(\\sw+\\)" . (1 font-lock-variable-name-face))
           ("\\([][!%&()---=~^|`@{+;*:}<,>.?/]\\)" . (1 font-lock-comment-face))
           ("\\<\\([ABCDEFGHIJKLMNOPQRSTUVWXYZ]\\sw*\\)\\>\\." . (1 font-lock-type-face t))
           ("\\(\"\\(\\\\[\\\"]\\|[^\"\n]\\)*\"\\)" . (1 font-lock-string-face t))
           ("\\('\\(\\\\[\\']\\|[^'\n]\\)*'\\)" . (1 font-lock-string-face t))
           ("\\(//[^\n]*\\)$" . (1 font-lock-comment-face t))
           )))
    (setq font-lock-defaults
          (list
           yuzu-mode-font-lock-keywords
           t
           nil
           '((?_ . "w"))
           nil
           )))
  (add-hook 'yuzu-mode-hook 'turn-on-font-lock))
