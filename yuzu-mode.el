;;
;; usage:
;;
;; (autoload 'yuzu-mode "yuzu-mode")
;; (add-to-list 'auto-mode-alist '("\\.yz$" . yuzu-mode))
;;

(define-derived-mode yuzu-mode fundamental-mode "Yuzu"
  (make-local-variable 'font-lock-defaults)
  (let ((yuzu-mode-font-lock-keywords
         '(("\\<\\(open\\|type\\|def\\|var\\)\\>" . (1 font-lock-function-name-face))
           ("\\<\\(if\\|else\\|match\\|case\\|when\\|mutable\\)\\>" . (1 font-lock-keyword-face))
           ("type\\s +\\(\\sw+\\)" . (1 font-lock-type-face))
           ("open\\s +\\(\\sw+\\)" . (1 font-lock-string-face))
           ("def\\s +\\(\\sw+\\)" . (1 font-lock-variable-name-face))
           ("var\\s +\\(\\sw+\\)" . (1 font-lock-variable-name-face))
           ("case\\s +\\(\\sw+\\)" . (1 font-lock-constant-face))
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
