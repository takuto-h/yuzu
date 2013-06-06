;;
;; usage:
;;
;; (autoload 'yuzu-mode "yuzu-mode")
;; (add-to-list 'auto-mode-alist '("\\.yz$" . yuzu-mode))
;;

(define-derived-mode yuzu-mode fundamental-mode "Yuzu"
  (make-local-variable 'font-lock-defaults)
  (let ((yuzu-mode-font-lock-keywords
         '(("\\<\\(open\\|type\\|def\\|var\\|if\\|else\\|match\\|case\\)\\>" .
            (1 font-lock-keyword-face))
           ("def\\s +\\(\\(\\sw\\|\\s_\\)+\\)" . (1 font-lock-function-name-face))
           ("var\\s +\\(\\(\\sw\\|\\s_\\)+\\)" . (1 font-lock-variable-name-face))
           ("type\\s +\\(\\(\\sw\\|\\s_\\)+\\)" . (1 font-lock-type-face))
           )))
    (setq font-lock-defaults (list yuzu-mode-font-lock-keywords nil nil nil nil)))
  (add-hook 'yuzu-mode-hook 'turn-on-font-lock))
