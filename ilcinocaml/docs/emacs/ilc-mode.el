;;; Code:
(defvar ilc-mode-hook nil)
(defvar ilc-mode-map
  (let ((ilc-mode-map (make-keymap)))
    (define-key ilc-mode-map "\C-j" 'newline-and-indent)
    ilc-mode-map)
  "Keymap for ILC major mode")

(add-to-list 'auto-mode-alist '("\\.ilc\\'" . ilc-mode))

(defconst ilc-font-lock-keywords-1
  (list
   ; (regexp-opt '("let" "in" "letrec" "lam" "ref") t)
   '("\\<\\(in\\|l\\(?:am\\|et\\(?:rec\\)?\\)\\|ref\\)\\>"
  . font-lock-variable-name-face))
  "Minimal highlighting expressions for ILC mode.")

(defconst ilc-font-lock-keywords-2
  (append ilc-font-lock-keywords-1
          (list
           ; (regexp-opt '("if" "then" "else" "match" "with") t)
           '("\\<\\(else\\|if\\|match\\|then\\|with\\)\\>" . font-lock-keyword-face)))
  
  "Balls-out highlighting in ILC mode.")

(defconst ilc-font-lock-keywords-3
  (append ilc-font-lock-keywords-2
          (list
           ; (regexp-opt '("nu" "rd" "wr" "->" "|>") t)
           '("\\<\\(->\\|nu\\|rd\\|wr\\||>\\)\\>" . font-lock-constant-face)
           '("\\<\\(true\\|false\\)\\>" . font-lock-builtin-face)))
  "Balls-out highlighting in ILC mode.")


(defvar ilc-font-lock-keywords ilc-font-lock-keywords-3
  "Default highlighting expressions for ILC mode.")

;;(defun ilc-indent-line ()
;;  "Indent current line as ILC code."
;;  (interactive)
;;  (beginning-of-line)
;;  (if (bobp)
;;      (indent-line-to 0)           ; First line is always non-indented
;;    (let ((not-indented t) cur-indent)
;;      (if (looking-at "^[ \t]*END_") ; If the line we are looking at is the end of a block, then decrease the indentation
;;          (progn
;;            (save-excursion
;;              (forward-line -1)
;;              (setq cur-indent (- (current-indentation) default-tab-width)))
;;            (if (< cur-indent 0) ; We can't indent past the left margin
;;                (setq cur-indent 0)))
;;        (save-excursion
;;          (while not-indented ; Iterate backwards until we find an indentation hint
;;            (forward-line -1)
;;            (if (looking-at "^[ \t]*END_") ; This hint indicates that we need to indent at the level of the END_ token
;;                (progn
;;                  (setq cur-indent (current-indentation))
;;                  (setq not-indented nil))
;;              (if (looking-at "^[ \t]*\\(PARTICIPANT\\|MODEL\\|APPLICATION\\|WORKFLOW\\|ACTIVITY\\|DATA\\|TOOL_LIST\\|TRANSITION\\)") ; This hint indicates that we need to indent an extra level
;;                  (progn
;;                    (setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
;;                    (setq not-indented nil))
;;                (if (bobp)
;;                    (setq not-indented nil)))))))
;;      (if cur-indent
;;          (indent-line-to cur-indent)
;;        (indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

;;(defvar ilc-mode-syntax-table
;;  (let ((ilc-mode-syntax-table (make-syntax-table)))
;;    
;;    ; This is added so entity names with underscores can be more easily parsed
;;    (modify-syntax-entry ?_ "w" ilc-mode-syntax-table)
;;    
;;    ; Comment styles are same as C++
;;    (modify-syntax-entry ?/ ". 124b" ilc-mode-syntax-table)
;;    (modify-syntax-entry ?* ". 23" ilc-mode-syntax-table)
;;    (modify-syntax-entry ?\n "> b" ilc-mode-syntax-table)
;;    ilc-mode-syntax-table)
;;  "Syntax table for ilc-mode")

(defvar ilc-mode-syntax-table nil "Syntax table for `ilc-mode'.")

(setq ilc-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; Haskell style comment “{- … -}”
        (modify-syntax-entry ?\{ ". 1" synTable)
        (modify-syntax-entry ?\} ". 4" synTable)
        (modify-syntax-entry ?- ". 23" synTable)
        synTable))

(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lam" . 955 ) ; λ
          ("nu" . 957 ) ;
          ("->" . 8594 ) ;
          ("|>" . 10704 ) ;
          ("!=" . 8800 ) ;
          (":=" . 8788 ) ;
          )))

(add-hook 'ilc-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

;;(define-derived-mode ilc-mode prog-mode "ilc"
;;  "ilc-mode is a major mode for editing language ilc."
;;  (setq font-lock-defaults (list nil)))
  
(defun ilc-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map ilc-mode-map)
  (set-syntax-table ilc-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(ilc-font-lock-keywords))
  ;; Register our indentation function
  ;;(set (make-local-variable 'indent-line-function) 'ilc-indent-line)  
  (setq major-mode 'ilc-mode)
  (setq mode-name "ILC")
  (run-hooks 'ilc-mode-hook))

(provide 'ilc-mode)

;;; ilc-mode.el ends here
