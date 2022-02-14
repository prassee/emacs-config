;; mac specific
(cua-mode t)
(show-paren-mode 1)
(column-number-mode 1)
(winner-mode 1)
(electric-indent-mode 1)
(global-auto-revert-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-unset-key (kbd "C-z"))
(global-linum-mode 0)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(tooltip-mode 1)
(ivy-rich-mode 1)
(mood-line-mode 1)


(setq file-name-handler-alist nil)

(setq ivy-format-function 'ivy-format-function-line)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq markdown-hr-display-char  nil)

(setq-default line-spacing 0)

(defun disable-linum-setup-hook () (setq display-line-numbers-mode -1))

(defface custom-line-highlight
  '((t (:background "#181868" :foreground "white" :extend t)))
  "")

(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'lsp-java-lens-mode)

(add-hook 'treemacs-mode-hook
          (defun change-hl-line-mode ()
            (setq-local hl-line-face 'custom-line-highlight)
            (overlay-put hl-line-overlay 'face hl-line-face)
            (treemacs--setup-icon-background-colors)))


(add-hook 'treemacs-mode-hook
          (lambda()
            (display-line-numbers-mode -1)
            (dolist (face
                     '(treemacs-root-face
                       treemacs-git-unmodified-face
                       treemacs-git-modified-face
                       treemacs-git-renamed-face
                       treemacs-git-ignored-face
                       treemacs-git-untracked-face
                       treemacs-git-added-face
                       treemacs-git-conflict-face
                       treemacs-directory-face
                       treemacs-directory-collapsed-face
                       treemacs-file-face
                       treemacs-tags-face))
              (set-face-attribute face nil :family "ubuntu" :height 120))))

;; (add-hook 'after-init-hook 'org-agenda-list)

;; (with-eval-after-load 'lsp-mode
;;   ;; (add-hook 'lsp-managed-mode-hook 'lsp-modeline-diagnostics-mode)
;;   ;; (add-hook 'lsp-managed-mode-hook 'lsp-modeline-code-actions-mode)
;;   ;; :project/:workspace/:file
;;   (setq lsp-modeline-diagnostics-scope :workspace)
;;   )

(with-eval-after-load 'lsp-mode
  ;; :project/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-modeline-code-actions-segments '(count icon name)))

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;; (global-flycheck-mode 1)

;; (set-face-attribute 'hl-line nil :background "#454545")

(let ((alist
       `((?! . ,(regexp-opt '("!!" "!=" "!==")))
         (?# .
             ,(regexp-opt
               '("##" "###" "####" "#(" "#?" "#[" "#_" "#_(" "#{")))
         (?$ . ,(regexp-opt '("$>")))
         (?% . ,(regexp-opt '("%%")))
         (?& . ,(regexp-opt '("&&")))
         ;; (?* . ,(regexp-opt '("*" "**" "***" "**/" "*/" "*>")))
		 (?* . "\\(?:\\*\\(?:\\*\\*\\|[/>]\\)\\)")
         (?+ . ,(regexp-opt '("+" "++" "+++" "+>")))
         (?- .
             ,(regexp-opt
               '("--" "---" "-->" "-<" "-<<" "->" "->>" "-}" "-~")))
         (?. . ,(regexp-opt '(".-" ".." "..." "..<" ".=")))
         (?/ . ,(regexp-opt '("/*" "/**" "//" "///" "/=" "/==" "/>")))
         ;;(?: . ,(regexp-opt '(":" "::" ":::" ":=")))
		 (?: . "\\(?::\\(?:::\\|\\?>\\|[:<-?]\\)\\)")
         (?\; . ,(regexp-opt '(";;")))
         (?< .
             ,(regexp-opt
               '("<!--" "<$" "<$>" "<*" "<*>" "<+" "<+>" "<-" "<--" "<->" "</"
                 "</>" "<<" "<<-" "<<<" "<<=" "<=" "<=" "<=<" "<==" "<=>" "<>"
                 "<|" "<|>" "<~" "<~~")))
         (?= . ,(regexp-opt '("=/=" "=:=" "=<<" "==" "===" "==>" "=>" "=>>")))
         (?> . ,(regexp-opt '(">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>")))
         (?? . ,(regexp-opt '("??" "?=")))
         (?\[ . ,(regexp-opt '("[]")))
         (?\\ . ,(regexp-opt '("\\\\" "\\\\\\")))
         (?^ . ,(regexp-opt '("^=")))
         (?w . ,(regexp-opt '("www")))
         (?x . ,(regexp-opt '("x")))
         (?{ . ,(regexp-opt '("{-")))
         (?| . ,(regexp-opt '("|=" "|>" "||" "||=")))
         (?~ . ,(regexp-opt '("~-" "~=" "~>" "~@" "~~" "~~>"))))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table
                          (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(setq warning-minimum-level :emergency)


;; 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:background "#181868" :foreground "white"))))
 '(ivy-highlight-face ((t (:background "#181868" :foreground "green"))))
 '(markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
 '(markdown-header-face-1
   ((t (:foreground "violet" :weight bold :height 1.0))))
 '(markdown-header-face-2
   ((t (:foreground "lightslateblue" :weight bold :height 1.0))))
 '(markdown-header-face-3
   ((t (:foreground "mediumpurple1" :weight bold :height 1.0))))
 '(markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
 '(markdown-list-face ((t (:foreground "mediumpurple"))))
 '(markdown-pre-face ((t (:foreground "#bd98fe"))))
 )

;; (load-theme 'modus-vivendi t)
(load-theme 'vscode-dark-plus t)

(set-face-attribute 'variable-pitch nil :family "Ubuntu" :height 180)

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)


(provide 'custom)
