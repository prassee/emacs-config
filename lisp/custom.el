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

;; (global-flycheck-mode 1)

(setq file-name-handler-alist nil)

(setq ivy-format-function 'ivy-format-function-line)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq markdown-hr-display-char  nil)

(defun disable-linum-setup-hook ()
  (setq display-line-numbers-mode -1))

;; (defun my-minibuffer-setup-hook ()
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun my-minibuffer-exit-hook ()
;;   (setq gc-cons-threshold 100000000))


;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
(defface custom-line-highlight '((t (:background "blue" :foreground "white"  :extend t))) "")
(add-hook
 'treemacs-mode-hook
 (defun channge-hl-line-mode ()
   (setq-local hl-line-face 'custom-line-highlight)
   (overlay-put hl-line-overlay 'face hl-line-face)
   (treemacs--setup-icon-background-colors)))

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
  (setq lsp-modeline-code-actions-segments '(count icon name))
  (setq lsp-headerline-breadcrumb-mode nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  )

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;; ;; (global-flycheck-mode 1)

;; (set-face-attribute 'hl-line nil :background "#454545")

(set-face-attribute 'default nil
                    :family "PragmataPro Liga"
                    :height 182
                    :weight 'normal
                    :width 'normal)

(let ((alist `((?! . ,(regexp-opt '("!!" "!=" "!==")))
               (?# . ,(regexp-opt '("##" "###" "####" "#(" "#?" "#[" "#_" "#_(" "#{")))
               (?$ . ,(regexp-opt '("$>")))
               (?% . ,(regexp-opt '("%%")))
               (?& . ,(regexp-opt '("&&")))
               ;; (?* . ,(regexp-opt '("*" "**" "***" "**/" "*/" "*>")))
					(?* . "\\(?:\\*\\(?:\\*\\*\\|[/>]\\)\\)")
               (?+ . ,(regexp-opt '("+" "++" "+++" "+>")))
               (?- . ,(regexp-opt '("--" "---" "-->" "-<" "-<<" "->" "->>" "-}" "-~")))
               (?. . ,(regexp-opt '(".-" ".." "..." "..<" ".=")))
               (?/ . ,(regexp-opt '("/*" "/**" "//" "///" "/=" "/==" "/>")))
               ;;(?: . ,(regexp-opt '(":" "::" ":::" ":=")))
					(?: . "\\(?::\\(?:::\\|\\?>\\|[:<-?]\\)\\)")
               (?\; . ,(regexp-opt '(";;")))
               (?< . ,(regexp-opt '("<!--" "<$" "<$>" "<*" "<*>" "<+" "<+>" "<-" "<--" "<->" "</" "</>" "<<" "<<-" "<<<" "<<=" "<=" "<=" "<=<" "<==" "<=>" "<>" "<|" "<|>" "<~" "<~~")))
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
               (?~ . ,(regexp-opt '("~-" "~=" "~>" "~@" "~~" "~~>")))))
)
(dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

;;(zerodark-setup-modeline-format)

(mood-line-mode)

(setq modus-vivendi-theme-intense-hl-line t
        modus-vivendi-theme-no-mixed-fonts t
        modus-vivendi-theme-completions 'opinionated
        modus-vivendi-theme-fringes 'intense
        modus-vivendi-theme-intense-paren-match t
        )

;; (load-theme 'modus-vivendi t)

;; (load-theme 'dracula t)

(set-face-attribute 'default nil :font "Pragmatapro Mono" :height 194)
