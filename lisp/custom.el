;;; mac specific
(cua-mode t)
(show-paren-mode 1)
(column-number-mode 1)
(winner-mode 1)
(electric-indent-mode 1)
(global-auto-revert-mode 1)
(global-git-gutter-mode 1)
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
(treemacs-hide-gitignored-files-mode 1)
(lsp-treemacs-sync-mode 1)
(all-the-icons-ivy-rich-mode 1)
(customize-set-variable 'tramp-use-ssh-controlmaster-options nil)

(setq comint-process-echoes t)
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-interpolation-factor 30)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq eshell-toggle-size-fraction 3)
(setq eshell-toggle-use-projectile-root nil)
(setq eshell-toggle-run-command nil)
(setq eshell-toggle-init-function 'eshell-toggle-init-eshell)
(setq file-name-handler-alist nil)
(setq ivy-format-function 'ivy-format-function-line)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq markdown-hr-display-char  nil)
(setq sqlformat-command 'sqlfluff)
(setq-default line-spacing 0)

(defun disable-linum-setup-hook () (setq display-line-numbers-mode -1))

(defface custom-line-highlight
  '((t (:background "#2a2a66" :foreground "#bfebe0" :extend t)))
  "")

(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'lsp-java-lens-mode)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'org-superstar-mode)

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
              (set-face-attribute face nil :family "ubuntu" :height 150))))


(with-eval-after-load 'lsp-mode
  ;; :project/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-modeline-code-actions-segments '(count icon name)))


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

;;; Custom Faces 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(ivy-current-match ((t (:background "#181868" :foreground "white"))))
;; '(ivy-highlight-face ((t (:background "#181868" :foreground "green"))))
 '(markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
 '(markdown-header-face-1
   ((t
     (:foreground "violet" :weight bold :height 1.0 :family "PragmataPro Liga"))))
 '(markdown-header-face-2
   ((t
     (:foreground "lightslateblue" :weight bold :height 1.0 :family "PragmataPro Liga"))))
 '(markdown-header-face-3
   ((t
     (:foreground "mediumpurple1" :weight bold :height 1.0 :family "PragmataPro Liga"))))
 '(markdown-link-face
   ((t
     (:background "#0e1014" :foreground "#bd93f9" :family "PragmataPro Liga" :height 1.0))))
 '(markdown-list-face
   ((t (:foreground "mediumpurple" :family "PragmataPro Liga" :height 1.0))))

 '(markdown-pre-face
   ((t (:foreground "#bd98fe" :family "PragmataPro Liga" :height 1.0 )))))


(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning) max (region-end))
      (setq min (point-at-bol) max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      (setq comint-scroll-to-bottom-on-output t)) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step (goto-char max) (next-line))))

(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))

(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

(when (and (executable-find "fish") (require 'fish-completion nil t))
  (global-fish-completion-mode))


;; (load-theme 'leuven-dark t)

(load-theme 'modus-vivendi t)
;; (set-face-attribute 'mood-line-buffer-name nil :foreground "skyblue")

;; (set-face-attribute 'mode-line nil
;;                     :background "#353644"
;;                     :foreground "skyblue"
;;                     :box '(:line-width 5 :color "deepskyblue")
;;                     :family "ubuntu"
;;                     :height 144
;;                     :overline nil
;;                     :underline nil)

;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#565063"
;;                     :foreground "grey"
;;                     :box '(:line-width 5 :color "LightSlateGray" )
;;                     :family "ubuntu"
;;                     :height 144
;;                     :overline nil
;;                     :underline nil)


;;; Disable bold face across after loading everything
(mapc
 (lambda (face) (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

(set-face-bold-p 'bold nil)

(provide 'custom)
