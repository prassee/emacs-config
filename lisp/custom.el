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
(treemacs-hide-gitignored-files-mode 1)
(all-the-icons-ivy-rich-mode 1)
;; (auto-dim-other-buffers-mode 1)
(setq comint-process-echoes t)
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-interpolation-factor 30)
 ;; scroll one line at a time (less "jumpy" than defaults)
    
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    
(setq scroll-step 1) ;; keyboard scroll one line at a time


(setq eshell-toggle-size-fraction 3)
(setq eshell-toggle-use-projectile-root nil)
(setq eshell-toggle-run-command nil)
;; (setq eshell-toggle-init-function 'eshell-toggle-init-ansi-term)

(setq file-name-handler-alist nil)

(setq ivy-format-function 'ivy-format-function-line)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq markdown-hr-display-char  nil)

(setq-default line-spacing 0)

(defun disable-linum-setup-hook () (setq display-line-numbers-mode -1))

(defface custom-line-highlight
  '((t (:background "#2a2a66" :foreground "#bfebe0" :extend t)))
  "")

(add-hook 'java-mode-hook 'yas-minor-mode)
(add-hook 'java-mode-hook 'lsp-java-lens-mode)

(add-hook 'python-mode-hook 'code-cells-mode-maybe)
(add-hook 'julia-mode-hook 'code-cells-mode-maybe)

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

;; (with-eval-after-load 'code-cells
;;   (let ((map code-cells-mode-map))
;;     (define-key map (kbd "M-p") 'code-cells-backward-cell)
;;     (define-key map (kbd "M-n") 'code-cells-forward-cell)
;;     (define-key map (kbd "C-c C-c") 'code-cells-eval)
;; ))

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
 '(mc/cursor-bar-face ((t (:height 1 :foreground "white"))))
 '(mc/cursor-face ((t (:foreground "white" )))))


;; (load-theme 'vscode-dark-plus t)

;; disable bold face across after loading everything
;; (set-face-bold-p 'bold nil)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)


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

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts nil
      modus-themes-subtle-line-numbers nil
      modus-themes-intense-mouseovers nil
      modus-themes-deuteranopia t
      modus-themes-tabs-accented t
      modus-themes-variable-pitch-ui nil
      modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

      modus-themes-fringes nil ; {nil,'subtle,'intense}

      ;; Options for `modus-themes-lang-checkers' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `straight-underline', `text-also', `background',
      ;; `intense' OR `faint'.
      modus-themes-lang-checkers nil

      ;; Options for `modus-themes-mode-line' are either nil, or a list
      ;; that can combine any of `3d' OR `moody', `borderless',
      ;; `accented', a natural number for extra padding (or a cons cell
      ;; of padding and NATNUM), and a floating point for the height of
      ;; the text relative to the base font size (or a cons cell of
      ;; height and FLOAT)
      modus-themes-mode-line '(accented borderless (padding . 2) (height . 1.1))

      ;; Same as above:
      ;; modus-themes-mode-line '(accented borderless 4 0.9)

      ;; Options for `modus-themes-markup' are either nil, or a list
      ;; that can combine any of `bold', `italic', `background',
      ;; `intense'.
      modus-themes-markup '(background italic)

      ;; Options for `modus-themes-syntax' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
      modus-themes-syntax nil

      ;; Options for `modus-themes-hl-line' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `accented', `underline', `intense'
      modus-themes-hl-line '(underline accented)

      ;; Options for `modus-themes-paren-match' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `bold', `intense', `underline'
      modus-themes-paren-match '(bold intense)

      ;; Options for `modus-themes-links' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
      ;; `bold', `italic', `background'
      modus-themes-links '(neutral-underline background)

      ;; Options for `modus-themes-box-buttons' are either nil (the
      ;; default), or a list that can combine any of `flat', `accented',
      ;; `faint', `variable-pitch', `underline', the symbol of any font
      ;; weight as listed in `modus-themes-weights', and a floating
      ;; point number (e.g. 0.9) for the height of the button's text.
      modus-themes-box-buttons '(variable-pitch flat faint 0.9)

      ;; Options for `modus-themes-prompts' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `background', `bold', `gray', `intense', `italic'
      modus-themes-prompts '(intense bold)

      ;; The `modus-themes-completions' is an alist that reads three
      ;; keys: `matches', `selection', `popup'.  Each accepts a nil
      ;; value (or empty list) or a list of properties that can include
      ;; any of the following (for WEIGHT read further below):
      ;;
      ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
      ;; `selection' - `accented', `intense', `underline', `italic', `text-also' WEIGHT
      ;; `popup' - same as `selected'
      ;; `t' - applies to any key not explicitly referenced (check docs)
      ;;
      ;; WEIGHT is a symbol such as `semibold', `light', or anything
      ;; covered in `modus-themes-weights'.  Bold is used in the absence
      ;; of an explicit WEIGHT.
      modus-themes-completions '((matches . (extrabold))
                                 (selection . (semibold accented))
                                 (popup . (accented intense)))

      modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

      ;; Options for `modus-themes-region' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `no-extend', `bg-only', `accented'
      modus-themes-region '(bg-only no-extend)

      ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
      modus-themes-diffs 'desaturated

      modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

      modus-themes-org-agenda ; this is an alist: read the manual or its doc string
      '((header-block . (variable-pitch 1.3))
        (header-date . (grayscale workaholic bold-today 1.1))
        (event . (accented varied))
        (scheduled . uniform)
        (habit . traffic-light))

      modus-themes-headings ; this is an alist: read the manual or its doc string
      '((1 . (overline background variable-pitch 1.3))
        (2 . (rainbow overline 1.1))
        (t . (semibold))))


(load-theme 'modus-vivendi t)

(provide 'custom)
