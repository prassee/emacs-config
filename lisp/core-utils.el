(use-package 
  exec-path-from-shell 
  :ensure t 
  :pin melpa 
  :config (exec-path-from-shell-initialize) 
  (exec-path-from-shell-copy-env "JAVA_HOME")
  (exec-path-from-shell-copy-env "GOPATH") 
  (exec-path-from-shell-copy-env "JULIA_BINDIR") 
  (exec-path-from-shell-copy-env "WORKON_HOME") 
  (exec-path-from-shell-copy-env "PATH"))

(use-package 
  highlight-symbol 
  :diminish highlight-symbol-mode 
  :commands highlight-symbol 
  :bind ("s-h" . highlight-symbol))

(use-package 
  expand-region 
  :commands 'er/expand-region 
  :bind ("C-=" . er/expand-region))

(use-package magit)

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-height 15)
  (setq ivy-display-style nil)
  (setq ivy-re-builders-alist
        '((counsel-rg            . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (swiper                . ivy--regex-plus)
          (t                     . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-mode-map       (kbd "<escape>") nil)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))



(use-package 
  ivy-rich 
  :after ivy 
  :custom (ivy-virtual-abbreviate 'full ivy-rich-switch-buffer-align-virtual-buffer t
                                  ivy-rich-path-style 'abbrev) 
  :config (setq ivy-rich-mode 1) 
  (ivy-set-display-transformer 'ivy-switch-buffer-other-window 'ivy-rich-switch-buffer-transformer))

;; (use-package
;;   smex)

(use-package 
  counsel 
  :after ivy 
  :bind*                                ; load when pressed
  (("M-x"     . counsel-M-x) 
   ("C-s"     . swiper) 
   ("C-x C-b" . counsel-projectile-switch-to-buffer) 
   ("C-x C-f" . counsel-find-file) 
   ("C-x C-r" . counsel-recentf)        ; search for recently edited
   ("C-c g"   . counsel-git)            ; search for files in git repo
   ("C-c j"   . counsel-git-grep)       ; search for regexp in git repo
   ("C-c /"   . counsel-ag)             ; Use ag for regexp
   ("C-x l"   . counsel-locate) 
   ("C-x C-f" . counsel-find-file) 
   ("<f1> f"  . counsel-describe-function) 
   ("<f1> v"  . counsel-describe-variable) 
   ("<f1> l"  . counsel-find-library) 
   ("<f2> i"  . counsel-info-lookup-symbol) 
   ("<f2> u"  . counsel-unicode-char) 
   ;;("<f2> r" . ivy-resume) ; Resume last Ivy-based completion
   ))            

(use-package projectile
  :bind
  ("M-p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-prefix " ")
  (projectile-mode +1)

  (add-to-list 'projectile-globally-ignored-directories "node_modules"))


(use-package swiper
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))


(use-package 
  counsel-projectile
  :config
  (counsel-projectile-mode +1)
  :bind*                                ; load when pressed
  (("C-x t" . counsel-projectile) 
   ("C-x C-t" . counsel-projectile-find-file)))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy counsel)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper
               counsel-grep
               counsel-rg
               counsel-projectile-rg
               ivy-switch-buffer
               counsel-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))


;; move lines with ease
(use-package 
  drag-stuff 
  :defer t 
  :init (progn (drag-stuff-mode t) 
               (drag-stuff-define-keys) 
               (drag-stuff-global-mode 1)))

(use-package 
  buffer-move 
  :defer t)

(use-package 
  persistent-scratch 
  :init (progn (persistent-scratch-setup-default)))

(use-package 
  popup-imenu 
  :commands popup-imenu 
  :bind ("M-i" . popup-imenu))

(use-package 
  restclient 
  :config (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package 
  fish-mode 
  :mode "\\.fish\\'")

(use-package 
  markdown-mode 
  :custom (markdown-hide-markup nil) 
  (markdown-bold-underscore t) 
  (markdown-italic-underscore t) 
  (markdown-header-scaling t) 
  (markdown-indent-function t) 
  (markdown-enable-math t)
  (markdown-hide-urls nil) 
  :custom-face (markdown-header-delimiter-face ((t 
                                                 (:foreground "mediumpurple")))) 
  (markdown-header-face-1 ((t 
                            (:foreground "violet" 
                                         :weight bold 
                                         :height 1.0)))) 
  (markdown-header-face-2 ((t 
                            (:foreground "lightslateblue" 
                                         :weight bold 
                                         :height 1.0)))) 
  (markdown-header-face-3 ((t 
                            (:foreground "mediumpurple1" 
                                         :weight bold 
                                         :height 1.0)))) 
  (markdown-link-face ((t 
                        (:background "#0e1014" 
                                     :foreground "#bd93f9")))) 
  (markdown-list-face ((t 
                        (:foreground "mediumpurple")))) 
  (markdown-pre-face ((t 
                       (:foreground "#bd98fe")))) 
  :mode "\\.md\\'")

(use-package iedit)

(use-package 
  diff-hl 
  :ensure t 
  :init (setq diff-hl-margin-mode 1 diff-hl-flydiff-mode 1) 
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh) 
  (global-diff-hl-mode))

(use-package 
  hl-line 
  :ensure nil 
  :hook (after-init . global-hl-line-mode))

(use-package 
  highlight-indent-guides 
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode) 
  (setq highlight-indent-guides-method 'character highlight-indent-guides-auto-enabled nil
        highlight-indent-guides-responsive 'stack highlight-indent-guides-delay 0) 
  (set-face-background 'highlight-indent-guides-odd-face "darkgray") 
  (set-face-background 'highlight-indent-guides-even-face "dimgray") 
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray"))

(use-package 
  transpose-frame)


(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.8))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode);
  (setq all-the-icons-dired-monochrome nil))


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :custom-face
  (treemacs-fringe-indicator-face ((t (:inherit font-lock-doc-face))))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   1
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 t
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      nil
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         'none
          treemacs-user-header-line-format       nil
          treemacs-width                         20
          treemacs-workspace-switch-cleanup 'files   
          )

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 22)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-all-the-icons)
;; (require 'treemacs-all-the-icons)
;; (treemacs-load-theme "all-the-icons")

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; (use-package modus-vivendi-theme)


(use-package vscode-dark-plus-theme
  :ensure t
  :after solaire-mode
  :config
  (load-theme 'vscode-dark-plus t))

(use-package solaire-mode
  :ensure t
  :hook ((change-major-mode . turn-on-solaire-mode)
         (after-revert . turn-on-solaire-mode)
         (ediff-prepare-buffer . solaire-mode)
         (minibuffer-setup . solaire-mode-in-minibuffer))
  :config
  (add-to-list 'solaire-mode-themes-to-face-swap '"vscode-dark-plus")
  (setq solaire-mode-auto-swap-bg t)
  (solaire-global-mode +1))

;; (use-package vscode-dark-plus-theme)

(use-package dockerfile-mode)

(use-package good-scroll
  :config
  (good-scroll-mode 1)
  )

(use-package mood-line)

(provide 'core-utils)
