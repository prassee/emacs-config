(use-package gcmh :demand :config (gcmh-mode 1))

(use-package
  exec-path-from-shell
  :ensure t
  :pin melpa
  :config (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "JAVA_HOME")
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "JULIA_BINDIR")
  (exec-path-from-shell-copy-env "WORKON_HOME")
  (exec-path-from-shell-copy-env "PATH")
  )

(use-package
  highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

(use-package
  expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package
  projectile
  :bind ("M-p" . projectile-command-map)
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package
  magit
  :if (executable-find "git")
  :bind (("C-x g" . magit-status)
         (:map magit-status-mode-map ("M-RET" . magit-diff-visit-file-other-window)))
  :config (defun magit-log-follow-current-file
              ()
            "A wrapper around `magit-log-buffer-file' with `--follow' argument."
            (interactive)
            (magit-log-buffer-file t)))

(use-package
  ivy
  :defer 0.1
  :diminish ;; :bind (("C-c C-r" . ivy-resume)
  ;;        ("C-x B" . ivy-switch-buffer-other-window))
  :config (custom-set-faces
           '(ivy-current-match ((t (:background "black" :foreground "cyan"))))
           '(ivy-highlight-face ((t (:background "black" :foreground "green")))))
  :custom (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers nil)
  :config (ivy-mode))


(use-package
  ivy-rich
  :after ivy
  :custom (ivy-virtual-abbreviate 'full ivy-rich-switch-buffer-align-virtual-buffer t
                                  ivy-rich-path-style 'abbrev)
  :config (setq ivy-rich-mode 1)
  (ivy-set-display-transformer 'ivy-switch-buffer-other-window 'ivy-rich-switch-buffer-transformer))


(use-package all-the-icons
  :if (display-graphic-p)
  :config (setq all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ivy-rich
  :after counsel-projectile
  :init (all-the-icons-ivy-rich-mode 1)
  :config (setq all-the-icons-ivy-rich-icon-size 0.6))

(use-package smex)

(use-package
  counsel
  :after ivy
  :bind*                                ; load when pressed
  (("M-x"     . counsel-M-x)
   ;; ("C-x C-s" . swiper)
   ("C-x C-r" . counsel-recentf)        ; search for recently edited
   ("C-c g"   . counsel-git)            ; search for files in git repo
   ("C-c j"   . counsel-git-grep)       ; search for regexp in git repo
   ("C-c /"   . counsel-ag) ; Use ag for regexp
   ("C-o"     . counsel-imenu)
   ("C-x l"   . counsel-locate)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)
   ;;("<f2> r" . ivy-resume) ; Resume last Ivy-based completion
   ))

(use-package
  counsel-projectile
  :bind*                                ; load when pressed
  (("C-x t" . counsel-projectile)
   ("C-x C-b" . counsel-projectile-switch-to-buffer)
   ("C-x C-t" . counsel-projectile-find-file)))


;; move lines with ease
(use-package
  drag-stuff
  :defer t
  :init (progn
          (drag-stuff-mode t)
          (drag-stuff-define-keys)
          (drag-stuff-global-mode 1)))

(use-package buffer-move :defer t)

(use-package
  persistent-scratch
  :init (progn (persistent-scratch-setup-default)))

(use-package popup-imenu :commands popup-imenu :bind ("M-i" . popup-imenu))

(use-package
  restclient
  :config (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package fish-mode :mode "\\.fish\\'")

(use-package
  markdown-mode
  :custom (markdown-hide-markup nil)
  (markdown-bold-underscore t)
  (markdown-italic-underscore t)
  (markdown-header-scaling t)
  (markdown-indent-function t)
  (markdown-enable-math t)
  (markdown-hide-urls nil)
  :custom-face (markdown-header-delimiter-face ((t (:foreground "mediumpurple"))))
  (markdown-header-face-1
   ((t (:foreground "violet" :weight bold :height 1.0))))
  (markdown-header-face-2
   ((t (:foreground "lightslateblue" :weight bold :height 1.0))))
  (markdown-header-face-3
   ((t (:foreground "mediumpurple1" :weight bold :height 1.0))))
  (markdown-link-face ((t (:background "#0e1014" :foreground "#bd93f9"))))
  (markdown-list-face ((t (:foreground "mediumpurple"))))
  (markdown-pre-face ((t (:foreground "#bd98fe"))))
  :mode "\\.md\\'")


(use-package multiple-cursors :init (progn (require 'multiple-cursors)))

(use-package
  diff-hl
  :ensure t
  :init (setq diff-hl-margin-mode 1 diff-hl-flydiff-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(use-package hl-line :ensure nil :hook (after-init . global-hl-line-mode))

(use-package
  highlight-indent-guides
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character highlight-indent-guides-auto-enabled nil
        highlight-indent-guides-responsive 'stack highlight-indent-guides-delay 0)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray"))

(use-package transpose-frame)

;; (use-package treemacs-all-the-icons
;;   :config (setq all-the-icons-scale-factor 1.2)
;;   )

;; (use-package
;;   treemacs
;;   :ensure t
;;   :defer t
;;   :init (with-eval-after-load 'winum
;;           (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   ;; :custom-face (treemacs-fringe-indicator-face ((t (:inherit font-lock-doc-face))))
;;   :config (progn
;;             (setq treemacs-collapse-dirs
;;                   (if treemacs-python-executable 3 0)
;;                   treemacs-deferred-git-apply-delay      0.5
;;                   treemacs-directory-name-transformer #'identity treemacs-display-in-side-window        t
;;                   treemacs-eldoc-display t
;;                   treemacs-text-scale -4
;;                   treemacs-file-event-delay              5000
;;                   treemacs-file-extension-regex
;;                   treemacs-last-period-regex-value treemacs-file-follow-delay             0.2
;;                   treemacs-file-name-transformer         #'identity treemacs-follow-after-init t
;;                   treemacs-git-command-pipe              ""
;;                   treemacs-goto-tag-strategy 'refetch-index treemacs-indentation                   1
;;                   treemacs-indentation-string            " "
;;                   treemacs-is-never-other-window nil
;;                   treemacs-max-git-entries               5000
;;                   treemacs-missing-project-action 'ask
;;                   treemacs-move-forward-on-expand        nil
;;                   treemacs-no-png-images t
;;                   treemacs-no-delete-other-windows       t
;;                   treemacs-project-follow-cleanup t
;;                   treemacs-persist-file
;;                   (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;                   treemacs-position                  'left
;;                   treemacs-recenter-distance 0.2
;;                   treemacs-recenter-after-file-follow    nil
;;                   treemacs-recenter-after-tag-follow nil
;;                   treemacs-recenter-after-project-jump 'always
;;                   treemacs-recenter-after-project-expand 'on-distance treemacs-show-cursor nil
;;                   treemacs-show-hidden-files             t
;;                   treemacs-silent-filewatch nil
;;                   treemacs-silent-refresh                nil
;;                   treemacs-sorting 'alphabetic-asc
;;                   treemacs-space-between-root-nodes      nil
;;                   treemacs-tag-follow-cleanup            t
;;                   treemacs-tag-follow-delay 1.5
;;                   treemacs-user-mode-line-format 'none
;;                   treemacs-user-header-line-format nil
;;                   treemacs-width                         20
;;                   treemacs-fringe-indicator-mode 'always
;;                   treemacs-indent-guide-style 'line
;;                   treemacs-width-is-initially-locked       nil
;;                   treemacs-hide-gitignored-files-mode t
;;                   treemacs-workspace-switch-cleanup 'files)

;;             ;; The default width and height of the icons is 22 pixels. If you are
;;             ;; using a Hi-DPI display, uncomment this to double the icon size.
;;             ;; (treemacs-load-theme "Default")
;;             (treemacs-resize-icons 20)
;;             (treemacs-follow-mode t)
;;             (treemacs-indent-guide-mode t)
;;             (cfrs-border-width 2)
;;             (treemacs-filewatch-mode t)
;;             (treemacs-fringe-indicator-mode t)
;;             (treemacs-hide-gitignored-files-mode t)
;;             (pcase (cons
;;                     (not (null (executable-find "git")))
;;                     (not (null treemacs-python-executable)))
;;               (`(t . t)
;;                (treemacs-git-mode 'deferred))
;;               (`(t . _)
;;                (treemacs-git-mode 'simple))))
;;   :bind (:map global-map
;;               ("M-0"       . treemacs-select-window)
;;               ("C-x t 1"   . treemacs-delete-other-windows)
;;               ("C-x t t"   . treemacs)
;;               ("C-x t B"   . treemacs-bookmark)
;;               ("C-x t C-t" . treemacs-find-file)
;;               ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-projectile :after treemacs projectile :ensure t)
;; (use-package treemacs-magit :after treemacs magit :ensure t)
;; (use-package lsp-treemacs :after treemacs)


(use-package blamer
  ;; :bind (("C-c g" . blamer-mode))
  :custom-face (blamer-face ((t :foreground "violet" ;; "#7a88cf"
                                ;;                    :family "Monego Ligatures"
                                :background nil
                                :height 196)))
  :config (setq blamer-idle-time 0.05)
  (setq blamer-author-formatter " %s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter " ● %s")
  (setq blamer-max-commit-message-length 100)
  (setq blamer-min-offset 70))

(use-package mood-line :config
  (setq mood-line-show-cursor-point t)
  (setq mood-line-show-encoding-information nil))

(use-package mermaid-mode
  :ensure t
  :init (setq
         mermaid-mmdc-location "/data/cellar/mermaid-cli/node_modules/.bin/mmdc"
         mermaid-tmp-dir "/media/saipranav/6664-6532/mermaidDiagrams/")
  :mode "\\.mermaid\\'")

(use-package ob-mermaid
  :config (setq ob-mermaid-cli-path "/data/cellar/mermaid-cli/node_modules/.bin/mmdc")
  )

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config (set-face-attribute 'default nil :family "PragmataPro Liga"  :height 162)
  (set-face-attribute 'fixed-pitch nil :family "PragmataPro Liga"  :height 162)
  (set-face-attribute 'variable-pitch nil :family "PragmataPro Liga" :height 162))

(use-package org-superstar              ; supersedes `org-bullets'
  :ensure :after org
  :config (setq org-superstar-remove-leading-stars t)
  (setq org-superstar-headline-bullets-list '(" ")) ;; '("🞛" "◉" "○" "▷")
  (setq org-superstar-item-bullet-alist
        '((?+ . ?•)
          (?* . ?➤)
          (?- . ?–)))
  (org-superstar-mode -1))


(use-package wrap-region)

(use-package emojify)

(use-package modus-themes
  :ensure
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  ;; :config
  ;; Load the theme of your choice:
  ;; (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
  ;; :bind ("<f5>" . modus-themes-toggle)

  )


(provide 'core-utils)
