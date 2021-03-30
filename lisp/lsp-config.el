(use-package 
  company
  :config (setq company-idle-delay 0 company-echo-delay 0) 
  (global-company-mode 1) 
  (global-set-key (kbd "C-SPC") 'company-complete))

(use-package
  flycheck
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-set-indication-mode 'left-fringe)
  )

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-json-use-lists t)
  (lsp-signature-doc-lines 2)
  (lsp-completion-provider :capf)
  (lsp-modeline-diagnostics-mode t)
  (lsp-modeline-code-actions-mode t)
  (lsp-keep-workspace-alive nil) ; Auto-kill LSP server
  (lsp-report-if-no-buffer t)
  (lsp-eldoc-hook nil)
  (lsp-rust-analyzer-inlay-hints-mode t)
  ;;  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :config
  (setq gc-cons-threshold 100000000)
  (setq lsp-log-io nil) ; if set to true can cause a performance hit
  (setq lsp-enable-file-watchers nil)
  (setq lsp-headerline-breadcrumb-mode nil)
  (setq lsp-modeline-diagnostics-scope :workspace)
  :hook ((go-mode julia-mode rust-mode
                      js-mode js2-mode typescript-mode web-mode)
          . lsp
         )
  )

(use-package 
  lsp-ui
  ;; :custom-face (lsp-ui-doc-background ((t 
  ;;                                       (:background nil)))) 
  ;; (lsp-ui-doc-header ((t 
  ;;                      (:inherit (font-lock-string-face italic))))) 
  ;; :init
  ;; (setq lsp-ui-doc-use-webkit t) 
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  
  (lsp-ui-doc-use-webkit nil) 
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
;;  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-border   "orange")
  (lsp-ui-doc-include-signature t) 
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  (lsp-ui-doc-max-width 65) 
  (lsp-ui-doc-max-height 29) 
  (lsp-ui-doc-use-childframe t)
  ;; lsp-ui-flycheck
  (lsp-ui-flycheck--start t) 
  (lsp-ui-flycheck-enable t) 
  (lsp-ui-flycheck-list-position 'bottom) 
  (lsp-ui-flycheck-live-reporting t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil) 
  (lsp-ui-sideline-ignore-duplicate t) 
  (lsp-ui-sideline-show-symbol t) 
  (lsp-ui-sideline-show-hover t) 
  (lsp-ui-sideline-show-diagnostics nil) 
  (lsp-ui-sideline-show-code-actions t) 
  (lsp-ui-sideline-code-actions-prefix "")
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t) 
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t) 
  (lsp-ui-peek-show t) 
  (lsp-ui-peek-peek-height 20) 
  (lsp-ui-peek-list-width 50) 
  (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
  ;; :config
  ;; Use lsp-ui-doc-webkit only in GUI
  :bind 
  (:map lsp-mode-map
        ("C-c C-r" . lsp-ui-peek-find-references) 
        ("C-c C-j" . lsp-ui-peek-find-definitions) 
        ("C-c i"   . lsp-ui-peek-find-implementation) 
        ;;("C-c m"   . prassee/toggle-lsp-ui-doc) 
        ;;("C-c s"   . lsp-ui-sideline-mode)
        ) 
  :hook (lsp-mode . lsp-ui-mode))


(use-package 
  go-mode 
  :commands go-mode 
  :mode (("\\.go?\\'" . go-mode)) 
  :defer t
  :init
  (add-hook 'go-mode-hook #'company-mode) 
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'flycheck-mode) 
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)  
  :config
  (lsp-register-custom-settings
   '(
     ("gopls.completeUnimported" t t)
     ("gopls.deepCompletion" t t)
     ("gopls.usePlaceholders" t t)
     ("gopls.staticcheck" t t)
     )
   )
  (setq indent-tabs-mode nil
        c-basic-offset 4
        tab-width 4) 
  )

(use-package 
  yasnippet  )

;; (use-package python-black
;;   :demand t
;;   :after python)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))
                                        ; or lsp-deferred

;; (use-package
;;   pyvenv
;;   :init
;;   (pyvenv-mode)
;;   )

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (python-lsp-setup)))

(use-package 
  rust-mode
  ;; :hook (rust-mode . lsp)
  :init
  (setq lsp-rust-server 'rust-analyzer)
  ;; (setq lsp-rust-all-features 'all)
  (setq lsp-rust-analyzer-inlay-hints-mode t)
  (add-hook 'rust-mode-hook #'lsp-mode)
  (add-hook 'rust-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'yas-minor-mode))

(use-package 
  toml-mode)

(use-package
  typescript-mode
  :hook (typescript-mode . lsp)
  )

;; Add keybindings for interacting with Cargo
(use-package 
  cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package
  flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(with-eval-after-load 'lsp-mode
  ;; :project/:workspace/:file
  (setq lsp-diagnostics-modeline-scope :workspace)
  (add-hook 'lsp-managed-mode-hook 'lsp-modeline-diagnostics-mode))

(use-package
  julia-mode)

(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.5")
  (add-hook 'julia-mode-hook #'lsp)
  )

;; (use-package jupyter)

;; (add-to-list 'load-path "~/.emacs.d/lisp/julia-repl.el")
;; (require 'julia-repl)
;; (add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

(provide 'lsp-config)
