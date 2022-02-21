(use-package
  company
  :config (setq company-idle-delay 0 company-echo-delay 0)
  (global-company-mode 1)
  (global-set-key (kbd "C-SPC") 'company-complete))

(use-package
  flycheck
  :custom (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-set-indication-mode 'left-fringe))

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred)
  :custom (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-json-use-lists t)
  (lsp-signature-doc-lines 4)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :capf)
  (lsp-modeline-diagnostics-mode t)
  (lsp-modeline-code-actions-mode t)
  (lsp-keep-workspace-alive nil) ; Auto-kill LSP server
  (lsp-report-if-no-buffer t)
  (lsp-eldoc-hook nil)
  ;;  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((python-mode go-mode julia-mode rust-mode java-mode
                      js-mode js2-mode typescript-mode web-mode)
         . lsp))


(use-package
  lsp-ui
  ;; :custom-face (lsp-ui-doc-background ((t
  ;;                                       (:background nil))))
  ;; (lsp-ui-doc-header ((t
  ;;                      (:inherit (font-lock-string-face italic)))))
  ;; :init
  ;; (setq lsp-ui-doc-use-webkit t)
  :custom ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-enhanced-markdown nil)
  (lsp-ui-doc-use-webkit nil)
  (lsp-ui-doc-border   "orange")
  (lsp-headerline-breadcrumb-enable nil)
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
  :bind (:map lsp-mode-map
              ("C-c C-r" . lsp-ui-peek-find-references)
              ("C-c C-j" . lsp-ui-peek-find-definitions)
              ("C-c i"   . lsp-ui-peek-find-implementation)
              ;;("C-c m"   . prassee/toggle-lsp-ui-doc)
              ;;("C-c s"   . lsp-ui-sideline-mode)
              )
  :hook (lsp-mode . lsp-ui-mode))

(use-package
  company-box
  :diminish :hook
  (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)
  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond
         ((fboundp sym)
          'Function)
         ((featurep sym)
          'Module)
         ((facep sym)
          'Color)
         ((boundp sym)
          'Variable)
         ((symbolp sym)
          'Text)
         (t . nil)))))
  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown .
                     ,(all-the-icons-material
                       "find_in_page"
                       :height 0.7
                       :v-adjust -0.15))
            (Text .
                  ,(all-the-icons-faicon
                    "book"
                    :height 0.68
                    :v-adjust -0.15))
            (Method .
                    ,(all-the-icons-faicon
                      "cube"
                      :height 0.7
                      :v-adjust -0.05
                      :face 'font-lock-constant-face))
            (Function .
                      ,(all-the-icons-faicon
                        "cube"
                        :height 0.7
                        :v-adjust -0.05
                        :face 'font-lock-constant-face))
            (Constructor .
                         ,(all-the-icons-faicon
                           "cube"
                           :height 0.7
                           :v-adjust -0.05
                           :face 'font-lock-constant-face))
            (Field .
                   ,(all-the-icons-faicon
                     "tags"
                     :height 0.65
                     :v-adjust -0.15
                     :face 'font-lock-warning-face))
            (Variable .
                      ,(all-the-icons-faicon
                        "tag"
                        :height 0.7
                        :v-adjust -0.05
                        :face 'font-lock-warning-face))
            (Class .
                   ,(all-the-icons-faicon
                     "clone"
                     :height 0.65
                     :v-adjust 0.01
                     :face 'font-lock-constant-face))
            (Interface .
                       ,(all-the-icons-faicon
                         "clone"
                         :height 0.65
                         :v-adjust 0.01))
            (Module .
                    ,(all-the-icons-octicon
                      "package"
                      :height 0.7
                      :v-adjust -0.15))
            (Property .
                      ,(all-the-icons-octicon
                        "package"
                        :height 0.7
                        :v-adjust -0.05
                        :face 'font-lock-warning-face))
            ;; Golang module
            (Unit .
                  ,(all-the-icons-material
                    "settings_system_daydream"
                    :height 0.7
                    :v-adjust -0.15))
            (Value .
                   ,(all-the-icons-material
                     "format_align_right"
                     :height 0.7
                     :v-adjust -0.15
                     :face 'font-lock-constant-face))
            (Enum .
                  ,(all-the-icons-material
                    "storage"
                    :height 0.7
                    :v-adjust -0.15
                    :face 'all-the-icons-orange))
            (Keyword .
                     ,(all-the-icons-material
                       "filter_center_focus"
                       :height 0.7
                       :v-adjust -0.15))
            (Snippet .
                     ,(all-the-icons-faicon
                       "code"
                       :height 0.7
                       :v-adjust 0.02
                       :face 'font-lock-variable-name-face))
            (Color .
                   ,(all-the-icons-material
                     "palette"
                     :height 0.7
                     :v-adjust -0.15))
            (File .
                  ,(all-the-icons-faicon
                    "file-o"
                    :height 0.7
                    :v-adjust -0.05))
            (Reference .
                       ,(all-the-icons-material
                         "collections_bookmark"
                         :height 0.7
                         :v-adjust -0.15))
            (Folder .
                    ,(all-the-icons-octicon
                      "file-directory"
                      :height 0.7
                      :v-adjust -0.05))
            (EnumMember .
                        ,(all-the-icons-material
                          "format_align_right"
                          :height 0.7
                          :v-adjust -0.15
                          :face 'all-the-icons-blueb))
            (Constant .
                      ,(all-the-icons-faicon
                        "tag"
                        :height 0.7
                        :v-adjust -0.05))
            (Struct .
                    ,(all-the-icons-faicon
                      "clone"
                      :height 0.65
                      :v-adjust 0.01
                      :face 'font-lock-constant-face))
            (Event .
                   ,(all-the-icons-faicon
                     "bolt"
                     :height 0.7
                     :v-adjust -0.05
                     :face 'all-the-icons-orange))
            (Operator .
                      ,(all-the-icons-fileicon
                        "typedoc"
                        :height 0.65
                        :v-adjust 0.05))
            (TypeParameter .
                           ,(all-the-icons-faicon
                             "hashtag"
                             :height 0.65
                             :v-adjust 0.07
                             :face 'font-lock-const-face))
            (Template .
                      ,(all-the-icons-faicon
                        "code"
                        :height 0.7
                        :v-adjust 0.02
                        :face 'font-lock-variable-name-face))))))


(use-package
  go-mode
  :commands go-mode
  :mode (("\\.go?\\'" . go-mode))
  :defer t
  :init (add-hook 'go-mode-hook #'company-mode)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'flycheck-mode)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  :config (lsp-register-custom-settings
           '(("gopls.completeUnimported" t t)
             ("gopls.deepCompletion" t t)
             ("gopls.usePlaceholders" t t)
             ("gopls.staticcheck" t t)))
  (setq indent-tabs-mode nil c-basic-offset 4 tab-width 4))

(use-package yasnippet)

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp))))  ; or lsp-deferred


(use-package
  rust-mode
  :hook (rust-mode . lsp)
  :init (setq lsp-rust-server 'rust-analyzer)
  ;; (setq lsp-rust-all-features 'all)
  (setq lsp-rust-analyzer-inlay-hints-mode t)
  (add-hook 'rust-mode-hook #'lsp-mode)
  (add-hook 'rust-mode-hook #'yas-minor-mode))

;; Add keybindings for interacting with Cargo
(use-package cargo :hook (rust-mode . cargo-minor-mode))

(use-package
  flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode)

(use-package typescript-mode :hook (typescript-mode . lsp))

(use-package lsp-java
  :config (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
                lsp-java-format-settings-profile "GoogleStyle"))

(with-eval-after-load 'lsp-mode
  ;; :project/:workspace/:file
  (setq lsp-diagnostics-modeline-scope :workspace)
  (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode))

(use-package julia-mode)

(use-package lsp-julia
  :config (setq lsp-julia-default-environment "~/.julia/environments/v1.7")
  (add-hook 'julia-mode-hook #'lsp))

(use-package dockerfile-mode)

(use-package lsp-docker)

(provide 'lsp-config)
