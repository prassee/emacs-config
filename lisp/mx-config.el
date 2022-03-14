;; (use-package helm
;;   :diminish helm-mode
;;   :init (progn
;;           (require 'helm-config)
;;           (setq helm-candidate-number-limit 100)
;;           ;; From https://gist.github.com/antifuchs/9238468
;;           (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
;;                 helm-input-idle-delay 0.01  ; this actually updates things ; reeeelatively quickly.
;;                 helm-yas-display-key-on-candidate t
;;                 helm-quick-update t
;;                 helm-M-x-requires-pattern nil
;;                 helm-ff-skip-boring-files t
;;                 helm-autoresize-mode t
;;                 helm-semantic-fuzzy-match t
;;                 helm-imenu-fuzzy-match    t
;;                )
;;           (helm-mode))
;;   :bind (("C-h a" . helm-apropos)
;;          ("C-x C-b" . helm-buffers-list)
;;          ("C-x C-f" . helm-find-files)
;;          ("M-y" . helm-show-kill-ring)
;;          ("M-x" . helm-M-x)
;;          ("C-x c o" . helm-occur)
;;          ("C-x c s" . helm-swoop)
;;          ("C-x c y" . helm-yas-complete)
;;          ("C-x c Y" . helm-yas-create-snippet-on-region)
;;          ;; ("C-x c b" . my/helm-do-grep-book-notes)
;;          ("C-x c SPC" . helm-all-mark-rings)))


;; (use-package helm-projectile
;;   :init (progn (require 'helm-projectile) (helm-projectile-on))
;;   :bind (("C-x t" . helm-projectile)))

(use-package selectrum)

(use-package selectrum-prescient :init (selectrum-prescient-mode +1))

;; (setq helm-display-function 'helm-display-buffer-in-own-frame)

(provide 'mx-config)
