(fset `yes-or-no-p `y-or-n-p)

;; (global-set-key (kbd "C-x 2") 'split-window-vertically)
;; (global-set-key (kbd "C-x 3") 'split-window-horizontally)

(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f8>") 'treemacs)
(global-set-key (kbd "<f9>") 'treemacs-switch-workspace)

(global-set-key "\C-x2" 'split-window-vertically)
(global-set-key "\C-x3" 'split-window-horizontally)

(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

(global-set-key (kbd "C-c w") 'kill-this-buffer)
(global-set-key (kbd "C-c t") 'toggle-window-split)
(global-set-key (kbd "C-c T") 'window-toggle-split-direction)

(global-set-key (kbd "C-c l") 'lsp-format-buffer)
(global-set-key (kbd "s-l fe") 'lsp-ui-flycheck-list)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x a") 'org-capture)
(global-set-key (kbd "C-x 9") 'org-agenda)

(global-set-key (kbd "C-o") 'counsel-imenu)

(global-set-key (kbd "C-c d") 'kill-whole-line)
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x o") 'create-org-file)
  
(global-set-key (kbd "C-S-q") 'mc/edit-lines)
(global-set-key (kbd "C-S-l") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(global-unset-key (kbd "C-z"))

;; (define-key python-mode-map (kbd "s-l ==") 'python-black-buffer)

(provide 'keybindings)
