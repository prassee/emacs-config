(add-hook 'window-setup-hook 'toggle-frame-maximized t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; (setq warning-minimum-level :emergency)
(setq auto-revert-check-vc-info t)

(defun remap-faces-default-attributes ()
   (let ((family (face-attribute 'default :family))
         (height (face-attribute 'default :height)))
     (mapcar (lambda (face)
              (face-remap-add-relative
               face :family family :weight 'normal :height height))
          (face-list))))

(when (display-graphic-p)
   (add-hook 'minibuffer-setup-hook 'remap-faces-default-attributes)
   (add-hook 'change-major-mode-after-body-hook 'remap-faces-default-attributes))


;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist default-file-name-handler-alist)))

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              visual-fill-column-width 100
              word-wrap t
              blink-cursor-mode t
              blink-cursor-interval 0.2
              cursor-type '(bar . 2)
              right-fringe-width 0
              left-fringe-width  3
              frame-title-format "%f")

;; global variables
(setq inhibit-startup-screen t
      inhibit-compacting-font-caches t
      find-file-visit-truename t
      create-lockfiles nil
      make-backup-files nil
      column-number-mode t
      scroll-error-top-bottom t
      show-paren-delay 0.5
      use-package-always-ensure t
      sentence-end-double-space nil
      use-file-dialog nil
      use-dialog-box nil
      ring-bell-function 'ignore
      mouse-wheel-scroll-amount '(1 ((shift) . 1) 
                                    ((control) . nil))
      mouse-wheel-progressive-speed nil
      global-auto-revert-non-file-buffers t
      warning-minimum-level :emergency
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/") 
                         ("org" . "http://orgmode.org/elpa/") 
                         ("melpa" . "http://melpa.org/packages/") 
                         ("melpa-stable" . "http://stable.melpa.org/packages/"))
      package-archive-priorities '(("melpa" . 1)))


;; the package manager
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents) 
  (package-refresh-contents) 
  (package-install 'use-package))

(defconst user-init-dir 
  (cond ((boundp 'user-emacs-directory) user-emacs-directory) 
        ((boundp 'user-init-directory) user-init-directory) 
        (t "~/.emacs.d/")))

(defun load-user-file (file) 
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(load "core-utils")

(load "lsp-config")

(load "keybindings")

(load "org-config")

(load "custom")

;; disable bold face across after loading everything
(set-face-bold-p 'bold nil)

(mapc
(lambda (face)
(set-face-attribute face nil :weight 'normal :underline nil))
(face-list))

(shell-command "truncate -s 0 ~/.emacs.d/temp.el")

(setq custom-file (concat user-emacs-directory "temp.el"))

(load-file "~/.emacs.d/temp.el")
