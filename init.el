(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)                      
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; mac specific

;; BetterGC
(defvar better-gc-cons-threshold 134217728 ; 64mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold better-gc-cons-threshold))
          (setq gc-cons-threshold 100000000))

(setq read-process-output-max (* 1024 1024))
;; BetterGC

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state) (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

;; (setq warning-minimum-level :emergency)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              visual-fill-column-width 100
              word-wrap t
              blink-cursor-mode t
              blink-cursor-interval 0.3
              cursor-type
              '(bar . 2)
              right-fringe-width 0
              left-fringe-width  2
              frame-title-format "%f")


;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t
      ;; Set the file-name-handler to nil (because regexing is cpu intensive)
      file-name-handler-alist nil
      inhibit-startup-screen t
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
      mouse-wheel-scroll-amount
      '(1
        ((shift)
         . 1)
        ((control)
         . nil))
      mouse-wheel-progressive-speed nil
      global-auto-revert-non-file-buffers t
      package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/"))
      package-archive-priorities
      '(("melpa" . 1)))


;; the package manager
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(defconst user-init-dir
  (cond
   ((boundp 'user-emacs-directory)
    user-emacs-directory)
   ((boundp 'user-init-directory)
    user-init-directory)
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

(load "elfmt")

(load "eshell-toggle")

(load "custom")

(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :height 140
                    :weight 'normal
                    :width  'normal)

(shell-command "truncate -s 0 ~/.emacs.d/temp.el")

(setq custom-file (concat user-emacs-directory "temp.el"))

(load-file "~/.emacs.d/temp.el")

(customize-set-variable 'tramp-use-ssh-controlmaster-options nil)
