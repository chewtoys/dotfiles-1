(setq user-full-name "Denis Evsyukov"
      user-mail-address "denis@evsyukov.org")

(require 'cl-lib)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)

(setq debug-on-error t)
(setq edebug-trace t)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(setq default-input-method "russian-computer")

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq sentence-end-double-space nil)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(setq make-backup-files nil)
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default vc-follow-symlinks nil)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq-default buffer-file-coding-system 'utf-8-unix)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x g") 'magit-status)

(setq column-number-mode t)

(use-package company
  :ensure t
  :init
  (progn
    (setq company-idle-delay 0)
    (add-hook 'prog-mode-hook 'company-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (progn
    (yas-reload-all)
    (add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))))

(use-package color-theme
  :ensure t
  :init
  (use-package color-theme-solarized
    :ensure t
    :init
    (color-theme-solarized 'dark)))

(when window-system
  (set-frame-size (selected-frame) 160 54)
  (set-default-font "Consolas 13"))

(when (eq system-type 'darwin)
  (set-default-font "Inconsolata 15")
  (setq mac-command-modifier 'meta)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line))

(use-package autopair
  :ensure t
  :init
    (autopair-global-mode))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'js-mode-hook
            (lambda () (flycheck-mode t))))

(use-package better-defaults
  :ensure t)
(use-package markdown-mode
  :ensure t)
(use-package htmlize
  :ensure t)
(use-package org)
(use-package js2-mode
  :ensure t
  :mode ("\\.js\'" . js2-mode))
(use-package magit
  :ensure t)
(use-package emmet-mode
  :ensure t
  :init
  (progn
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)))

(use-package ethan-wspace
  :ensure t
  :init
  (progn
    (global-ethan-wspace-mode 1)
    (setq require-final-newline nil)
    (setq mode-require-final-newline nil)))

(use-package ido-vertical-mode
  :ensure t
  :init
  (progn
    (ido-mode 1)
    (ido-vertical-mode 1)
    (ido-mode t)
    (setq ido-enable-flex-matching t
          ido-use-virtual-buffers t)))

(setq org-default-notes-file (concat org-directory "~/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-x" "C-c"))
    (setq guide-key/recursive-key-sequence-flag t)
    (guide-key-mode 1)))


(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(use-package erefactor
  :ensure t
  :init
  (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map))

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(bind-key "C-c f" 'find-function)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (projectile-global-mode)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package miniedit
  :ensure t
  :commands minibuffer-edit
  :init (miniedit-install))
