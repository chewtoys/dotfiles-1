(setq user-full-name "Denis Evsyukov"
      user-mail-address "denis@evsyukov.org")

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)

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

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(setq column-number-mode t)

(use-package company
  :ensure company
  :config
  (progn
    (setq company-idle-delay 0)
    (add-hook 'prog-mode-hook 'company-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (progn
    (yas-reload-all)
    (add-hook 'prog-mode-hook
          '(lambda ()
             (yas-minor-mode)))))

(if window-system
    (progn
      (use-package solarized-theme
        :config (load-theme 'solarized-light t)
        :ensure t)
      (set-default-font "Consolas 13"))
  (load-theme 'wombat t))

(when (eq system-type 'darwin)
  (set-default-font "Inconsolata 15")
  (setq mac-command-modifier 'meta)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  )

(use-package autopair
  :ensure t
  :config
    (autopair-global-mode))

(use-package flycheck
  :ensure t
  :config
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
  :config
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
  :config
  (progn
    (ido-mode 1)
    (ido-vertical-mode 1)))

(setq org-default-notes-file (concat org-directory "~/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))
