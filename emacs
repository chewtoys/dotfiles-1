(setq user-full-name "Denis Evsyukov"
      user-mail-address "denis@evsyukov.org")

;;(require 'cl-lib)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)

(setq debug-on-error t)
(setq edebug-trace t)

(setq inhibit-splash-screen t
      initial-scratch-message nil)
      ;; initial-major-mode 'org-mode)

(setq default-input-method "russian-computer")

(setq-default indent-tabs-mode nil)

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
  :idle
  (progn
    (setq company-idle-delay 0)
    (add-hook 'prog-mode-hook 'company-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :idle
  (progn
    (yas-reload-all)
    (add-hook 'prog-mode-hook
              '(lambda ()
                 (yas-minor-mode)))))

(use-package solarized-theme
  :ensure t
  :config
  (progn
    (load-theme 'solarized-dark t)
    (setq solarized-scale-org-headlines nil)
    (setq solarized-height-minus-1 1)
    (setq solarized-height-plus-1 1)
    (setq solarized-height-plus-2 1)
    (setq solarized-height-plus-3 1)
    (setq solarized-height-plus-4 1)
    (setq solarized-distinct-fringe-background t)
    (setq solarized-high-contrast-mode-line t)))

(when window-system
  (set-frame-size (selected-frame) 170 50)
  (set-default-font "Consolas 13"))

(when (eq system-type 'darwin)
  (set-default-font "Inconsolata 15")
  (set-frame-size (selected-frame) 160 54)
  (setq mac-command-modifier 'meta)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line))

(use-package flycheck
  :ensure t
  :idle)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$". markdown-mode))

(use-package js2-mode
  :ensure t
  :mode ("\\.js" . js2-mode)
  :init
  (progn
    (add-hook 'js2-mode-hook 'tern-mode)
    (add-hook 'js2-mode-hook 'smartparens-mode)
    (add-hook 'js2-mode-hook 'flycheck-mode)
    (add-hook 'js2-mode-hook 'company-mode)))

(use-package emmet-mode
  :ensure t
  :idle
  (progn
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)))

(use-package ido-vertical-mode
  :ensure t
  :config
  (progn
    (ido-mode t)
    (ido-vertical-mode 1)
    (setq ido-enable-flex-matching t
          ido-use-virtual-buffers t)))

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :config
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
  :idle
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :idle
  (progn
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'json-mode-hook 'enable-paredit-mode)))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (progn
    (bind-keys
     :map json-mode-map
     ("{" . paredit-open-curly)
     ("}" . paredit-close-curly))))
