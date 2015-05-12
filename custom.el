(setq  user-full-name "Denis Evsyukov"
       user-mail-address "denis@evsyukov.org")

(eval-when-compile
  (require 'use-package))
(require 'diminish
         'bind-key)

(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)

(load "~/.emacs.secrets" t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(setq inhibit-splash-screen t
      inhbit-startup-message t
      initial-scratch-message ""
      inhibit-startup-echo-area-message t)

(when window-system
  (set-frame-size (selected-frame) 170 50)
  (set-default-font "Consolas 13" nil t))

(blink-cursor-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(setq-default buffer-file-coding-system 'utf-8-unix)

(setq default-input-method "russian-computer")

;;(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line))

(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq-default tab-width 2)
(global-set-key (kbd "RET") 'newline-and-indent)
(column-number-mode 1)

(use-package ido-vertical-mode
  :ensure t
  :init
  (progn
    (ido-mode t)
    (ido-vertical-mode 1)
    (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*"
                               "*Messages*" "Async Shell Command"))
    (setq ido-enable-flex-matching t
          ido-use-virtual-buffers t)))

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package irony-mode
  :ensure irony
  :init
  (progn
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)))

(use-package clojure-mode
  :ensure t
  :init
  (progn
    (use-package inf-clojure
      :ensure t
      :init
      (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode))))

(use-package smartparents
  :ensure smartparens
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (add-hook 'clojure-mode-hook #'smartparens-strict-mode)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))
