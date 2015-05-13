(setq  user-full-name "Denis Evsyukov"
       user-mail-address "denis@evsyukov.org")

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)
(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)

(load "~/.emacs.secrets" t)

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

(setq make-backup-files nil)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(setq-default buffer-file-coding-system 'utf-8-unix)

(setq default-input-method "russian-computer")

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin"))))

(setq-default indent-tabs-mode nil)

(setq-default tab-width 2)
(global-set-key (kbd "RET") 'newline-and-indent)
(column-number-mode 1)

(use-package better-defaults
  :ensure t)

(use-package ido-vertical-mode
  :ensure t
  :defer t
  :init
  (progn
    (ido-mode t)
    (ido-vertical-mode 1))
  :config
  (progn
    (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "Async Shell Command"))
    (setq ido-enable-flex-matching t
          ido-use-virtual-buffers t)))

(use-package clojure-mode
  :ensure t
  :mode (("\.clj$"      . clojure-mode)
         ("\.cljs$"     . clojure-mode)
         ("\.cljx$"     . clojure-mode)
         ("\.edn$"      . clojure-mode)
         ("\.boot$"     . clojure-mode)
         ("\.cljs\.hl$" . clojure-mode))
  :config
  (use-package cider
    :ensure t))

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode t)
  :config
  (progn
    (require 'smartparens-config)
    (add-hook 'clojure-mode-hook #'smartparens-strict-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0"))
