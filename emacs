(require 'cl-lib)
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize nil)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-enable-at-startup nil)

;;(load "~/.custom.el" t)

(setq  user-full-name "Denis Evsyukov"
       user-mail-address "denis@evsyukov.org")

(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t
      auto-compile-display-buffer nil
      auto-compile-mode-line-counter t
      load-prefer-newer t)

;;(load "~/.emacs.secrets" t)

(use-package better-defaults
  :ensure t
  :config
  (progn
    (setq inhibit-splash-screen t
          inhbit-startup-message t
          initial-scratch-message ""
          inhibit-startup-echo-area-message t
          visible-bell nil
          ring-bell-function 'ignore
          make-backup-files nil
          sentence-end-double-space nil
          scroll-preserve-screen-position 'always
          find-file-visit-truename t
          default-input-method "russian-computer")

    (when window-system
      (set-frame-size (selected-frame) 170 50)
      (set-default-font "Consolas 13" nil t))

    (when (display-graphic-p)
      (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
    (setq-default buffer-file-coding-system 'utf-8-unix)

    (when (eq system-type 'darwin) ;; mac specific settings
      (setq mac-command-modifier 'meta)
      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
      (setq exec-path (append exec-path '("/usr/local/bin"))))

    (setq-default indent-tabs-mode nil
                  tab-width 2)

    (blink-cursor-mode -1)
    (fset 'yes-or-no-p 'y-or-n-p)
    (prefer-coding-system 'utf-8)
    (column-number-mode 1)

    (global-set-key (kbd "RET") 'newline-and-indent)
    (global-set-key (kbd "M-/") 'hippie-expand)
    (define-key global-map [home] 'back-to-indentation)
    (define-key global-map [end] 'end-of-line)))

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
  (progn
    (use-package cider
      :ensure t
      :config
      (add-to-list 'ac-modes 'cider-mode)
      (add-to-list 'ac-modes 'cider-repl-mode))
    (use-package ac-cider
      :ensure t
      :init
      (progn
        (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
        (add-hook 'cider-mode-hook 'ac-cider-setup)
        (add-hook 'cider-repl-mode-hook 'ac-cider-setup))
      (setq clojure-defun-style-default-indent t))))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (progn
    (add-hook 'clojure-mode-hook #'enable-paredit-mode)
    (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook #'enable-paredit-mode)
    (add-hook 'json-mode-hook #'enable-paredit-mode)))

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

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

(use-package solarized-theme
  :ensure t
  :config
  (progn
    (setq solarized-scale-org-headlines nil)
    (setq solarized-use-variable-pitch nil)
    ;;(setq solarized-distinct-fringe-background t)
    (load-theme 'solarized-light t)))
