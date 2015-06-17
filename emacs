(eval-when-compile
  (require 'cl))
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

(setq  user-full-name "Denis Evsyukov"
       user-mail-address "denis@evsyukov.org")

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-verbose t
      auto-compile-display-buffer nil
      auto-compile-mode-line-counter t
      load-prefer-newer t)

;;(load "~/.emacs.secrets" t)

;; This advises ido-find-file to reopen the selected file as root
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Functions
;; Jekyll new post
;;(require 'cl)
(setq website-dir "~/Projects/juev.org/")

(defun sluggify (str)
  (replace-regexp-in-string
   "[^a-z0-9-]" ""
   (mapconcat 'identity
              (remove-if-not 'identity
                             (subseq (split-string
                                      (downcase str) " ")
                                     0 6))
              "-")))

(defun new-post (title)
  (interactive "MTitle: ")
  (let ((slug (sluggify title))
        (date (current-time)))
    (find-file (concat website-dir "source/_posts/"
                       (format-time-string "%Y-%m-%d") "-" slug
                       ".markdown"))
    (insert "---\n")
    (insert "layout: post\n")
    (insert "title: \"") (insert title) (insert "\"\n")
    (insert "date: ") (insert (format-time-string "%Y-%m-%d %H:%M")) (insert "\n")
    (insert "image: \n")
    (insert "tags:\n")
    (insert "- \n")
    (insert "---\n\n")))

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
          default-input-method "russian-computer"
          confirm-nonexistent-file-or-buffer nil
          ido-create-new-buffer 'always
					vc-follow-link t
					vc-follow-symlinks t
          echo-keystrokes 0.1)

    (when window-system
      (set-frame-size (selected-frame) 170 50)
      (set-default-font "DejaVu Sans Mono 13" nil t))

    (when (display-graphic-p)
      (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
    (setq-default buffer-file-coding-system 'utf-8-unix)

    (when (eq system-type 'darwin) ;; mac specific settings
      (setq mac-command-modifier 'meta)
      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
      (setq exec-path (append exec-path '("/usr/local/bin")))
			(when window-system
				(set-frame-size (selected-frame) 190 55)
				(set-default-font "DejaVu Sans Mono 15" nil t)))

    (setq-default indent-tabs-mode t
                  tab-width 2)

    (auto-compression-mode t)
    (global-font-lock-mode t)
    (blink-cursor-mode -1)
    (fset 'yes-or-no-p 'y-or-n-p)

    (setq locale-coding-system 'utf-8) ; pretty
    (set-terminal-coding-system 'utf-8) ; pretty
    (set-keyboard-coding-system 'utf-8) ; pretty
    (set-selection-coding-system 'utf-8) ; please
    (prefer-coding-system 'utf-8) ; with sugar on top
    
    (column-number-mode 1)
    (windmove-default-keybindings)
    (delete-selection-mode 1)
    ;; Show me empty lines after buffer end
    (set-default 'indicate-empty-lines t)
    (global-subword-mode 1)
    (setq-default truncate-lines t)

    (define-key global-map [home] 'beginning-of-line)
    (define-key global-map [end] 'end-of-line))

  (use-package ido-vertical-mode
    :ensure t
    :defer t
    :init
    (progn
      (ido-mode t)
      (ido-vertical-mode t))
    :config
    (progn
      (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "Async Shell Command"))
      (setq ido-enable-flex-matching t
            ido-use-virtual-buffers t
            ido-everywhere t))))

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
      :ensure t)
    (setq clojure-defun-style-default-indent t)))

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
	:defer t
	:diminish projectile-mode
  :config
  (projectile-global-mode))

(use-package magit
  :ensure t
	:defer t
	:bind (("C-x v s" . magit-status)
				 ("C-x v p" . magit-push))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
	:config
	(progn
		(set-face-foreground 'magit-diff-add "green3")
		(set-face-foreground 'magit-diff-del "red3")))

(if window-system
		(use-package basic-theme
			:ensure t
			:config
			(load-theme 'basic t))

	(use-package material-theme
		:ensure t
		:config
		(load-theme 'material t)))

(use-package markdown-mode
  :ensure t
  :mode (("\.markdown$"      . markdown-mode)
         ("\.md$"      . markdown-mode))
  :config
  (progn
    (add-hook 'markdown-mode-hook #'visual-line-mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml$" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook (lambda () (electric-indent-local-mode -1))))

(use-package mmm-mode
  :ensure t
	:defer t
	:diminish mmm-mode
  :config
  (progn
    (setq mmm-global-mode 'maybe)
    (mmm-add-classes
     '((yaml-header-matters
        :submode yaml-mode
        :face mmm-code-submode-face
        :front "\\`---"
        :back "^---")))    
    (mmm-add-mode-ext-class 'markdown-mode nil 'yaml-header-matters)))

(use-package helm
  :ensure t
  :bind (("C-c h" . helm-mini)				 
         ("C-x C-b" . helm-buffers-list)
				 ("C-h a" . helm-apropos)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop))
	:diminish helm-mode
  :init
	(progn
		(require 'helm-config)
		(helm-mode))
  :config
  (progn
    (helm-autoresize-mode t)
    (setq helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match    t) ))

(use-package auto-complete
  :ensure t
  :bind ("M-/" . auto-complete)
  :init
  (ac-config-default)
  :config
  (setq ac-auto-start nil))

(use-package evil-leader
	:commands (evil-leader-mode)
	:ensure t
	:demand evil-leader
	:init
	(global-evil-leader-mode)
	:config
	(progn
		(evil-leader/set-leader ",")
		(evil-leader/set-key "w" 'save-buffer)
		(evil-leader/set-key "q" 'kill-buffer-and-window)
		(evil-leader/set-key "x" 'helm-M-x)
		(evil-leader/set-key "f" 'find-file)))

(use-package evil
	:ensure t
	:config
	(progn
		;; change mode-line color by evil state
		(lexical-let ((default-color (cons (face-background 'mode-line)
																			 (face-foreground 'mode-line))))
			(add-hook 'post-command-hook
								(lambda ()
									(let ((color (cond ((minibufferp) default-color)
																		 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
																		 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
																		 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
																		 (t default-color))))
										(set-face-background 'mode-line (car color))
										(set-face-foreground 'mode-line (cdr color))))))
		(evil-mode t)))
	
