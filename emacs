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

(setq user-full-name "Denis Evsyukov"
      user-mail-address "denis@evsyukov.org")

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

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
    (insert "date: ")
    (insert (format-time-string "%Y-%m-%d %H:%M")) (insert "\n")
    (insert "image: \n")
    (insert "tags:\n")
    (insert "- \n")
    (insert "---\n\n")))

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

(setq-default indent-tabs-mode nil
              tab-width 2)

(auto-compression-mode t)
(global-font-lock-mode t)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(column-number-mode 1)
(windmove-default-keybindings)
(delete-selection-mode 1)
(setq-default truncate-lines t)

(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)

(require 'whitespace)
(global-whitespace-mode +1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray80")
(set-face-attribute 'whitespace-trailing nil :background "plum1" :foreground "gray80")
(setq whitespace-style '(face tabs spaces tabs-mark space-mark trailing))

(use-package better-defaults
  :ensure t
  :config
  (global-set-key
   (kbd "M-/")
   (make-hippie-expand-function
    '(try-complete-file-name-partially
      try-complete-file-name
      try-expand-dabbrev
      try-expand-dabbrev-all-buffers
      try-expand-dabbrev-from-kill
      try-expand-all-abbrevs
      try-expand-list
      try-expand-line
      try-complete-lisp-symbol-partially
      try-complete-lisp-symbol) t)))

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
          ido-everywhere t)))

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

(custom-set-faces
 '(mode-line ((default (:foreground "ivory" :background "DarkOrange2")))))

(use-package markdown-mode
  :ensure t
  :mode (("\.markdown$" . markdown-mode)
         ("\.md$"       . markdown-mode))
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
