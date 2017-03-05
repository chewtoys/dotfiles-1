;; TODO Move config to ORG

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

;;(load "~/.emacs.secrets" t)

;; Functions
;; Jekyll new post
(setq website-dir "~/Projects/juev.org/")

(defun juev/sluggify (str)
  (replace-regexp-in-string
   "[^a-z0-9-]" ""
   (mapconcat 'identity
              (split-string
               (downcase str) " ")
              "-")))

(defun juev/new-post (title)
  (interactive "MTitle: ")
  (let ((slug (juev/sluggify title))
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
    (insert "  - \n")
    (insert "---\n\n")))
;; End Jekyll functions

;; Open notes file
(defun juev/open-my-notes ()
  (interactive)
  (find-file "~/Documents/notes.org"))

(global-set-key (kbd "C-~") 'juev/open-my-notes)
;; End Open notes file

(defun juev/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(add-hook 'kill-buffer-query-functions
          (lambda() (not (equal (buffer-name) "*scratch*"))))

(global-set-key (kbd "C-x k") 'juev/kill-current-buffer)

(defun juev/find-file-as-sudo ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun juev/insert-random-string (len)
  "Insert a random alphanumeric string of length len."
  (interactive)
  (let ((mycharset "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstyvwxyz"))
    (dotimes (i len)
      (insert (elt mycharset (random (length mycharset)))))))

(defun juev/generate-password ()
  "Insert a good alphanumeric password of length 30."
  (interactive)
  (juev/insert-random-string 30))

(setq inhibit-splash-screen t
      inhbit-startup-message t
      initial-scratch-message ""
      inhibit-startup-echo-area-message t
      visible-bell nil
      ring-bell-function 'ignore
      make-backup-files nil
      auto-save-default nil
      sentence-end-double-space nil
      scroll-preserve-screen-position 'always
      default-input-method "russian-computer"
      confirm-nonexistent-file-or-buffer nil
      ido-create-new-buffer 'always
      vc-follow-link t
      vc-follow-symlinks t
      compilation-scroll-output t
      echo-keystrokes 0.1)

(when (eq system-type 'darwin) ;; mac specific settings
  (exec-path-from-shell-initialize))
  ;; (setq mac-option-modifier 'control)
  ;; (setq mac-command-modifier 'meta))

(auto-compression-mode t)
(global-font-lock-mode t)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-prettify-symbols-mode t)

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(setq-default buffer-file-coding-system 'utf-8-unix)

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(column-number-mode 1)
(windmove-default-keybindings)
(delete-selection-mode 1)
(setq-default truncate-lines t)

(when window-system
  (require 'whitespace)
  (global-whitespace-mode +1)
  (set-face-attribute 'whitespace-space nil :background nil :foreground "gray80")
  (set-face-attribute 'whitespace-trailing nil :background "plum1" :foreground "gray80")
  (setq whitespace-style '(face tabs spaces tabs-mark space-mark trailing))
  (set-frame-size (selected-frame) 140 40)
  (set-default-font "Fira Code 14" nil t))

(set-face-attribute 'mode-line nil :foreground "ivory" :background "DarkOrange2")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package better-defaults
  :ensure t
  :config
  (progn
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
        try-complete-lisp-symbol) t))
    (when window-system
      (menu-bar-mode))))

;; https://github.com/hrs/sensible-defaults.el
(use-package sensible-defaults
  :init
  (progn
    (load-file "~/.emacs.d/sensible-defaults.el")
    (sensible-defaults/use-all-settings)
    (sensible-defaults/use-all-keybindings)))

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
  :diminish projectile-mode
  :config
  (projectile-global-mode))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x v s" . magit-status)
         ("C-x v p" . magit-push))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))

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

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    (setq-default ac-auto-start t)
    (setq-default ac-auto-show-menu t)))

(use-package slime
  :ensure t
  :init
  (progn
    ;; (setq inferior-lisp-program "sbcl")
    (setq inferior-lisp-program "sbcl --noinform --no-linedit")
    ;; (setq inferior-lisp-program "ros -Q run")
    (slime-setup '(slime-asdf
                   slime-fancy
                   slime-indentation))
    (setq-default slime-net-coding-system 'utf-8-unix)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (progn
    (which-key-setup-side-window-right)
    (which-key-mode)))

(use-package rust-mode
  :ensure t)

;; (use-package ghc
;;   :ensure t
;;   :config
;;   (progn
;;     (autoload 'ghc-init "ghc" nil t)
;;     (autoload 'ghc-debug "ghc" nil t)
;;     (add-hook 'haskell-mode-hook (lambda () (ghc-init)))))

(use-package haskell-mode
  :ensure t
  :commands haskell-mode)

(use-package centered-cursor-mode
  :ensure t
  :defer t
  :diminish centered-cursor-mode
  ;; :init (global-centered-cursor-mode +1)
  :config
  (progn
    (setq ccm-recenter-at-end-of-file t
          ccm-ignored-commands '(mouse-drag-region
                                 mouse-set-point
                                 widget-button-click
                                 scroll-bar-toolkit-scroll
                                 evil-mouse-drag-region))))

(use-package crux :defer 2)

;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
            (kill-buffer buffer)))))

(use-package guess-language         ; Automatically detect language for Flyspell
  :ensure t
  :commands guess-language-mode
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-languages '(en ru)
        guess-language-min-paragraph-length 35)
  :diminish guess-language-mode)

(use-package restart-emacs
  :ensure t
  :commands restart-emacs)
