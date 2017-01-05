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
  (let ((slug title)
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
  (set-frame-size (selected-frame) 140 40)
  (set-default-font "Fira Code 14" nil t))

(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
(setq-default buffer-file-coding-system 'utf-8-unix)

(when (eq system-type 'darwin) ;; mac specific settings
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  ;; (setq mac-option-modifier 'alt)
  ;; (setq mac-command-modifier 'control)
  )

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

(when window-system
  (require 'whitespace)
  (global-whitespace-mode +1)
  (set-face-attribute 'whitespace-space nil :background nil :foreground "gray80")
  (set-face-attribute 'whitespace-trailing nil :background "plum1" :foreground "gray80")
  (setq whitespace-style '(face tabs spaces tabs-mark space-mark trailing)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    (set-face-foreground 'magit-diff-add "green3")
    (set-face-foreground 'magit-diff-del "red3")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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

(use-package anaconda-mode
  :ensure t
  :init
  (progn
    (use-package ac-anaconda
      :ensure t)
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    (add-hook 'python-mode-hook 'ac-anaconda-setup)
    (setq py-isort-options '("-sl"))))

(use-package auto-complete
  :ensure t
  :init
  (ac-config-default))

;; (use-package evil
;;   :ensure t
;;   :init
;;   (progn
;;     (evil-mode t)
;;     (setq evil-want-fine-undo t)
;;     (define-key evil-normal-state-map [escape] 'keyboard-quit)
;;     (define-key evil-visual-state-map [escape] 'keyboard-quit)
;;     (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;;     (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;;     (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;;     (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;;     (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;;     (define-key evil-motion-state-map (kbd "SPC") 'evil-ex)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
'(package-selected-packages
   (quote
    (go-rename go-autocomplete go-eldoc flycheck flycheck-gometalinter go-mode ledger ledger-mode ac-anaconda evil anaconda-mode mmm-mode yaml-mode markdown-mode magit projectile rainbow-delimiters paredit ido-vertical-mode better-defaults use-package))))

;;gometalinter
(use-package flycheck-gometalinter
  :ensure t
  :config
  (progn
    (flycheck-gometalinter-setup))

  ;; gometalinter: skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (setq flycheck-gometalinter-vendor t)
  ;; gometalinter: only enable selected linters
  (setq flycheck-gometalinter-disable-all t)
  (setq flycheck-gometalinter-enable-linters
        '("golint" "vet" "vetshadow" "golint" "ineffassign" "goconst" "gocyclo" "errcheck" "deadcode")))

;; go-mode
(use-package go-mode
  :ensure t
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)
    (setq-default gofmt-command "goimports")
    (add-hook 'go-mode-hook 'go-eldoc-setup)
   ; (add-hook 'go-mode-hook 'yas-minor-mode)
    (add-hook 'go-mode-hook 'flycheck-mode)))

;; go-eldoc
(use-package go-eldoc
  :ensure t
  :config
  (progn
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

;; go-autocomlete
(use-package go-autocomplete
  :ensure t)

(use-package go-rename
  :ensure t)

;; (global-set-key (kbd "C-c C-n") (find-file "~/Documents/notes.org"))
