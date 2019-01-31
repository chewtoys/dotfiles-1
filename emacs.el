(load "~/.emacs-proxy.el" t)

(eval-when-compile
  (require 'cl))
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-enable-at-startup nil)
(setq use-package-always-ensure t)

(setq user-full-name "Denis Evsyukov"
      user-mail-address "denis@evsyukov.org")

(load "~/.emacs.secrets" t)

(setq inhibit-splash-screen t)
(setq inhbit-startup-message t)
(setq initial-scratch-message "")
(setq inhibit-startup-echo-area-message t)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq sentence-end-double-space nil)

(setq scroll-preserve-screen-position 'always)
(setq scroll-margin 4)

(setq default-input-method "russian-computer")

(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

(setq vc-follow-link nil)
(setq vc-follow-symlinks nil)

(setq compilation-scroll-output t)

(setq echo-keystrokes 0.1)

(setq default-directory "~/")

(setq gc-cons-threshold 20000000)

(setq require-final-newline t)

(setq ns-pop-up-frames nil)

(setq tab-always-indent 'complete)

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(auto-compression-mode t)

(global-font-lock-mode t)

(blink-cursor-mode -1)

(electric-pair-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; (global-prettify-symbols-mode t)

(setq org-src-fontify-natively t)

(transient-mark-mode t)

(delete-selection-mode t)

(show-paren-mode t)
(setq show-paren-delay 0.0)

(column-number-mode 1)

;;  (setq-default truncate-lines t)
(setq-default global-visual-line-mode t)

(global-auto-revert-mode t)

(setq use-dialog-box nil)

(when window-system
    ;; (require 'whitespace)
    ;; (global-whitespace-mode +1)
    ;; (set-face-attribute 'whitespace-space nil :background nil :foreground "gray80")
    ;; (set-face-attribute 'whitespace-trailing nil :background "plum1" :foreground "gray80")
    ;; (setq whitespace-style '(face tabs spaces tabs-mark space-mark trailing))
    (set-frame-size (selected-frame) 190 60)
    (set-default-font "Fira Code 14" nil t))

(if (eq system-type 'windows-nt)
         (set-default-font "Fira Code 12" nil t))

;; (set-face-attribute 'mode-line nil :foreground "ivory" :background "DarkOrange2")

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'prog-mode-hook 'subword-mode)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

(add-hook 'kill-buffer-query-functions
          (lambda() (not (equal (buffer-name) "*scratch*"))))

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
    (find-file (concat website-dir "_posts/"
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

(defun juev/open-my-notes ()
  (interactive)
  (find-file "~/Documents/notes.org"))

(global-set-key (kbd "C-~") 'juev/open-my-notes)

(defun juev/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

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

(defun juev/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "M-;")
                'juev/comment-or-uncomment-region-or-line)

(defun juev/reset-text-size ()
  (interactive)
  (text-scale-set 0))

(define-key global-map (kbd "C-)") 'juev/reset-text-size)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C-_") 'text-scale-decrease)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)
(global-set-key (kbd "s-<left>") #'beginning-of-line)
(global-set-key (kbd "s-<right>") #'end-of-line)
(global-set-key (kbd "s-<up>") #'backward-paragraph)
(global-set-key (kbd "s-<down>") #'forward-paragraph)
(global-set-key (kbd "s-}") #'next-buffer)
(global-set-key (kbd "s-{") #'previous-buffer)

(prefer-coding-system 'windows-1251)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(unless (eq system-type 'windows-nt)
   (set-selection-coding-system 'utf-8))

(use-package server
  :config
  (unless (server-running-p) (server-start)))

(use-package diminish)

(use-package better-defaults
  :config
  (when window-system
    (menu-bar-mode)))

(use-package ido-vertical-mode
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

(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode))

(use-package magit
  :defer t
  :bind (("C-x v s" . magit-status)
         ("C-x v p" . magit-push))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package markdown-mode
  :mode (("\.markdown$" . markdown-mode)
         ("\.md$"       . markdown-mode))
  :config
  (progn
    (add-hook 'markdown-mode-hook #'visual-line-mode)))

(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook (lambda () (electric-indent-local-mode -1))))

(use-package mmm-mode
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

(use-package which-key
  :diminish which-key-mode
  :init
  (progn
    (which-key-setup-side-window-right)
    (which-key-mode)))

(use-package rust-mode)

(use-package guess-language         ; Automatically detect language for Flyspell
  :commands guess-language-mode
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-languages '(en ru)
        guess-language-min-paragraph-length 35)
  :diminish guess-language-mode)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package helpful
  :bind
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h C" . helpful-command)
  ("C-h F" . helpful-function)
  (:map emacs-lisp-mode-map
        ("C-c C-d" . helpful-at-point)))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package neotree
  :bind ("<f8>" . neotree-toggle))
   (setq projectile-switch-project-action 'neotree-projectile-action)

(use-package ansible
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  (setq ansible::vault-password-file "~/.vault_pass"))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package git-gutter
  :diminish 'git-gutter-mode
  :config
  (global-git-gutter-mode +1)
  (custom-set-variables
   '(git-gutter:window-width 2)
   '(git-gutter:modified-sign "☁")
   '(git-gutter:added-sign "☀")
   '(git-gutter:deleted-sign "☂")))

(use-package clang-format
  :config
  (global-set-key (kbd "C-c i") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer)

  (setq clang-format-style-option "llvm"))

(use-package ivy
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package swiper
  :init
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :init
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package yasnippet
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
	      ("C-x i i" . yas-insert-snippet)
	      ("C-x i n" . yas-new-snippet)
	      ("C-x i v" . yas-visit-snippet-file)
	      ("C-x i g" . yas-reload-all))
  :init
  (use-package yasnippet-snippets)
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  (yas-global-mode 1))

(use-package company
  :diminish company-mode
  :demand t
  :bind (("C-c /" . company-files)
         ("C-M-i" . company-complete)
         :map company-active-map
         ("C-s" . company-filter-candidates)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-i" . company-complete-selection)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (global-company-mode +1)
  (setq company-dabbrev-downcase nil)
  (custom-set-variables
   '(company-minimum-prefix-length 3)
   '(company-idle-delay 0)
   '(company-selection-wrap-around t)
   '(company-dabbrev-downcase nil)
   '(company-dabbrev-ignore-case 'nil))
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet)))))

;; backends for company
(use-package company-shell
  :config
  (add-to-list 'company-backends 'company-shell))
(use-package company-web
  :config
  (add-to-list 'company-backends 'company-web-html))
(use-package company-quickhelp
  :bind
  (:map company-active-map
        ("C-c h" . company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode))

(use-package hippie-exp
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))
