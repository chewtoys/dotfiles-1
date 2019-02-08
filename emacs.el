(load "~/.emacs-proxy.el" t)

(setq custom-file "~/.emacs-custom.el")
;; (load custom-file t)

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

(setq vc-follow-link t)
(setq vc-follow-symlinks t)

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
    (require 'whitespace)
    (global-whitespace-mode +1)
    (set-face-attribute 'whitespace-space nil :background nil :foreground "gray80")
    (set-face-attribute 'whitespace-trailing nil :background "plum1" :foreground "gray80")
    (setq whitespace-style '(face tabs spaces tabs-mark space-mark trailing))
    (set-frame-size (selected-frame) 190 60)
    (set-default-font "Fira Code 14" nil t))

(if (eq system-type 'windows-nt)
         (set-default-font "Fira Code 12" nil t))

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

(require 'recentf)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \\[find-file] a recent file"
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

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
(global-set-key (kbd "s-w") #'kill-ring-save)
(global-set-key (kbd "s-g") #'magit-status)

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-separator "/"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*"
      uniquify-min-dir-content 20)

;; (setq quelpa-self-upgrade-p nil)
;; (setq quelpa-update-melpa-p nil)
;; (use-package quelpa :ensure t)

;; (use-package quelpa-use-package :ensure t)
;; ;; (setq use-package-ensure-function 'quelpa)
;; ;; (setq use-package-always-ensure t)

(use-package server
  :config
  (unless (server-running-p) (server-start)))

(use-package diminish :ensure t)

(use-package exec-path-from-shell :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package better-defaults
  :ensure t
  :config
  (when window-system
    (menu-bar-mode)))

;; (use-package ido-vertical-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   (progn
;;     (ido-mode t)
;;     (ido-vertical-mode t))
;;   :config
;;   (progn
;;     (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "Async Shell Command" "*Ibuffer*"))
;;     (setq ido-enable-flex-matching t
;;           ido-everywhere t)))

(use-package rainbow-delimiters
  :ensure t
  :hook
  ((clojure-mode . rainbow-delimiters-mode)
   (prog-mode . rainbow-delimiters-mode)))

(use-package projectile
  :ensure t
  ;; :diminish projectile-mode
  :config
  (projectile-global-mode))

(defun juev/magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers.
Attribution: URL `https://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/'"
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x v s" . magit-status)
         ("C-x v p" . magit-push))
  :init
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-bury-buffer-function (lambda(&optional kill-buffer) (interactive) (magit-restore-window-configuration t)))
    (setq magit-commit-show-diff nil
          magit-revert-buffers 1))
  :config
  (progn
    (bind-key "q" #'juev/magit-kill-buffers magit-status-mode-map)
    (add-to-list 'magit-no-confirm 'stage-all-changes)))

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

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (progn
    (which-key-setup-side-window-right)
    (which-key-mode)))

(use-package rust-mode :ensure t)

(use-package guess-language         ; Automatically detect language for Flyspell
  :ensure t
  :commands guess-language-mode
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-languages '(en ru)
        guess-language-min-paragraph-length 35)
  :diminish guess-language-mode)

(use-package helpful
  :ensure t
  :bind
  ("C-h k" . helpful-key)
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h C" . helpful-command)
  ("C-h F" . helpful-function)
  (:map emacs-lisp-mode-map
        ("C-c C-d" . helpful-at-point)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; (use-package neotree
;;   :ensure t
;;   :bind ("<f8>" . neotree-toggle))
;;    (setq projectile-switch-project-action 'neotree-projectile-action)

(use-package ansible
  :ensure t
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  (setq ansible::vault-password-file "~/.vault_pass"))

(use-package git-gutter
  :ensure t
  :diminish 'git-gutter-mode
  :config
  (global-git-gutter-mode +1))

(use-package clang-format
  :ensure t
  :config
  (global-set-key (kbd "C-c i") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer)

  (setq clang-format-style-option "llvm"))

(use-package yasnippet
  :ensure t
  ;; :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-x i i" . yas-insert-snippet)
              ("C-x i n" . yas-new-snippet)
              ("C-x i v" . yas-visit-snippet-file)
              ("C-x i g" . yas-reload-all))
  :init
  (use-package yasnippet-snippets :ensure t)
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
  (yas-global-mode 1))

(use-package company
  :ensure t
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
  :ensure t
  :config
  (add-to-list 'company-backends 'company-shell))
(use-package company-web
  :ensure t
  :config
  (add-to-list 'company-backends 'company-web-html))
(use-package company-quickhelp
  :ensure t
  :bind
  (:map company-active-map
        ("C-c h" . company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode))

(use-package hippie-exp
  :ensure t
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material-light t))
  ;; (set-face-attribute 'mode-line nil :foreground "ivory" :background "DarkOrange2"))

(use-package neotree :ensure t
  :bind ("C-c C-n" . neotree-toggle)
  :config
  (progn
    (setq neo-smart-open t)
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)))

(use-package flycheck :ensure t)

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun go/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-capf
           company-yasnippet))
    (company-mode)

    (use-package company-go
      :ensure t
      :after company
      :init
      (push 'company-go company-backends)
      )
    )

  (defun go/setup-env-var (&optional gopath)
    (unless gopath (setq gopath (concat (getenv "HOME") "/go")))
    (setenv "GOPATH" gopath)
    (setenv "PATH" (concat (getenv "PATH") ":" (concat gopath "/bin")))
    (setq exec-path (append exec-path (list (concat gopath "/bin"))))
    )
  :commands (gofmt-before-save)
  :hook
  ((go-mode . hs-minor-mode)
   (go-mode . flycheck-mode)
   (go-mode . go/init-company)
   (go-mode . go/setup-env-var)
   (go-mode . (lambda ()
                (add-hook 'before-save-hook 'gofmt-before-save)))
   )
  :bind
  (:map go-mode-map
        ("C-c g a" . go-imports-insert-import)
        ("C-c g p" . go-direx-pop-to-buffer)
        ("C-c g b" . go-direx-switch-to-buffer)
        ("C-c g i" . go-impl)
        ("C-c g f" . go-fill-struct)
        ("C-c g r" . go-rename)
        ("C-c g l" . go-imports-reload-packages-list)
        ("C-c g t" . go-tag-add)
        ("C-c g v" . go-tag-remove)
        ("C-c t g" . go-gen-test-dwim)
        ("C-c t a" . go-gen-test-all)
        ("C-c t e" . go-gen-test-exported)
        ("C-c t f" . go-test-current-file)
        ("C-c t t" . go-test-current-test)
        ("C-c t p" . go-test-current-project)
        ("C-c t b" . go-test-current-benchmark)
        ("C-c t x" . go-run))
  )

(use-package go-snippets
  :ensure t
  :after yasnippets
  :config
  (go-snippets-initialize)
  )

(use-package go-dlv :ensure t)

(use-package go-gen-test :ensure t)

(use-package go-eldoc
  :ensure t
  :hook
  (go-mode . go-eldoc-setup)
  )

(use-package go-guru
  :ensure t
  :hook
  (go-mode . go-guru-hl-identifier-mode)
  )

(use-package go-playground :ensure t)

(use-package gorepl-mode
  :ensure t
  :hook
  (go-mode . gorepl-mode))

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

;; (use-package dired+
;;   :quelpa (dired+ :fetcher github :repo "emacsmirror/dired-plus")
;;   :config
;;   (diredp-toggle-find-file-reuse-dir 1))

(use-package dired
  :config
  (progn
    ;; brew install coreutils
    (if (executable-find "gls")
        (progn
          (setq insert-directory-program "gls")
          (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
      (setq dired-listing-switches "-ahlF"))
    (setq ls-lisp-dirs-first t)
    (setq dired-recursive-copies 'always)
    (setq dired-recursive-deletes 'always)
    (setq dired-ls-F-marks-symlinks t)
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))))

(use-package dired-x)

(use-package evil
  :ensure t
  :config
  (evil-mode))

(use-package evil-magit
  :ensure t
  :after magit
  :init
  (setq evil-magit-want-horizontal-movement nil))

(define-key evil-normal-state-map (kbd "q") 'magit-mode-bury-buffer)

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
(spaceline-emacs-theme))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  :config
  (progn
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
     "f" 'find-file
     "b" 'switch-to-buffer
     "k" 'kill-buffer)))

(use-package evil-mark-replace :ensure t)
(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode 1))

(use-package org-evil :ensure t)
(put 'dired-find-alternate-file 'disabled nil)

;; (use-package helm :ensure t
;;   :config
;;   (progn
;;     (global-set-key (kbd "M-x") #'helm-M-x)
;;     (global-set-key (kbd "s-x") 'helm-M-x)
;;     (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;;     (global-set-key (kbd "C-x C-f") #'helm-find-files)
;;     (helm-mode 1)))

;; (use-package helm-projectile :ensure t)
;; (use-package helm-rg :ensure t)

(use-package ag :ensure t)
(use-package helm-ag
  :ensure t
  :after (ag helm)
  :init (setq helm-ag-fuzzy-match t))
(use-package helm
    :ensure t
    :demand t
    :diminish helm-mode
    :init
      (require 'helm-config)
      ;; (use-package helm-c-yasnippet :ensure t)
      ;; (use-package helm-clojuredocs :ensure t)
      ;; (use-package helm-company :ensure t)
      ;; (use-package helm-core :ensure t) ; Is this ever needed on top of helm?
      ;; (use-package helm-css-scss :ensure t)
      ;; (use-package helm-dash :ensure t) ;; Offline docsets viewer. SET THIS UP!
      (use-package helm-descbinds :ensure t
        :config (helm-descbinds-mode))
      ;; (use-package helm-describe-modes :ensure t)
      ;; (use-package helm-dictionary :ensure t)
      ;; (use-package helm-dired-history :ensure t)
      ;; (use-package helm-dired-recent-dirs :ensure t)
      ;; (use-package helm-filesets :ensure t)
      ;; (use-package helm-firefox :ensure t)
      (use-package helm-flx :ensure t
        :defer t
        :init (setq helm-flx-for-helm-locate t)
        :config (helm-flx-mode))
      (use-package helm-flycheck :ensure t)
      ;; (use-package helm-flymake :ensure t)
      ;; (use-package helm-flyspell :ensure t)
      (use-package helm-fuzzier :ensure t :disabled
        :init (helm-fuzzier-mode))
      ;; (use-package helm-git :ensure t)
      ;; (use-package helm-git-grep :ensure t)
      ;; (use-package helm-gitignore :ensure t)
      ;; (use-package helm-google :ensure t)
      ;; (use-package helm-gtags :ensure t)
      ;; (use-package helm-ispell :ensure t)
      ;; (use-package helm-ls-git :ensure t)
      ;; (use-package helm-make :ensure t)
      (use-package helm-mode-manager :ensure t)
      (use-package helm-org-rifle :ensure t)
      ;; (use-package helm-project-persist :ensure t)
      (use-package helm-swoop :ensure t
        :bind (("C-c h M-S" . helm-multi-swoop)
               ("C-c h S"   . helm-multi-swoop-projectile)))
      (use-package helm-themes :ensure t)

      ;;; Global Keybindings

      ;; These must be set globally at startup since `helm-command-prefix-key'
      ;; can't be changed after `helm-config' is loaded.
      (global-set-key   (kbd "C-c h") 'helm-command-prefix)
      (global-set-key   (kbd "C-x h") 'helm-command-prefix)
      (global-unset-key (kbd "C-x c"))

      ;;; Helm Google Suggest Settings
      ;; Make helm-google-suggest prefer using curl
      (when (executable-find "curl")
        (setq helm-google-suggest-use-curl-p t))

      ;;; Replace grep with ack-grep
      (when (executable-find "ack-grep")
        (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color %e %p %f"
              helm-grep-default-recurse-command "ack-grep -H --no-group --no-color %e %p %f"))

      ;;; Other Settings
      (setq ; open helm buffer inside cur window, don't jump to whole other window
            helm-split-window-in-side-p           t
            ; move to beg/end of source when end/beg is reached
            helm-move-to-line-cycle-in-source     t
            ; scroll 8 lines other window using M-<next>/M-<prior>
            helm-scroll-amount                    8
            helm-ff-file-name-history-use-recentf t
            helm-ff-skip-boring-files             t
            helm-ff-search-library-in-sexp        t
            helm-echo-input-in-header-line        t
  ;          helm-exit-idle-delay                  0
            helm-M-x-fuzzy-match                  t
            helm-apropos-fuzzy-match              t
            helm-buffers-fuzzy-matching           nil
            helm-completion-in-region-fuzzy-match t
            helm-etags-fuzzy-match                t
            helm-ff-fuzzy-matching                t
            helm-file-cache-fuzzy-match           t
            helm-imenu-fuzzy-match                t
            helm-lisp-fuzzy-completion            t
            helm-locate-fuzzy-match               t
            helm-locate-library-fuzzy-match       t
            helm-mode-fuzzy-match                 t
            helm-recentf-fuzzy-match              t
            helm-semantic-fuzzy-match             t)

      ;; C-c h i settings
      (setq helm-semantic-fuzzy-match t
            helm-imenu-fuzzy-match    t)

      (defun spacemacs//helm-hide-minibuffer-maybe ()
        "Hide minibuffer in Helm session if we use the header line as input field."
        (when (with-helm-buffer helm-echo-input-in-header-line)
          (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
            (overlay-put ov 'window (selected-window))
            (overlay-put ov 'face
                         (let ((bg-color (face-background 'default nil)))
                           `(:background ,bg-color :foreground ,bg-color)))
            (setq-local cursor-type nil))))

      (add-hook 'helm-minibuffer-set-up-hook
                'spacemacs//helm-hide-minibuffer-maybe)

      (setq helm-locate-fuzzy-match t)
      (setq helm-apropos-fuzzy-match t)
      (setq helm-lisp-fuzzy-completion t)

      (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

      (helm-adaptive-mode)
      (helm-mode)
      (ido-mode -1)

    :config
      ;; Write $<FOO>/ in helm-find-files to expand any of the following folder
      ;; shortcuts (just like ~/)
      (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

    :bind (("C-x b"     . helm-mini)
           ("C-x C-b"   . helm-mini)
           ("C-h a"     . helm-apropos)
           ("M-y"       . helm-show-kill-ring) ; Tweak/remove if annoying
           ("M-x"       . helm-M-x)
           ("s-x"       . helm-M-x)
           ("C-x C-f"   . helm-find-files)
           ("C-x C-r"   . helm-recentf)
           ("C-c h o"   . helm-occur)
           ("C-c h s"   . helm-swoop)
           ("C-c h y"   . helm-yas-complete)
           ("C-c h Y"   . helm-yas-create-snippet-on-region)
           ("C-c h SPC" . helm-all-mark-rings)
           ("C-c h x"   . helm-register)
           ([f10] . helm-buffers-list)
           ([S-f10] . helm-recentf)
  ; I think I need to install something to use this:
  ;         ("C-c h M-:" . helm-eval-expression-with-eldoc)
           :map helm-command-map
                ("C-c h" . helm-execute-persistent-action)
           :map helm-map
                ;; rebind tab to run persistent action
                ("<tab>" . helm-execute-persistent-action)
                ;; Also rebind <tab> in terminals (i.e., the cryptic "C-i") to do the same
                ("C-i"   . helm-execute-persistent-action)
                ;; List actions using C-z
                ("C-z"   . helm-select-action)))
