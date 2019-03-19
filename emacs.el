(setq custom-proxy "~/.emacs-proxy.el")
(when (file-exists-p custom-proxy)
  (load custom-proxy t))

(setq custom-file "~/.emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file t))

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize nil)
(setq package-enable-at-startup nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-expand-minimally t)
(setq package-enable-at-startup nil)

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq message-log-max 16384)

(setq user-full-name "Denis Evsyukov"
      user-mail-address "denis@evsyukov.org")

(setq secrets-file "~/.emacs.secrets")
(when (file-exists-p secrets-file)
  (load secrets-file t))

(load "~/.emacs.secrets" t)

(setq inhibit-splash-screen t)
(setq inhbit-startup-message t)
(setq initial-scratch-message "")
(setq inhibit-startup-echo-area-message t)

(setq auto-window-vscroll nil)

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

(auto-compression-mode t)

(global-font-lock-mode t)

(blink-cursor-mode -1)

;; (electric-pair-mode t)

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
  (progn
    (require 'whitespace)
    (global-whitespace-mode +1)
    (set-face-attribute 'whitespace-space nil :background nil :foreground "dark slate gray")
    (set-face-attribute 'whitespace-trailing nil :background "tomato" :foreground "dark slate gray")
    (setq whitespace-style '(face tabs spaces tabs-mark space-mark trailing))
    (setq initial-frame-alist '( (tool-bar-lines . 0) (width . 160) (height . 60)))
    (setq deafult-frame-alist '( (tool-bar-lines . 0) (width . 160) (height . 60)))
    ;; https://github.com/adobe-fonts/source-code-pro
    (set-default-font "Fira Code 14" nil t)))

(if (eq system-type 'windows-nt)
    (set-default-font "Fira Code 12" nil t))

(defalias 'e 'find-file)
(defalias 'eo 'find-file-other-window)

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
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; enable recent files mode.
(recentf-mode t)

                                        ; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

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
(global-set-key (kbd "M-<up>") #'backward-paragraph)
(global-set-key (kbd "M-<down>") #'forward-paragraph)
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

(defconst juev/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))
(defun juev/emacs-subdirectory (d) (expand-file-name d juev/emacs-directory))

(defun juev/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;; (custom-set-variables
;;  '(auto-insert 'other)
;;  '(auto-insert-directory "~/autoinsert/")
;;  '(auto-insert-alist '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["template.h" c++-mode my/autoinsert-yas-expand])
;;                        (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . ["template.cc" my/autoinsert-yas-expand])
;;                        (("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
;;                        (("\\.el\\'" . "Emacs Lisp") . ["template.el" my/autoinsert-yas-expand])
;;                        (("\\.pl\\'" . "Perl script") . ["template.pl" my/autoinsert-yas-expand])
;;                        (("\\.pm\\'" . "Perl module") . ["template.pm" my/autoinsert-yas-expand])
;;                        (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand])
;;                        (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
;;                        (("\\.tex\\'" . "TeX/LaTeX") . ["template.tex" my/autoinsert-yas-expand]))))

(use-package autoinsert
  :init
  (progn
    (setq auto-insert-directory (juev/emacs-subdirectory "templates/"))
    ;; Don't want to be prompted before insertion:
    (setq auto-insert-query nil)
    (auto-insert-mode 1)
    (add-hook 'find-file-hook 'auto-insert))
  :config
  (progn
    (define-auto-insert "\\.py$" ["=template=.py" juev/autoinsert-yas-expand])
    (define-auto-insert "\\.sh$" ["default-sh.sh" juev/autoinsert-yas-expand])
    (define-auto-insert "\\.html?$" ["default-html.html" juev/autoinsert-yas-expand])))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (read-kbd-macro (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  ;; (after-load 'nxml-mode
              ;; (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-ˍ") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )

(use-package no-littering :ensure t)
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package server
  :config (or (server-running-p) (server-mode)))

(use-package diminish :ensure t)
(use-package bind-key :ensure t)

;; (use-package realgud :ensure t)
(use-package command-log-mode :ensure t)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package better-defaults :ensure t
  :config (when window-system
            (menu-bar-mode)))

(use-package ido-vertical-mode :ensure t
  :init
  (progn
    (ido-mode t)
    (ido-vertical-mode t))
  :config
  (progn
    (setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "Async Shell Command" "*Ibuffer*"))
    (setq ido-enable-flex-matching t
          ido-everywhere t)))

(use-package rainbow-delimiters :ensure t
  :hook
  ((clojure-mode . rainbow-delimiters-mode)
   (prog-mode . rainbow-delimiters-mode)))

(use-package projectile :ensure t
  ;; :diminish projectile-mode
  :config (projectile-global-mode))

(defun juev/magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers.
Attribution: URL `https://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/'"
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

;; (use-package git-commit :ensure t)

(use-package magit :ensure t
  :defer 5
  :bind (("C-x v s" . magit-status)
         ("C-x v p" . magit-push))
  :custom
  (magit-last-seen-setup-instructions "1.4.0")
  (magit-bury-buffer-function (lambda(&optional kill-buffer) (interactive) (magit-restore-window-configuration t)))
  (magit-commit-show-diff nil)
  (magit-revert-buffers 1)
  :config
  (progn
    (bind-key "q" #'juev/magit-kill-buffers magit-status-mode-map)
    (add-to-list 'magit-no-confirm 'stage-all-changes)))

(use-package magit-filenotify :ensure t
  :after magit)
(use-package magit-find-file :ensure t
  :after magit)

(use-package markdown-mode :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown"))

(use-package yaml-mode :ensure t
  :mode (("\\.yml$" . yaml-mode))
  :hook
  (yaml-mode-hook . (lambda ()
                      (electric-indent-local-mode -1))))

(use-package mmm-mode :ensure t
  :diminish mmm-mode
  :defer 5
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

(use-package which-key :ensure t
  :diminish which-key-mode
  :init
  (progn
    (which-key-setup-side-window-right)
    (which-key-mode)))

(use-package rust-mode :ensure t
  :mode "\\.rs\\'"
  :config
  (use-package flycheck-rust :ensure t
    :after flycheck
    :commands flycheck-rust-setup
    :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  (use-package cargo :ensure t
    :commands cargo-minor-mode
    :init (add-hook 'rust-mode-hook #'cargo-minor-mode))
  (use-package racer :ensure t
    :commands racer-mode
    :hook (rust-mode . racer-mode)
    :config (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)))

(use-package guess-language :ensure t
  :commands guess-language-mode
  :hook (text-mode-hook . guess-language-mode)
  :config
  (setq guess-language-languages '(en ru)
        guess-language-min-paragraph-length 35)
  :diminish guess-language-mode)

(use-package helpful :ensure t
  :bind (("C-h k" . helpful-key)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h C" . helpful-command)
         ("C-h F" . helpful-function))
  (:map emacs-lisp-mode-map
        ("C-c C-d" . helpful-at-point)))

(use-package ace-window :ensure t
  :bind (("M-o" . ace-window)))

(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region))

(use-package ansible :ensure t :defer t
  :hook
  (yaml-mode-hook . (lambda ()
                      (ansible 1)))
  :config (setq ansible::vault-password-file "~/.vault_pass"))

(use-package git-gutter :ensure t
  :diminish 'git-gutter-mode
  :config (global-git-gutter-mode +1))

(use-package clang-format :ensure t :defer t
  :bind (("C-c i" . clang-format-region)
         ("C-c u" . clang-format-buffer))
  :config (setq clang-format-style-option "google"))

(use-package yasnippet :ensure t
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode
  ;; :bind (:map yas-minor-mode-map
  ;;             ("C-x i i" . yas-insert-snippet)
  ;;             ("C-x i n" . yas-new-snippet)
  ;;             ("C-x i v" . yas-visit-snippet-file)
  ;;             ("C-x i g" . yas-reload-all))
  :init (use-package yasnippet-snippets :ensure t)
  :config
  (progn
    ;; (yas-reload-all)
    (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "snippets"))
    (yas-global-mode 1)))

(use-package company :ensure t
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
  (progn
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
                '(:with company-yasnippet))))))

;; backends for company
(use-package company-shell :ensure t
  :config (add-to-list 'company-backends 'company-shell))

(use-package company-web :ensure t
  :config (add-to-list 'company-backends 'company-web-html))

(use-package company-quickhelp :ensure t
  :bind (:map company-active-map
              ("C-c h" . company-quickhelp-manual-begin))
  :config (company-quickhelp-mode))

(use-package hippie-exp :ensure t)

(use-package neotree :ensure t
  :bind ("C-c C-n" . neotree-toggle)
  :config
  (progn
    (setq neo-smart-open t)
    (setq projectile-switch-project-action 'neotree-projectile-action)))

(use-package flycheck :ensure t)

(use-package go-mode :ensure t :defer t
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun go/init-company ()
    (set (make-local-variable 'company-backends)
         '(company-capf
           company-yasnippet))
    (company-mode)

    (use-package company-go :ensure t
      :after company
      :init
      (push 'company-go company-backends)
      ))

  (defun go/setup-env-var (&optional gopath)
    (unless gopath (setq gopath (concat (getenv "HOME") "/go")))
    (setenv "GOPATH" gopath)
    (setenv "PATH" (concat (getenv "PATH") ":" (concat gopath "/bin")))
    (setq exec-path (append exec-path (list (concat gopath "/bin")))))
  :commands (gofmt-before-save)
  :hook
  ((go-mode . hs-minor-mode)
   (go-mode . flycheck-mode)
   (go-mode . go/init-company)
   (go-mode . go/setup-env-var)
   (go-mode . (lambda ()
                (add-hook 'before-save-hook 'gofmt-before-save))))
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
        ("C-c t x" . go-run)))

;; (use-package go-snippets :ensure t :defer t
;;   :after yasnippets
;;   :config
;;   (go-snippets-initialize))

(use-package go-dlv :ensure t :defer t)

(use-package go-gen-test :ensure t :defer t)

(use-package go-eldoc :ensure t :defer t
  :hook (go-mode . go-eldoc-setup))

(use-package go-playground :ensure t :defer t)

(use-package gorepl-mode :ensure t :defer t
  :hook (go-mode . gorepl-mode))

(use-package protobuf-mode :ensure t :defer t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package spaceline :ensure t
  :config
  (progn
    (require 'spaceline-config)
    (spaceline-emacs-theme)))

(use-package vlf :ensure t
  :config
  (progn
    (require 'vlf-setup)
    (custom-set-variables
     '(vlf-application 'dont-ask))))

(use-package logview :ensure t
  :defer t)

(use-package log4j-mode :ensure t
  :disabled t
  :hook
  ((log4j-mode-hook . view-mode)
   (log4j-mode-hook . read-only-mode)))

(use-package view :ensure t
  :config
  (progn
    (defun View-goto-line-last (&optional line)
      "goto last line"
      (interactive "P")
      (goto-line (line-number-at-pos (point-max))))

    (define-key view-mode-map (kbd "e") 'View-scroll-half-page-forward)
    (define-key view-mode-map (kbd "u") 'View-scroll-half-page-backward)

    ;; less like
    (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
    (define-key view-mode-map (kbd "?") 'View-search-regexp-backward?)
    (define-key view-mode-map (kbd "g") 'View-goto-line)
    (define-key view-mode-map (kbd "G") 'View-goto-line-last)
    ;; vi/w3m like
    (define-key view-mode-map (kbd "h") 'backward-char)
    (define-key view-mode-map (kbd "j") 'next-line)
    (define-key view-mode-map (kbd "k") 'previous-line)
    (define-key view-mode-map (kbd "l") 'forward-char)))

(use-package dired-single :ensure t
  :config
  (progn
    (setq insert-directory-program "gls" dired-use-ls-dired t)
    (setq dired-listing-switches "-lXGh --group-directories-first")
    (setq dired-single-use-magic-buffer t)
    (defun my-dired-init ()
      "Bunch of stuff to run for dired, either immediately or when it's loaded."
      ;; <add other stuff here>
      (define-key dired-mode-map [return] 'dired-single-buffer)
      (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
      (define-key dired-mode-map "^"
        (function
         (lambda nil (interactive) (dired-single-buffer "..")))))
    (if (boundp 'dired-mode-map)
        (my-dired-init)
      (add-hook 'dired-load-hook 'my-dired-init))
    (setq-default dired-omit-files-p t) ; Buffer-local variable
    (setq dired-omit-files "^\\..*$")))

(use-package ivy :ensure t
  :config
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (use-package swiper :ensure t
      :bind (("C-S-s" . isearch-forward)            ;; Keep isearch-forward on Shift-Ctrl-s
             ("C-s" . swiper)                       ;; Use swiper for search and reverse search
             ("C-S-r" . isearch-backward)           ;; Keep isearch-backward on Shift-Ctrl-r
             ("C-r" . swiper)))
    (use-package counsel :ensure t)
    (use-package avy :ensure t
      :bind (("C-:" . avy-goto-char)))))

(use-package prescient :ensure t
  :defer t
  :config (prescient-persist-mode))
(use-package ivy-prescient :ensure t
  :after ivy
  :config (ivy-prescient-mode))
(use-package company-prescient :ensure t
  :after company
  :config (company-prescient-mode))

(use-package haskell-mode :ensure t
  :hook (haskell-mode . haskell-indentation-mode))

(use-package intero :ensure t
  :hook (haskell-mode . intero-mode))

(use-package material-theme :ensure t
  :init (load-theme 'material t))

;; (use-package solarized-theme :ensure t
;;   :init (load-theme 'solarized-light)
;;   :custom
;;   (solarized-distinct-fringe-background t)
;;   (solarized-use-variable-pitch nil)
;;   (solarized-high-contrast-mode-line t)
;;   (solarized-scale-org-headlines nil)
;;   (solarized-height-minus-1 1.0)
;;   (solarized-height-plus-1 1.0)
;;   (solarized-height-plus-2 1.0)
;;   (solarized-height-plus-3 1.0)
;;   (solarized-height-plus-4 1.0))

(use-package smartparens :ensure t
  :config
  (smartparens-global-mode))

(use-package ledger-mode :ensure t)

(defvar before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")
(message "Loading Emacs...done (%.3fs)"
         (float-time (time-subtract before-user-init-time
                                    before-init-time)))
