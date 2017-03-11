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
    (defun sensible-defaults/comment-or-uncomment-region-or-line ()
      "Comments or uncomments the region or the current line if there's no active region."
      (interactive)
      (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
          (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

    (defun sensible-defaults/reset-text-size ()
      (interactive)
      (text-scale-set 0))

    ;; Settings:
    (defun sensible-defaults/open-files-from-home-directory ()
      "When opening a file, start searching at the user's home directory."
      (setq default-directory "~/"))

    (defun sensible-defaults/increase-gc-threshold ()
      "Allow 20MB of memory (instead of 0.76MB) before calling garbage collection.
This means GC runs less often, which speeds up some operations."
      (setq gc-cons-threshold 20000000))

    (defun sensible-defaults/delete-trailing-whitespace ()
      "Call DELETE-TRAILING-WHITESPACE every time a buffer is saved."
      (add-hook 'before-save-hook 'delete-trailing-whitespace))

    (defun sensible-defaults/treat-camelcase-as-separate-words ()
      "Treat CamelCaseSubWords as separate words in every programming mode."
      (add-hook 'prog-mode-hook 'subword-mode))

    (defun sensible-defaults/automatically-follow-symlinks ()
      "When opening a file, always follow symlinks."
      (setq vc-follow-symlinks t))

    (defun sensible-defaults/make-scripts-executable ()
      "When saving a file that starts with `#!', make it executable."
      (add-hook 'after-save-hook
                'executable-make-buffer-file-executable-if-script-p))

    (defun sensible-defaults/single-space-after-periods ()
      "Don't assume that sentences should have two spaces after periods. This ain't a typewriter."
      (setq sentence-end-double-space nil))

    (defun sensible-defaults/offer-to-create-parent-directories-on-save ()
      "When saving a file in a directory that doesn't exist, offer to (recursively) create the file's parent directories."
      (add-hook 'before-save-hook
                (lambda ()
                  (when buffer-file-name
                    (let ((dir (file-name-directory buffer-file-name)))
                      (when (and (not (file-exists-p dir))
                                 (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                        (make-directory dir t)))))))

    (defun sensible-defaults/apply-changes-to-highlighted-region ()
      "Turn on transient-mark-mode."
      (transient-mark-mode t))

    (defun sensible-defaults/overwrite-selected-text ()
      "If some text is selected, and you type some text, delete the selected text and start inserting your typed text."
      (delete-selection-mode t))

    (defun sensible-defaults/ensure-that-files-end-with-newline ()
      "If you save a file that doesn't end with a newline, automatically append one."
      (setq require-final-newline t))

    (defun sensible-defaults/confirm-closing-emacs ()
      "Ask if you're sure that you want to close Emacs."
      (setq confirm-kill-emacs 'y-or-n-p))

    (defun sensible-defaults/quiet-startup ()
      "Don't present the usual startup message, and clear the scratch buffer."
      (setq inhibit-startup-message t)
      (setq initial-scratch-message nil))

    (defun sensible-defaults/make-dired-file-sizes-human-readable ()
      "Add file sizes in human-readable units (KB, MB, etc) to dired buffers."
      (setq-default dired-listing-switches "-alh"))

    (defun sensible-defaults/shorten-yes-or-no ()
      "Don't ask `yes/no?', ask `y/n?'."
      (fset 'yes-or-no-p 'y-or-n-p))

    (defun sensible-defaults/always-highlight-code ()
      "Turn on syntax highlighting whenever possible."
      (global-font-lock-mode t))

    (defun sensible-defaults/refresh-buffers-when-files-change ()
      "When something changes a file, automatically refresh the buffer containing that file so they can't get out of sync."
      (global-auto-revert-mode t))

    (defun sensible-defaults/show-matching-parens ()
      "Visually indicate matching pairs of parentheses."
      (show-paren-mode t)
      (setq show-paren-delay 0.0))

    (defun sensible-defaults/flash-screen-instead-of-ringing-bell ()
      "When you perform a problematic operation, flash the screen instead of ringing the terminal bell."
      (setq visible-bell t))

    (defun sensible-defaults/set-default-line-length-to (line-length)
      "Set the default line length to LINE-LENGTH."
      (setq-default fill-column line-length))

    (defun sensible-defaults/open-clicked-files-in-same-frame-on-mac ()
      "When you double-click on a file in the Mac Finder open it as a buffer in the existing Emacs frame, rather than creating a new frame just for that file."
      (setq ns-pop-up-frames nil))

    (defun sensible-defaults/yank-to-point-on-mouse-click ()
      "When middle-clicking the mouse to yank from the clipboard, insert the text where point is, not where the mouse cursor is."
      (setq mouse-yank-at-point t))

    (defun sensible-defaults/use-all-settings ()
      "Use all of the sensible-defaults settings."
      (sensible-defaults/open-files-from-home-directory)
      (sensible-defaults/increase-gc-threshold)
      (sensible-defaults/delete-trailing-whitespace)
      (sensible-defaults/treat-camelcase-as-separate-words)
      (sensible-defaults/automatically-follow-symlinks)
      (sensible-defaults/make-scripts-executable)
      (sensible-defaults/single-space-after-periods)
      (sensible-defaults/offer-to-create-parent-directories-on-save)
      (sensible-defaults/apply-changes-to-highlighted-region)
      (sensible-defaults/overwrite-selected-text)
      (sensible-defaults/ensure-that-files-end-with-newline)
      (sensible-defaults/confirm-closing-emacs)
      (sensible-defaults/quiet-startup)
      (sensible-defaults/make-dired-file-sizes-human-readable)
      (sensible-defaults/shorten-yes-or-no)
      (sensible-defaults/always-highlight-code)
      (sensible-defaults/refresh-buffers-when-files-change)
      (sensible-defaults/show-matching-parens)
      (sensible-defaults/flash-screen-instead-of-ringing-bell)
      (sensible-defaults/set-default-line-length-to 80)
      (sensible-defaults/open-clicked-files-in-same-frame-on-mac)
      (sensible-defaults/yank-to-point-on-mouse-click))

    ;; Keybindings:

    (defun sensible-defaults/bind-commenting-and-uncommenting ()
      "Comment or uncomment a region by hitting M-;."
      (global-set-key (kbd "M-;")
                      'sensible-defaults/comment-or-uncomment-region-or-line))

    (defun sensible-defaults/bind-home-and-end-keys ()
      "Make <home> and <end> move point to the beginning and end of the line, respectively."
      (global-set-key (kbd "<home>") 'move-beginning-of-line)
      (global-set-key (kbd "<end>") 'move-end-of-line))

    (defun sensible-defaults/bind-keys-to-change-text-size ()
      "Bind C-+ and C-- to increase and decrease text size, respectively."
      (define-key global-map (kbd "C-)") 'sensible-defaults/reset-text-size)
      (define-key global-map (kbd "C-+") 'text-scale-increase)
      (define-key global-map (kbd "C-=") 'text-scale-increase)
      (define-key global-map (kbd "C-_") 'text-scale-decrease)
      (define-key global-map (kbd "C--") 'text-scale-decrease))

    (defun sensible-defaults/use-all-keybindings ()
      "Use all of the sensible-defaults keybindings."
      (sensible-defaults/bind-commenting-and-uncommenting)
      (sensible-defaults/bind-home-and-end-keys)
      (sensible-defaults/bind-keys-to-change-text-size))

    (sensible-defaults/open-files-from-home-directory)
    (sensible-defaults/increase-gc-threshold)
    (sensible-defaults/delete-trailing-whitespace)
    (sensible-defaults/treat-camelcase-as-separate-words)
    (sensible-defaults/automatically-follow-symlinks)
    (sensible-defaults/make-scripts-executable)
    (sensible-defaults/single-space-after-periods)
    (sensible-defaults/offer-to-create-parent-directories-on-save)
    (sensible-defaults/apply-changes-to-highlighted-region)
    (sensible-defaults/overwrite-selected-text)
    (sensible-defaults/ensure-that-files-end-with-newline)
    (sensible-defaults/quiet-startup)
    (sensible-defaults/make-dired-file-sizes-human-readable)
    (sensible-defaults/shorten-yes-or-no)
    (sensible-defaults/always-highlight-code)
    (sensible-defaults/refresh-buffers-when-files-change)
    (sensible-defaults/show-matching-parens)
    (sensible-defaults/flash-screen-instead-of-ringing-bell)
    (sensible-defaults/set-default-line-length-to 80)
    (sensible-defaults/open-clicked-files-in-same-frame-on-mac)
    (sensible-defaults/yank-to-point-on-mouse-click)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode which-key use-package slime rust-mode restart-emacs rainbow-delimiters projectile paredit mmm-mode markdown-mode magit ledger-mode ido-vertical-mode guess-language go-rename go-eldoc go-autocomplete ghc flycheck-gometalinter exec-path-from-shell evil centered-cursor-mode better-defaults ac-anaconda))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
