(setq user-full-name "Denis Evsyukov"
      user-mail-address "denis@evsyukov.org")

(require 'cl)

(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar juev/packages '(auto-complete
			coffee-mode
			flycheck
			htmlize
			magit
			markdown-mode
			org
			solarized-theme
			yaml-mode
			js2-mode
			yasnippet)
  "Default packages")

(defun juev/packages-installed-p ()
  (loop for pkg in juev/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (juev/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg juev/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(setq default-input-method "russian-computer")

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq sentence-end-double-space nil)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(setq column-number-mode t)

(require 'auto-complete-config)
(ac-config-default)

(require 'yasnippet)
(yas-global-mode 1)

(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'wombat t))

;;(set-default-font "Inconsolata 14")
(electric-pair-mode)

(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
