(setq user-full-name "Denis Evsyukov"
      user-mail-address "denis@evsyukov.org")

(require 'cl)

(load "package")
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-archive-enable-alist '(("melpa" deft magit)))

(defvar juev/packages '(auto-complete
			autopair
                        better-defaults
			coffee-mode
                        dash
                        dash-functional
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

(setq sentence-end-double-space nil)

(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'yasnippet)
(yas-global-mode 1)

(if window-system
    (load-theme 'solarized-light t)
  (load-theme 'wombat t))

(when (eq system-type 'darwin)
  (set-default-font "Inconsolata 15")
  (setq mac-command-modifier 'meta)
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  )

(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

(eval-after-load "dash" '(dash-enable-font-lock))
