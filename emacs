(require 'cl)
(setq ring-bell-function 'ignore)

;; packages
(require 'package)
(add-to-list 'package-archives 
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings starter-kit-ruby
                                  solarized-theme clojure-mode fuzzy auto-indent-mode
                                  clojure-test-mode markdown-mode yaml-mode paredit marmalade
                                  slime nrepl guru-mode ruby-block
                                  ruby-end ruby-tools ack-and-a-half rainbow-delimiters)

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(set-default-font "Monaco 14")

(load-theme 'solarized-dark t)
(server-start)

(setq mac-option-modifier 'control)
(setq mac-command-modifier 'meta)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; shorter aliases for ack-and-a-half commands
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; nrepl 
(add-hook 'clojure-mode-hook
          'nrepl-interaction-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;; guru-mode
(require 'guru-mode)

(setq default-frame-alist (append (list 
                                   '(width  . 181)  ; Width set to 81 characters 
                                   '(height . 50)) ; Height set to 60 lines 
                                  default-frame-alist))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(make-backup-files nil)
  '(auto-save-default nil)
  '(default-input-method "russian-typewriter")
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
