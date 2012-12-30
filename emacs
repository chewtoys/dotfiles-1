(require 'cl)
(require 'package)
(add-to-list 'package-archives 
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages
  '(solarized-theme clojure-mode fuzzy auto-indent-mode yasnippet
                    clojure-test-mode markdown-mode yaml-mode marmalade
                    slime nrepl starter-kit starter-kit-lisp starter-kit-bindings
                    rainbow-delimiters exec-path-from-shell expand-region)
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'clojure-mode-hook
          'nrepl-interaction-mode)
(add-hook 'nrepl-mode-hook
          'rainbow-delimiters-mode)

(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)
(global-set-key (kbd "C-=") 'er/expand-region)

(server-start)

(if window-system
    (progn
      (set-default-font "Menlo 15")
      (set-frame-position (selected-frame) 180 100)
      (set-frame-size (selected-frame) 181 50)
      (load-theme 'solarized-dark t)
      )
  )

(require 'yasnippet) ;; not yasnippet-bundle
(setq yas/root-directory '("~/.emacs.d/elpa/yasnippet-0.8.0/snippets"
                           "~/.emacs.custom/snippets"))
(mapc 'yas/load-directory yas/root-directory)
(yas/global-mode t)

(custom-set-variables
 '(make-backup-files nil)
 '(auto-save-default nil)
 '(default-input-method "russian-typewriter")
 '(ring-bell-function 'ignore)
 '(global-auto-revert-mode t)
;; '(mac-command-modifier 'meta)
;; '(mac-option-modifier 'control)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
