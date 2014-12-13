;;; Установка имени пользователя
(setq
 user-full-name "Denis Evsyukov"
 user-mail-address "denis@evsyukov.org")

;;; Включаем поддержку package.el
(require 'cl-lib)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)

;;; Задаем основные переменные окружения
(setq
 debug-on-error t                  ; включаем отладку
 edebug-trace t      
 inhibit-splash-screen t           ; отключаем показ первой страницы
 initial-scratch-message nil       ; отключаем приветствие при запуске
 default-input-method "russian-computer" ; задаем русский как второй язык для ввода      
 visible-bell nil                  ; отключаем нотификацию
 ring-bell-function 'ignore      
 sentence-end-double-space nil     ; между предложениями один пробел      
 make-backup-files nil             ; отключаем бекапы
 savehist-file "~/.emacs.d/savehist" ; задаем парамтеры хранения истории
 history-length t
 history-delete-duplicates t
 savehist-save-minibuffer-history 1
 savehist-additional-variables
 '(kill-ring
   search-ring
   regexp-search-ring)
 x-select-enable-clipboard t       ; работа с буфером обмена      
 size-indication-mode t            ; Показывать в mode-line размер файла
 column-number-mode t              ; Показываем в mode-line позицию курсора      
 scroll-margin 10                  ; параметры скрола
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;;; Параметры по умолчанию
(setq-default
 indent-tabs-mode nil                   ; отключаем Tab
 vc-follow-symlinks nil                 ; не следуем за ссылками
 buffer-file-coding-system 'utf-8-unix) ; перевод строки по умолчанию в стиле unix

;;; Дополнительные параметры
(savehist-mode 1)                       ; включение истории 
(scroll-bar-mode -1)                    ; отключение экранных элементов
(tool-bar-mode -1)
(menu-bar-mode -1)
(delete-selection-mode t)               ; параметры работы с выделением
(transient-mark-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)       ; подтверждение по одному символу
(blink-cursor-mode -1)                  ; отключение моргания курсора

;;; UTF-8 по-умолчанию
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;;; Переопределеяем ряд клавиатурных комбинаций
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x g") 'magit-status)

;;; Задаем размер окна при запуске и шрифт
(when window-system
  (set-frame-size (selected-frame) 170 50)
  (set-default-font "Consolas 13"))

;;; Переопределяем для OSX ряд параметров
(when (eq system-type 'darwin)
  (set-default-font "Inconsolata 15")   ; шрифт по умолчанию
  (set-frame-size (selected-frame) 160 54) ; размер окна при запуске
  (menu-bar-mode t)                     ; включаем меню
  (setq mac-command-modifier 'meta)     ; переопределяем CMD на Meta
  (define-key global-map [home] 'beginning-of-line-text) ; Home в начало строки
  (define-key global-map [end] 'end-of-line) ; End в конец строки
  ;; (setq ns-use-srgb-colorspace nil)
  ) 

;;; Загрузка темы цвета
(load-theme 'misterioso t)

;;; Далее определяем ряд дополнительных пакетов
(use-package company
  :ensure t
  :init
  (progn
    (setq company-idle-delay 0)
    (add-hook 'prog-mode-hook 'company-mode)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (progn
    (yas-reload-all)
    (add-hook 'prog-mode-hook
              '(lambda ()
                 (yas-minor-mode)))))

(use-package flycheck)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$". markdown-mode))

(use-package js2-mode
  :ensure t
  :mode ("\\.js" . js2-mode)
  :init
  (progn
    (add-hook 'js2-mode-hook 'flycheck-mode)
    (add-hook 'js2-mode-hook 'company-mode)))

(use-package emmet-mode
  :init
  (progn
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)))

(use-package ido-vertical-mode
  :ensure t
  :init
  (progn
    (ido-mode t)
    (ido-vertical-mode 1)
    (setq ido-enable-flex-matching t
          ido-use-virtual-buffers t)))

(use-package guide-key
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-x" "C-c"))
    (setq guide-key/recursive-key-sequence-flag t)
    (guide-key-mode 1)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (progn
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'json-mode-hook 'enable-paredit-mode)))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (progn
    (bind-keys
     :map json-mode-map
     ("{" . paredit-open-curly)
     ("}" . paredit-close-curly))))
