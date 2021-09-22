
(scroll-bar-mode -1) ; scroll bar disabled
(tool-bar-mode -1)   ; tool bar disabled
(menu-bar-mode -1)   ; menu bar disabled

;; Line wrap
(visual-line-mode t)

;; Line numbers enabled
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for the following modes
(dolist (mode '(org-mode-hook
		doc-view-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; use-package (https://github.com/jwiegley/use-package)
;; The use-package macro allows you to isolate package configuration
;; in your .emacs file in a way that is both performance-oriented and,
;; well, tidy.

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; doom-themes (https://github.com/hlissner/emacs-doom-themes)
;; A theme megapack for GNU Emacs, inspired by community favorites.
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t))

;; doom-modeline (https://github.com/seagle0128/doom-modeline)
;; A fancy and fast mode-line inspired by minimalism design.  
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Solve icons not showing in doom-modeline
(use-package all-the-icons)

;; ivy (https://github.com/abo-abo/swiper)
;; A generic completion frontend for emacs
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

;; counsel (https://github.com/abo-abo/swiper)
;; A collection of ivy-enhanced versions of common emacs commands.
(use-package counsel)

;; ivy-rich (https://github.com/Yevgnen/ivy-rich)
;; Rich transformers for commands from ivy and counsel
(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

;; which-key (https://github.com/justbur/emacs-which-key)
;; displays the key bindings following your currently entered incomplete
;; command (a prefix) in a popup
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05))

;; org-mode
(use-package org
  :config
  (setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))))

;; rainbow-delimiters (https://github.com/Fanael/rainbow-delimiters)
;; Highlights delimiters such as parentheses, brackets or braces
;; according to their depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Key bindings
;; Global key bindings
;; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(doom-modeline-mode t)
 '(global-display-line-numbers-mode t)
 '(org-agenda-files
   '("~/Documents/Inglés B1+B2/AgendaIngles.org" "~/Documents/InternacionalUNCUYO/NotasConvocatoria.org"))
 '(package-selected-packages
   '(doom-themes doom-modeline counsel ivy-rich which-key rainbow-delimiters use-package ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
