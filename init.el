;;; emacs --- configuration
;;; Commentary:
;;; Code:
;; User data
(setq user-full-name "Gonzalo G. Fernández"
      user-login-name "gonzafernan"
      user-mail-address "fernandez.gfg@gmail.com")

(scroll-bar-mode -1) ; scroll bar disabled
(tool-bar-mode -1)   ; tool bar disabled
(menu-bar-mode -1)   ; menu bar disabled

(global-visual-line-mode t) ; line wrapping
(global-hl-line-mode 1)     ; highlight current row

(set-face-attribute 'default nil :height 115) ; increase font size

;; start with scratch buffer
(setq inhibit-startup-message t)

;; Wind Move
;; Move point from window to window using Shift and the arrow keys.
(windmove-default-keybindings)

;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

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
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
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

;; Fix icons not showing in doom-modeline
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

;; helpful (https://github.com/Wilfred/helpful)
;; An alternative to the built-in Emacs help that provides much more
;; contextual information
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; magit
;; An interface to the version control system Git, implemented as an Emacs package.
;; magit manual: https://magit.vc/manual/magit.html
(use-package magit)

;; projectile (https://github.com/bbatsov/projectile)
;; A project interaction library for Emacs
(use-package projectile
  :diminish projectile-mode
  :pin melpa-stable
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :init
  (setq projectile-project-seach-path '("~/Documents/")))

;; counsel-projectile (https://github.com/ericdanan/counsel-projectile)
;; Provides further ivy integration into projectile by taking advantage of ivy's
;; support for selecting from a list of actions and applying an action without
;; leaving the completion session
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; evil (https://github.com/emacs-evil/evil)
;; An extensible vi layer for Emacs
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; org-mode (custom setup)
(use-package org
  :config
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-image-actual-width nil)
  ;; Make windmove work in Org mode
  :hook ((org-shiftup-final . windmove-up)
	 (org-shiftleft-final . windmove-left)
	 (org-shiftdown-final . windmove-down)
	 (org-shiftright-final . windmove-right)))

;; org-bullets (https://github.com/sabof/org-bullets)
;; utf-8 bullets for org-mode
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; visual-fill-column (https://github.com/joostkremers/visual-fill-column)
;; Wraps lines at "fill-column"
;; org-mode usage
(defun ggf/org-mode-visual-fill ()
  (setq visual-fill-column-width 140
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; org-roam (https://github.com/org-roam/org-roam)
;; A plain-text knowledge management system. It brings some of Roam's more
;; powerful features into the Org-mode ecosystem.
(use-package org-roam
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  (org-roam-dailies-directory "daily/")
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ; Ensure the keymap is avaiable
  (org-roam-db-autosync-mode))

(use-package visual-fill-column
  :hook (org-mode . ggf/org-mode-visual-fill))


;; rainbow-delimiters (https://github.com/Fanael/rainbow-delimiters)
;; Highlights delimiters such as parentheses, brackets or braces
;; according to their depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; flycheck (https://github.com/flycheck/flycheck)
;; A modern on-the-fly syntax checking extension for GNU Emacs.
(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-checkers '(emacs-lisp emacs-lisp-checkdoc ; emacs-lisp
		       tex-lacheck                    ; LaTeX
		       verilog-verilator))            ; verilog
  (flycheck-check-syntax-automatically '(save) "Chack automatically on save.")
  :config
  (setq flycheck-verilog-verilator-executable "C:/msys64/mingw64/bin/verilator_bin.exe")
  (setq flycheck-global-modes '(not org-mode))) ; Disable flycheck in org buffer

;;; AUCTeX LaTeX cutomization
(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable)) ; load automatically preview-pane onl atex file

(setq TeX-auto-save t)       ; enable parse on load
(setq TeX-parse-self t)      ; enable parse on save
(setq TeX-global-PDF-mode t) ; PDF mode (rather than DVI mode)

;;; verilog-mode Verilog development
(setq verilog-auto-newline nil)


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
   '(evil helpful latex-preview-pane counsel-projectile projectile flycheck magit org-roam visual-fill-column org-bullets doom-themes doom-modeline counsel ivy-rich which-key rainbow-delimiters use-package ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
