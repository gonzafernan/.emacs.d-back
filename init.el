(scroll-bar-mode -1) ; scroll bar disabled
(tool-bar-mode -1)   ; tool bar disabled
(menu-bar-mode -1)   ; menu bar disabled

;; Load editor theme
(load-theme 'wombat)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

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

;; ivy (https://github.com/abo-abo/swiper)
;; A generic completion frontend for emacs
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))
