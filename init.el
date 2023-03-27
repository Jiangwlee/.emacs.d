(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "69ecb7a75a0a8440df4b9ffe28b46dadf849b499c7e10680c26b99a84df773ca" "39f0ac86b012062fed46469cc5ea1b00aa534db587ad21d55a9717a1bac99a27" "b54bf2fa7c33a63a009f249958312c73ec5b368b1094e18e5953adb95ad2ec3a" default))
 '(global-company-mode t)
 '(global-linum-mode t)
 '(ivy-mode t)
 '(org-agenda-files '("~/Git/org-mode/agenda.org"))
 '(package-selected-packages
   '(org which-key use-package color-theme-modern org ace-window spaceline-all-the-icons spaceline counsel swiper ace-jump-mode ivy company projectile neotree magit dracula-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;;; Formatting

;; Linenum
(setq linum-format "%4d \u2502 ")

;; Show matching parens
(show-paren-mode 1)

;; Indentation
(setq-default tab-width 2
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Word wrapping
(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil)

;;; Disable auto backup
(setq make-backup-files nil)

;; Color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t)

;; Evil configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (evil-mode 1)
  :bind (:map evil-normal-state-map
	      ("SPC" . ace-jump-mode)))

;; NeoTree
(use-package neotree
  :ensure t
  :bind
  ("C-x n" . 'neotree-toggle)
 )

;; Ivy mode
(use-package ivy
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq ivy-use-virtual-buffers t)  ;; add 'recentf-mode' and bookmarks to 'ivy-switch-buffer'
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 15) ;; number of result line to display
  (setq ivy-fixed-height-minibuffer t)
  ;; Enable ivy mode
  (ivy-mode 1)
  :bind
  ("C-c C-r" . 'ivy-resume)
  ("C-x b" . 'ivy-switch-buffer)
  ("C-x C-b" . 'ivy-switch-buffer)
  )

;; Swiper 
(use-package swiper
  :ensure t
  :bind
  ("C-s" . 'swiper)
  )

;; Counsel
(use-package counsel
  :ensure t
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-x C-f" . 'counsel-find-file)
  )

;; projectile
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (progn
    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action 'neotree-projectile-action)
    (projectile-mode))
  )

;; Company
(use-package company
  :ensure t
  :init
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (global-company-mode)
  )

;; Magit
(use-package magit
  :ensure t
  :bind
  ("C-c m" . 'magit-status)
  )

;; spaceline
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (spaceline-highlight-face-evil-state)
  )

;; ace-window
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . 'ace-window)
  )

;; org-mode
(use-package org
  :ensure t
  :config
  (setq org-todo-keywords '((sequence "TODO" "DOING" "DONE")))
  :bind
  ("C-c a" . 'org-agenda)
  ("C-c c" . 'org-capture)
  )

;;; which-key
(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.2)
  :config
  (which-key-mode))
