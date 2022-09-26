(setq custom-file (concat user-emacs-directory "custom.el"))
(setq backup-directory-alist (concat user-emacs-directory "backup"))

(setq gc-cons-threshold (expt 2 24))
(tool-bar-mode -1)

;;; C-w to close buffer
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'kill-current-buffer)

;;; Recently opened files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;; Cua mode
(cua-mode t)

;;; Line numbers
(global-linum-mode t)

;;; Add melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Install use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
       use-package-expand-minimally t))

;;; Themes
(use-package dracula-theme
  :ensure t)

(use-package gruber-darker-theme
  :ensure t)

(use-package gruvbox-theme
  :ensure t)

(load-theme 'gruber-darker t)

;;; Multiple cursors
(global-unset-key (kbd "C-<mouse-1>"))
(use-package multiple-cursors
  :ensure t
  :config (define-key mc/keymap (kbd "<return>") nil)
  :bind (
      ("C->" . mc/mark-previous-like-this)
      ("C-<" . mc/mark-next-like-this)
      ("C-<mouse-1>" . mc/add-cursor-on-click)
      )
  )

;;; Project manager
(use-package projectile
  :ensure t
  :config (define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
  )

;;; Icons
(use-package all-the-icons
  :ensure t)

;;; Side tree
(use-package neotree
  :ensure t
  :bind ("C-b" . neotree-toggle)
  :config (setq neo-theme (if (display-graphic-p) 'icons))
  )

;;; Move lines up/down
(use-package drag-stuff
  :ensure t
  :config (drag-stuff-define-keys)
  :config (drag-stuff-global-mode 1)
  )
