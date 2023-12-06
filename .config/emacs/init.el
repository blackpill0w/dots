(setq gc-cons-threshold (expt 2 24))

;;; Add melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; use-package
(if (version< emacs-version "29")
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  ())
;; use-package options

(setq use-package-always-ensure t
      use-package-expand-minimally t)

;;; Cua mode
(cua-mode t)

;;; UI tweaks
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq ring-bell-function 'ignore)

;;; Font
(add-to-list 'default-frame-alist
             '(font . "Iosevka Fixed ss07 12"))
(setq font-lock-maximum-decoration 2)

;;; Themes
;; Add themes directory
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;; Disable all previous themes before changing themes
(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(use-package dracula-theme
  :ensure t)
(use-package nord-theme
  :ensure t)

(load-theme 'electric-ice-darker t)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq scroll-step 1)
(set-window-scroll-bars (minibuffer-window) 0 'none)
;;; Custom variables file
(setq-default custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file))

(setq-default ocaml-stuff-file "/home/blackpill0w/.opam/default/share/emacs/site-lisp")
(if (file-exists-p ocaml-stuff-file)
    (add-to-list 'load-path ocaml-stuff-file))
(use-package ocp-indent
  :ensure t)

;;; backups
(defconst backup-dir
       (concat user-emacs-directory "backup/"))
(setq backup-directory-alist `(("." . ,backup-dir))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 10)

;;; Hide useless buffers when cycling through buffers
(defvar buffers-not-to-ignore '("*shell*" "*ielm*" "*eww*" "*terminal*" "*ansi-term*" "*eshell*"))
(defun ignore-useless-buffers ()
  (set-frame-parameter (selected-frame) 'buffer-predicate
                       (lambda (buf) (or (member (buffer-name buf) buffers-not-to-ignore)
                                         (not (string-match-p "^*" (buffer-name buf)))
                                         ))))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (ignore-useless-buffers))))
  (ignore-useless-buffers))

;;; Changing buffers
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)

;;; Rename file and buffer
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))
(global-set-key (kbd "C-x C-S-s") 'rename-file-and-buffer)

;;; Terminal
(global-set-key (kbd "C-t") (lambda() (interactive) (term "bash")))

;;; Remove trailing spaces on save
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;;; Whitespace
;;(global-whitespace-mode 1) TODO: whitespace only with selection
(setq whitespace-line-column 5000)
(setq-default whitespace4r-file (concat user-emacs-directory "other/whitespace4r.el"))
(if (file-exists-p whitespace4r-file)
    (progn
      (load-file whitespace4r-file)
      (add-hook 'prog-mode-hook #'whitespace4r-mode))
  )

;;; C-w to close buffer
(global-unset-key (kbd "C-w"))
(global-set-key (kbd "C-w") 'kill-current-buffer)

;;; Move arround split windows
(global-set-key (kbd "C-, <right>") 'windmove-right)
(global-set-key (kbd "C-, <left>")  'windmove-left)
(global-set-key (kbd "C-, <up>")    'windmove-up)
(global-set-key (kbd "C-, <down>")  'windmove-down)

;;; Recently opened files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;;; Remember history
(setq history-length 25)
(savehist-mode 1)

(save-place-mode 1)

; Stroustrup style without namespace indentation
(c-add-style "modified-stroustrup"
             '("stroustrup"
               (c-basic-offset . 2)
               (tab-width . 2)
               (c-offsets-alist
                (innamespace . 0)
                )))

(setq-default indent-tabs-mode nil
              c-basic-offset 2
              tab-width 2
              c-default-style "modified-stroustrup")

;;; Set file's name as title
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;;; Line numbers
(setq column-number-mode t)
(setq global-line-number-mode t)
(global-display-line-numbers-mode)

;;; Comment/uncomment
(global-set-key (kbd "C-S-/")  'comment-or-uncomment-region)
(global-set-key (kbd "C-S-:")  'comment-or-uncomment-region)

;;; Install use package
;(unless (package-installed-p 'use-package)
;  (package-refresh-contents)
;  (package-install 'use-package))
;(eval-and-compile

;;; Folding
(setq-default hideshowvis-file (concat user-emacs-directory "other/hideshowvis.el"))
;(if (file-exists-p hideshowvis-file)
;    (progn
;      (load-file hideshowvis-file)
;      (add-hook 'prog-mode-hook #'hideshowvis-minor-mode)))

(use-package yafolding
  :ensure t
  :config (yafolding-mode t)
  (global-set-key (kbd "C-, C-s")  'yafolding-show-parent-element)
  (global-set-key (kbd "C-, C-h")  'yafolding-hide-parent-element)
  (global-set-key (kbd "C-, s")  'yafolding-show-element)
  (global-set-key (kbd "C-, h")  'yafolding-hide-element)
  (global-set-key (kbd "C-, C-S-s")  'yafolding-show-all)
  (global-set-key (kbd "C-, C-S-h")  'yafolding-hide-all)
)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/Pictures/Random/bugs_bunny.jpg")
  (setq dashboard-center-content t)
  (setq dashboard-image-banner-max-height 350)
  (setq dashboard-image-banner-max-width 500)
  )

;;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-unset-key (kbd "C-<mouse-1>"))
  (define-key mc/keymap (kbd "<return>") nil)
  :bind (
         ("C-S-<up>" . mc/mark-previous-like-this)
         ("C-S-<down>" . mc/mark-next-like-this)
         ("C-M-<mouse-1>" . mc/add-cursor-on-click)
         )
  )

;;; Project manager
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
  (projectile-global-mode)
  )

;;; Icons
;(use-package all-the-icons
;  :ensure t)

;;; Side tree
;(use-package treemacs
;  :ensure t
;  :bind ("C-b" . treemacs)
;  :config
;  (progn
;    (setq treemacs-show-cursor nil)
;    )
;  )

;(use-package treemacs-all-the-icons
;  :ensure t
;  :config
;  (treemacs-load-theme "all-the-icons"))

;;; Move lines up/down
(use-package drag-stuff
  :ensure t
  :config (drag-stuff-define-keys)
  (drag-stuff-global-mode 1)
)

;;; Company
(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 2)
)

(use-package latex-preview-pane
  :ensure t)

;;; LSP
(use-package lsp-mode
  :ensure t
  :hook ;(c++-mode . lsp)
  (python-mode . lsp)
  :config (setq lsp-clients-clangd-args '("--header-insertion=never"))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++23")))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++23")))
  (setq lsp-haskell-server-path "haskell-language-server"))


(use-package lsp-pyright
  :ensure t)
(setq python-indent-offset 2)

(use-package haskell-mode
  :ensure t)

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package tuareg
  :ensure t)
(use-package ocp-indent
  :ensure t)
(add-to-list 'load-path "/home/blackpill0w/.opam/default/share/emacs/site-lisp")

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode))

(use-package rust-mode
  :ensure t
  :config (defun set-rustfmt-keybinding ()
            (local-set-key (kbd "C-, f") 'rust-format-buffer))
  (add-hook 'rust-mode-hook 'set-rustfmt-keybinding)
  (add-hook 'rust-mode-hook #'lsp))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" . cmake-mode))

(use-package meson-mode
  :ensure t
  :mode ("meson.build" . meson-mode))

(use-package bison-mode
  :ensure t)

(use-package lsp-java
   :ensure t)

;;; Automatically refreshes the buffer for changes outside of Emacs
(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;;; Other
(global-set-key (kbd "C-, C-e")  'flymake-show-buffer-diagnostics)

(use-package clang-format
  :ensure t
  :config (defun set-clang-format-keybinding ()
            (local-set-key (kbd "C-, f") 'clang-format-buffer))
  (add-hook 'c-mode-hook 'set-clang-format-keybinding)
  (add-hook 'c++-mode-hook 'set-clang-format-keybinding)
  )

(use-package smex
  :ensure t
  :config (global-set-key (kbd "M-x") 'smex))
(ido-mode 1)

(use-package nov
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Iosevka Fixed ss07"
                                           :height 1.0))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
)
