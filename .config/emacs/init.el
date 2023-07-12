(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq gc-cons-threshold (expt 2 24))
(setq ring-bell-function 'ignore)

;;; UI tweaks
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(setq scroll-step 1)
(set-window-scroll-bars (minibuffer-window) 0 'none)
;;; Custom variables file
(setq-default custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)

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

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (set-window-scroll-bars
               (minibuffer-window frame) 0 nil 0 nil t)
              (set-window-fringes
               (minibuffer-window frame) 0 0 nil t))))

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
(load-file (concat user-emacs-directory "other/whitespace4r.el"))
(add-hook 'prog-mode-hook #'whitespace4r-mode)

;;; Font
(add-to-list 'default-frame-alist
             '(font . "iosevka 13"))

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

;;; Cua mode
(cua-mode t)

;;; Set file's name as title
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;;; Line numbers
(setq column-number-mode t)
(setq global-line-number-mode t)
(global-display-line-numbers-mode)

;;; Comment/uncomment
(global-set-key (kbd "C-S-/")  'comment-or-uncomment-region)
(global-set-key (kbd "C-S-:")  'comment-or-uncomment-region)

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

;;; Folding
(load-file (concat user-emacs-directory "other/hideshowvis.el"))
(add-hook 'prog-mode-hook #'hideshowvis-minor-mode)

;(use-package yafolding
;  :ensure t
;  :config (yafolding-mode t))
;(global-set-key (kbd "C-, C-s")  'yafolding-show-parent-element)
;(global-set-key (kbd "C-, C-h")  'yafolding-hide-parent-element)
;(global-set-key (kbd "C-, s")  'yafolding-show-element)
;(global-set-key (kbd "C-, h")  'yafolding-hide-element)
;(global-set-key (kbd "C-, C-S-s")  'yafolding-show-all)
;(global-set-key (kbd "C-, C-S-h")  'yafolding-hide-all)

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

;;; Multiple cursors
(global-unset-key (kbd "C-<mouse-1>"))
(use-package multiple-cursors
  :ensure t
  :config (define-key mc/keymap (kbd "<return>") nil)
  :bind (
      ("C->" . mc/mark-previous-like-this)
      ("C-<" . mc/mark-next-like-this)
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
(use-package all-the-icons
  :ensure t)

;;; Side tree
(use-package treemacs
  :ensure t
  :bind ("C-b" . treemacs)
  :config
  (progn
    (setq treemacs-show-cursor nil)
    )
  )

(use-package treemacs-all-the-icons
  :ensure t
  :config
  (treemacs-load-theme "all-the-icons"))

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

;;; LSP
(use-package lsp-mode
  :ensure t
  :hook (c++-mode . lsp)
  :config (setq lsp-clients-clangd-args '("--header-insertion=never"))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++20")))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++20"))))
(use-package lsp-pyright
  :ensure t)

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

;;; Haskell
(use-package haskell-mode
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

;;; Web mode
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode)))

;;; Other
(global-set-key (kbd "C-, C-e")  'flymake-show-buffer-diagnostics)

(use-package tree-sitter
  :ensure t)
(setq font-lock-maximum-decoration 2)

(use-package clang-format
  :ensure t
  :config (defun set-clang-format-keybinding ()
            (local-set-key (kbd "C-, f") 'clang-format-buffer))
  (add-hook 'c-mode-hook 'set-clang-format-keybinding)
  (add-hook 'c++-mode-hook 'set-clang-format-keybinding)
  )
