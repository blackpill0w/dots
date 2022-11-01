(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(setq scroll-step 1)
(set-window-scroll-bars (minibuffer-window) nil nil)

(setq custom-file (concat user-emacs-directory "custom.el"))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup/"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(setq gc-cons-threshold (expt 2 24))

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

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
(global-set-key (kbd "C-t") (lambda() (interactive) (term "/usr/bin/bash")))

;;; Hide useless buffers when cycling through buffers
(defvar buffers-not-to-ignore '("*shell*" "*ielm*" "*eww*" "*terminal*" "*ansi-term*" "*eshell*"))
(set-frame-parameter (selected-frame) 'buffer-predicate
                     (lambda (buf) (or
                                    (member (buffer-name buf) buffers-not-to-ignore)
                                    (not (string-match-p "^*" (buffer-name buf)))
                                    )))

;;; Remove trailing spaces on save
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;;; Whitespace
(global-whitespace-mode 1)
(setq whitespace-line-column 5000)

;;; Font
(add-to-list 'default-frame-alist
             '(font . "iosevka term-13"))

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

;;; Use spaces instead of tabs

; Stroustrup style without namespace indentation
(c-add-style "modified-stroustrup"
             '("stroustrup"
               (c-basic-offset . 3)
               (tab-width . 3)
               (c-offsets-alist
                (innamespace . 0)
                )))

(setq-default indent-tabs-mode nil
              c-basic-offset 3
              tab-width 3
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
(use-package yafolding
  :ensure t
  :config (yafolding-mode t))
(global-set-key (kbd "C-, C-s")  'yafolding-show-parent-element)
(global-set-key (kbd "C-, C-h")  'yafolding-hide-parent-element)
(global-set-key (kbd "C-, s")  'yafolding-show-element)
(global-set-key (kbd "C-, h")  'yafolding-hide-element)
(global-set-key (kbd "C-, C-S-s")  'yafolding-show-all)
(global-set-key (kbd "C-, C-S-h")  'yafolding-hide-all)

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

(load-theme 'outrun t)

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
  :config (define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
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
  :config (drag-stuff-global-mode 1)
  )

;;; Company
(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode)
  :config (setq company-minimum-prefix-length 2)
)

;;; LSP
(use-package lsp-mode
  :ensure t)
(use-package lsp-pyright
  :ensure t)

;;; CMake
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt"  . cmake-mode))

;;; Meson
(use-package meson-mode
  :ensure t
  :mode ("meson.build"  . meson-mode))

;;; Lex
(use-package bison-mode
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
