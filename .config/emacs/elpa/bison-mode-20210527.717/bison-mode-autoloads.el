;;; bison-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from bison-mode.el

(add-to-list 'auto-mode-alist '("\\.y\\'" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.l\\'" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.jison\\'" . jison-mode))
(autoload 'bison-mode "bison-mode" "\
Major mode for editing bison/yacc files.

(fn)" t)
(autoload 'jison-mode "bison-mode" "\
Major mode for editing jison files.

(fn)" t)
(autoload 'flex-mode "bison-mode" "\
Major mode for editing flex files. (bison-mode by any other name)

(fn)" t)
(register-definition-prefixes "bison-mode" '("bison-"))

;;; End of scraped data

(provide 'bison-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; bison-mode-autoloads.el ends here
