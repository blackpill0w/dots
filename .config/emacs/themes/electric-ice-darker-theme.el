;;; mayukai-dark-theme.el --- Mayukai-dark Theme

;;; Code:
(deftheme electric-ice-darker)
;;;; Configuration options:

(defgroup electric-ice-darker nil
  "Electric-Ice-Darker theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom electric-ice-darker-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'belectric-ice-darkerlean
  :group 'electric-ice-darker)

(defcustom electric-ice-darker-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'electric-ice-darker)

(defcustom electric-ice-darker-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'electric-ice-darker)

(defcustom electric-ice-darker-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'electric-ice-darker)

(defcustom electric-ice-darker-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'electric-ice-darker)

(defcustom electric-ice-darker-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'belectric-ice-darkerlean
  :group 'electric-ice-darker)

(defvar electric-ice-darker-use-24-bit-colors-on-256-colors-terms nil)

;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (electric-ice-darker-bg            "#000000" "unspecified-bg" "unspecified-bg") ; official background
                (electric-ice-darker-fg            "#ffffff" "#dadbc0"        "brightwhite") ; official foreground
                (electric-ice-darker-current       "#44474a" "#303030"        "brightblack") ; official current-line/selection
                (electric-ice-darker-comment       "#666666" "#5c6773"        "blue")        ; official comment
                (electric-ice-darker-cyan          "#7df4f8" "#7df4f8"        "cyan")  ; official cyan
                (electric-ice-darker-lightred      "#ffc0c0" "#ffc0c0"        "lightred")       ; official lightred
                (electric-ice-darker-lightred2     "#ff4040" "#ff4040"        "lightred")       ; official lightred
                (electric-ice-darker-pink          "#D86BFF" "#D86BFF"        "magenta")     ; official pink
                (electric-ice-darker-purple        "#D86BFF" "#A875FF"        "brightmagenta") ; official purple
                (electric-ice-darker-red           "#ff5555" "#ff8787"        "red")         ; official red
                (electric-ice-darker-yellow        "#ffff50" "#ffff87"        "yellow")      ; official yellow
                (electric-ice-darker-linenum       "#acacac" "#bbb"           "brightblack")
                (electric-ice-darker-grey          "#919191" "#919191"        "grey")
                (electric-ice-darker-darkgrey      "#404040" "#1b1b1b"        "darkgrey")
                ;; Other colors
                (bg2             "#0d1016" "#0d1016" "brightblack")
                (bg3             "#464752" "#262626" "brightblack")
                (bg4             "#565761" "#444444" "brightblack")
                (bg5             "#0c0d10" "#0c0d10" "brightblack")
                (fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
                (fg3             "#ccccc7" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                (other-blue      "#0189cc" "#0087ff" "brightblue")))
      (faces '(;; default / basic faces
               (cursor :background ,fg3)
               (default :background ,electric-ice-darker-bg :foreground ,electric-ice-darker-fg)
               (default-italic :slant italic)
               (error :foreground ,electric-ice-darker-red)
               (ffap :foreground ,fg4)
               (fringe :background ,electric-ice-darker-bg :foreground ,fg4)
               (header-line :inherit 'mode-line)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,bg5 :extend t)
               (info-quoted-name :foreground ,electric-ice-darker-lightred)
               (info-string :foreground ,electric-ice-darker-lightred)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,electric-ice-darker-cyan :underline t)
               (linum :foreground ,electric-ice-darker-darkgrey :background ,electric-ice-darker-bg)
               (line-number :foreground ,electric-ice-darker-darkgrey :background ,electric-ice-darker-bg)
               (line-number-current-line :foreground ,electric-ice-darker-linenum :background ,electric-ice-darker-bg)
               (nlinum-current-line :slant italic :foreground ,electric-ice-darker-linenum :background ,electric-ice-darker-bg)
               (match :background ,electric-ice-darker-yellow :foreground ,electric-ice-darker-bg)
               (menu :background ,electric-ice-darker-current :inverse-video nil
                     ,@(if electric-ice-darker-alternate-mode-line-and-minibuffer
                           (list :foreground fg3)
                         (list :foreground electric-ice-darker-fg)))
               (minibuffer-prompt
                ,@(if electric-ice-darker-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground electric-ice-darker-fg)
                    (list  :foreground electric-ice-darker-pink)))
               (mode-line :background ,electric-ice-darker-current
                          :box ,electric-ice-darker-current :inverse-video nil
                          ,@(if electric-ice-darker-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground electric-ice-darker-fg)))
               (mode-line-inactive
                :background ,electric-ice-darker-bg :inverse-video nil
                ,@(if electric-ice-darker-alternate-mode-line-and-minibuffer
                      (list :foreground electric-ice-darker-comment)
                    (list :foreground fg4 :box bg2)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :background ,electric-ice-darker-current)
               (shadow :foreground ,electric-ice-darker-comment)
               (success :foreground ,electric-ice-darker-lightred)
               (telectric-ice-darkerltip :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-current)
               (trailing-whitespace :background ,electric-ice-darker-lightred :foreground ,electric-ice-darker-current)
               (vertical-border :foreground ,bg2)
               (warning :foreground ,electric-ice-darker-lightred)
               ;; syntax / font-lock
               (font-lock-builtin-face :foreground ,electric-ice-darker-cyan)
               (font-lock-comment-face :foreground ,electric-ice-darker-comment)
               (font-lock-comment-delimiter-face :inherit shadow)
               (font-lock-constant-face :foreground ,electric-ice-darker-fg)
               (font-lock-doc-face :foreground ,electric-ice-darker-comment)
               (font-lock-function-name-face :foreground ,electric-ice-darker-fg :weight normal)
               (font-lock-keyword-face :foreground ,electric-ice-darker-cyan :weight normal)
               (font-lock-negation-char-face :foreground ,electric-ice-darker-cyan)
               (font-lock-preprocessor-face :foreground ,electric-ice-darker-cyan)
               (font-lock-reference-face :inherit font-lock-constant-face) ;; obsolete
               (font-lock-regexp-grouping-backslash :foreground ,electric-ice-darker-cyan)
               (font-lock-regexp-grouping-construct :foreground ,electric-ice-darker-purple)
               (font-lock-string-face :foreground ,electric-ice-darker-lightred)
               (font-lock-type-face :inherit font-lock-builtin-face :foreground ,electric-ice-darker-fg)
               (font-lock-variable-name-face :foreground ,electric-ice-darker-fg :weight normal)
               (font-lock-warning-face :inherit warning :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,electric-ice-darker-pink)
               ;; company
               (company-echo-common :foreground ,electric-ice-darker-bg :background ,electric-ice-darker-fg)
               (company-preview :background ,electric-ice-darker-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,electric-ice-darker-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,electric-ice-darker-lightred)
               (company-scrollbar-bg :background ,electric-ice-darker-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,electric-ice-darker-fg :background ,bg2)
               (company-tooltip-common :foreground ,electric-ice-darker-cyan)
               (company-tooltip-selection :background ,electric-ice-darker-current)
               (company-telectric-ice-darkerltip :inherit telectric-ice-darkerltip)
               (company-telectric-ice-darkerltip-search :foreground ,electric-ice-darker-lightred
                                           :underline t)
               (company-telectric-ice-darkerltip-search-selection :background ,electric-ice-darker-lightred
                                                     :foreground ,electric-ice-darker-bg)
               (company-telectric-ice-darkerltip-selection :inherit match)
               (company-telectric-ice-darkerltip-mouse :background ,electric-ice-darker-bg)
               (company-telectric-ice-darkerltip-common :foreground ,electric-ice-darker-pink)
               ;;(company-telectric-ice-darkerltip-common-selection :inherit company-telectric-ice-darkerltip-common)
               (company-telectric-ice-darkerltip-annotation :foreground ,electric-ice-darker-cyan)
               ;;(company-telectric-ice-darkerltip-annotation-selection :inherit company-telectric-ice-darkerltip-annotation)
               ;; completions (minibuffer.el)
               (completions-annotations :inherit font-lock-comment-face)
               (completions-common-part :foreground ,electric-ice-darker-pink)
               (completions-first-difference :foreground ,electric-ice-darker-fg)
               ;; diff-hl
               (diff-hl-change :foreground ,electric-ice-darker-lightred :background ,electric-ice-darker-lightred)
               (diff-hl-delete :foreground ,electric-ice-darker-red :background ,electric-ice-darker-red)
               (diff-hl-insert :foreground ,electric-ice-darker-lightred :background ,electric-ice-darker-lightred)
               ;; dired
               (dired-directory :foreground ,electric-ice-darker-lightred :weight normal)
               (dired-flagged :foreground ,electric-ice-darker-pink)
               (dired-header :foreground ,fg3 :background ,electric-ice-darker-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,electric-ice-darker-fg)
               (dired-marked :foreground ,electric-ice-darker-lightred)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,electric-ice-darker-yellow :weight normal :slant italic)
               (dired-warning :foreground ,electric-ice-darker-lightred :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,electric-ice-darker-fg)
               (diredp-deletion-file-name :foreground ,electric-ice-darker-pink :background ,electric-ice-darker-current)
               (diredp-deletion :foreground ,electric-ice-darker-pink)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,electric-ice-darker-lightred)
               (diredp-file-name :foreground ,electric-ice-darker-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,electric-ice-darker-current)
               (diredp-flag-mark :foreground ,fg2 :background ,electric-ice-darker-current)
               (diredp-ignored-file-name :foreground ,electric-ice-darker-fg)
               (diredp-mode-line-flagged :foreground ,electric-ice-darker-lightred)
               (diredp-mode-line-marked :foreground ,electric-ice-darker-lightred)
               (diredp-no-priv :foreground ,electric-ice-darker-fg)
               (diredp-number :foreground ,electric-ice-darker-cyan)
               (diredp-other-priv :foreground ,electric-ice-darker-lightred)
               (diredp-rare-priv :foreground ,electric-ice-darker-lightred)
               (diredp-read-priv :foreground ,electric-ice-darker-purple)
               (diredp-write-priv :foreground ,electric-ice-darker-pink)
               (diredp-exec-priv :foreground ,electric-ice-darker-yellow)
               (diredp-symlink :foreground ,electric-ice-darker-lightred)
               (diredp-link-priv :foreground ,electric-ice-darker-lightred)
               (diredp-autofile-name :foreground ,electric-ice-darker-yellow)
               (diredp-tagged-autofile-name :foreground ,electric-ice-darker-yellow)
               ;; eldoc-box
               (eldoc-box-border :background ,electric-ice-darker-current)
               (eldoc-box-body :background ,electric-ice-darker-current)
               ;; elfeed
               (elfeed-search-date-face :foreground ,electric-ice-darker-comment)
               (elfeed-search-title-face :foreground ,electric-ice-darker-fg)
               (elfeed-search-unread-title-face :foreground ,electric-ice-darker-pink)
               (elfeed-search-feed-face :foreground ,electric-ice-darker-fg)
               (elfeed-search-tag-face :foreground ,electric-ice-darker-lightred)

               (elfeed-search-unread-count-face :foreground ,electric-ice-darker-pink)
               (elfeed-search-filter-face :foreground ,electric-ice-darker-lightred)
               ;;(elfeed-log-date-face :inherit font-lock-type-face)
               (elfeed-log-error-level-face :foreground ,electric-ice-darker-red)
               (elfeed-log-warn-level-face :foreground ,electric-ice-darker-lightred)
               (elfeed-log-info-level-face :foreground ,electric-ice-darker-cyan)
               (elfeed-log-debug-level-face :foreground ,electric-ice-darker-comment)
               ;; elpher
               (elpher-gemini-heading1 :inherit bold :foreground ,electric-ice-darker-pink
                                       ,@(when electric-ice-darker-enlarge-headings
                                           (list :height electric-ice-darker-height-title-1)))
               (elpher-gemini-heading2 :inherit bold :foreground ,electric-ice-darker-purple
                                       ,@(when electric-ice-darker-enlarge-headings
                                           (list :height electric-ice-darker-height-title-2)))
               (elpher-gemini-heading3 :weight normal :foreground ,electric-ice-darker-lightred
                                       ,@(when electric-ice-darker-enlarge-headings
                                           (list :height electric-ice-darker-height-title-3)))
               (elpher-gemini-preformatted :inherit fixed-pitch
                                           :foreground ,electric-ice-darker-lightred)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,electric-ice-darker-yellow)
               (enh-ruby-op-face :foreground ,electric-ice-darker-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,electric-ice-darker-yellow)
               (enh-ruby-string-delimiter-face :foreground ,electric-ice-darker-cyan)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,electric-ice-darker-lightred))
               (flyspell-incorrect :underline (:style wave :color ,electric-ice-darker-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,electric-ice-darker-purple :weight 'bold)
               (font-latex-italic-face :foreground ,electric-ice-darker-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,electric-ice-darker-cyan)
               (font-latex-match-variable-keywords :foreground ,electric-ice-darker-fg)
               (font-latex-string-face :foreground ,electric-ice-darker-cyan)
               ;; gemini
               (gemini-heading-face-1 :inherit bold :foreground ,electric-ice-darker-pink
                                      ,@(when electric-ice-darker-enlarge-headings
                                          (list :height electric-ice-darker-height-title-1)))
               (gemini-heading-face-2 :inherit bold :foreground ,electric-ice-darker-purple
                                      ,@(when electric-ice-darker-enlarge-headings
                                          (list :height electric-ice-darker-height-title-2)))
               (gemini-heading-face-3 :weight normal :foreground ,electric-ice-darker-lightred
                                      ,@(when electric-ice-darker-enlarge-headings
                                          (list :height electric-ice-darker-height-title-3)))
               (gemini-heading-face-rest :weight normal :foreground ,electric-ice-darker-yellow)
               (gemini-quote-face :foreground ,electric-ice-darker-purple)
               ;; go-test
               (go-test--ok-face :inherit success)
               (go-test--error-face :inherit error)
               (go-test--warning-face :inherit warning)
               (go-test--pointer-face :foreground ,electric-ice-darker-pink)
               (go-test--standard-face :foreground ,electric-ice-darker-cyan)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,electric-ice-darker-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,electric-ice-darker-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,electric-ice-darker-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,electric-ice-darker-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,electric-ice-darker-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,electric-ice-darker-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,electric-ice-darker-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,electric-ice-darker-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,electric-ice-darker-purple)
               (gnus-header-from :foreground ,electric-ice-darker-fg)
               (gnus-header-name :foreground ,electric-ice-darker-lightred)
               (gnus-header-subject :foreground ,electric-ice-darker-pink :weight bold)
               (gnus-summary-markup-face :foreground ,electric-ice-darker-cyan)
               (gnus-summary-high-unread :foreground ,electric-ice-darker-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,electric-ice-darker-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,electric-ice-darker-pink :weight bold)
               (gnus-summary-low-unread :foreground ,electric-ice-darker-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,electric-ice-darker-cyan)
               (haskell-type-face :foreground ,electric-ice-darker-fg)
               ;; helm
               (helm-belectric-ice-darkerkmark-w3m :foreground ,electric-ice-darker-purple)
               (helm-buffer-not-saved :foreground ,electric-ice-darker-purple :background ,electric-ice-darker-bg)
               (helm-buffer-process :foreground ,electric-ice-darker-lightred :background ,electric-ice-darker-bg)
               (helm-buffer-saved-out :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-bg)
               (helm-buffer-size :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-bg)
               (helm-candidate-number :foreground ,electric-ice-darker-bg :background ,electric-ice-darker-fg)
               (helm-ff-directory :foreground ,electric-ice-darker-lightred :background ,electric-ice-darker-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,electric-ice-darker-lightred :background ,electric-ice-darker-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,electric-ice-darker-bg :weight normal)
               (helm-ff-file :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,electric-ice-darker-pink :background ,electric-ice-darker-bg :weight bold)
               (helm-ff-prefix :foreground ,electric-ice-darker-bg :background ,electric-ice-darker-pink :weight normal)
               (helm-ff-symlink :foreground ,electric-ice-darker-pink :background ,electric-ice-darker-bg :weight bold)
               (helm-grep-cmd-line :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-bg)
               (helm-grep-file :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-bg)
               (helm-grep-finish :foreground ,fg2 :background ,electric-ice-darker-bg)
               (helm-grep-lineno :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-bg)
               (helm-grep-match :inherit match)
               (helm-grep-running :foreground ,electric-ice-darker-lightred :background ,electric-ice-darker-bg)
               (helm-header :foreground ,fg2 :background ,electric-ice-darker-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,electric-ice-darker-lightred :background ,electric-ice-darker-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,electric-ice-darker-purple :background ,electric-ice-darker-bg)
               (helm-source-go-package-godoc-description :foreground ,electric-ice-darker-yellow)
               (helm-source-header :foreground ,electric-ice-darker-pink :background ,electric-ice-darker-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,electric-ice-darker-lightred :background ,electric-ice-darker-bg)
               (helm-time-zone-home :foreground ,electric-ice-darker-purple :background ,electric-ice-darker-bg)
               (helm-visible-mark :foreground ,electric-ice-darker-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,electric-ice-darker-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,electric-ice-darker-fg)
               (icicle-search-current-input :foreground ,electric-ice-darker-pink)
               (icicle-search-context-level-8 :foreground ,electric-ice-darker-lightred)
               (icicle-search-context-level-7 :foreground ,electric-ice-darker-lightred)
               (icicle-search-context-level-6 :foreground ,electric-ice-darker-lightred)
               (icicle-search-context-level-5 :foreground ,electric-ice-darker-lightred)
               (icicle-search-context-level-4 :foreground ,electric-ice-darker-lightred)
               (icicle-search-context-level-3 :foreground ,electric-ice-darker-lightred)
               (icicle-search-context-level-2 :foreground ,electric-ice-darker-lightred)
               (icicle-search-context-level-1 :foreground ,electric-ice-darker-lightred)
               (icicle-search-main-regexp-current :foreground ,electric-ice-darker-fg)
               (icicle-saved-candidate :foreground ,electric-ice-darker-fg)
               (icicle-proxy-candidate :foreground ,electric-ice-darker-fg)
               (icicle-mustmatch-completion :foreground ,electric-ice-darker-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,electric-ice-darker-lightred)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,electric-ice-darker-lightred)
               (icicle-match-highlight-Completions :foreground ,electric-ice-darker-lightred)
               (icicle-key-complete-menu-local :foreground ,electric-ice-darker-fg)
               (icicle-key-complete-menu :foreground ,electric-ice-darker-fg)
               (icicle-input-completion-fail-lax :foreground ,electric-ice-darker-pink)
               (icicle-input-completion-fail :foreground ,electric-ice-darker-pink)
               (icicle-historical-candidate-other :foreground ,electric-ice-darker-fg)
               (icicle-historical-candidate :foreground ,electric-ice-darker-fg)
               (icicle-current-candidate-highlight :foreground ,electric-ice-darker-lightred :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,electric-ice-darker-fg)
               (icicle-complete-input :foreground ,electric-ice-darker-lightred)
               (icicle-common-match-highlight-Completions :foreground ,electric-ice-darker-purple)
               (icicle-candidate-part :foreground ,electric-ice-darker-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,electric-ice-darker-lightred)
               ;; ido
               (ido-first-match
                ,@(if electric-ice-darker-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground electric-ice-darker-lightred)
                    (list  :foreground electric-ice-darker-pink)))
               (ido-only-match :foreground ,electric-ice-darker-lightred)
               (ido-subdir :foreground ,electric-ice-darker-yellow)
               (ido-virtual :foreground ,electric-ice-darker-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-pink)
               ;; ivy
               (ivy-current-match
                ,@(if electric-ice-darker-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :background electric-ice-darker-current :foreground electric-ice-darker-lightred)
                    (list :background electric-ice-darker-current :foreground electric-ice-darker-pink)))
               ;; Highlights the background of the match.
               (ivy-minibuffer-match-face-1 :background ,electric-ice-darker-current)
               ;; Highlights the first matched group.
               (ivy-minibuffer-match-face-2 :foreground ,electric-ice-darker-lightred)
               ;; Highlights the second matched group.
               (ivy-minibuffer-match-face-3 :background ,electric-ice-darker-yellow
                                            :foreground ,electric-ice-darker-bg)
               ;; Highlights the third matched group.
               (ivy-minibuffer-match-face-4 :background ,electric-ice-darker-pink
                                            :foreground ,electric-ice-darker-bg)
               (ivy-confirm-face :foreground ,electric-ice-darker-lightred)
               (ivy-match-required-face :foreground ,electric-ice-darker-red)
               (ivy-subdir :foreground ,electric-ice-darker-yellow)
               (ivy-remote :foreground ,electric-ice-darker-pink)
               (ivy-virtual :foreground ,electric-ice-darker-cyan)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,electric-ice-darker-bg :background ,electric-ice-darker-lightred)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,electric-ice-darker-cyan)
               (jde-java-font-lock-modifier-face :foreground ,electric-ice-darker-pink)
               (jde-java-font-lock-number-face :foreground ,electric-ice-darker-fg)
               (jde-java-font-lock-package-face :foreground ,electric-ice-darker-fg)
               (jde-java-font-lock-private-face :foreground ,electric-ice-darker-pink)
               (jde-java-font-lock-public-face :foreground ,electric-ice-darker-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,electric-ice-darker-purple)
               (js2-function-param :foreground ,electric-ice-darker-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,electric-ice-darker-yellow)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,electric-ice-darker-yellow)
               (js2-private-function-call :foreground ,electric-ice-darker-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,electric-ice-darker-lightred)
               (js3-external-variable-face :foreground ,electric-ice-darker-fg)
               (js3-function-param-face :foreground ,electric-ice-darker-pink)
               (js3-instance-member-face :foreground ,electric-ice-darker-cyan)
               (js3-jsdoc-tag-face :foreground ,electric-ice-darker-pink)
               (js3-warning-face :underline ,electric-ice-darker-pink)
               ;; lsp
               (lsp-ui-peek-peek :background ,electric-ice-darker-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,electric-ice-darker-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,electric-ice-darker-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,electric-ice-darker-bg)
               (lsp-ui-peek-header :background ,bg3 :foreground ,fg3, :weight bold)
               (lsp-ui-peek-felectric-ice-darkerter :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,electric-ice-darker-fg :weight ultra-bold
                                               :box (:line-width -1 :color electric-ice-darker-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,electric-ice-darker-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,electric-ice-darker-bg)
               (lsp-ui-doc-header :foreground ,electric-ice-darker-bg :background ,electric-ice-darker-cyan)
               ;; magit
               (magit-branch-local :foreground ,electric-ice-darker-cyan)
               (magit-branch-remote :foreground ,electric-ice-darker-lightred)
               (magit-tag :foreground ,electric-ice-darker-lightred)
               (magit-section-heading :foreground ,electric-ice-darker-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,electric-ice-darker-lightred
                                            :background ,electric-ice-darker-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,electric-ice-darker-lightred
                                                      :background ,bg3
                                                      :weight bold
                                                      :extend t)
               ;; the four following lines are just a patch of the
               ;; upstream color to add the extend keyword.
               (magit-diff-added :background "#335533"
                                 :foreground "#ddffdd"
                                 :extend t)
               (magit-diff-added-highlight :background "#336633"
                                           :foreground "#cceecc"
                                           :extend t)
               (magit-diff-removed :background "#553333"
                                   :foreground "#ffdddd"
                                   :extend t)
               (magit-diff-removed-highlight :background "#663333"
                                             :foreground "#eecccc"
                                             :extend t)
               (magit-diff-file-heading :foreground ,electric-ice-darker-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,electric-ice-darker-lightred)
               (magit-diffstat-removed :foreground ,electric-ice-darker-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,electric-ice-darker-lightred :weight bold)
               (magit-process-ok :foreground ,electric-ice-darker-lightred :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,electric-ice-darker-yellow
                                         :slant italic)
               (markdown-code-face :foreground ,electric-ice-darker-lightred)
               (markdown-felectric-ice-darkertnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,electric-ice-darker-pink
                ,@(when electric-ice-darker-enlarge-headings
                    (list :height electric-ice-darker-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,electric-ice-darker-purple
                ,@(when electric-ice-darker-enlarge-headings
                    (list :height electric-ice-darker-height-title-2)))
               (markdown-header-face-3
                :foreground ,electric-ice-darker-lightred
                ,@(when electric-ice-darker-enlarge-headings
                    (list :height electric-ice-darker-height-title-3)))
               (markdown-header-face-4 :foreground ,electric-ice-darker-yellow)
               (markdown-header-face-5 :foreground ,electric-ice-darker-cyan)
               (markdown-header-face-6 :foreground ,electric-ice-darker-lightred)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,electric-ice-darker-fg)
               (markdown-inline-code-face :foreground ,electric-ice-darker-lightred)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,electric-ice-darker-lightred)
               (markdown-table-face :foreground ,electric-ice-darker-purple)
               (markdown-list-face :foreground ,electric-ice-darker-cyan)
               (markdown-language-keyword-face :foreground ,electric-ice-darker-comment)
               ;; message
               (message-header-to :foreground ,electric-ice-darker-fg :weight bold)
               (message-header-cc :foreground ,electric-ice-darker-fg :bold bold)
               (message-header-subject :foreground ,electric-ice-darker-lightred)
               (message-header-newsgroups :foreground ,electric-ice-darker-purple)
               (message-header-other :foreground ,electric-ice-darker-purple)
               (message-header-name :foreground ,electric-ice-darker-lightred)
               (message-header-xheader :foreground ,electric-ice-darker-cyan)
               (message-separator :foreground ,electric-ice-darker-cyan :slant italic)
               (message-cited-text :foreground ,electric-ice-darker-purple)
               (message-cited-text-1 :foreground ,electric-ice-darker-purple)
               (message-cited-text-2 :foreground ,electric-ice-darker-lightred)
               (message-cited-text-3 :foreground ,electric-ice-darker-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,electric-ice-darker-lightred :weight normal)
               ;; mini-modeline
               (mini-modeline-mode-line :inherit mode-line :height 0.1 :box nil)
               ;; mu4e
               (mu4e-unread-face :foreground ,electric-ice-darker-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,electric-ice-darker-purple)
               (mu4e-highlight-face :background ,electric-ice-darker-bg
                                    :foreground ,electric-ice-darker-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,electric-ice-darker-current
                                           :foreground ,electric-ice-darker-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,electric-ice-darker-purple)
               (mu4e-cited-1-face :foreground ,electric-ice-darker-purple)
               (mu4e-cited-2-face :foreground ,electric-ice-darker-lightred)
               (mu4e-cited-3-face :foreground ,electric-ice-darker-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; neotree
               (neo-banner-face :foreground ,electric-ice-darker-lightred :weight bold)
               ;;(neo-button-face :underline nil)
               (neo-dir-link-face :foreground ,electric-ice-darker-purple)
               (neo-expand-btn-face :foreground ,electric-ice-darker-fg)
               (neo-file-link-face :foreground ,electric-ice-darker-cyan)
               (neo-header-face :background ,electric-ice-darker-bg
                                :foreground ,electric-ice-darker-fg
                                :weight bold)
               (neo-relectric-ice-darkert-dir-face :foreground ,electric-ice-darker-purple :weight bold)
               (neo-vc-added-face :foreground ,electric-ice-darker-lightred)
               (neo-vc-conflict-face :foreground ,electric-ice-darker-red)
               (neo-vc-default-face :inherit neo-file-link-face)
               (neo-vc-edited-face :foreground ,electric-ice-darker-lightred)
               (neo-vc-ignored-face :foreground ,electric-ice-darker-comment)
               (neo-vc-missing-face :foreground ,electric-ice-darker-red)
               (neo-vc-needs-merge-face :foreground ,electric-ice-darker-red
                                        :weight bold)
               ;;(neo-vc-needs-update-face :underline t)
               ;;(neo-vc-removed-face :strike-through t)
               (neo-vc-unlocked-changes-face :foreground ,electric-ice-darker-red)
               ;;(neo-vc-unregistered-face nil)
               (neo-vc-up-to-date-face :foreground ,electric-ice-darker-lightred)
               (neo-vc-user-face :foreground ,electric-ice-darker-purple)
               ;; org
               (org-agenda-date :foreground ,electric-ice-darker-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,electric-ice-darker-comment)
               (org-agenda-done :foreground ,electric-ice-darker-lightred)
               (org-agenda-structure :foreground ,electric-ice-darker-purple)
               (org-block :foreground ,electric-ice-darker-lightred)
               (org-code :foreground ,electric-ice-darker-lightred)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,electric-ice-darker-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,electric-ice-darker-comment)
               (org-document-title :weight bold :foreground ,electric-ice-darker-lightred
                                   ,@(when electric-ice-darker-enlarge-headings
                                       (list :height electric-ice-darker-height-doc-title)))
               (org-done :foreground ,electric-ice-darker-lightred)
               (org-ellipsis :foreground ,electric-ice-darker-comment)
               (org-felectric-ice-darkertnote :foreground ,other-blue)
               (org-formula :foreground ,electric-ice-darker-pink)
               (org-headline-done :foreground ,electric-ice-darker-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,electric-ice-darker-bg :background ,electric-ice-darker-bg)
               (org-level-1 :inherit bold :foreground ,electric-ice-darker-yellow
                            ,@(when electric-ice-darker-enlarge-headings
                                (list :height electric-ice-darker-height-title-1)))
               (org-level-2 :inherit bold :foreground ,electric-ice-darker-pink
                            ,@(when electric-ice-darker-enlarge-headings
                                (list :height electric-ice-darker-height-title-2)))
               (org-level-3 :weight normal :foreground ,electric-ice-darker-cyan
                            ,@(when electric-ice-darker-enlarge-headings
                                (list :height electric-ice-darker-height-title-3)))
               (org-level-4 :weight normal :foreground ,electric-ice-darker-yellow)
               (org-level-5 :weight normal :foreground ,electric-ice-darker-cyan)
               (org-level-6 :weight normal :foreground ,electric-ice-darker-lightred)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,electric-ice-darker-fg)
               (org-link :foreground ,electric-ice-darker-cyan :underline t)
               (org-priority :foreground ,electric-ice-darker-cyan)
               (org-quote :foreground ,electric-ice-darker-yellow :slant italic)
               (org-scheduled :foreground ,electric-ice-darker-lightred)
               (org-scheduled-previously :foreground ,electric-ice-darker-yellow)
               (org-scheduled-today :foreground ,electric-ice-darker-lightred)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,electric-ice-darker-yellow)
               (org-table :foreground ,electric-ice-darker-purple)
               (org-tag :foreground ,electric-ice-darker-pink :weight bold :background ,bg2)
               ;(org-todo :foreground ,electric-ice-darker-lightred :weight)
               (org-upcoming-deadline :foreground ,electric-ice-darker-yellow)
               (org-verbatim :inherit org-quote)
               (org-warning :weight bold :foreground ,electric-ice-darker-pink)
               ;; outline
               (outline-1 :foreground ,electric-ice-darker-pink)
               (outline-2 :foreground ,electric-ice-darker-purple)
               (outline-3 :foreground ,electric-ice-darker-lightred)
               (outline-4 :foreground ,electric-ice-darker-yellow)
               (outline-5 :foreground ,electric-ice-darker-cyan)
               (outline-6 :foreground ,electric-ice-darker-lightred)
               ;; perspective
               (persp-selected-face :weight bold :foreground ,electric-ice-darker-pink)
               ;; powerline
               (powerline-active1 :background ,electric-ice-darker-bg :foreground ,electric-ice-darker-pink)
               (powerline-active2 :background ,electric-ice-darker-bg :foreground ,electric-ice-darker-pink)
               (powerline-inactive1 :background ,bg2 :foreground ,electric-ice-darker-purple)
               (powerline-inactive2 :background ,bg2 :foreground ,electric-ice-darker-purple)
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,electric-ice-darker-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,electric-ice-darker-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,electric-ice-darker-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,electric-ice-darker-lightred)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,electric-ice-darker-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,electric-ice-darker-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,electric-ice-darker-lightred)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,electric-ice-darker-fg)
               (rainbow-delimiters-depth-2-face :foreground ,electric-ice-darker-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,electric-ice-darker-purple)
               (rainbow-delimiters-depth-4-face :foreground ,electric-ice-darker-pink)
               (rainbow-delimiters-depth-5-face :foreground ,electric-ice-darker-lightred)
               (rainbow-delimiters-depth-6-face :foreground ,electric-ice-darker-lightred)
               (rainbow-delimiters-depth-7-face :foreground ,electric-ice-darker-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,electric-ice-darker-lightred)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,electric-ice-darker-lightred)
               (rpm-spec-doc-face :foreground ,electric-ice-darker-pink)
               (rpm-spec-ghost-face :foreground ,electric-ice-darker-purple)
               (rpm-spec-macro-face :foreground ,electric-ice-darker-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,electric-ice-darker-purple)
               (rpm-spec-section-face :foreground ,electric-ice-darker-yellow)
               (rpm-spec-tag-face :foreground ,electric-ice-darker-cyan)
               (rpm-spec-var-face :foreground ,electric-ice-darker-lightred)
               ;; rst (reStructuredText)
               (rst-level-1 :foreground ,electric-ice-darker-pink :weight bold)
               (rst-level-2 :foreground ,electric-ice-darker-purple :weight bold)
               (rst-level-3 :foreground ,electric-ice-darker-lightred)
               (rst-level-4 :foreground ,electric-ice-darker-yellow)
               (rst-level-5 :foreground ,electric-ice-darker-cyan)
               (rst-level-6 :foreground ,electric-ice-darker-lightred)
               (rst-level-7 :foreground ,other-blue)
               (rst-level-8 :foreground ,electric-ice-darker-fg)
               ;; selectrum-mode
               (selectrum-current-candidate :weight bold)
               (selectrum-primary-highlight :foreground ,electric-ice-darker-pink)
               (selectrum-secondary-highlight :foreground ,electric-ice-darker-lightred)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,electric-ice-darker-fg
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,electric-ice-darker-fg
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,electric-ice-darker-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,electric-ice-darker-lightred
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,electric-ice-darker-lightred)
               (speedbar-file-face :foreground ,electric-ice-darker-cyan)
               (speedbar-directory-face :foreground ,electric-ice-darker-purple)
               (speedbar-tag-face :foreground ,electric-ice-darker-yellow)
               (speedbar-selected-face :foreground ,electric-ice-darker-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,electric-ice-darker-bg
                                        :foreground ,electric-ice-darker-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,electric-ice-darker-purple :background ,electric-ice-darker-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,electric-ice-darker-pink :background ,electric-ice-darker-bg
                            :box (:line-width 2 :color ,electric-ice-darker-bg :style nil))
               (tab-bar-tab-inactive :foreground ,electric-ice-darker-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,electric-ice-darker-purple :background ,electric-ice-darker-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,electric-ice-darker-pink :background ,electric-ice-darker-bg
                             :box (:line-width 2 :color ,electric-ice-darker-bg :style nil))
               (tab-line-tab-inactive :foreground ,electric-ice-darker-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,electric-ice-darker-red)
               ;; telephone-line
               (telephone-line-accent-active :background ,electric-ice-darker-bg :foreground ,electric-ice-darker-pink)
               (telephone-line-accent-inactive :background ,bg2 :foreground ,electric-ice-darker-purple)
               (telephone-line-unimportant :background ,electric-ice-darker-bg :foreground ,electric-ice-darker-comment)
               ;; term
               (term :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-bg)
               (term-color-black :foreground ,electric-ice-darker-bg :background ,electric-ice-darker-comment)
               (term-color-blue :foreground ,electric-ice-darker-purple :background ,electric-ice-darker-purple)
               (term-color-cyan :foreground ,electric-ice-darker-cyan :background ,electric-ice-darker-cyan)
               (term-color-green :foreground ,electric-ice-darker-lightred :background ,electric-ice-darker-lightred)
               (term-color-magenta :foreground ,electric-ice-darker-pink :background ,electric-ice-darker-pink)
               (term-color-red :foreground ,electric-ice-darker-red :background ,electric-ice-darker-red)
               (term-color-white :foreground ,electric-ice-darker-fg :background ,electric-ice-darker-fg)
               (term-color-yellow :foreground ,electric-ice-darker-yellow :background ,electric-ice-darker-yellow)
               ;; tree-sitter
               (tree-sitter-hl-face:attribute :inherit font-lock-constant-face)
               (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
               (tree-sitter-hl-face:constant :inherit font-lock-constant-face)
               (tree-sitter-hl-face:constant.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:constructor :inherit font-lock-constant-face)
               (tree-sitter-hl-face:escape :foreground ,electric-ice-darker-pink)
               (tree-sitter-hl-face:function :inherit font-lock-function-name-face)
               (tree-sitter-hl-face:function.builtin :foreground "#00f")
               (tree-sitter-hl-face:function.call :inherit font-lock-function-name-face
                                                  :weight normal)
               (tree-sitter-hl-face:function.macro :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:function.special :foreground "#0f0")
               (tree-sitter-hl-face:keyword :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:punctuation :foreground "#0f0")
               (tree-sitter-hl-face:punctuation.bracket :foreground ,electric-ice-darker-fg)
               (tree-sitter-hl-face:punctuation.delimiter :foreground ,electric-ice-darker-fg)
               (tree-sitter-hl-face:punctuation.special :foreground "#0f0")
               (tree-sitter-hl-face:string :inherit font-lock-string-face)
               (tree-sitter-hl-face:string.special :foreground ,electric-ice-darker-red)
               (tree-sitter-hl-face:tag :foreground "#00f")
               (tree-sitter-hl-face:type :foreground "#f00")
               (tree-sitter-hl-face:type.parameter :foreground ,electric-ice-darker-pink)
               (tree-sitter-hl-face:variable :inherit font-lock-variable-name-face)
               (tree-sitter-hl-face:variable.parameter :inherit tree-sitter-hl-face:variable
                                                       :weight normal)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,electric-ice-darker-lightred)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,electric-ice-darker-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,electric-ice-darker-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit font-lock-builtin-face)
               (web-mode-comment-face :inherit font-lock-comment-face)
               (web-mode-constant-face :inherit font-lock-constant-face)
               (web-mode-css-property-name-face :foreground ,electric-ice-darker-cyan)
               (web-mode-doctype-face :inherit font-lock-comment-face)
               (web-mode-function-name-face :inherit font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,electric-ice-darker-purple)
               (web-mode-html-attr-value-face :foreground ,electric-ice-darker-lightred)
               (web-mode-html-tag-face :foreground ,electric-ice-darker-lightred)
               (web-mode-keyword-face :foreground ,electric-ice-darker-pink)
               (web-mode-type-face :inherit font-lock-type-face)
               (web-mode-warning-face :inherit font-lock-warning-face)
               ;; which-func
               (which-func :inherit font-lock-function-name-face)
               ;; which-key
               (which-key-key-face :inherit font-lock-builtin-face)
               (which-key-command-description-face :inherit default)
               (which-key-separator-face :inherit font-lock-comment-delimiter-face)
               (which-key-local-map-description-face :foreground ,electric-ice-darker-lightred)
               ;; whitespace
               (whitespace-big-indent :foreground ,electric-ice-darker-red)
               (whitespace-empty :foreground ,electric-ice-darker-red)
               (whitespace-hspace :background ,bg3 :foreground ,electric-ice-darker-comment)
               (whitespace-indentation :foreground ,electric-ice-darker-red)
               (whitespace-line :background ,electric-ice-darker-bg :foreground ,electric-ice-darker-pink)
               (whitespace-newline :foreground ,electric-ice-darker-bg)
               (whitespace-space :background ,electric-ice-darker-current :foreground ,electric-ice-darker-bg)
               (whitespace-space-after-tab :foreground ,electric-ice-darker-red)
               (whitespace-space-before-tab :foreground ,electric-ice-darker-red)
               (whitespace-tab :background ,electric-ice-darker-current :foreground ,electric-ice-darker-bg)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit font-lock-builtin-face)
               (yard-directive-face :inherit font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'electric-ice-darker
         (let ((expand-with-func
                (lambda (func spec)
                  (let (reduced-color-list)
                    (dolist (col colors reduced-color-list)
                      (push (list (car col) (funcall func col))
                            reduced-color-list))
                    (eval `(let ,reduced-color-list
                             (backquote ,spec))))))
               whole-theme)
           (pcase-dolist (`(,face . ,spec) faces)
             (push `(,face
                     ((((min-colors 16777216)) ; fully graphical envs
                       ,(funcall expand-with-func 'cadr spec))
                      (((min-colors 256))      ; terminal withs 256 colors
                       ,(if electric-ice-darker-use-24-bit-colors-on-256-colors-terms
                            (funcall expand-with-func 'cadr spec)
                          (funcall expand-with-func 'caddr spec)))
                      (t                       ; should be only tty-like envs
                       ,(funcall expand-with-func 'cadddr spec))))
                   whole-theme))
           whole-theme))

  (apply #'custom-theme-set-variables
         'electric-ice-darker
         (let ((get-func
                (pcase (display-color-cells)
                  ((pred (<= 16777216)) 'car) ; fully graphical envs
                  ((pred (<= 256)) 'cadr)     ; terminal withs 256 colors
                  (_ 'caddr))))               ; should be only tty-like envs
           `((ansi-color-names-vector
              [,(funcall get-func (alist-get 'electric-ice-darker-bg colors))
               ,(funcall get-func (alist-get 'electric-ice-darker-red colors))
               ,(funcall get-func (alist-get 'electric-ice-darker-lightred colors))
               ,(funcall get-func (alist-get 'electric-ice-darker-yellow colors))
               ,(funcall get-func (alist-get 'electric-ice-darker-comment colors))
               ,(funcall get-func (alist-get 'electric-ice-darker-purple colors))
               ,(funcall get-func (alist-get 'electric-ice-darker-cyan colors))
               ,(funcall get-func (alist-get 'electric-ice-darker-fg colors))])))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'electric-ice-darker)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; electric-ice-darker-theme.el ends here
