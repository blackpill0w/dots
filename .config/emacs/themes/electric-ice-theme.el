;;; electric-ice-theme.el --- Electric-Ice Theme

;;; Code:
(deftheme electric-ice)

;;;; Configuration options:

(defgroup electric-ice nil
  "Electric-Ice theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom electric-ice-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'belectric-icelean
  :group 'electric-ice)

(defcustom electric-ice-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'electric-ice)

(defcustom electric-ice-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'electric-ice)

(defcustom electric-ice-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'electric-ice)

(defcustom electric-ice-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'electric-ice)

(defcustom electric-ice-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'belectric-icelean
  :group 'electric-ice)

(defvar electric-ice-use-24-bit-colors-on-256-colors-terms nil)


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (electric-ice-bg            "#0a0820" "unspecified-bg" "unspecified-bg") ; official background
                (electric-ice-fg            "#ffffff" "#ffffff" "brightwhite") ; official foreground
                (electric-ice-current       "#44475a" "#303030" "brightblack") ; official current-line/selection
                (electric-ice-comment       "#6272A4" "#6272A4" "blue")        ; official comment
                (electric-ice-cyan          "#1ea8fc" "#1ea8fc" "cyan")  ; official cyan
                (electric-ice-brightcyan    "#70f0ff" "#70f0ff" "brightcyan")  ; official cyan
                (electric-ice-green         "#50fa7b" "#5fff87" "green")       ; official green
                (electric-ice-orange        "#ffb86c" "#ffaf5f" "brightred")   ; official orange
                (electric-ice-pink          "#FF0081" "#FF0081" "magenta")     ; official pink
                (electric-ice-purple        "#D86BFF" "#A875FF" "brightmagenta") ; official purple
                (electric-ice-red           "#ff5555" "#ff8787" "red")         ; official red
                (electric-ice-yellow        "#ffff50" "#ffff87" "yellow")      ; official yellow
                ;; Other colors
                (bg2             "#070618" "#12ff12" "brightblack")
                (bg3             "#464752" "#262626" "brightblack")
                (bg4             "#565761" "#444444" "brightblack")
                (fg2             "#e2e2dc" "#e4e4e4" "brightwhite")
                (fg3             "#ccccc7" "#c6c6c6" "white")
                (fg4             "#b6b6b2" "#b2b2b2" "white")
                (other-blue      "#0189cc" "#0087ff" "brightblue")))
      (faces '(;; default / basic faces
               (cursor :background ,fg3)
               (default :background ,electric-ice-bg :foreground ,electric-ice-fg)
               (default-italic :slant italic)
               (error :foreground ,electric-ice-red)
               (ffap :foreground ,fg4)
               (fringe :background ,electric-ice-bg :foreground ,fg4)
               (header-line :inherit 'mode-line)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,electric-ice-cyan :extend t)
               (info-quoted-name :foreground ,electric-ice-orange)
               (info-string :foreground ,electric-ice-cyan)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,electric-ice-cyan :underline t)
               (linum :slant italic :foreground ,electric-ice-comment :background ,electric-ice-bg)
               (line-number :slant italic :foreground ,electric-ice-comment :background ,electric-ice-bg)
               (line-number-current-line :slant italic :foreground ,electric-ice-cyan :background ,electric-ice-bg)
               (nlinum-current-line :slant italic :foreground ,electric-ice-cyan :background ,electric-ice-bg)
               (match :background ,electric-ice-yellow :foreground ,electric-ice-bg)
               (menu :background ,electric-ice-current :inverse-video nil
                     ,@(if electric-ice-alternate-mode-line-and-minibuffer
                           (list :foreground fg3)
                         (list :foreground electric-ice-fg)))
               (minibuffer-prompt
                ,@(if electric-ice-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground electric-ice-fg)
                    (list  :foreground electric-ice-pink)))
               (mode-line :background ,electric-ice-current
                          :box ,electric-ice-current :inverse-video nil
                          ,@(if electric-ice-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground electric-ice-fg)))
               (mode-line-inactive
                :background ,electric-ice-bg :inverse-video nil
                ,@(if electric-ice-alternate-mode-line-and-minibuffer
                      (list :foreground electric-ice-comment)
                    (list :foreground fg4 :box bg2)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :background ,electric-ice-current)
               (shadow :foreground ,electric-ice-comment)
               (success :foreground ,electric-ice-green)
               (telectric-iceltip :foreground ,electric-ice-fg :background ,electric-ice-current)
               (trailing-whitespace :background ,electric-ice-orange :foreground ,electric-ice-bg)
               (vertical-border :foreground ,bg2)
               (warning :foreground ,electric-ice-orange)
               ;; syntax / font-lock
               (font-lock-builtin-face :foreground ,electric-ice-cyan :slant italic)
               (font-lock-comment-face :foreground ,electric-ice-comment)
               (font-lock-comment-delimiter-face :inherit shadow)
               (font-lock-constant-face :foreground ,electric-ice-purple)
               (font-lock-doc-face :foreground ,electric-ice-comment)
               (font-lock-function-name-face :foreground ,electric-ice-yellow :weight normal)
               (font-lock-keyword-face :foreground ,electric-ice-pink :weight normal)
               (font-lock-negation-char-face :foreground ,electric-ice-pink)
               (font-lock-preprocessor-face :foreground ,electric-ice-pink)
               (font-lock-reference-face :inherit font-lock-constant-face) ;; obsolete
               (font-lock-regexp-grouping-backslash :foreground ,electric-ice-cyan)
               (font-lock-regexp-grouping-construct :foreground ,electric-ice-purple)
               (font-lock-string-face :foreground ,electric-ice-brightcyan)
               (font-lock-type-face :inherit font-lock-builtin-face :foreground ,electric-ice-cyan)
               (font-lock-variable-name-face :foreground ,electric-ice-fg :weight normal)
               (font-lock-warning-face :inherit warning :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,electric-ice-pink)
               ;; company
               (company-echo-common :foreground ,electric-ice-bg :background ,electric-ice-fg)
               (company-preview :background ,electric-ice-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,electric-ice-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,electric-ice-green)
               (company-scrollbar-bg :background ,electric-ice-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,electric-ice-fg :background ,bg2)
               (company-tooltip-common :foreground ,electric-ice-purple)
               (company-tooltip-selection :background ,electric-ice-current)
               (company-telectric-iceltip :inherit telectric-iceltip)
               (company-telectric-iceltip-search :foreground ,electric-ice-green
                                           :underline t)
               (company-telectric-iceltip-search-selection :background ,electric-ice-green
                                                     :foreground ,electric-ice-bg)
               (company-telectric-iceltip-selection :inherit match)
               (company-telectric-iceltip-mouse :background ,electric-ice-bg)
               (company-telectric-iceltip-common :foreground ,electric-ice-pink)
               ;;(company-telectric-iceltip-common-selection :inherit company-telectric-iceltip-common)
               (company-telectric-iceltip-annotation :foreground ,electric-ice-cyan)
               ;;(company-telectric-iceltip-annotation-selection :inherit company-telectric-iceltip-annotation)
               ;; completions (minibuffer.el)
               (completions-annotations :inherit font-lock-comment-face)
               (completions-common-part :foreground ,electric-ice-pink)
               (completions-first-difference :foreground ,electric-ice-fg)
               ;; diff-hl
               (diff-hl-change :foreground ,electric-ice-orange :background ,electric-ice-orange)
               (diff-hl-delete :foreground ,electric-ice-red :background ,electric-ice-red)
               (diff-hl-insert :foreground ,electric-ice-green :background ,electric-ice-green)
               ;; dired
               (dired-directory :foreground ,electric-ice-green :weight normal)
               (dired-flagged :foreground ,electric-ice-pink)
               (dired-header :foreground ,fg3 :background ,electric-ice-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,electric-ice-fg)
               (dired-marked :foreground ,electric-ice-orange)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,electric-ice-yellow :weight normal :slant italic)
               (dired-warning :foreground ,electric-ice-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,electric-ice-fg)
               (diredp-deletion-file-name :foreground ,electric-ice-pink :background ,electric-ice-current)
               (diredp-deletion :foreground ,electric-ice-pink)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,electric-ice-orange)
               (diredp-file-name :foreground ,electric-ice-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,electric-ice-current)
               (diredp-flag-mark :foreground ,fg2 :background ,electric-ice-current)
               (diredp-ignored-file-name :foreground ,electric-ice-fg)
               (diredp-mode-line-flagged :foreground ,electric-ice-orange)
               (diredp-mode-line-marked :foreground ,electric-ice-orange)
               (diredp-no-priv :foreground ,electric-ice-fg)
               (diredp-number :foreground ,electric-ice-cyan)
               (diredp-other-priv :foreground ,electric-ice-orange)
               (diredp-rare-priv :foreground ,electric-ice-orange)
               (diredp-read-priv :foreground ,electric-ice-purple)
               (diredp-write-priv :foreground ,electric-ice-pink)
               (diredp-exec-priv :foreground ,electric-ice-yellow)
               (diredp-symlink :foreground ,electric-ice-orange)
               (diredp-link-priv :foreground ,electric-ice-orange)
               (diredp-autofile-name :foreground ,electric-ice-yellow)
               (diredp-tagged-autofile-name :foreground ,electric-ice-yellow)
               ;; eldoc-box
               (eldoc-box-border :background ,electric-ice-current)
               (eldoc-box-body :background ,electric-ice-current)
               ;; elfeed
               (elfeed-search-date-face :foreground ,electric-ice-comment)
               (elfeed-search-title-face :foreground ,electric-ice-fg)
               (elfeed-search-unread-title-face :foreground ,electric-ice-pink)
               (elfeed-search-feed-face :foreground ,electric-ice-fg)
               (elfeed-search-tag-face :foreground ,electric-ice-green)

               (elfeed-search-unread-count-face :foreground ,electric-ice-pink)
               (elfeed-search-filter-face :foreground ,electric-ice-green)
               ;;(elfeed-log-date-face :inherit font-lock-type-face)
               (elfeed-log-error-level-face :foreground ,electric-ice-red)
               (elfeed-log-warn-level-face :foreground ,electric-ice-orange)
               (elfeed-log-info-level-face :foreground ,electric-ice-cyan)
               (elfeed-log-debug-level-face :foreground ,electric-ice-comment)
               ;; elpher
               (elpher-gemini-heading1 :inherit bold :foreground ,electric-ice-pink
                                       ,@(when electric-ice-enlarge-headings
                                           (list :height electric-ice-height-title-1)))
               (elpher-gemini-heading2 :inherit bold :foreground ,electric-ice-purple
                                       ,@(when electric-ice-enlarge-headings
                                           (list :height electric-ice-height-title-2)))
               (elpher-gemini-heading3 :weight normal :foreground ,electric-ice-green
                                       ,@(when electric-ice-enlarge-headings
                                           (list :height electric-ice-height-title-3)))
               (elpher-gemini-preformatted :inherit fixed-pitch
                                           :foreground ,electric-ice-orange)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,electric-ice-yellow)
               (enh-ruby-op-face :foreground ,electric-ice-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,electric-ice-yellow)
               (enh-ruby-string-delimiter-face :foreground ,electric-ice-cyan)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,electric-ice-orange))
               (flyspell-incorrect :underline (:style wave :color ,electric-ice-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,electric-ice-purple :weight 'bold)
               (font-latex-italic-face :foreground ,electric-ice-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,electric-ice-cyan)
               (font-latex-match-variable-keywords :foreground ,electric-ice-fg)
               (font-latex-string-face :foreground ,electric-ice-cyan)
               ;; gemini
               (gemini-heading-face-1 :inherit bold :foreground ,electric-ice-pink
                                      ,@(when electric-ice-enlarge-headings
                                          (list :height electric-ice-height-title-1)))
               (gemini-heading-face-2 :inherit bold :foreground ,electric-ice-purple
                                      ,@(when electric-ice-enlarge-headings
                                          (list :height electric-ice-height-title-2)))
               (gemini-heading-face-3 :weight normal :foreground ,electric-ice-green
                                      ,@(when electric-ice-enlarge-headings
                                          (list :height electric-ice-height-title-3)))
               (gemini-heading-face-rest :weight normal :foreground ,electric-ice-yellow)
               (gemini-quote-face :foreground ,electric-ice-purple)
               ;; go-test
               (go-test--ok-face :inherit success)
               (go-test--error-face :inherit error)
               (go-test--warning-face :inherit warning)
               (go-test--pointer-face :foreground ,electric-ice-pink)
               (go-test--standard-face :foreground ,electric-ice-cyan)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,electric-ice-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,electric-ice-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,electric-ice-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,electric-ice-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,electric-ice-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,electric-ice-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,electric-ice-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,electric-ice-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,electric-ice-purple)
               (gnus-header-from :foreground ,electric-ice-fg)
               (gnus-header-name :foreground ,electric-ice-green)
               (gnus-header-subject :foreground ,electric-ice-pink :weight bold)
               (gnus-summary-markup-face :foreground ,electric-ice-cyan)
               (gnus-summary-high-unread :foreground ,electric-ice-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,electric-ice-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,electric-ice-pink :weight bold)
               (gnus-summary-low-unread :foreground ,electric-ice-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,electric-ice-pink)
               (haskell-constructor-face :foreground ,electric-ice-purple)
               ;; helm
               (helm-belectric-icekmark-w3m :foreground ,electric-ice-purple)
               (helm-buffer-not-saved :foreground ,electric-ice-purple :background ,electric-ice-bg)
               (helm-buffer-process :foreground ,electric-ice-orange :background ,electric-ice-bg)
               (helm-buffer-saved-out :foreground ,electric-ice-fg :background ,electric-ice-bg)
               (helm-buffer-size :foreground ,electric-ice-fg :background ,electric-ice-bg)
               (helm-candidate-number :foreground ,electric-ice-bg :background ,electric-ice-fg)
               (helm-ff-directory :foreground ,electric-ice-green :background ,electric-ice-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,electric-ice-green :background ,electric-ice-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,electric-ice-bg :weight normal)
               (helm-ff-file :foreground ,electric-ice-fg :background ,electric-ice-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,electric-ice-pink :background ,electric-ice-bg :weight bold)
               (helm-ff-prefix :foreground ,electric-ice-bg :background ,electric-ice-pink :weight normal)
               (helm-ff-symlink :foreground ,electric-ice-pink :background ,electric-ice-bg :weight bold)
               (helm-grep-cmd-line :foreground ,electric-ice-fg :background ,electric-ice-bg)
               (helm-grep-file :foreground ,electric-ice-fg :background ,electric-ice-bg)
               (helm-grep-finish :foreground ,fg2 :background ,electric-ice-bg)
               (helm-grep-lineno :foreground ,electric-ice-fg :background ,electric-ice-bg)
               (helm-grep-match :inherit match)
               (helm-grep-running :foreground ,electric-ice-green :background ,electric-ice-bg)
               (helm-header :foreground ,fg2 :background ,electric-ice-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,electric-ice-green :background ,electric-ice-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,electric-ice-purple :background ,electric-ice-bg)
               (helm-source-go-package-godoc-description :foreground ,electric-ice-yellow)
               (helm-source-header :foreground ,electric-ice-pink :background ,electric-ice-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,electric-ice-orange :background ,electric-ice-bg)
               (helm-time-zone-home :foreground ,electric-ice-purple :background ,electric-ice-bg)
               (helm-visible-mark :foreground ,electric-ice-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,electric-ice-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,electric-ice-fg)
               (icicle-search-current-input :foreground ,electric-ice-pink)
               (icicle-search-context-level-8 :foreground ,electric-ice-orange)
               (icicle-search-context-level-7 :foreground ,electric-ice-orange)
               (icicle-search-context-level-6 :foreground ,electric-ice-orange)
               (icicle-search-context-level-5 :foreground ,electric-ice-orange)
               (icicle-search-context-level-4 :foreground ,electric-ice-orange)
               (icicle-search-context-level-3 :foreground ,electric-ice-orange)
               (icicle-search-context-level-2 :foreground ,electric-ice-orange)
               (icicle-search-context-level-1 :foreground ,electric-ice-orange)
               (icicle-search-main-regexp-current :foreground ,electric-ice-fg)
               (icicle-saved-candidate :foreground ,electric-ice-fg)
               (icicle-proxy-candidate :foreground ,electric-ice-fg)
               (icicle-mustmatch-completion :foreground ,electric-ice-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,electric-ice-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,electric-ice-orange)
               (icicle-match-highlight-Completions :foreground ,electric-ice-green)
               (icicle-key-complete-menu-local :foreground ,electric-ice-fg)
               (icicle-key-complete-menu :foreground ,electric-ice-fg)
               (icicle-input-completion-fail-lax :foreground ,electric-ice-pink)
               (icicle-input-completion-fail :foreground ,electric-ice-pink)
               (icicle-historical-candidate-other :foreground ,electric-ice-fg)
               (icicle-historical-candidate :foreground ,electric-ice-fg)
               (icicle-current-candidate-highlight :foreground ,electric-ice-orange :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,electric-ice-fg)
               (icicle-complete-input :foreground ,electric-ice-orange)
               (icicle-common-match-highlight-Completions :foreground ,electric-ice-purple)
               (icicle-candidate-part :foreground ,electric-ice-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,electric-ice-orange)
               ;; ido
               (ido-first-match
                ,@(if electric-ice-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground electric-ice-green)
                    (list  :foreground electric-ice-pink)))
               (ido-only-match :foreground ,electric-ice-orange)
               (ido-subdir :foreground ,electric-ice-yellow)
               (ido-virtual :foreground ,electric-ice-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,electric-ice-fg :background ,electric-ice-pink)
               ;; ivy
               (ivy-current-match
                ,@(if electric-ice-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :background electric-ice-current :foreground electric-ice-green)
                    (list :background electric-ice-current :foreground electric-ice-pink)))
               ;; Highlights the background of the match.
               (ivy-minibuffer-match-face-1 :background ,electric-ice-current)
               ;; Highlights the first matched group.
               (ivy-minibuffer-match-face-2 :foreground ,electric-ice-green)
               ;; Highlights the second matched group.
               (ivy-minibuffer-match-face-3 :background ,electric-ice-yellow
                                            :foreground ,electric-ice-bg)
               ;; Highlights the third matched group.
               (ivy-minibuffer-match-face-4 :background ,electric-ice-pink
                                            :foreground ,electric-ice-bg)
               (ivy-confirm-face :foreground ,electric-ice-orange)
               (ivy-match-required-face :foreground ,electric-ice-red)
               (ivy-subdir :foreground ,electric-ice-yellow)
               (ivy-remote :foreground ,electric-ice-pink)
               (ivy-virtual :foreground ,electric-ice-cyan)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,electric-ice-bg :background ,electric-ice-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,electric-ice-cyan)
               (jde-java-font-lock-modifier-face :foreground ,electric-ice-pink)
               (jde-java-font-lock-number-face :foreground ,electric-ice-fg)
               (jde-java-font-lock-package-face :foreground ,electric-ice-fg)
               (jde-java-font-lock-private-face :foreground ,electric-ice-pink)
               (jde-java-font-lock-public-face :foreground ,electric-ice-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,electric-ice-purple)
               (js2-function-param :foreground ,electric-ice-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,electric-ice-yellow)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,electric-ice-yellow)
               (js2-private-function-call :foreground ,electric-ice-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,electric-ice-orange)
               (js3-external-variable-face :foreground ,electric-ice-fg)
               (js3-function-param-face :foreground ,electric-ice-pink)
               (js3-instance-member-face :foreground ,electric-ice-cyan)
               (js3-jsdoc-tag-face :foreground ,electric-ice-pink)
               (js3-warning-face :underline ,electric-ice-pink)
               ;; lsp
               (lsp-ui-peek-peek :background ,electric-ice-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,electric-ice-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,electric-ice-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,electric-ice-bg)
               (lsp-ui-peek-header :background ,bg3 :foreground ,fg3, :weight bold)
               (lsp-ui-peek-felectric-iceter :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,electric-ice-fg :weight ultra-bold
                                               :box (:line-width -1 :color electric-ice-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,electric-ice-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,electric-ice-bg)
               (lsp-ui-doc-header :foreground ,electric-ice-bg :background ,electric-ice-cyan)
               ;; magit
               (magit-branch-local :foreground ,electric-ice-cyan)
               (magit-branch-remote :foreground ,electric-ice-green)
               (magit-tag :foreground ,electric-ice-orange)
               (magit-section-heading :foreground ,electric-ice-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,electric-ice-orange
                                            :background ,electric-ice-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,electric-ice-orange
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
               (magit-diff-file-heading :foreground ,electric-ice-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,electric-ice-green)
               (magit-diffstat-removed :foreground ,electric-ice-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,electric-ice-orange :weight bold)
               (magit-process-ok :foreground ,electric-ice-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,electric-ice-yellow
                                         :slant italic)
               (markdown-code-face :foreground ,electric-ice-orange)
               (markdown-felectric-icetnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,electric-ice-pink
                ,@(when electric-ice-enlarge-headings
                    (list :height electric-ice-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,electric-ice-purple
                ,@(when electric-ice-enlarge-headings
                    (list :height electric-ice-height-title-2)))
               (markdown-header-face-3
                :foreground ,electric-ice-green
                ,@(when electric-ice-enlarge-headings
                    (list :height electric-ice-height-title-3)))
               (markdown-header-face-4 :foreground ,electric-ice-yellow)
               (markdown-header-face-5 :foreground ,electric-ice-cyan)
               (markdown-header-face-6 :foreground ,electric-ice-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,electric-ice-fg)
               (markdown-inline-code-face :foreground ,electric-ice-green)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,electric-ice-orange)
               (markdown-table-face :foreground ,electric-ice-purple)
               (markdown-list-face :foreground ,electric-ice-cyan)
               (markdown-language-keyword-face :foreground ,electric-ice-comment)
               ;; message
               (message-header-to :foreground ,electric-ice-fg :weight bold)
               (message-header-cc :foreground ,electric-ice-fg :bold bold)
               (message-header-subject :foreground ,electric-ice-orange)
               (message-header-newsgroups :foreground ,electric-ice-purple)
               (message-header-other :foreground ,electric-ice-purple)
               (message-header-name :foreground ,electric-ice-green)
               (message-header-xheader :foreground ,electric-ice-cyan)
               (message-separator :foreground ,electric-ice-cyan :slant italic)
               (message-cited-text :foreground ,electric-ice-purple)
               (message-cited-text-1 :foreground ,electric-ice-purple)
               (message-cited-text-2 :foreground ,electric-ice-orange)
               (message-cited-text-3 :foreground ,electric-ice-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,electric-ice-green :weight normal)
               ;; mini-modeline
               (mini-modeline-mode-line :inherit mode-line :height 0.1 :box nil)
               ;; mu4e
               (mu4e-unread-face :foreground ,electric-ice-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,electric-ice-purple)
               (mu4e-highlight-face :background ,electric-ice-bg
                                    :foreground ,electric-ice-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,electric-ice-current
                                           :foreground ,electric-ice-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,electric-ice-purple)
               (mu4e-cited-1-face :foreground ,electric-ice-purple)
               (mu4e-cited-2-face :foreground ,electric-ice-orange)
               (mu4e-cited-3-face :foreground ,electric-ice-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; neotree
               (neo-banner-face :foreground ,electric-ice-orange :weight bold)
               ;;(neo-button-face :underline nil)
               (neo-dir-link-face :foreground ,electric-ice-purple)
               (neo-expand-btn-face :foreground ,electric-ice-fg)
               (neo-file-link-face :foreground ,electric-ice-cyan)
               (neo-header-face :background ,electric-ice-bg
                                :foreground ,electric-ice-fg
                                :weight bold)
               (neo-relectric-icet-dir-face :foreground ,electric-ice-purple :weight bold)
               (neo-vc-added-face :foreground ,electric-ice-orange)
               (neo-vc-conflict-face :foreground ,electric-ice-red)
               (neo-vc-default-face :inherit neo-file-link-face)
               (neo-vc-edited-face :foreground ,electric-ice-orange)
               (neo-vc-ignored-face :foreground ,electric-ice-comment)
               (neo-vc-missing-face :foreground ,electric-ice-red)
               (neo-vc-needs-merge-face :foreground ,electric-ice-red
                                        :weight bold)
               ;;(neo-vc-needs-update-face :underline t)
               ;;(neo-vc-removed-face :strike-through t)
               (neo-vc-unlocked-changes-face :foreground ,electric-ice-red)
               ;;(neo-vc-unregistered-face nil)
               (neo-vc-up-to-date-face :foreground ,electric-ice-green)
               (neo-vc-user-face :foreground ,electric-ice-purple)
               ;; org
               (org-agenda-date :foreground ,electric-ice-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,electric-ice-comment)
               (org-agenda-done :foreground ,electric-ice-green)
               (org-agenda-structure :foreground ,electric-ice-purple)
               (org-block :foreground ,electric-ice-orange)
               (org-code :foreground ,electric-ice-green)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,electric-ice-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,electric-ice-comment)
               (org-document-title :weight bold :foreground ,electric-ice-orange
                                   ,@(when electric-ice-enlarge-headings
                                       (list :height electric-ice-height-doc-title)))
               (org-done :foreground ,electric-ice-green)
               (org-ellipsis :foreground ,electric-ice-comment)
               (org-felectric-icetnote :foreground ,other-blue)
               (org-formula :foreground ,electric-ice-pink)
               (org-headline-done :foreground ,electric-ice-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,electric-ice-bg :background ,electric-ice-bg)
               (org-level-1 :inherit bold :foreground ,electric-ice-pink
                            ,@(when electric-ice-enlarge-headings
                                (list :height electric-ice-height-title-1)))
               (org-level-2 :inherit bold :foreground ,electric-ice-purple
                            ,@(when electric-ice-enlarge-headings
                                (list :height electric-ice-height-title-2)))
               (org-level-3 :weight normal :foreground ,electric-ice-green
                            ,@(when electric-ice-enlarge-headings
                                (list :height electric-ice-height-title-3)))
               (org-level-4 :weight normal :foreground ,electric-ice-yellow)
               (org-level-5 :weight normal :foreground ,electric-ice-cyan)
               (org-level-6 :weight normal :foreground ,electric-ice-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,electric-ice-fg)
               (org-link :foreground ,electric-ice-cyan :underline t)
               (org-priority :foreground ,electric-ice-cyan)
               (org-quote :foreground ,electric-ice-yellow :slant italic)
               (org-scheduled :foreground ,electric-ice-green)
               (org-scheduled-previously :foreground ,electric-ice-yellow)
               (org-scheduled-today :foreground ,electric-ice-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,electric-ice-yellow)
               (org-table :foreground ,electric-ice-purple)
               (org-tag :foreground ,electric-ice-pink :weight bold :background ,bg2)
               (org-todo :foreground ,electric-ice-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,electric-ice-yellow)
               (org-verbatim :inherit org-quote)
               (org-warning :weight bold :foreground ,electric-ice-pink)
               ;; outline
               (outline-1 :foreground ,electric-ice-pink)
               (outline-2 :foreground ,electric-ice-purple)
               (outline-3 :foreground ,electric-ice-green)
               (outline-4 :foreground ,electric-ice-yellow)
               (outline-5 :foreground ,electric-ice-cyan)
               (outline-6 :foreground ,electric-ice-orange)
               ;; perspective
               (persp-selected-face :weight bold :foreground ,electric-ice-pink)
               ;; powerline
               (powerline-active1 :background ,electric-ice-bg :foreground ,electric-ice-pink)
               (powerline-active2 :background ,electric-ice-bg :foreground ,electric-ice-pink)
               (powerline-inactive1 :background ,bg2 :foreground ,electric-ice-purple)
               (powerline-inactive2 :background ,bg2 :foreground ,electric-ice-purple)
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,electric-ice-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,electric-ice-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,electric-ice-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,electric-ice-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,electric-ice-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,electric-ice-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,electric-ice-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,electric-ice-fg)
               (rainbow-delimiters-depth-2-face :foreground ,electric-ice-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,electric-ice-purple)
               (rainbow-delimiters-depth-4-face :foreground ,electric-ice-pink)
               (rainbow-delimiters-depth-5-face :foreground ,electric-ice-orange)
               (rainbow-delimiters-depth-6-face :foreground ,electric-ice-green)
               (rainbow-delimiters-depth-7-face :foreground ,electric-ice-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,electric-ice-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,electric-ice-green)
               (rpm-spec-doc-face :foreground ,electric-ice-pink)
               (rpm-spec-ghost-face :foreground ,electric-ice-purple)
               (rpm-spec-macro-face :foreground ,electric-ice-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,electric-ice-purple)
               (rpm-spec-section-face :foreground ,electric-ice-yellow)
               (rpm-spec-tag-face :foreground ,electric-ice-cyan)
               (rpm-spec-var-face :foreground ,electric-ice-orange)
               ;; rst (reStructuredText)
               (rst-level-1 :foreground ,electric-ice-pink :weight bold)
               (rst-level-2 :foreground ,electric-ice-purple :weight bold)
               (rst-level-3 :foreground ,electric-ice-green)
               (rst-level-4 :foreground ,electric-ice-yellow)
               (rst-level-5 :foreground ,electric-ice-cyan)
               (rst-level-6 :foreground ,electric-ice-orange)
               (rst-level-7 :foreground ,other-blue)
               (rst-level-8 :foreground ,electric-ice-fg)
               ;; selectrum-mode
               (selectrum-current-candidate :weight bold)
               (selectrum-primary-highlight :foreground ,electric-ice-pink)
               (selectrum-secondary-highlight :foreground ,electric-ice-green)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,electric-ice-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,electric-ice-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,electric-ice-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,electric-ice-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,electric-ice-green)
               (speedbar-file-face :foreground ,electric-ice-cyan)
               (speedbar-directory-face :foreground ,electric-ice-purple)
               (speedbar-tag-face :foreground ,electric-ice-yellow)
               (speedbar-selected-face :foreground ,electric-ice-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,electric-ice-bg
                                        :foreground ,electric-ice-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,electric-ice-purple :background ,electric-ice-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,electric-ice-pink :background ,electric-ice-bg
                            :box (:line-width 2 :color ,electric-ice-bg :style nil))
               (tab-bar-tab-inactive :foreground ,electric-ice-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,electric-ice-purple :background ,electric-ice-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,electric-ice-pink :background ,electric-ice-bg
                             :box (:line-width 2 :color ,electric-ice-bg :style nil))
               (tab-line-tab-inactive :foreground ,electric-ice-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,electric-ice-red)
               ;; telephone-line
               (telephone-line-accent-active :background ,electric-ice-bg :foreground ,electric-ice-pink)
               (telephone-line-accent-inactive :background ,bg2 :foreground ,electric-ice-purple)
               (telephone-line-unimportant :background ,electric-ice-bg :foreground ,electric-ice-comment)
               ;; term
               (term :foreground ,electric-ice-fg :background ,electric-ice-bg)
               (term-color-black :foreground ,electric-ice-bg :background ,electric-ice-comment)
               (term-color-blue :foreground ,electric-ice-purple :background ,electric-ice-purple)
               (term-color-cyan :foreground ,electric-ice-cyan :background ,electric-ice-cyan)
               (term-color-green :foreground ,electric-ice-green :background ,electric-ice-green)
               (term-color-magenta :foreground ,electric-ice-pink :background ,electric-ice-pink)
               (term-color-red :foreground ,electric-ice-red :background ,electric-ice-red)
               (term-color-white :foreground ,electric-ice-fg :background ,electric-ice-fg)
               (term-color-yellow :foreground ,electric-ice-yellow :background ,electric-ice-yellow)
               ;; tree-sitter
               (tree-sitter-hl-face:attribute :inherit font-lock-constant-face)
               (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
               (tree-sitter-hl-face:constant :inherit font-lock-constant-face)
               (tree-sitter-hl-face:constant.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:constructor :inherit font-lock-constant-face)
               (tree-sitter-hl-face:escape :foreground ,electric-ice-pink)
               (tree-sitter-hl-face:function :inherit font-lock-function-name-face)
               (tree-sitter-hl-face:function.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:function.call :inherit font-lock-function-name-face
                                                  :weight normal)
               (tree-sitter-hl-face:function.macro :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:function.special :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:keyword :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:punctuation :foreground ,electric-ice-pink)
               (tree-sitter-hl-face:punctuation.bracket :foreground ,electric-ice-fg)
               (tree-sitter-hl-face:punctuation.delimiter :foreground ,electric-ice-fg)
               (tree-sitter-hl-face:punctuation.special :foreground ,electric-ice-pink)
               (tree-sitter-hl-face:string :inherit font-lock-string-face)
               (tree-sitter-hl-face:string.special :foreground ,electric-ice-red)
               (tree-sitter-hl-face:tag :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:type :inherit font-lock-type-face)
               (tree-sitter-hl-face:type.parameter :foreground ,electric-ice-pink)
               (tree-sitter-hl-face:variable :inherit font-lock-variable-name-face)
               (tree-sitter-hl-face:variable.parameter :inherit tree-sitter-hl-face:variable
                                                       :weight normal)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,electric-ice-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,electric-ice-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,electric-ice-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit font-lock-builtin-face)
               (web-mode-comment-face :inherit font-lock-comment-face)
               (web-mode-constant-face :inherit font-lock-constant-face)
               (web-mode-css-property-name-face :inherit font-lock-constant-face)
               (web-mode-doctype-face :inherit font-lock-comment-face)
               (web-mode-function-name-face :inherit font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,electric-ice-purple)
               (web-mode-html-attr-value-face :foreground ,electric-ice-green)
               (web-mode-html-tag-face :foreground ,electric-ice-pink)
               (web-mode-keyword-face :foreground ,electric-ice-pink)
               (web-mode-string-face :foreground ,electric-ice-cyan)
               (web-mode-type-face :inherit font-lock-type-face)
               (web-mode-warning-face :inherit font-lock-warning-face)
               (web-mode-current-element-highlight-face :foreground ,electric-ice-cyan)
               (web-mode-current-column-highlight-face :background ,electric-ice-cyan)
               ;; which-func
               (which-func :inherit font-lock-function-name-face)
               ;; which-key
               (which-key-key-face :inherit font-lock-builtin-face)
               (which-key-command-description-face :inherit default)
               (which-key-separator-face :inherit font-lock-comment-delimiter-face)
               (which-key-local-map-description-face :foreground ,electric-ice-green)
               ;; whitespace
               (whitespace-big-indent :foreground ,electric-ice-red)
               (whitespace-empty :foreground ,electric-ice-red)
               (whitespace-hspace :background ,bg3 :foreground ,electric-ice-comment)
               (whitespace-indentation :foreground ,electric-ice-red)
               (whitespace-line :background ,electric-ice-bg :foreground ,electric-ice-pink)
               (whitespace-newline :foreground ,electric-ice-bg)
               (whitespace-space :background ,electric-ice-current :foreground ,electric-ice-bg)
               (whitespace-space-after-tab :foreground ,electric-ice-red)
               (whitespace-space-before-tab :foreground ,electric-ice-red)
               (whitespace-tab :background ,electric-ice-current :foreground ,electric-ice-bg)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit font-lock-builtin-face)
               (yard-directive-face :inherit font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'electric-ice
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
                       ,(if electric-ice-use-24-bit-colors-on-256-colors-terms
                            (funcall expand-with-func 'cadr spec)
                          (funcall expand-with-func 'caddr spec)))
                      (t                       ; should be only tty-like envs
                       ,(funcall expand-with-func 'cadddr spec))))
                   whole-theme))
           whole-theme))

  (apply #'custom-theme-set-variables
         'electric-ice
         (let ((get-func
                (pcase (display-color-cells)
                  ((pred (<= 16777216)) 'car) ; fully graphical envs
                  ((pred (<= 256)) 'cadr)     ; terminal withs 256 colors
                  (_ 'caddr))))               ; should be only tty-like envs
           `((ansi-color-names-vector
              [,(funcall get-func (alist-get 'electric-ice-bg colors))
               ,(funcall get-func (alist-get 'electric-ice-red colors))
               ,(funcall get-func (alist-get 'electric-ice-green colors))
               ,(funcall get-func (alist-get 'electric-ice-yellow colors))
               ,(funcall get-func (alist-get 'electric-ice-comment colors))
               ,(funcall get-func (alist-get 'electric-ice-purple colors))
               ,(funcall get-func (alist-get 'electric-ice-cyan colors))
               ,(funcall get-func (alist-get 'electric-ice-fg colors))])))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'electric-ice)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; electric-ice-theme.el ends here
