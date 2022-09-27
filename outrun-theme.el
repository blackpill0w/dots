;;; outrun-theme.el --- Outrun Theme

;;; Code:
(deftheme outrun)


;;;; Configuration options:

(defgroup outrun nil
  "Outrun theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom outrun-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'boutrunlean
  :group 'outrun)

(defcustom outrun-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'outrun)

(defcustom outrun-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'outrun)

(defcustom outrun-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'outrun)

(defcustom outrun-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'outrun)

(defcustom outrun-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'boutrunlean
  :group 'outrun)

(defvar outrun-use-24-bit-colors-on-256-colors-terms nil)


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (outrun-bg            "#0c0a20" "unspecified-bg" "unspecified-bg") ; official background
                (outrun-fg            "#ffffff" "#ffffff" "brightwhite") ; official foreground
                (outrun-current       "#44475a" "#303030" "brightblack") ; official current-line/selection
                (outrun-comment       "#6272A4" "#6272A4" "blue")        ; official comment
                (outrun-cyan          "#1ea8fc" "#1ea8fc" "cyan")  ; official cyan
                (outrun-brightcyan    "#70f0ff" "#70f0ff" "brightcyan")  ; official cyan
                (outrun-green         "#50fa7b" "#5fff87" "green")       ; official green
                (outrun-orange        "#ffb86c" "#ffaf5f" "brightred")   ; official orange
                (outrun-pink          "#FF0081" "#FF0081" "magenta")     ; official pink
                (outrun-purple        "#D86BFF" "#A875FF" "brightmagenta") ; official purple
                (outrun-red           "#ff5555" "#ff8787" "red")         ; official red
                (outrun-yellow        "#ffff50" "#ffff87" "yellow")      ; official yellow
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
               (default :background ,outrun-bg :foreground ,outrun-fg)
               (default-italic :slant italic)
               (error :foreground ,outrun-red)
               (ffap :foreground ,fg4)
               (fringe :background ,outrun-bg :foreground ,fg4)
               (header-line :inherit 'mode-line)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,outrun-cyan :extend t)
               (info-quoted-name :foreground ,outrun-orange)
               (info-string :foreground ,outrun-cyan)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,outrun-cyan :underline t)
               (linum :slant italic :foreground ,outrun-comment :background ,outrun-bg)
               (line-number :slant italic :foreground ,outrun-comment :background ,outrun-bg)
               (line-number-current-line :slant italic :foreground ,outrun-cyan :background ,outrun-bg)
               (nlinum-current-line :slant italic :foreground ,outrun-cyan :background ,outrun-bg)
               (match :background ,outrun-yellow :foreground ,outrun-bg)
               (menu :background ,outrun-current :inverse-video nil
                     ,@(if outrun-alternate-mode-line-and-minibuffer
                           (list :foreground fg3)
                         (list :foreground outrun-fg)))
               (minibuffer-prompt
                ,@(if outrun-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground outrun-fg)
                    (list  :foreground outrun-pink :background outrun-bg)))
               (mode-line :background ,outrun-current
                          :box ,outrun-current :inverse-video nil
                          ,@(if outrun-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground outrun-fg)))
               (mode-line-inactive
                :background ,outrun-bg :inverse-video nil
                ,@(if outrun-alternate-mode-line-and-minibuffer
                      (list :foreground outrun-comment :box outrun-bg)
                    (list :foreground fg4 :box bg2)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :inherit match :extend t)
               (shadow :foreground ,outrun-comment)
               (success :foreground ,outrun-green)
               (toutrunltip :foreground ,outrun-fg :background ,outrun-current)
               (trailing-whitespace :background ,outrun-orange)
               (vertical-border :foreground ,bg2)
               (warning :foreground ,outrun-orange)
               ;; syntax / font-lock
               (font-lock-builtin-face :foreground ,outrun-cyan :slant italic)
               (font-lock-comment-face :foreground ,outrun-comment)
               (font-lock-comment-delimiter-face :inherit shadow)
               (font-lock-constant-face :foreground ,outrun-purple)
               (font-lock-doc-face :foreground ,outrun-comment)
               (font-lock-function-name-face :foreground ,outrun-yellow :weight normal)
               (font-lock-keyword-face :foreground ,outrun-pink :weight normal)
               (font-lock-negation-char-face :foreground ,outrun-pink)
               (font-lock-preprocessor-face :foreground ,outrun-pink)
               (font-lock-reference-face :inherit font-lock-constant-face) ;; obsolete
               (font-lock-regexp-grouping-backslash :foreground ,outrun-cyan)
               (font-lock-regexp-grouping-construct :foreground ,outrun-purple)
               (font-lock-string-face :foreground ,outrun-brightcyan)
               (font-lock-type-face :inherit font-lock-builtin-face :foreground ,outrun-cyan)
               (font-lock-variable-name-face :foreground ,outrun-fg :weight normal)
               (font-lock-warning-face :inherit warning :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,outrun-pink)
               ;; company
               (company-echo-common :foreground ,outrun-bg :background ,outrun-fg)
               (company-preview :background ,outrun-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,outrun-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,outrun-green)
               (company-scrollbar-bg :background ,outrun-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,outrun-fg :background ,bg2)
               (company-tooltip-common :foreground ,outrun-purple)
               (company-tooltip-selection :background ,outrun-current)
               (company-toutrunltip :inherit toutrunltip)
               (company-toutrunltip-search :foreground ,outrun-green
                                           :underline t)
               (company-toutrunltip-search-selection :background ,outrun-green
                                                     :foreground ,outrun-bg)
               (company-toutrunltip-selection :inherit match)
               (company-toutrunltip-mouse :background ,outrun-bg)
               (company-toutrunltip-common :foreground ,outrun-pink)
               ;;(company-toutrunltip-common-selection :inherit company-toutrunltip-common)
               (company-toutrunltip-annotation :foreground ,outrun-cyan)
               ;;(company-toutrunltip-annotation-selection :inherit company-toutrunltip-annotation)
               ;; completions (minibuffer.el)
               (completions-annotations :inherit font-lock-comment-face)
               (completions-common-part :foreground ,outrun-pink)
               (completions-first-difference :foreground ,outrun-fg)
               ;; diff-hl
               (diff-hl-change :foreground ,outrun-orange :background ,outrun-orange)
               (diff-hl-delete :foreground ,outrun-red :background ,outrun-red)
               (diff-hl-insert :foreground ,outrun-green :background ,outrun-green)
               ;; dired
               (dired-directory :foreground ,outrun-green :weight normal)
               (dired-flagged :foreground ,outrun-pink)
               (dired-header :foreground ,fg3 :background ,outrun-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,outrun-fg)
               (dired-marked :foreground ,outrun-orange)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,outrun-yellow :weight normal :slant italic)
               (dired-warning :foreground ,outrun-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,outrun-fg)
               (diredp-deletion-file-name :foreground ,outrun-pink :background ,outrun-current)
               (diredp-deletion :foreground ,outrun-pink)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,outrun-orange)
               (diredp-file-name :foreground ,outrun-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,outrun-current)
               (diredp-flag-mark :foreground ,fg2 :background ,outrun-current)
               (diredp-ignored-file-name :foreground ,outrun-fg)
               (diredp-mode-line-flagged :foreground ,outrun-orange)
               (diredp-mode-line-marked :foreground ,outrun-orange)
               (diredp-no-priv :foreground ,outrun-fg)
               (diredp-number :foreground ,outrun-cyan)
               (diredp-other-priv :foreground ,outrun-orange)
               (diredp-rare-priv :foreground ,outrun-orange)
               (diredp-read-priv :foreground ,outrun-purple)
               (diredp-write-priv :foreground ,outrun-pink)
               (diredp-exec-priv :foreground ,outrun-yellow)
               (diredp-symlink :foreground ,outrun-orange)
               (diredp-link-priv :foreground ,outrun-orange)
               (diredp-autofile-name :foreground ,outrun-yellow)
               (diredp-tagged-autofile-name :foreground ,outrun-yellow)
               ;; eldoc-box
               (eldoc-box-border :background ,outrun-current)
               (eldoc-box-body :background ,outrun-current)
               ;; elfeed
               (elfeed-search-date-face :foreground ,outrun-comment)
               (elfeed-search-title-face :foreground ,outrun-fg)
               (elfeed-search-unread-title-face :foreground ,outrun-pink)
               (elfeed-search-feed-face :foreground ,outrun-fg)
               (elfeed-search-tag-face :foreground ,outrun-green)

               (elfeed-search-unread-count-face :foreground ,outrun-pink)
               (elfeed-search-filter-face :foreground ,outrun-green)
               ;;(elfeed-log-date-face :inherit font-lock-type-face)
               (elfeed-log-error-level-face :foreground ,outrun-red)
               (elfeed-log-warn-level-face :foreground ,outrun-orange)
               (elfeed-log-info-level-face :foreground ,outrun-cyan)
               (elfeed-log-debug-level-face :foreground ,outrun-comment)
               ;; elpher
               (elpher-gemini-heading1 :inherit bold :foreground ,outrun-pink
                                       ,@(when outrun-enlarge-headings
                                           (list :height outrun-height-title-1)))
               (elpher-gemini-heading2 :inherit bold :foreground ,outrun-purple
                                       ,@(when outrun-enlarge-headings
                                           (list :height outrun-height-title-2)))
               (elpher-gemini-heading3 :weight normal :foreground ,outrun-green
                                       ,@(when outrun-enlarge-headings
                                           (list :height outrun-height-title-3)))
               (elpher-gemini-preformatted :inherit fixed-pitch
                                           :foreground ,outrun-orange)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,outrun-yellow)
               (enh-ruby-op-face :foreground ,outrun-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,outrun-yellow)
               (enh-ruby-string-delimiter-face :foreground ,outrun-cyan)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,outrun-orange))
               (flyspell-incorrect :underline (:style wave :color ,outrun-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,outrun-purple :weight 'bold)
               (font-latex-italic-face :foreground ,outrun-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,outrun-cyan)
               (font-latex-match-variable-keywords :foreground ,outrun-fg)
               (font-latex-string-face :foreground ,outrun-cyan)
               ;; gemini
               (gemini-heading-face-1 :inherit bold :foreground ,outrun-pink
                                      ,@(when outrun-enlarge-headings
                                          (list :height outrun-height-title-1)))
               (gemini-heading-face-2 :inherit bold :foreground ,outrun-purple
                                      ,@(when outrun-enlarge-headings
                                          (list :height outrun-height-title-2)))
               (gemini-heading-face-3 :weight normal :foreground ,outrun-green
                                      ,@(when outrun-enlarge-headings
                                          (list :height outrun-height-title-3)))
               (gemini-heading-face-rest :weight normal :foreground ,outrun-yellow)
               (gemini-quote-face :foreground ,outrun-purple)
               ;; go-test
               (go-test--ok-face :inherit success)
               (go-test--error-face :inherit error)
               (go-test--warning-face :inherit warning)
               (go-test--pointer-face :foreground ,outrun-pink)
               (go-test--standard-face :foreground ,outrun-cyan)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,outrun-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,outrun-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,outrun-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,outrun-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,outrun-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,outrun-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,outrun-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,outrun-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,outrun-purple)
               (gnus-header-from :foreground ,outrun-fg)
               (gnus-header-name :foreground ,outrun-green)
               (gnus-header-subject :foreground ,outrun-pink :weight bold)
               (gnus-summary-markup-face :foreground ,outrun-cyan)
               (gnus-summary-high-unread :foreground ,outrun-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,outrun-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,outrun-pink :weight bold)
               (gnus-summary-low-unread :foreground ,outrun-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,outrun-pink)
               (haskell-constructor-face :foreground ,outrun-purple)
               ;; helm
               (helm-boutrunkmark-w3m :foreground ,outrun-purple)
               (helm-buffer-not-saved :foreground ,outrun-purple :background ,outrun-bg)
               (helm-buffer-process :foreground ,outrun-orange :background ,outrun-bg)
               (helm-buffer-saved-out :foreground ,outrun-fg :background ,outrun-bg)
               (helm-buffer-size :foreground ,outrun-fg :background ,outrun-bg)
               (helm-candidate-number :foreground ,outrun-bg :background ,outrun-fg)
               (helm-ff-directory :foreground ,outrun-green :background ,outrun-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,outrun-green :background ,outrun-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,outrun-bg :weight normal)
               (helm-ff-file :foreground ,outrun-fg :background ,outrun-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,outrun-pink :background ,outrun-bg :weight bold)
               (helm-ff-prefix :foreground ,outrun-bg :background ,outrun-pink :weight normal)
               (helm-ff-symlink :foreground ,outrun-pink :background ,outrun-bg :weight bold)
               (helm-grep-cmd-line :foreground ,outrun-fg :background ,outrun-bg)
               (helm-grep-file :foreground ,outrun-fg :background ,outrun-bg)
               (helm-grep-finish :foreground ,fg2 :background ,outrun-bg)
               (helm-grep-lineno :foreground ,outrun-fg :background ,outrun-bg)
               (helm-grep-match :inherit match)
               (helm-grep-running :foreground ,outrun-green :background ,outrun-bg)
               (helm-header :foreground ,fg2 :background ,outrun-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,outrun-green :background ,outrun-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,outrun-purple :background ,outrun-bg)
               (helm-source-go-package-godoc-description :foreground ,outrun-yellow)
               (helm-source-header :foreground ,outrun-pink :background ,outrun-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,outrun-orange :background ,outrun-bg)
               (helm-time-zone-home :foreground ,outrun-purple :background ,outrun-bg)
               (helm-visible-mark :foreground ,outrun-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,outrun-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,outrun-fg)
               (icicle-search-current-input :foreground ,outrun-pink)
               (icicle-search-context-level-8 :foreground ,outrun-orange)
               (icicle-search-context-level-7 :foreground ,outrun-orange)
               (icicle-search-context-level-6 :foreground ,outrun-orange)
               (icicle-search-context-level-5 :foreground ,outrun-orange)
               (icicle-search-context-level-4 :foreground ,outrun-orange)
               (icicle-search-context-level-3 :foreground ,outrun-orange)
               (icicle-search-context-level-2 :foreground ,outrun-orange)
               (icicle-search-context-level-1 :foreground ,outrun-orange)
               (icicle-search-main-regexp-current :foreground ,outrun-fg)
               (icicle-saved-candidate :foreground ,outrun-fg)
               (icicle-proxy-candidate :foreground ,outrun-fg)
               (icicle-mustmatch-completion :foreground ,outrun-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,outrun-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,outrun-orange)
               (icicle-match-highlight-Completions :foreground ,outrun-green)
               (icicle-key-complete-menu-local :foreground ,outrun-fg)
               (icicle-key-complete-menu :foreground ,outrun-fg)
               (icicle-input-completion-fail-lax :foreground ,outrun-pink)
               (icicle-input-completion-fail :foreground ,outrun-pink)
               (icicle-historical-candidate-other :foreground ,outrun-fg)
               (icicle-historical-candidate :foreground ,outrun-fg)
               (icicle-current-candidate-highlight :foreground ,outrun-orange :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,outrun-fg)
               (icicle-complete-input :foreground ,outrun-orange)
               (icicle-common-match-highlight-Completions :foreground ,outrun-purple)
               (icicle-candidate-part :foreground ,outrun-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,outrun-orange)
               ;; ido
               (ido-first-match
                ,@(if outrun-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground outrun-green)
                    (list  :foreground outrun-pink)))
               (ido-only-match :foreground ,outrun-orange)
               (ido-subdir :foreground ,outrun-yellow)
               (ido-virtual :foreground ,outrun-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,outrun-fg :background ,outrun-pink)
               ;; ivy
               (ivy-current-match
                ,@(if outrun-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :background outrun-current :foreground outrun-green)
                    (list :background outrun-current :foreground outrun-pink)))
               ;; Highlights the background of the match.
               (ivy-minibuffer-match-face-1 :background ,outrun-current)
               ;; Highlights the first matched group.
               (ivy-minibuffer-match-face-2 :background ,outrun-green
                                            :foreground ,outrun-bg)
               ;; Highlights the second matched group.
               (ivy-minibuffer-match-face-3 :background ,outrun-yellow
                                            :foreground ,outrun-bg)
               ;; Highlights the third matched group.
               (ivy-minibuffer-match-face-4 :background ,outrun-pink
                                            :foreground ,outrun-bg)
               (ivy-confirm-face :foreground ,outrun-orange)
               (ivy-match-required-face :foreground ,outrun-red)
               (ivy-subdir :foreground ,outrun-yellow)
               (ivy-remote :foreground ,outrun-pink)
               (ivy-virtual :foreground ,outrun-cyan)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,outrun-bg :background ,outrun-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,outrun-cyan)
               (jde-java-font-lock-modifier-face :foreground ,outrun-pink)
               (jde-java-font-lock-number-face :foreground ,outrun-fg)
               (jde-java-font-lock-package-face :foreground ,outrun-fg)
               (jde-java-font-lock-private-face :foreground ,outrun-pink)
               (jde-java-font-lock-public-face :foreground ,outrun-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,outrun-purple)
               (js2-function-param :foreground ,outrun-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,outrun-yellow)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,outrun-yellow)
               (js2-private-function-call :foreground ,outrun-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,outrun-orange)
               (js3-external-variable-face :foreground ,outrun-fg)
               (js3-function-param-face :foreground ,outrun-pink)
               (js3-instance-member-face :foreground ,outrun-cyan)
               (js3-jsdoc-tag-face :foreground ,outrun-pink)
               (js3-warning-face :underline ,outrun-pink)
               ;; lsp
               (lsp-ui-peek-peek :background ,outrun-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,outrun-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,outrun-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,outrun-bg)
               (lsp-ui-peek-header :background ,bg3 :foreground ,fg3, :weight bold)
               (lsp-ui-peek-foutrunter :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,outrun-fg :weight ultra-bold
                                               :box (:line-width -1 :color outrun-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,outrun-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,outrun-bg)
               (lsp-ui-doc-header :foreground ,outrun-bg :background ,outrun-cyan)
               ;; magit
               (magit-branch-local :foreground ,outrun-cyan)
               (magit-branch-remote :foreground ,outrun-green)
               (magit-tag :foreground ,outrun-orange)
               (magit-section-heading :foreground ,outrun-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,outrun-orange
                                            :background ,outrun-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,outrun-orange
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
               (magit-diff-file-heading :foreground ,outrun-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,outrun-green)
               (magit-diffstat-removed :foreground ,outrun-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,outrun-orange :weight bold)
               (magit-process-ok :foreground ,outrun-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,outrun-yellow
                                         :slant italic)
               (markdown-code-face :foreground ,outrun-orange)
               (markdown-foutruntnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,outrun-pink
                ,@(when outrun-enlarge-headings
                    (list :height outrun-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,outrun-purple
                ,@(when outrun-enlarge-headings
                    (list :height outrun-height-title-2)))
               (markdown-header-face-3
                :foreground ,outrun-green
                ,@(when outrun-enlarge-headings
                    (list :height outrun-height-title-3)))
               (markdown-header-face-4 :foreground ,outrun-yellow)
               (markdown-header-face-5 :foreground ,outrun-cyan)
               (markdown-header-face-6 :foreground ,outrun-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,outrun-fg)
               (markdown-inline-code-face :foreground ,outrun-green)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,outrun-orange)
               (markdown-table-face :foreground ,outrun-purple)
               (markdown-list-face :foreground ,outrun-cyan)
               (markdown-language-keyword-face :foreground ,outrun-comment)
               ;; message
               (message-header-to :foreground ,outrun-fg :weight bold)
               (message-header-cc :foreground ,outrun-fg :bold bold)
               (message-header-subject :foreground ,outrun-orange)
               (message-header-newsgroups :foreground ,outrun-purple)
               (message-header-other :foreground ,outrun-purple)
               (message-header-name :foreground ,outrun-green)
               (message-header-xheader :foreground ,outrun-cyan)
               (message-separator :foreground ,outrun-cyan :slant italic)
               (message-cited-text :foreground ,outrun-purple)
               (message-cited-text-1 :foreground ,outrun-purple)
               (message-cited-text-2 :foreground ,outrun-orange)
               (message-cited-text-3 :foreground ,outrun-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,outrun-green :weight normal)
               ;; mini-modeline
               (mini-modeline-mode-line :inherit mode-line :height 0.1 :box nil)
               ;; mu4e
               (mu4e-unread-face :foreground ,outrun-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,outrun-purple)
               (mu4e-highlight-face :background ,outrun-bg
                                    :foreground ,outrun-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,outrun-current
                                           :foreground ,outrun-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,outrun-purple)
               (mu4e-cited-1-face :foreground ,outrun-purple)
               (mu4e-cited-2-face :foreground ,outrun-orange)
               (mu4e-cited-3-face :foreground ,outrun-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; neotree
               (neo-banner-face :foreground ,outrun-orange :weight bold)
               ;;(neo-button-face :underline nil)
               (neo-dir-link-face :foreground ,outrun-purple)
               (neo-expand-btn-face :foreground ,outrun-fg)
               (neo-file-link-face :foreground ,outrun-cyan)
               (neo-header-face :background ,outrun-bg
                                :foreground ,outrun-fg
                                :weight bold)
               (neo-routrunt-dir-face :foreground ,outrun-purple :weight bold)
               (neo-vc-added-face :foreground ,outrun-orange)
               (neo-vc-conflict-face :foreground ,outrun-red)
               (neo-vc-default-face :inherit neo-file-link-face)
               (neo-vc-edited-face :foreground ,outrun-orange)
               (neo-vc-ignored-face :foreground ,outrun-comment)
               (neo-vc-missing-face :foreground ,outrun-red)
               (neo-vc-needs-merge-face :foreground ,outrun-red
                                        :weight bold)
               ;;(neo-vc-needs-update-face :underline t)
               ;;(neo-vc-removed-face :strike-through t)
               (neo-vc-unlocked-changes-face :foreground ,outrun-red)
               ;;(neo-vc-unregistered-face nil)
               (neo-vc-up-to-date-face :foreground ,outrun-green)
               (neo-vc-user-face :foreground ,outrun-purple)
               ;; org
               (org-agenda-date :foreground ,outrun-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,outrun-comment)
               (org-agenda-done :foreground ,outrun-green)
               (org-agenda-structure :foreground ,outrun-purple)
               (org-block :foreground ,outrun-orange)
               (org-code :foreground ,outrun-green)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,outrun-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,outrun-comment)
               (org-document-title :weight bold :foreground ,outrun-orange
                                   ,@(when outrun-enlarge-headings
                                       (list :height outrun-height-doc-title)))
               (org-done :foreground ,outrun-green)
               (org-ellipsis :foreground ,outrun-comment)
               (org-foutruntnote :foreground ,other-blue)
               (org-formula :foreground ,outrun-pink)
               (org-headline-done :foreground ,outrun-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,outrun-bg :background ,outrun-bg)
               (org-level-1 :inherit bold :foreground ,outrun-pink
                            ,@(when outrun-enlarge-headings
                                (list :height outrun-height-title-1)))
               (org-level-2 :inherit bold :foreground ,outrun-purple
                            ,@(when outrun-enlarge-headings
                                (list :height outrun-height-title-2)))
               (org-level-3 :weight normal :foreground ,outrun-green
                            ,@(when outrun-enlarge-headings
                                (list :height outrun-height-title-3)))
               (org-level-4 :weight normal :foreground ,outrun-yellow)
               (org-level-5 :weight normal :foreground ,outrun-cyan)
               (org-level-6 :weight normal :foreground ,outrun-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,outrun-fg)
               (org-link :foreground ,outrun-cyan :underline t)
               (org-priority :foreground ,outrun-cyan)
               (org-quote :foreground ,outrun-yellow :slant italic)
               (org-scheduled :foreground ,outrun-green)
               (org-scheduled-previously :foreground ,outrun-yellow)
               (org-scheduled-today :foreground ,outrun-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,outrun-yellow)
               (org-table :foreground ,outrun-purple)
               (org-tag :foreground ,outrun-pink :weight bold :background ,bg2)
               (org-todo :foreground ,outrun-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,outrun-yellow)
               (org-verbatim :inherit org-quote)
               (org-warning :weight bold :foreground ,outrun-pink)
               ;; outline
               (outline-1 :foreground ,outrun-pink)
               (outline-2 :foreground ,outrun-purple)
               (outline-3 :foreground ,outrun-green)
               (outline-4 :foreground ,outrun-yellow)
               (outline-5 :foreground ,outrun-cyan)
               (outline-6 :foreground ,outrun-orange)
               ;; perspective
               (persp-selected-face :weight bold :foreground ,outrun-pink)
               ;; powerline
               (powerline-active1 :background ,outrun-bg :foreground ,outrun-pink)
               (powerline-active2 :background ,outrun-bg :foreground ,outrun-pink)
               (powerline-inactive1 :background ,bg2 :foreground ,outrun-purple)
               (powerline-inactive2 :background ,bg2 :foreground ,outrun-purple)
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,outrun-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,outrun-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,outrun-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,outrun-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,outrun-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,outrun-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,outrun-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,outrun-fg)
               (rainbow-delimiters-depth-2-face :foreground ,outrun-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,outrun-purple)
               (rainbow-delimiters-depth-4-face :foreground ,outrun-pink)
               (rainbow-delimiters-depth-5-face :foreground ,outrun-orange)
               (rainbow-delimiters-depth-6-face :foreground ,outrun-green)
               (rainbow-delimiters-depth-7-face :foreground ,outrun-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,outrun-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,outrun-green)
               (rpm-spec-doc-face :foreground ,outrun-pink)
               (rpm-spec-ghost-face :foreground ,outrun-purple)
               (rpm-spec-macro-face :foreground ,outrun-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,outrun-purple)
               (rpm-spec-section-face :foreground ,outrun-yellow)
               (rpm-spec-tag-face :foreground ,outrun-cyan)
               (rpm-spec-var-face :foreground ,outrun-orange)
               ;; rst (reStructuredText)
               (rst-level-1 :foreground ,outrun-pink :weight bold)
               (rst-level-2 :foreground ,outrun-purple :weight bold)
               (rst-level-3 :foreground ,outrun-green)
               (rst-level-4 :foreground ,outrun-yellow)
               (rst-level-5 :foreground ,outrun-cyan)
               (rst-level-6 :foreground ,outrun-orange)
               (rst-level-7 :foreground ,other-blue)
               (rst-level-8 :foreground ,outrun-fg)
               ;; selectrum-mode
               (selectrum-current-candidate :weight bold)
               (selectrum-primary-highlight :foreground ,outrun-pink)
               (selectrum-secondary-highlight :foreground ,outrun-green)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,outrun-cyan
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,outrun-cyan
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,outrun-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,outrun-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,outrun-green)
               (speedbar-file-face :foreground ,outrun-cyan)
               (speedbar-directory-face :foreground ,outrun-purple)
               (speedbar-tag-face :foreground ,outrun-yellow)
               (speedbar-selected-face :foreground ,outrun-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,outrun-bg
                                        :foreground ,outrun-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,outrun-purple :background ,outrun-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,outrun-pink :background ,outrun-bg
                            :box (:line-width 2 :color ,outrun-bg :style nil))
               (tab-bar-tab-inactive :foreground ,outrun-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,outrun-purple :background ,outrun-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,outrun-pink :background ,outrun-bg
                             :box (:line-width 2 :color ,outrun-bg :style nil))
               (tab-line-tab-inactive :foreground ,outrun-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,outrun-red)
               ;; telephone-line
               (telephone-line-accent-active :background ,outrun-bg :foreground ,outrun-pink)
               (telephone-line-accent-inactive :background ,bg2 :foreground ,outrun-purple)
               (telephone-line-unimportant :background ,outrun-bg :foreground ,outrun-comment)
               ;; term
               (term :foreground ,outrun-fg :background ,outrun-bg)
               (term-color-black :foreground ,outrun-bg :background ,outrun-comment)
               (term-color-blue :foreground ,outrun-purple :background ,outrun-purple)
               (term-color-cyan :foreground ,outrun-cyan :background ,outrun-cyan)
               (term-color-green :foreground ,outrun-green :background ,outrun-green)
               (term-color-magenta :foreground ,outrun-pink :background ,outrun-pink)
               (term-color-red :foreground ,outrun-red :background ,outrun-red)
               (term-color-white :foreground ,outrun-fg :background ,outrun-fg)
               (term-color-yellow :foreground ,outrun-yellow :background ,outrun-yellow)
               ;; tree-sitter
               (tree-sitter-hl-face:attribute :inherit font-lock-constant-face)
               (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
               (tree-sitter-hl-face:constant :inherit font-lock-constant-face)
               (tree-sitter-hl-face:constant.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:constructor :inherit font-lock-constant-face)
               (tree-sitter-hl-face:escape :foreground ,outrun-pink)
               (tree-sitter-hl-face:function :inherit font-lock-function-name-face)
               (tree-sitter-hl-face:function.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:function.call :inherit font-lock-function-name-face
                                                  :weight normal)
               (tree-sitter-hl-face:function.macro :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:function.special :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:keyword :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:punctuation :foreground ,outrun-pink)
               (tree-sitter-hl-face:punctuation.bracket :foreground ,outrun-fg)
               (tree-sitter-hl-face:punctuation.delimiter :foreground ,outrun-fg)
               (tree-sitter-hl-face:punctuation.special :foreground ,outrun-pink)
               (tree-sitter-hl-face:string :inherit font-lock-string-face)
               (tree-sitter-hl-face:string.special :foreground ,outrun-red)
               (tree-sitter-hl-face:tag :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:type :inherit font-lock-type-face)
               (tree-sitter-hl-face:type.parameter :foreground ,outrun-pink)
               (tree-sitter-hl-face:variable :inherit font-lock-variable-name-face)
               (tree-sitter-hl-face:variable.parameter :inherit tree-sitter-hl-face:variable
                                                       :weight normal)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,outrun-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,outrun-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,outrun-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit font-lock-builtin-face)
               (web-mode-comment-face :inherit font-lock-comment-face)
               (web-mode-constant-face :inherit font-lock-constant-face)
               (web-mode-css-property-name-face :inherit font-lock-constant-face)
               (web-mode-doctype-face :inherit font-lock-comment-face)
               (web-mode-function-name-face :inherit font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,outrun-purple)
               (web-mode-html-attr-value-face :foreground ,outrun-green)
               (web-mode-html-tag-face :foreground ,outrun-pink :weight bold)
               (web-mode-keyword-face :foreground ,outrun-pink)
               (web-mode-string-face :foreground ,outrun-cyan)
               (web-mode-type-face :inherit font-lock-type-face)
               (web-mode-warning-face :inherit font-lock-warning-face)
               ;; which-func
               (which-func :inherit font-lock-function-name-face)
               ;; which-key
               (which-key-key-face :inherit font-lock-builtin-face)
               (which-key-command-description-face :inherit default)
               (which-key-separator-face :inherit font-lock-comment-delimiter-face)
               (which-key-local-map-description-face :foreground ,outrun-green)
               ;; whitespace
               (whitespace-big-indent :background ,outrun-red :foreground ,outrun-red)
               (whitespace-empty :background ,outrun-orange :foreground ,outrun-red)
               (whitespace-hspace :background ,bg3 :foreground ,outrun-comment)
               (whitespace-indentation :background ,outrun-orange :foreground ,outrun-red)
               (whitespace-line :background ,outrun-bg :foreground ,outrun-pink)
               (whitespace-newline :foreground ,outrun-comment)
               (whitespace-space :background ,outrun-bg :foreground ,outrun-comment)
               (whitespace-space-after-tab :background ,outrun-orange :foreground ,outrun-red)
               (whitespace-space-before-tab :background ,outrun-orange :foreground ,outrun-red)
               (whitespace-tab :background ,bg2 :foreground ,outrun-comment)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit font-lock-builtin-face)
               (yard-directive-face :inherit font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'outrun
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
                       ,(if outrun-use-24-bit-colors-on-256-colors-terms
                            (funcall expand-with-func 'cadr spec)
                          (funcall expand-with-func 'caddr spec)))
                      (t                       ; should be only tty-like envs
                       ,(funcall expand-with-func 'cadddr spec))))
                   whole-theme))
           whole-theme))

  (apply #'custom-theme-set-variables
         'outrun
         (let ((get-func
                (pcase (display-color-cells)
                  ((pred (<= 16777216)) 'car) ; fully graphical envs
                  ((pred (<= 256)) 'cadr)     ; terminal withs 256 colors
                  (_ 'caddr))))               ; should be only tty-like envs
           `((ansi-color-names-vector
              [,(funcall get-func (alist-get 'outrun-bg colors))
               ,(funcall get-func (alist-get 'outrun-red colors))
               ,(funcall get-func (alist-get 'outrun-green colors))
               ,(funcall get-func (alist-get 'outrun-yellow colors))
               ,(funcall get-func (alist-get 'outrun-comment colors))
               ,(funcall get-func (alist-get 'outrun-purple colors))
               ,(funcall get-func (alist-get 'outrun-cyan colors))
               ,(funcall get-func (alist-get 'outrun-fg colors))])))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'outrun)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; outrun-theme.el ends here
