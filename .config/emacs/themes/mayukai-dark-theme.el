;;; mayukai-dark-theme.el --- Mayukai-dark Theme

;;; Code:
(deftheme mayukai-dark)

;;;; Configuration options:

(defgroup mayukai-dark nil
  "Mayukai-Dark theme options.

The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom mayukai-dark-enlarge-headings t
  "Use different font sizes for some headings and titles."
  :type 'bmayukai-darklean
  :group 'mayukai-dark)

(defcustom mayukai-dark-height-title-1 1.3
  "Font size 100%."
  :type 'number
  :group 'mayukai-dark)

(defcustom mayukai-dark-height-title-2 1.1
  "Font size 110%."
  :type 'number
  :group 'mayukai-dark)

(defcustom mayukai-dark-height-title-3 1.0
  "Font size 130%."
  :type 'number
  :group 'mayukai-dark)

(defcustom mayukai-dark-height-doc-title 1.44
  "Font size 144%."
  :type 'number
  :group 'mayukai-dark)

(defcustom mayukai-dark-alternate-mode-line-and-minibuffer nil
  "Use less bold and pink in the minibuffer."
  :type 'bmayukai-darklean
  :group 'mayukai-dark)

(defvar mayukai-dark-use-24-bit-colors-on-256-colors-terms nil)


;;;; Theme definition:

;; Assigment form: VARIABLE COLOR [256-COLOR [TTY-COLOR]]
(let ((colors '(;; Upstream theme color
                (mayukai-dark-bg            "#0d1016" "unspecified-bg" "unspecified-bg") ; official background
                (mayukai-dark-fg            "#dfefd0" "#dadbc0" "brightwhite") ; official foreground
                (mayukai-dark-current       "#44475a" "#303030" "brightblack") ; official current-line/selection
                (mayukai-dark-comment       "#5c6773" "#5c6773" "blue")        ; official comment
                (mayukai-dark-cyan          "#1ea8fc" "#1ea8fc" "cyan")  ; official cyan
                (mayukai-dark-brightcyan    "#70f0ff" "#70f0ff" "brightcyan")  ; official cyan
                (mayukai-dark-green         "#C2F94C" "#C2F94C" "green")       ; official green
                (mayukai-dark-lightgreen    "#95e6cb" "#95e6cb" "lightgreen")       ; official green
                (mayukai-dark-orange        "#ffa759" "#ffa759" "brightred")   ; official orange
                (mayukai-dark-pink          "#D86BFF" "#D86BFF" "magenta")     ; official pink
                (mayukai-dark-purple        "#D86BFF" "#A875FF" "brightmagenta") ; official purple
                (mayukai-dark-red           "#ff5555" "#ff8787" "red")         ; official red
                (mayukai-dark-yellow        "#ffff50" "#ffff87" "yellow")      ; official yellow
                (mayukai-dark-linenum       "#3d424d" "#3d424d" "brightblack")
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
               (default :background ,mayukai-dark-bg :foreground ,mayukai-dark-fg)
               (default-italic :slant italic)
               (error :foreground ,mayukai-dark-red)
               (ffap :foreground ,fg4)
               (fringe :background ,mayukai-dark-bg :foreground ,fg4)
               (header-line :inherit 'mode-line)
               (highlight :foreground ,fg3 :background ,bg3)
               (hl-line :background ,bg5 :extend t)
               (info-quoted-name :foreground ,mayukai-dark-orange)
               (info-string :foreground ,mayukai-dark-green)
               (lazy-highlight :foreground ,fg2 :background ,bg2)
               (link :foreground ,mayukai-dark-cyan :underline t)
               (linum :slant italic :foreground ,mayukai-dark-comment :background ,mayukai-dark-bg)
               (line-number :slant italic :foreground ,mayukai-dark-comment :background ,mayukai-dark-bg)
               (line-number-current-line :slant italic :foreground ,mayukai-dark-linenum :background ,mayukai-dark-bg)
               (nlinum-current-line :slant italic :foreground ,mayukai-dark-linenum :background ,mayukai-dark-bg)
               (match :background ,mayukai-dark-yellow :foreground ,mayukai-dark-bg)
               (menu :background ,mayukai-dark-current :inverse-video nil
                     ,@(if mayukai-dark-alternate-mode-line-and-minibuffer
                           (list :foreground fg3)
                         (list :foreground mayukai-dark-fg)))
               (minibuffer-prompt
                ,@(if mayukai-dark-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground mayukai-dark-fg)
                    (list  :foreground mayukai-dark-pink)))
               (mode-line :background ,mayukai-dark-current
                          :box ,mayukai-dark-current :inverse-video nil
                          ,@(if mayukai-dark-alternate-mode-line-and-minibuffer
                                (list :foreground fg3)
                              (list :foreground mayukai-dark-fg)))
               (mode-line-inactive
                :background ,mayukai-dark-bg :inverse-video nil
                ,@(if mayukai-dark-alternate-mode-line-and-minibuffer
                      (list :foreground mayukai-dark-comment)
                    (list :foreground fg4 :box bg2)))
               (read-multiple-choice-face :inherit completions-first-difference)
               (region :background ,mayukai-dark-current)
               (shadow :foreground ,mayukai-dark-comment)
               (success :foreground ,mayukai-dark-green)
               (tmayukai-darkltip :foreground ,mayukai-dark-fg :background ,mayukai-dark-current)
               (trailing-whitespace :background ,mayukai-dark-orange :foreground ,mayukai-dark-current)
               (vertical-border :foreground ,bg2)
               (warning :foreground ,mayukai-dark-orange)
               ;; syntax / font-lock
               (font-lock-builtin-face :foreground ,mayukai-dark-green :slant italic)
               (font-lock-comment-face :foreground ,mayukai-dark-comment)
               (font-lock-comment-delimiter-face :inherit shadow)
               (font-lock-constant-face :foreground ,mayukai-dark-orange)
               (font-lock-doc-face :foreground ,mayukai-dark-comment)
               (font-lock-function-name-face :foreground ,mayukai-dark-yellow :weight normal)
               (font-lock-keyword-face :foreground ,mayukai-dark-orange :weight normal)
               (font-lock-negation-char-face :foreground ,mayukai-dark-pink)
               (font-lock-preprocessor-face :foreground ,mayukai-dark-orange)
               (font-lock-reference-face :inherit font-lock-constant-face) ;; obsolete
               (font-lock-regexp-grouping-backslash :foreground ,mayukai-dark-cyan)
               (font-lock-regexp-grouping-construct :foreground ,mayukai-dark-purple)
               (font-lock-string-face :foreground ,mayukai-dark-green)
               (font-lock-type-face :inherit font-lock-builtin-face :foreground ,mayukai-dark-lightgreen)
               (font-lock-variable-name-face :foreground ,mayukai-dark-fg :weight normal)
               (font-lock-warning-face :inherit warning :background ,bg2)
               ;; auto-complete
               (ac-completion-face :underline t :foreground ,mayukai-dark-pink)
               ;; company
               (company-echo-common :foreground ,mayukai-dark-bg :background ,mayukai-dark-fg)
               (company-preview :background ,mayukai-dark-current :foreground ,other-blue)
               (company-preview-common :inherit company-preview
                                       :foreground ,mayukai-dark-pink)
               (company-preview-search :inherit company-preview
                                       :foreground ,mayukai-dark-green)
               (company-scrollbar-bg :background ,mayukai-dark-comment)
               (company-scrollbar-fg :foreground ,other-blue)
               (company-tooltip :foreground ,mayukai-dark-fg :background ,bg2)
               (company-tooltip-common :foreground ,mayukai-dark-purple)
               (company-tooltip-selection :background ,mayukai-dark-current)
               (company-tmayukai-darkltip :inherit tmayukai-darkltip)
               (company-tmayukai-darkltip-search :foreground ,mayukai-dark-green
                                           :underline t)
               (company-tmayukai-darkltip-search-selection :background ,mayukai-dark-green
                                                     :foreground ,mayukai-dark-bg)
               (company-tmayukai-darkltip-selection :inherit match)
               (company-tmayukai-darkltip-mouse :background ,mayukai-dark-bg)
               (company-tmayukai-darkltip-common :foreground ,mayukai-dark-pink)
               ;;(company-tmayukai-darkltip-common-selection :inherit company-tmayukai-darkltip-common)
               (company-tmayukai-darkltip-annotation :foreground ,mayukai-dark-cyan)
               ;;(company-tmayukai-darkltip-annotation-selection :inherit company-tmayukai-darkltip-annotation)
               ;; completions (minibuffer.el)
               (completions-annotations :inherit font-lock-comment-face)
               (completions-common-part :foreground ,mayukai-dark-pink)
               (completions-first-difference :foreground ,mayukai-dark-fg)
               ;; diff-hl
               (diff-hl-change :foreground ,mayukai-dark-orange :background ,mayukai-dark-orange)
               (diff-hl-delete :foreground ,mayukai-dark-red :background ,mayukai-dark-red)
               (diff-hl-insert :foreground ,mayukai-dark-green :background ,mayukai-dark-green)
               ;; dired
               (dired-directory :foreground ,mayukai-dark-green :weight normal)
               (dired-flagged :foreground ,mayukai-dark-pink)
               (dired-header :foreground ,fg3 :background ,mayukai-dark-bg)
               (dired-ignored :inherit shadow)
               (dired-mark :foreground ,mayukai-dark-fg)
               (dired-marked :foreground ,mayukai-dark-orange)
               (dired-perm-write :foreground ,fg3 :underline t)
               (dired-symlink :foreground ,mayukai-dark-yellow :weight normal :slant italic)
               (dired-warning :foreground ,mayukai-dark-orange :underline t)
               (diredp-compressed-file-name :foreground ,fg3)
               (diredp-compressed-file-suffix :foreground ,fg4)
               (diredp-date-time :foreground ,mayukai-dark-fg)
               (diredp-deletion-file-name :foreground ,mayukai-dark-pink :background ,mayukai-dark-current)
               (diredp-deletion :foreground ,mayukai-dark-pink)
               (diredp-dir-heading :foreground ,fg2 :background ,bg4)
               (diredp-dir-name :inherit dired-directory)
               (diredp-dir-priv :inherit dired-directory)
               (diredp-executable-tag :foreground ,mayukai-dark-orange)
               (diredp-file-name :foreground ,mayukai-dark-fg)
               (diredp-file-suffix :foreground ,fg4)
               (diredp-flag-mark-line :foreground ,fg2 :slant italic :background ,mayukai-dark-current)
               (diredp-flag-mark :foreground ,fg2 :background ,mayukai-dark-current)
               (diredp-ignored-file-name :foreground ,mayukai-dark-fg)
               (diredp-mode-line-flagged :foreground ,mayukai-dark-orange)
               (diredp-mode-line-marked :foreground ,mayukai-dark-orange)
               (diredp-no-priv :foreground ,mayukai-dark-fg)
               (diredp-number :foreground ,mayukai-dark-cyan)
               (diredp-other-priv :foreground ,mayukai-dark-orange)
               (diredp-rare-priv :foreground ,mayukai-dark-orange)
               (diredp-read-priv :foreground ,mayukai-dark-purple)
               (diredp-write-priv :foreground ,mayukai-dark-pink)
               (diredp-exec-priv :foreground ,mayukai-dark-yellow)
               (diredp-symlink :foreground ,mayukai-dark-orange)
               (diredp-link-priv :foreground ,mayukai-dark-orange)
               (diredp-autofile-name :foreground ,mayukai-dark-yellow)
               (diredp-tagged-autofile-name :foreground ,mayukai-dark-yellow)
               ;; eldoc-box
               (eldoc-box-border :background ,mayukai-dark-current)
               (eldoc-box-body :background ,mayukai-dark-current)
               ;; elfeed
               (elfeed-search-date-face :foreground ,mayukai-dark-comment)
               (elfeed-search-title-face :foreground ,mayukai-dark-fg)
               (elfeed-search-unread-title-face :foreground ,mayukai-dark-pink)
               (elfeed-search-feed-face :foreground ,mayukai-dark-fg)
               (elfeed-search-tag-face :foreground ,mayukai-dark-green)

               (elfeed-search-unread-count-face :foreground ,mayukai-dark-pink)
               (elfeed-search-filter-face :foreground ,mayukai-dark-green)
               ;;(elfeed-log-date-face :inherit font-lock-type-face)
               (elfeed-log-error-level-face :foreground ,mayukai-dark-red)
               (elfeed-log-warn-level-face :foreground ,mayukai-dark-orange)
               (elfeed-log-info-level-face :foreground ,mayukai-dark-cyan)
               (elfeed-log-debug-level-face :foreground ,mayukai-dark-comment)
               ;; elpher
               (elpher-gemini-heading1 :inherit bold :foreground ,mayukai-dark-pink
                                       ,@(when mayukai-dark-enlarge-headings
                                           (list :height mayukai-dark-height-title-1)))
               (elpher-gemini-heading2 :inherit bold :foreground ,mayukai-dark-purple
                                       ,@(when mayukai-dark-enlarge-headings
                                           (list :height mayukai-dark-height-title-2)))
               (elpher-gemini-heading3 :weight normal :foreground ,mayukai-dark-green
                                       ,@(when mayukai-dark-enlarge-headings
                                           (list :height mayukai-dark-height-title-3)))
               (elpher-gemini-preformatted :inherit fixed-pitch
                                           :foreground ,mayukai-dark-orange)
               ;; enh-ruby
               (enh-ruby-heredoc-delimiter-face :foreground ,mayukai-dark-yellow)
               (enh-ruby-op-face :foreground ,mayukai-dark-pink)
               (enh-ruby-regexp-delimiter-face :foreground ,mayukai-dark-yellow)
               (enh-ruby-string-delimiter-face :foreground ,mayukai-dark-cyan)
               ;; flyspell
               (flyspell-duplicate :underline (:style wave :color ,mayukai-dark-orange))
               (flyspell-incorrect :underline (:style wave :color ,mayukai-dark-red))
               ;; font-latex
               (font-latex-bold-face :foreground ,mayukai-dark-purple :weight 'bold)
               (font-latex-italic-face :foreground ,mayukai-dark-pink :slant italic)
               (font-latex-match-reference-keywords :foreground ,mayukai-dark-orange)
               (font-latex-match-variable-keywords :foreground ,mayukai-dark-fg)
               (font-latex-string-face :foreground ,mayukai-dark-cyan)
               ;; gemini
               (gemini-heading-face-1 :inherit bold :foreground ,mayukai-dark-pink
                                      ,@(when mayukai-dark-enlarge-headings
                                          (list :height mayukai-dark-height-title-1)))
               (gemini-heading-face-2 :inherit bold :foreground ,mayukai-dark-purple
                                      ,@(when mayukai-dark-enlarge-headings
                                          (list :height mayukai-dark-height-title-2)))
               (gemini-heading-face-3 :weight normal :foreground ,mayukai-dark-green
                                      ,@(when mayukai-dark-enlarge-headings
                                          (list :height mayukai-dark-height-title-3)))
               (gemini-heading-face-rest :weight normal :foreground ,mayukai-dark-yellow)
               (gemini-quote-face :foreground ,mayukai-dark-purple)
               ;; go-test
               (go-test--ok-face :inherit success)
               (go-test--error-face :inherit error)
               (go-test--warning-face :inherit warning)
               (go-test--pointer-face :foreground ,mayukai-dark-pink)
               (go-test--standard-face :foreground ,mayukai-dark-cyan)
               ;; gnus-group
               (gnus-group-mail-1 :foreground ,mayukai-dark-pink :weight bold)
               (gnus-group-mail-1-empty :inherit gnus-group-mail-1 :weight normal)
               (gnus-group-mail-2 :foreground ,mayukai-dark-cyan :weight bold)
               (gnus-group-mail-2-empty :inherit gnus-group-mail-2 :weight normal)
               (gnus-group-mail-3 :foreground ,mayukai-dark-comment :weight bold)
               (gnus-group-mail-3-empty :inherit gnus-group-mail-3 :weight normal)
               (gnus-group-mail-low :foreground ,mayukai-dark-current :weight bold)
               (gnus-group-mail-low-empty :inherit gnus-group-mail-low :weight normal)
               (gnus-group-news-1 :foreground ,mayukai-dark-pink :weight bold)
               (gnus-group-news-1-empty :inherit gnus-group-news-1 :weight normal)
               (gnus-group-news-2 :foreground ,mayukai-dark-cyan :weight bold)
               (gnus-group-news-2-empty :inherit gnus-group-news-2 :weight normal)
               (gnus-group-news-3 :foreground ,mayukai-dark-comment :weight bold)
               (gnus-group-news-3-empty :inherit gnus-group-news-3 :weight normal)
               (gnus-group-news-4 :inherit gnus-group-news-low)
               (gnus-group-news-4-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-5 :inherit gnus-group-news-low)
               (gnus-group-news-5-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-6 :inherit gnus-group-news-low)
               (gnus-group-news-6-empty :inherit gnus-group-news-low-empty)
               (gnus-group-news-low :foreground ,mayukai-dark-current :weight bold)
               (gnus-group-news-low-empty :inherit gnus-group-news-low :weight normal)
               (gnus-header-content :foreground ,mayukai-dark-purple)
               (gnus-header-from :foreground ,mayukai-dark-fg)
               (gnus-header-name :foreground ,mayukai-dark-green)
               (gnus-header-subject :foreground ,mayukai-dark-pink :weight bold)
               (gnus-summary-markup-face :foreground ,mayukai-dark-cyan)
               (gnus-summary-high-unread :foreground ,mayukai-dark-pink :weight bold)
               (gnus-summary-high-read :inherit gnus-summary-high-unread :weight normal)
               (gnus-summary-high-ancient :inherit gnus-summary-high-read)
               (gnus-summary-high-ticked :inherit gnus-summary-high-read :underline t)
               (gnus-summary-normal-unread :foreground ,other-blue :weight bold)
               (gnus-summary-normal-read :foreground ,mayukai-dark-comment :weight normal)
               (gnus-summary-normal-ancient :inherit gnus-summary-normal-read :weight light)
               (gnus-summary-normal-ticked :foreground ,mayukai-dark-pink :weight bold)
               (gnus-summary-low-unread :foreground ,mayukai-dark-comment :weight bold)
               (gnus-summary-low-read :inherit gnus-summary-low-unread :weight normal)
               (gnus-summary-low-ancient :inherit gnus-summary-low-read)
               (gnus-summary-low-ticked :inherit gnus-summary-low-read :underline t)
               (gnus-summary-selected :inverse-video t)
               ;; haskell-mode
               (haskell-operator-face :foreground ,mayukai-dark-pink)
               (haskell-constructor-face :foreground ,mayukai-dark-purple)
               ;; helm
               (helm-bmayukai-darkkmark-w3m :foreground ,mayukai-dark-purple)
               (helm-buffer-not-saved :foreground ,mayukai-dark-purple :background ,mayukai-dark-bg)
               (helm-buffer-process :foreground ,mayukai-dark-orange :background ,mayukai-dark-bg)
               (helm-buffer-saved-out :foreground ,mayukai-dark-fg :background ,mayukai-dark-bg)
               (helm-buffer-size :foreground ,mayukai-dark-fg :background ,mayukai-dark-bg)
               (helm-candidate-number :foreground ,mayukai-dark-bg :background ,mayukai-dark-fg)
               (helm-ff-directory :foreground ,mayukai-dark-green :background ,mayukai-dark-bg :weight bold)
               (helm-ff-dotted-directory :foreground ,mayukai-dark-green :background ,mayukai-dark-bg :weight normal)
               (helm-ff-executable :foreground ,other-blue :background ,mayukai-dark-bg :weight normal)
               (helm-ff-file :foreground ,mayukai-dark-fg :background ,mayukai-dark-bg :weight normal)
               (helm-ff-invalid-symlink :foreground ,mayukai-dark-pink :background ,mayukai-dark-bg :weight bold)
               (helm-ff-prefix :foreground ,mayukai-dark-bg :background ,mayukai-dark-pink :weight normal)
               (helm-ff-symlink :foreground ,mayukai-dark-pink :background ,mayukai-dark-bg :weight bold)
               (helm-grep-cmd-line :foreground ,mayukai-dark-fg :background ,mayukai-dark-bg)
               (helm-grep-file :foreground ,mayukai-dark-fg :background ,mayukai-dark-bg)
               (helm-grep-finish :foreground ,fg2 :background ,mayukai-dark-bg)
               (helm-grep-lineno :foreground ,mayukai-dark-fg :background ,mayukai-dark-bg)
               (helm-grep-match :inherit match)
               (helm-grep-running :foreground ,mayukai-dark-green :background ,mayukai-dark-bg)
               (helm-header :foreground ,fg2 :background ,mayukai-dark-bg :underline nil :box nil)
               (helm-moccur-buffer :foreground ,mayukai-dark-green :background ,mayukai-dark-bg)
               (helm-selection :background ,bg2 :underline nil)
               (helm-selection-line :background ,bg2)
               (helm-separator :foreground ,mayukai-dark-purple :background ,mayukai-dark-bg)
               (helm-source-go-package-godoc-description :foreground ,mayukai-dark-yellow)
               (helm-source-header :foreground ,mayukai-dark-pink :background ,mayukai-dark-bg :underline nil :weight bold)
               (helm-time-zone-current :foreground ,mayukai-dark-orange :background ,mayukai-dark-bg)
               (helm-time-zone-home :foreground ,mayukai-dark-purple :background ,mayukai-dark-bg)
               (helm-visible-mark :foreground ,mayukai-dark-bg :background ,bg3)
               ;; highlight-indentation minor mode
               (highlight-indentation-face :background ,bg2)
               ;; icicle
               (icicle-whitespace-highlight :background ,mayukai-dark-fg)
               (icicle-special-candidate :foreground ,fg2)
               (icicle-extra-candidate :foreground ,fg2)
               (icicle-search-main-regexp-others :foreground ,mayukai-dark-fg)
               (icicle-search-current-input :foreground ,mayukai-dark-pink)
               (icicle-search-context-level-8 :foreground ,mayukai-dark-orange)
               (icicle-search-context-level-7 :foreground ,mayukai-dark-orange)
               (icicle-search-context-level-6 :foreground ,mayukai-dark-orange)
               (icicle-search-context-level-5 :foreground ,mayukai-dark-orange)
               (icicle-search-context-level-4 :foreground ,mayukai-dark-orange)
               (icicle-search-context-level-3 :foreground ,mayukai-dark-orange)
               (icicle-search-context-level-2 :foreground ,mayukai-dark-orange)
               (icicle-search-context-level-1 :foreground ,mayukai-dark-orange)
               (icicle-search-main-regexp-current :foreground ,mayukai-dark-fg)
               (icicle-saved-candidate :foreground ,mayukai-dark-fg)
               (icicle-proxy-candidate :foreground ,mayukai-dark-fg)
               (icicle-mustmatch-completion :foreground ,mayukai-dark-purple)
               (icicle-multi-command-completion :foreground ,fg2 :background ,bg2)
               (icicle-msg-emphasis :foreground ,mayukai-dark-green)
               (icicle-mode-line-help :foreground ,fg4)
               (icicle-match-highlight-minibuffer :foreground ,mayukai-dark-orange)
               (icicle-match-highlight-Completions :foreground ,mayukai-dark-green)
               (icicle-key-complete-menu-local :foreground ,mayukai-dark-fg)
               (icicle-key-complete-menu :foreground ,mayukai-dark-fg)
               (icicle-input-completion-fail-lax :foreground ,mayukai-dark-pink)
               (icicle-input-completion-fail :foreground ,mayukai-dark-pink)
               (icicle-historical-candidate-other :foreground ,mayukai-dark-fg)
               (icicle-historical-candidate :foreground ,mayukai-dark-fg)
               (icicle-current-candidate-highlight :foreground ,mayukai-dark-orange :background ,bg3)
               (icicle-Completions-instruction-2 :foreground ,fg4)
               (icicle-Completions-instruction-1 :foreground ,fg4)
               (icicle-completion :foreground ,mayukai-dark-fg)
               (icicle-complete-input :foreground ,mayukai-dark-orange)
               (icicle-common-match-highlight-Completions :foreground ,mayukai-dark-purple)
               (icicle-candidate-part :foreground ,mayukai-dark-fg)
               (icicle-annotation :foreground ,fg4)
               ;; icomplete
               (icompletep-determined :foreground ,mayukai-dark-orange)
               ;; ido
               (ido-first-match
                ,@(if mayukai-dark-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :foreground mayukai-dark-green)
                    (list  :foreground mayukai-dark-pink)))
               (ido-only-match :foreground ,mayukai-dark-orange)
               (ido-subdir :foreground ,mayukai-dark-yellow)
               (ido-virtual :foreground ,mayukai-dark-cyan)
               (ido-incomplete-regexp :inherit font-lock-warning-face)
               (ido-indicator :foreground ,mayukai-dark-fg :background ,mayukai-dark-pink)
               ;; ivy
               (ivy-current-match
                ,@(if mayukai-dark-alternate-mode-line-and-minibuffer
                      (list :weight 'normal :background mayukai-dark-current :foreground mayukai-dark-green)
                    (list :background mayukai-dark-current :foreground mayukai-dark-pink)))
               ;; Highlights the background of the match.
               (ivy-minibuffer-match-face-1 :background ,mayukai-dark-current)
               ;; Highlights the first matched group.
               (ivy-minibuffer-match-face-2 :foreground ,mayukai-dark-green)
               ;; Highlights the second matched group.
               (ivy-minibuffer-match-face-3 :background ,mayukai-dark-yellow
                                            :foreground ,mayukai-dark-bg)
               ;; Highlights the third matched group.
               (ivy-minibuffer-match-face-4 :background ,mayukai-dark-pink
                                            :foreground ,mayukai-dark-bg)
               (ivy-confirm-face :foreground ,mayukai-dark-orange)
               (ivy-match-required-face :foreground ,mayukai-dark-red)
               (ivy-subdir :foreground ,mayukai-dark-yellow)
               (ivy-remote :foreground ,mayukai-dark-pink)
               (ivy-virtual :foreground ,mayukai-dark-cyan)
               ;; isearch
               (isearch :inherit match :weight bold)
               (isearch-fail :foreground ,mayukai-dark-bg :background ,mayukai-dark-orange)
               ;; jde-java
               (jde-java-font-lock-constant-face :foreground ,mayukai-dark-cyan)
               (jde-java-font-lock-modifier-face :foreground ,mayukai-dark-pink)
               (jde-java-font-lock-number-face :foreground ,mayukai-dark-fg)
               (jde-java-font-lock-package-face :foreground ,mayukai-dark-fg)
               (jde-java-font-lock-private-face :foreground ,mayukai-dark-pink)
               (jde-java-font-lock-public-face :foreground ,mayukai-dark-pink)
               ;; js2-mode
               (js2-external-variable :foreground ,mayukai-dark-purple)
               (js2-function-param :foreground ,mayukai-dark-cyan)
               (js2-jsdoc-html-tag-delimiter :foreground ,mayukai-dark-yellow)
               (js2-jsdoc-html-tag-name :foreground ,other-blue)
               (js2-jsdoc-value :foreground ,mayukai-dark-yellow)
               (js2-private-function-call :foreground ,mayukai-dark-cyan)
               (js2-private-member :foreground ,fg3)
               ;; js3-mode
               (js3-error-face :underline ,mayukai-dark-orange)
               (js3-external-variable-face :foreground ,mayukai-dark-fg)
               (js3-function-param-face :foreground ,mayukai-dark-pink)
               (js3-instance-member-face :foreground ,mayukai-dark-cyan)
               (js3-jsdoc-tag-face :foreground ,mayukai-dark-pink)
               (js3-warning-face :underline ,mayukai-dark-pink)
               ;; lsp
               (lsp-ui-peek-peek :background ,mayukai-dark-bg)
               (lsp-ui-peek-list :background ,bg2)
               (lsp-ui-peek-filename :foreground ,mayukai-dark-pink :weight bold)
               (lsp-ui-peek-line-number :foreground ,mayukai-dark-fg)
               (lsp-ui-peek-highlight :inherit highlight :distant-foreground ,mayukai-dark-bg)
               (lsp-ui-peek-header :background ,bg3 :foreground ,fg3, :weight bold)
               (lsp-ui-peek-fmayukai-darkter :inherit lsp-ui-peek-header)
               (lsp-ui-peek-selection :inherit match)
               (lsp-ui-sideline-symbol :foreground ,fg4 :box (:line-width -1 :color ,fg4) :height 0.99)
               (lsp-ui-sideline-current-symbol :foreground ,mayukai-dark-fg :weight ultra-bold
                                               :box (:line-width -1 :color mayukai-dark-fg) :height 0.99)
               (lsp-ui-sideline-code-action :foreground ,mayukai-dark-yellow)
               (lsp-ui-sideline-symbol-info :slant italic :height 0.99)
               (lsp-ui-doc-background :background ,mayukai-dark-bg)
               (lsp-ui-doc-header :foreground ,mayukai-dark-bg :background ,mayukai-dark-cyan)
               ;; magit
               (magit-branch-local :foreground ,mayukai-dark-cyan)
               (magit-branch-remote :foreground ,mayukai-dark-green)
               (magit-tag :foreground ,mayukai-dark-orange)
               (magit-section-heading :foreground ,mayukai-dark-pink :weight bold)
               (magit-section-highlight :background ,bg3 :extend t)
               (magit-diff-context-highlight :background ,bg3
                                             :foreground ,fg3
                                             :extend t)
               (magit-diff-revision-summary :foreground ,mayukai-dark-orange
                                            :background ,mayukai-dark-bg
                                            :weight bold)
               (magit-diff-revision-summary-highlight :foreground ,mayukai-dark-orange
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
               (magit-diff-file-heading :foreground ,mayukai-dark-fg)
               (magit-diff-file-heading-highlight :inherit magit-section-highlight)
               (magit-diffstat-added :foreground ,mayukai-dark-green)
               (magit-diffstat-removed :foreground ,mayukai-dark-red)
               (magit-hash :foreground ,fg2)
               (magit-hunk-heading :background ,bg3)
               (magit-hunk-heading-highlight :background ,bg3)
               (magit-item-highlight :background ,bg3)
               (magit-log-author :foreground ,fg3)
               (magit-process-ng :foreground ,mayukai-dark-orange :weight bold)
               (magit-process-ok :foreground ,mayukai-dark-green :weight bold)
               ;; markdown
               (markdown-blockquote-face :foreground ,mayukai-dark-yellow
                                         :slant italic)
               (markdown-code-face :foreground ,mayukai-dark-orange)
               (markdown-fmayukai-darktnote-face :foreground ,other-blue)
               (markdown-header-face :weight normal)
               (markdown-header-face-1
                :inherit bold :foreground ,mayukai-dark-pink
                ,@(when mayukai-dark-enlarge-headings
                    (list :height mayukai-dark-height-title-1)))
               (markdown-header-face-2
                :inherit bold :foreground ,mayukai-dark-purple
                ,@(when mayukai-dark-enlarge-headings
                    (list :height mayukai-dark-height-title-2)))
               (markdown-header-face-3
                :foreground ,mayukai-dark-green
                ,@(when mayukai-dark-enlarge-headings
                    (list :height mayukai-dark-height-title-3)))
               (markdown-header-face-4 :foreground ,mayukai-dark-yellow)
               (markdown-header-face-5 :foreground ,mayukai-dark-cyan)
               (markdown-header-face-6 :foreground ,mayukai-dark-orange)
               (markdown-header-face-7 :foreground ,other-blue)
               (markdown-header-face-8 :foreground ,mayukai-dark-fg)
               (markdown-inline-code-face :foreground ,mayukai-dark-green)
               (markdown-plain-url-face :inherit link)
               (markdown-pre-face :foreground ,mayukai-dark-orange)
               (markdown-table-face :foreground ,mayukai-dark-purple)
               (markdown-list-face :foreground ,mayukai-dark-cyan)
               (markdown-language-keyword-face :foreground ,mayukai-dark-comment)
               ;; message
               (message-header-to :foreground ,mayukai-dark-fg :weight bold)
               (message-header-cc :foreground ,mayukai-dark-fg :bold bold)
               (message-header-subject :foreground ,mayukai-dark-orange)
               (message-header-newsgroups :foreground ,mayukai-dark-purple)
               (message-header-other :foreground ,mayukai-dark-purple)
               (message-header-name :foreground ,mayukai-dark-green)
               (message-header-xheader :foreground ,mayukai-dark-cyan)
               (message-separator :foreground ,mayukai-dark-cyan :slant italic)
               (message-cited-text :foreground ,mayukai-dark-purple)
               (message-cited-text-1 :foreground ,mayukai-dark-purple)
               (message-cited-text-2 :foreground ,mayukai-dark-orange)
               (message-cited-text-3 :foreground ,mayukai-dark-comment)
               (message-cited-text-4 :foreground ,fg2)
               (message-mml :foreground ,mayukai-dark-green :weight normal)
               ;; mini-modeline
               (mini-modeline-mode-line :inherit mode-line :height 0.1 :box nil)
               ;; mu4e
               (mu4e-unread-face :foreground ,mayukai-dark-pink :weight normal)
               (mu4e-view-url-number-face :foreground ,mayukai-dark-purple)
               (mu4e-highlight-face :background ,mayukai-dark-bg
                                    :foreground ,mayukai-dark-yellow
                                    :extend t)
               (mu4e-header-highlight-face :background ,mayukai-dark-current
                                           :foreground ,mayukai-dark-fg
                                           :underline nil :weight bold
                                           :extend t)
               (mu4e-header-key-face :inherit message-mml)
               (mu4e-header-marks-face :foreground ,mayukai-dark-purple)
               (mu4e-cited-1-face :foreground ,mayukai-dark-purple)
               (mu4e-cited-2-face :foreground ,mayukai-dark-orange)
               (mu4e-cited-3-face :foreground ,mayukai-dark-comment)
               (mu4e-cited-4-face :foreground ,fg2)
               (mu4e-cited-5-face :foreground ,fg3)
               ;; neotree
               (neo-banner-face :foreground ,mayukai-dark-orange :weight bold)
               ;;(neo-button-face :underline nil)
               (neo-dir-link-face :foreground ,mayukai-dark-purple)
               (neo-expand-btn-face :foreground ,mayukai-dark-fg)
               (neo-file-link-face :foreground ,mayukai-dark-cyan)
               (neo-header-face :background ,mayukai-dark-bg
                                :foreground ,mayukai-dark-fg
                                :weight bold)
               (neo-rmayukai-darkt-dir-face :foreground ,mayukai-dark-purple :weight bold)
               (neo-vc-added-face :foreground ,mayukai-dark-orange)
               (neo-vc-conflict-face :foreground ,mayukai-dark-red)
               (neo-vc-default-face :inherit neo-file-link-face)
               (neo-vc-edited-face :foreground ,mayukai-dark-orange)
               (neo-vc-ignored-face :foreground ,mayukai-dark-comment)
               (neo-vc-missing-face :foreground ,mayukai-dark-red)
               (neo-vc-needs-merge-face :foreground ,mayukai-dark-red
                                        :weight bold)
               ;;(neo-vc-needs-update-face :underline t)
               ;;(neo-vc-removed-face :strike-through t)
               (neo-vc-unlocked-changes-face :foreground ,mayukai-dark-red)
               ;;(neo-vc-unregistered-face nil)
               (neo-vc-up-to-date-face :foreground ,mayukai-dark-green)
               (neo-vc-user-face :foreground ,mayukai-dark-purple)
               ;; org
               (org-agenda-date :foreground ,mayukai-dark-cyan :underline nil)
               (org-agenda-dimmed-todo-face :foreground ,mayukai-dark-comment)
               (org-agenda-done :foreground ,mayukai-dark-green)
               (org-agenda-structure :foreground ,mayukai-dark-purple)
               (org-block :foreground ,mayukai-dark-orange)
               (org-code :foreground ,mayukai-dark-green)
               (org-column :background ,bg4)
               (org-column-title :inherit org-column :weight bold :underline t)
               (org-date :foreground ,mayukai-dark-cyan :underline t)
               (org-document-info :foreground ,other-blue)
               (org-document-info-keyword :foreground ,mayukai-dark-comment)
               (org-document-title :weight bold :foreground ,mayukai-dark-orange
                                   ,@(when mayukai-dark-enlarge-headings
                                       (list :height mayukai-dark-height-doc-title)))
               (org-done :foreground ,mayukai-dark-green)
               (org-ellipsis :foreground ,mayukai-dark-comment)
               (org-fmayukai-darktnote :foreground ,other-blue)
               (org-formula :foreground ,mayukai-dark-pink)
               (org-headline-done :foreground ,mayukai-dark-comment
                                  :weight normal :strike-through t)
               (org-hide :foreground ,mayukai-dark-bg :background ,mayukai-dark-bg)
               (org-level-1 :inherit bold :foreground ,mayukai-dark-pink
                            ,@(when mayukai-dark-enlarge-headings
                                (list :height mayukai-dark-height-title-1)))
               (org-level-2 :inherit bold :foreground ,mayukai-dark-purple
                            ,@(when mayukai-dark-enlarge-headings
                                (list :height mayukai-dark-height-title-2)))
               (org-level-3 :weight normal :foreground ,mayukai-dark-green
                            ,@(when mayukai-dark-enlarge-headings
                                (list :height mayukai-dark-height-title-3)))
               (org-level-4 :weight normal :foreground ,mayukai-dark-yellow)
               (org-level-5 :weight normal :foreground ,mayukai-dark-cyan)
               (org-level-6 :weight normal :foreground ,mayukai-dark-orange)
               (org-level-7 :weight normal :foreground ,other-blue)
               (org-level-8 :weight normal :foreground ,mayukai-dark-fg)
               (org-link :foreground ,mayukai-dark-cyan :underline t)
               (org-priority :foreground ,mayukai-dark-cyan)
               (org-quote :foreground ,mayukai-dark-yellow :slant italic)
               (org-scheduled :foreground ,mayukai-dark-green)
               (org-scheduled-previously :foreground ,mayukai-dark-yellow)
               (org-scheduled-today :foreground ,mayukai-dark-green)
               (org-sexp-date :foreground ,fg4)
               (org-special-keyword :foreground ,mayukai-dark-yellow)
               (org-table :foreground ,mayukai-dark-purple)
               (org-tag :foreground ,mayukai-dark-pink :weight bold :background ,bg2)
               (org-todo :foreground ,mayukai-dark-orange :weight bold :background ,bg2)
               (org-upcoming-deadline :foreground ,mayukai-dark-yellow)
               (org-verbatim :inherit org-quote)
               (org-warning :weight bold :foreground ,mayukai-dark-pink)
               ;; outline
               (outline-1 :foreground ,mayukai-dark-pink)
               (outline-2 :foreground ,mayukai-dark-purple)
               (outline-3 :foreground ,mayukai-dark-green)
               (outline-4 :foreground ,mayukai-dark-yellow)
               (outline-5 :foreground ,mayukai-dark-cyan)
               (outline-6 :foreground ,mayukai-dark-orange)
               ;; perspective
               (persp-selected-face :weight bold :foreground ,mayukai-dark-pink)
               ;; powerline
               (powerline-active1 :background ,mayukai-dark-bg :foreground ,mayukai-dark-pink)
               (powerline-active2 :background ,mayukai-dark-bg :foreground ,mayukai-dark-pink)
               (powerline-inactive1 :background ,bg2 :foreground ,mayukai-dark-purple)
               (powerline-inactive2 :background ,bg2 :foreground ,mayukai-dark-purple)
               (powerline-evil-base-face :foreground ,bg2)
               (powerline-evil-emacs-face :inherit powerline-evil-base-face :background ,mayukai-dark-yellow)
               (powerline-evil-insert-face :inherit powerline-evil-base-face :background ,mayukai-dark-cyan)
               (powerline-evil-motion-face :inherit powerline-evil-base-face :background ,mayukai-dark-purple)
               (powerline-evil-normal-face :inherit powerline-evil-base-face :background ,mayukai-dark-green)
               (powerline-evil-operator-face :inherit powerline-evil-base-face :background ,mayukai-dark-pink)
               (powerline-evil-replace-face :inherit powerline-evil-base-face :background ,mayukai-dark-red)
               (powerline-evil-visual-face :inherit powerline-evil-base-face :background ,mayukai-dark-orange)
               ;; rainbow-delimiters
               (rainbow-delimiters-depth-1-face :foreground ,mayukai-dark-fg)
               (rainbow-delimiters-depth-2-face :foreground ,mayukai-dark-cyan)
               (rainbow-delimiters-depth-3-face :foreground ,mayukai-dark-purple)
               (rainbow-delimiters-depth-4-face :foreground ,mayukai-dark-pink)
               (rainbow-delimiters-depth-5-face :foreground ,mayukai-dark-orange)
               (rainbow-delimiters-depth-6-face :foreground ,mayukai-dark-green)
               (rainbow-delimiters-depth-7-face :foreground ,mayukai-dark-yellow)
               (rainbow-delimiters-depth-8-face :foreground ,other-blue)
               (rainbow-delimiters-unmatched-face :foreground ,mayukai-dark-orange)
               ;; rpm-spec
               (rpm-spec-dir-face :foreground ,mayukai-dark-green)
               (rpm-spec-doc-face :foreground ,mayukai-dark-pink)
               (rpm-spec-ghost-face :foreground ,mayukai-dark-purple)
               (rpm-spec-macro-face :foreground ,mayukai-dark-yellow)
               (rpm-spec-obsolete-tag-face :inherit font-lock-warning-face)
               (rpm-spec-package-face :foreground ,mayukai-dark-purple)
               (rpm-spec-section-face :foreground ,mayukai-dark-yellow)
               (rpm-spec-tag-face :foreground ,mayukai-dark-cyan)
               (rpm-spec-var-face :foreground ,mayukai-dark-orange)
               ;; rst (reStructuredText)
               (rst-level-1 :foreground ,mayukai-dark-pink :weight bold)
               (rst-level-2 :foreground ,mayukai-dark-purple :weight bold)
               (rst-level-3 :foreground ,mayukai-dark-green)
               (rst-level-4 :foreground ,mayukai-dark-yellow)
               (rst-level-5 :foreground ,mayukai-dark-cyan)
               (rst-level-6 :foreground ,mayukai-dark-orange)
               (rst-level-7 :foreground ,other-blue)
               (rst-level-8 :foreground ,mayukai-dark-fg)
               ;; selectrum-mode
               (selectrum-current-candidate :weight bold)
               (selectrum-primary-highlight :foreground ,mayukai-dark-pink)
               (selectrum-secondary-highlight :foreground ,mayukai-dark-green)
               ;; show-paren
               (show-paren-match-face :background unspecified
                                      :foreground ,mayukai-dark-purple
                                      :weight bold)
               (show-paren-match :background unspecified
                                 :foreground ,mayukai-dark-purple
                                 :weight bold)
               (show-paren-match-expression :inherit match)
               (show-paren-mismatch :inherit font-lock-warning-face)
               ;; slime
               (slime-repl-inputed-output-face :foreground ,mayukai-dark-purple)
               ;; spam
               (spam :inherit gnus-summary-normal-read :foreground ,mayukai-dark-orange
                     :strike-through t :slant oblique)
               ;; speedbar (and sr-speedbar)
               (speedbar-button-face :foreground ,mayukai-dark-green)
               (speedbar-file-face :foreground ,mayukai-dark-cyan)
               (speedbar-directory-face :foreground ,mayukai-dark-purple)
               (speedbar-tag-face :foreground ,mayukai-dark-yellow)
               (speedbar-selected-face :foreground ,mayukai-dark-pink)
               (speedbar-highlight-face :inherit match)
               (speedbar-separator-face :background ,mayukai-dark-bg
                                        :foreground ,mayukai-dark-fg
                                        :weight bold)
               ;; tab-bar & tab-line (since Emacs 27.1)
               (tab-bar :foreground ,mayukai-dark-purple :background ,mayukai-dark-current
                        :inherit variable-pitch)
               (tab-bar-tab :foreground ,mayukai-dark-pink :background ,mayukai-dark-bg
                            :box (:line-width 2 :color ,mayukai-dark-bg :style nil))
               (tab-bar-tab-inactive :foreground ,mayukai-dark-purple :background ,bg2
                                     :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line :foreground ,mayukai-dark-purple :background ,mayukai-dark-current
                         :height 0.9 :inherit variable-pitch)
               (tab-line-tab :foreground ,mayukai-dark-pink :background ,mayukai-dark-bg
                             :box (:line-width 2 :color ,mayukai-dark-bg :style nil))
               (tab-line-tab-inactive :foreground ,mayukai-dark-purple :background ,bg2
                                      :box (:line-width 2 :color ,bg2 :style nil))
               (tab-line-tab-current :inherit tab-line-tab)
               (tab-line-close-highlight :foreground ,mayukai-dark-red)
               ;; telephone-line
               (telephone-line-accent-active :background ,mayukai-dark-bg :foreground ,mayukai-dark-pink)
               (telephone-line-accent-inactive :background ,bg2 :foreground ,mayukai-dark-purple)
               (telephone-line-unimportant :background ,mayukai-dark-bg :foreground ,mayukai-dark-comment)
               ;; term
               (term :foreground ,mayukai-dark-fg :background ,mayukai-dark-bg)
               (term-color-black :foreground ,mayukai-dark-bg :background ,mayukai-dark-comment)
               (term-color-blue :foreground ,mayukai-dark-purple :background ,mayukai-dark-purple)
               (term-color-cyan :foreground ,mayukai-dark-cyan :background ,mayukai-dark-cyan)
               (term-color-green :foreground ,mayukai-dark-green :background ,mayukai-dark-green)
               (term-color-magenta :foreground ,mayukai-dark-pink :background ,mayukai-dark-pink)
               (term-color-red :foreground ,mayukai-dark-red :background ,mayukai-dark-red)
               (term-color-white :foreground ,mayukai-dark-fg :background ,mayukai-dark-fg)
               (term-color-yellow :foreground ,mayukai-dark-yellow :background ,mayukai-dark-yellow)
               ;; tree-sitter
               (tree-sitter-hl-face:attribute :inherit font-lock-constant-face)
               (tree-sitter-hl-face:comment :inherit font-lock-comment-face)
               (tree-sitter-hl-face:constant :inherit font-lock-constant-face)
               (tree-sitter-hl-face:constant.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:constructor :inherit font-lock-constant-face)
               (tree-sitter-hl-face:escape :foreground ,mayukai-dark-pink)
               (tree-sitter-hl-face:function :inherit font-lock-function-name-face)
               (tree-sitter-hl-face:function.builtin :inherit font-lock-builtin-face)
               (tree-sitter-hl-face:function.call :inherit font-lock-function-name-face
                                                  :weight normal)
               (tree-sitter-hl-face:function.macro :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:function.special :inherit font-lock-preprocessor-face)
               (tree-sitter-hl-face:keyword :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:punctuation :foreground ,mayukai-dark-pink)
               (tree-sitter-hl-face:punctuation.bracket :foreground ,mayukai-dark-fg)
               (tree-sitter-hl-face:punctuation.delimiter :foreground ,mayukai-dark-fg)
               (tree-sitter-hl-face:punctuation.special :foreground ,mayukai-dark-pink)
               (tree-sitter-hl-face:string :inherit font-lock-string-face)
               (tree-sitter-hl-face:string.special :foreground ,mayukai-dark-red)
               (tree-sitter-hl-face:tag :inherit font-lock-keyword-face)
               (tree-sitter-hl-face:type :inherit font-lock-type-face)
               (tree-sitter-hl-face:type.parameter :foreground ,mayukai-dark-pink)
               (tree-sitter-hl-face:variable :inherit font-lock-variable-name-face)
               (tree-sitter-hl-face:variable.parameter :inherit tree-sitter-hl-face:variable
                                                       :weight normal)
               ;; undo-tree
               (undo-tree-visualizer-current-face :foreground ,mayukai-dark-orange)
               (undo-tree-visualizer-default-face :foreground ,fg2)
               (undo-tree-visualizer-register-face :foreground ,mayukai-dark-purple)
               (undo-tree-visualizer-unmodified-face :foreground ,mayukai-dark-fg)
               ;; web-mode
               (web-mode-builtin-face :inherit font-lock-builtin-face)
               (web-mode-comment-face :inherit font-lock-comment-face)
               (web-mode-constant-face :inherit font-lock-constant-face)
               (web-mode-css-property-name-face :foreground ,mayukai-dark-lightgreen)
               (web-mode-doctype-face :inherit font-lock-comment-face)
               (web-mode-function-name-face :inherit font-lock-function-name-face)
               (web-mode-html-attr-name-face :foreground ,mayukai-dark-purple)
               (web-mode-html-attr-value-face :foreground ,mayukai-dark-green)
               (web-mode-html-tag-face :foreground ,mayukai-dark-orange)
               (web-mode-keyword-face :foreground ,mayukai-dark-pink)
               (web-mode-type-face :inherit font-lock-type-face)
               (web-mode-warning-face :inherit font-lock-warning-face)
               ;; which-func
               (which-func :inherit font-lock-function-name-face)
               ;; which-key
               (which-key-key-face :inherit font-lock-builtin-face)
               (which-key-command-description-face :inherit default)
               (which-key-separator-face :inherit font-lock-comment-delimiter-face)
               (which-key-local-map-description-face :foreground ,mayukai-dark-green)
               ;; whitespace
               (whitespace-big-indent :foreground ,mayukai-dark-red)
               (whitespace-empty :foreground ,mayukai-dark-red)
               (whitespace-hspace :background ,bg3 :foreground ,mayukai-dark-comment)
               (whitespace-indentation :foreground ,mayukai-dark-red)
               (whitespace-line :background ,mayukai-dark-bg :foreground ,mayukai-dark-pink)
               (whitespace-newline :foreground ,mayukai-dark-bg)
               (whitespace-space :background ,mayukai-dark-current :foreground ,mayukai-dark-bg)
               (whitespace-space-after-tab :foreground ,mayukai-dark-red)
               (whitespace-space-before-tab :foreground ,mayukai-dark-red)
               (whitespace-tab :background ,mayukai-dark-current :foreground ,mayukai-dark-bg)
               (whitespace-trailing :inherit trailing-whitespace)
               ;; yard-mode
               (yard-tag-face :inherit font-lock-builtin-face)
               (yard-directive-face :inherit font-lock-builtin-face))))

  (apply #'custom-theme-set-faces
         'mayukai-dark
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
                       ,(if mayukai-dark-use-24-bit-colors-on-256-colors-terms
                            (funcall expand-with-func 'cadr spec)
                          (funcall expand-with-func 'caddr spec)))
                      (t                       ; should be only tty-like envs
                       ,(funcall expand-with-func 'cadddr spec))))
                   whole-theme))
           whole-theme))

  (apply #'custom-theme-set-variables
         'mayukai-dark
         (let ((get-func
                (pcase (display-color-cells)
                  ((pred (<= 16777216)) 'car) ; fully graphical envs
                  ((pred (<= 256)) 'cadr)     ; terminal withs 256 colors
                  (_ 'caddr))))               ; should be only tty-like envs
           `((ansi-color-names-vector
              [,(funcall get-func (alist-get 'mayukai-dark-bg colors))
               ,(funcall get-func (alist-get 'mayukai-dark-red colors))
               ,(funcall get-func (alist-get 'mayukai-dark-green colors))
               ,(funcall get-func (alist-get 'mayukai-dark-yellow colors))
               ,(funcall get-func (alist-get 'mayukai-dark-comment colors))
               ,(funcall get-func (alist-get 'mayukai-dark-purple colors))
               ,(funcall get-func (alist-get 'mayukai-dark-cyan colors))
               ,(funcall get-func (alist-get 'mayukai-dark-fg colors))])))))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mayukai-dark)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; mayukai-dark-theme.el ends here
