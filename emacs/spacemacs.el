(defun dotspacemacs/layers ()
  "Configuration Layers declaration."

  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(auctex
     auto-completion
     c-c++
     clojure
     csharp
     emacs-lisp
     eyebrowse
     extra-langs
     games
     git
     haskell
     html
     javascript
     markdown
     org
     python
     rcirc
     shell
     shell-scripts
     smex
     syntax-checking
     themes-megapack

     evil-little-word
     non-ascii
     )

   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()

   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."

  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim

   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official

   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects bookmarks)

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         material
                         solarized-dark
                         leuven
                         zenburn)

   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"

   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil

   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t

   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4

   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t

   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90

   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t

   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil

   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all

   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")

   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )

  ;; User initialization goes here
  (setq-default
   sentence-end-double-space nil
   git-gutter-use-fringe t
   make-backup-files nil
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   vc-follow-symlinks t
   ring-bell-function 'ignore
   require-final-newline t
   indent-tabs-mode nil
   whitespace-style '(face tabs tab-mark)
   whitespace-display-mappings '((newline-mark 10 [172 10])
                                 (tab-mark 9 [9655 9]))
   tab-width 8
   matlab-auto-fill nil
   matlab-fill-code nil
   matlab-functions-have-end t
   matlab-indent-function-body t
   font-latex-fontify-script nil
   TeX-newline-function 'reindent-then-newline-and-indent
   )
  )

(defun dotspacemacs/config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."

  (push '(undo discard-info) warning-suppress-types)

  ;; Custom leader and evil normal state keybindings
  (defun bb-def (keymap &rest bindings)
    (while bindings
      (define-key keymap (pop bindings) (pop bindings))))

  (bb-def evil-normal-state-map
          (kbd "<S-backspace>") 'spacemacs/insert-line-above-no-indent
          (kbd "<backspace>") 'spacemacs/insert-line-below-no-indent
          "gr" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
          "+" 'evil-numbers/inc-at-pt
          "_" 'evil-numbers/dec-at-pt
          "\\" 'evil-repeat-find-char-reverse
          "gt" 'eyebrowse-next-window-config
          "gT" 'eyebrowse-prev-window-config)
  (evil-leader/set-key
    "FN" 'set-frame-name
    "Fn" 'select-frame-by-name
    "FF" 'select-frame-by-name
    "Fo" 'other-frame
    "os" 'just-one-space)

  ;; Helm mode keys
  (with-eval-after-load "helm-files"
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (bb-def keymap
              (kbd "C-h") nil
              (kbd "C-l") 'helm-execute-persistent-action
              (kbd "C-h") 'helm-find-files-up-one-level)))

  ;; Fix up monokai a bit
  (let ((bwc-purple "#fc5fef")
        (bwc-black "#000000")
        (bwc-lightgravel "#998f84")
        (bwc-taffy "#ff2c4b")
        (bwc-dress "#ff9eb8"))
    (custom-set-faces
     `(font-lock-builtin-face ((t (:foreground ,bwc-dress))))
     `(font-lock-keyword-face ((t (:weight bold))))
     `(font-lock-comment-face ((t (:slant italic))))
     `(font-lock-string-face ((t (:slant italic))))
     `(font-lock-doc-face ((t (:slant italic))))
     `(font-lock-warning-face ((t (:underline nil))))
     `(evil-search-highlight-persist-highlight-face
       ((t (:background ,bwc-purple :foreground ,bwc-black))))
     `(region ((t (:background ,bwc-lightgravel))))
     `(font-latex-slide-title-face ((t (:inherit font-lock-type-face :height 1.0))))
     `(font-latex-sectioning-1-face ((t (:height 1.0))))
     `(font-latex-sectioning-2-face ((t (:height 1.0))))
     `(font-latex-sectioning-3-face ((t (:height 1.0))))
     `(font-latex-sectioning-4-face ((t (:height 1.0))))
     `(font-latex-sectioning-5-face ((t (:inherit default :height 1.0))))
     `(org-level-1 ((t (:inherit default :height 1.0 :weight bold))))
     `(org-level-2 ((t (:inherit default :height 1.0 :weight bold))))
     `(org-level-3 ((t (:inherit default :height 1.0 :weight bold))))
     `(org-level-4 ((t (:inherit default :height 1.0 :weight bold))))
     `(org-level-5 ((t (:inherit default :height 1.0 :weight bold))))
     `(org-level-6 ((t (:inherit default :height 1.0 :weight bold))))
     `(org-level-7 ((t (:inherit default :height 1.0 :weight bold))))
     `(org-level-8 ((t (:inherit default :height 1.0 :weight bold))))
     `(markdown-header-face ((t (:weight bold))))
     `(markdown-header-face-1 ((t (:height 1.0))))
     `(markdown-header-face-2 ((t (:height 1.0))))
     `(markdown-header-face-3 ((t (:height 1.0))))
     `(markdown-header-face-4 ((t (:height 1.0))))
     `(markdown-header-face-5 ((t (:height 1.0))))
     `(markdown-header-face-6 ((t (:height 1.0))))
     ))

  ;; C/C++ styles
  (c-add-style "bb-style"
               '((indent-tabs-mode . nil)
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (substatement-open . 0)
                  (inline-open . 0)
                  (statement-cont . c-lineup-assignments)
                  (inextern-lang . 0)
                  (innamespace . 0))))

  (c-add-style "sintef-style"
               '((indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-offsets-alist
                  (substatement-open . 0)
                  (inline-open . 0)
                  (statement-cont . c-lineup-assignments)
                  (inextern-lang . 0)
                  (innamespace . 0))))

  (defun bb-style () (interactive) (c-set-style "bb-style"))
  (defun sintef-style () (interactive) (c-set-style "sintef-style"))

  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-style "bb-style")
              (setq c-macro-names-with-semicolon
                    '("Q_OBJECT"
                      "Q_PROPERTY"
                      "Q_DECLARE"
                      "Q_ENUMS"
                      "Q_INTERFACES"))
              (c-make-macro-with-semi-re)))

  ;; Some fixes for comint-style buffers
  (dolist (hook '(eshell-mode-hook term-mode-hook rcirc-mode-hook))
    (add-hook hook
              (lambda ()
                (set (make-local-variable 'global-hl-line-mode) nil)
                (set (make-local-variable 'scroll-margin) 0)
                )))

  ;; LaTeX
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-shift-width) 2)
              (auto-fill-mode)))
  (setq font-latex-match-slide-title-keywords
        '(("frametitle" "{"))
        font-latex-match-function-keywords
        '(("setbeamercovered" "{")
          ("usetheme" "{")
          ("usecolortheme" "{")
          ("usetikzlibrary" "{")
          ("reserveinserts" "{")
          ("address" "{")
          ("definecolor" "{{{")
          ("includegraphics" "[{")
          ("titlegraphic" "{")
          ("newacronym" "{{{"))
        font-latex-match-textual-keywords
        '(("hfill" "" nil 'noarg)
          ("textwidth" "" nil 'noarg)
          ("titlepage" "" nil 'noarg)
          ("and" "")
          ("institute" "[{")
          ("abstract" "{")
          ("overview" "{")
          ("doList" "{")
          ("challengeList" "{")
          ("ldots" ""))
        font-latex-match-reference-keywords
        '(("autoref" "{")
          ("inst" "{"))
        )

  ;; IRC
  (setq rcirc-server-alist nil
        rcirc-time-format "%H:%M ")
  (add-hook 'rcirc-mode-hook
            (lambda ()
              (set (make-local-variable 'scroll-conservatively) 8192)
              (setq rcirc-time-format "%H:%M ")
              (dolist (color '("blue" "blue1" "blue2" "blue3" "blue4" "mediumblue" "darkblue"))
                (setq rcirc-colors (delete color rcirc-colors)))
              ))
  (defun bb-rcirc-generate-log-filename (process target)
    (if target
        (rcirc-generate-log-filename
         process (replace-regexp-in-string "/" "+" target))
      (rcirc-generate-log-filename process nil)))
  (setq rcirc-log-filename-function 'bb-rcirc-generate-log-filename)

  ;; Load local
  (when (file-exists-p "~/local.el")
    (load "~/local.el"))
  )
