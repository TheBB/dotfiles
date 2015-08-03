(defun dotspacemacs/layers ()
  "Configuration Layers declaration."

  (setq-default

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '((auto-completion
      :variables
      auto-completion-return-key-behavior nil
      auto-completion-tab-key-behavior 'cycle)
     c-c++
     clojure
     csharp
     emacs-lisp
     emoji
     erc
     ess
     extra-langs
     eyebrowse
     fasd
     games
     git
     github
     haskell
     html
     (ibuffer
      :variables
      ibuffer-group-buffers-by nil)
     javascript
     latex
     markdown
     org
     python
     semantic
     shell
     shell-scripts
     smex
     syntax-checking
     themes-megapack
     unimpaired
     version-control

     encoding
     evil-little-word
     evil-indent-textobject
     evil-shift-width
     (modify-theme
      :variables
      modify-theme-headings-inherit-from-default 'all
      modify-theme-headings-same-size 'all
      modify-theme-headings-bold 'all)
     no-dots)

   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages
   '(ag
     avy
     helm-flycheck
     helm-unicode)

   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(julia-mode)

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
   dotspacemacs-verbose-loading t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official

   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents bookmarks projects)

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         material
                         spacemacs-dark
                         spacemacs-light
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

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache

   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil

   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil

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
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90

   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols nil

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
   dotspacemacs-default-package-repository nil)

  ;; User initialization goes here
  (setq-default

   ;; Miscellaneous
   sentence-end-double-space nil
   vc-follow-symlinks t
   ring-bell-function 'ignore
   require-final-newline t
   indent-tabs-mode nil
   tab-width 8

   ;; Backups
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   make-backup-files nil

   ;; Whitespace mode
   whitespace-style '(face tabs tab-mark)
   whitespace-display-mappings '((newline-mark 10 [172 10])
                                 (tab-mark 9 [9655 9]))

   ;; Smartparens
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil

   ;; Magit
   magit-push-always-verify nil

   ;; Flycheck
   flycheck-check-syntax-automatically '(save mode-enabled)

   ;; Matlab
   matlab-auto-fill nil
   matlab-fill-code nil
   matlab-functions-have-end t
   matlab-indent-function-body t

   ;; LaTeX
   font-latex-fontify-script nil
   TeX-newline-function 'reindent-then-newline-and-indent
   shell-default-term-shell "/bin/zsh"

   ;; Web
   web-mode-markup-indent-offset 2

   ;; Org
   org-tags-column -80
   org-clock-into-drawer "LOGBOOK"
   org-log-into-drawer "LOGBOOK"
   org-startup-align-all-tables t
   org-footnote-auto-adjust t
   org-footnote-auto-label 'confirm
   org-M-RET-may-split-line '((headline . nil)
                              (item . nil)
                              (table . nil))

   ;; IRC
   erc-autojoin-channels-alist
   '(("1\\.0\\.0" "#syl20bnr/spacemacs") ; Gitter
     ("freenode\\.net" "#emacs"))
   erc-timestamp-format-left "\n%A %B %e, %Y\n\n"
   erc-timestamp-format-right "%H:%M"
   erc-timestamp-right-column 80
   erc-prompt-for-nickserv-password nil
   erc-image-inline-rescale 300
   erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
   erc-foolish-content '("\\[Github\\].* starred"
                         "\\[Github\\].* forked"
                         "\\[Github\\].* synchronize a Pull Request"
                         "\\[Github\\].* labeled an issue in"
                         "\\[Github\\].* unlabeled an issue in")

   ;; Avy
   avy-all-windows 'all-frames

   ;; IBuffer
   ibuffer-show-empty-filter-groups nil

   ;; Theme modifications
   modify-theme-modifications
   '((monokai (font-lock-comment-face ((t (:slant italic))))
              (font-lock-string-face ((t (:slant italic))))
              (font-lock-doc-face ((t (:slant italic))))
              (font-lock-keyword-face ((t (:weight bold))))
              (font-lock-builtin-face ((t (:foreground "#ff9eb8"))))
              (font-lock-warning-face ((t (:underline nil))))
              (evil-search-highlight-persist-highlight-face
               ((t (:background "#fc5fef" :foreground "#000000"))))
              (region ((t (:background "#998f84"))))
              (erc-timestamp-face
               ((t (:inherit font-lock-comment-face :foreground nil))))
              (web-mode-html-attr-value-face
               ((t (:inherit font-lock-string-face :foreground nil))))
              (web-mode-html-attr-name-face
               ((t (:inherit font-lock-variable-name-face :foreground nil))))
              (web-mode-html-tag-face
               ((t (:inherit font-lock-builtin-face :foreground nil :weight bold))))
              (web-mode-html-tag-bracket-face
               ((t (:inherit web-mode-html-tag-face :foreground nil))))
              (web-mode-comment-face
               ((t (:inherit font-lock-comment-face :foreground nil)))))
     (t (font-latex-slide-title-face
         ((t (:inherit font-lock-type-face :height 1.0 :weight bold)))))
     )))

(defun dotspacemacs/config ()
  "Configuration function.
This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; Utility functions
  (defun bb/define-key (keymap &rest bindings)
    (declare (indent 1))
    (while bindings
      (define-key keymap (pop bindings) (pop bindings))))

  (defmacro bb/remove-from-list (list-var element)
    `(setq ,list-var (remove ,element ,list-var)))

  ;; Miscellaneous
  (use-package warnings
    :defer t
    :config
    (push '(undo discard-info) warning-suppress-types))
  (add-hook 'text-mode-hook 'auto-fill-mode)

  ;; Auto modes
  (setq auto-mode-alist
        (append '(("\\.xml\\'" . web-mode)
                  ("\\.xinp\\'" . web-mode)
                  ("\\.C\\'" . c++-mode)
                  ("\\.h\\'" . c++-mode))
                auto-mode-alist))

  ;; Semantic fucks up scrolling
  (with-eval-after-load 'semantic
    (bb/remove-from-list semantic-submode-list 'global-semantic-stickyfunc-mode))

  ;; Avy
  (evil-leader/set-key
    "SPC" 'avy-goto-subword-1
    "l" 'avy-goto-line)
  (which-key-add-key-based-replacements
   "SPC SPC" "avy word"
   "SPC l" "avy line")

  ;; IBuffer
  (with-eval-after-load 'projectile
    (setq ibuffer-saved-filter-groups
          (list (cons "Default"
                      (append
                       (mapcar (lambda (it)
                                 (let ((name (file-name-nondirectory
                                              (directory-file-name it))))
                                   `(,name (filename . ,(expand-file-name it)))))
                               projectile-known-projects)
                       `(("Org" (mode . org-mode))
                         ("Dired" (mode . dired-mode))
                         ("IRC" (mode . erc-mode))
                         ("Emacs"
                          (or (name . "\\*Messages\\*")
                              (name . "\\*Compile-Log\\*")
                              (name . "\\*scratch\\*")
                              (name . "\\*spacemacs\\*")
                              (name . "\\*emacs\\*")))
                         ("Magit" (name . "\\*magit"))
                         ("Help" (name . "\\*Help\\*"))
                         ("Helm" (name . "\\*helm"))
                         ))))))
  (add-hook 'ibuffer-mode-hook
            (defun bb/switch-ibuffer-group ()
              (ibuffer-switch-to-saved-filter-groups "Default")))
  (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)

  ;; Keybindings
  (bb/define-key evil-normal-state-map
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
    "iu" 'helm-unicode
    "os" 'just-one-space
    "ot" 'helm-etags-select
    "os" 'flycheck-select-checker
    "ov" 'evilmi-select-items)

  ;; Helm mode keys
  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (bb/define-key keymap
        (kbd "C-h") nil
        (kbd "C-l") 'helm-execute-persistent-action
        (kbd "C-h") 'helm-find-files-up-one-level)))

  (setq helm-echo-input-in-header-line nil)

  (use-package helm-flycheck
    :defer t
    :init
    (evil-leader/set-key "eh" 'helm-flycheck))

  ;; C/C++ styles
  (c-add-style "bb"
               '((indent-tabs-mode . nil)
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (substatement-open . 0)
                  (inline-open . 0)
                  (statement-cont . c-lineup-assignments)
                  (inextern-lang . 0)
                  (innamespace . 0))))

  (c-add-style "sintef"
               '((indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-offsets-alist
                  (substatement-open . 0)
                  (inline-open . 0)
                  (statement-cont . c-lineup-assignments)
                  (inextern-lang . 0)
                  (innamespace . 0))))

  (dolist (style c-style-alist)
    (let* ((style-name (car style))
           (style-fcn (intern (format "bb/set-style-%s" style-name))))
      (eval `(defun ,style-fcn () (interactive) (c-set-style ,style-name)))))

  (add-hook 'c-mode-common-hook
            (defun bb/c-style ()
              (c-set-style "bb")
              (setq c-macro-names-with-semicolon
                    '("Q_OBJECT"
                      "Q_PROPERTY"
                      "Q_DECLARE"
                      "Q_ENUMS"
                      "Q_INTERFACES"))
              (c-make-macro-with-semi-re)))

  ;; Evil $ and g$
  (evil-define-motion evil-last-non-blank (count)
    "Move the cursor to the last non-blank character
on the current line. If COUNT is given, move COUNT - 1
lines downward first."
    :type inclusive
    (evil-end-of-line count)
    (re-search-backward "^\\|[^[:space:]]")
    (setq evil-this-type (if (eolp) 'exclusive 'inclusive)))
  (define-key evil-motion-state-map "g$" 'evil-end-of-line)
  (define-key evil-motion-state-map "$" 'evil-last-non-blank)

  ;; Some fixes for comint-style buffers
  (dolist (mode '(erc-mode comint-mode term-mode))
    (bb/remove-from-list evil-insert-state-modes mode))

  (let ((comint-hooks '(eshell-mode-hook
                        term-mode-hook
                        erc-mode-hook
                        messages-buffer-mode-hook)))
    (add-to-hooks (defun bb/no-hl-line-mode ()
                    (setq-local global-hl-line-mode nil))
                  comint-hooks)
    (add-to-hooks (defun bb/no-scroll-margin ()
                    (setq-local scroll-margin 0))
                  comint-hooks))

  ;; Org
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (evil-leader/set-key-for-mode 'org-mode
    "m*" 'org-ctrl-c-star
    "m RET" 'org-ctrl-c-ret
    "m-" 'org-ctrl-c-minus)

  ;; Makefiles
  (add-hook 'makefile-mode-hook 'whitespace-mode)

  ;; LaTeX
  (add-hook 'LaTeX-mode-hook
            (defun bb/shift-width-2 ()
              (setq-local evil-shift-width 2)))
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
          ("ldots" "" nil 'noarg))
        font-latex-match-reference-keywords
        '(("autoref" "{")
          ("inst" "{"))
        )

  ;; IRC
  (add-hook 'erc-insert-pre-hook
            (defun bb/erc-foolish-filter (msg)
              "Ignores messages matching `erc-foolish-content'."
              (when (erc-list-match erc-foolish-content msg)
                (setq erc-insert-this nil))))

  (add-hook 'erc-insert-modify-hook
            (defun bb/erc-github-filter ()
              "Shortens messages from gitter."
              (interactive)
              (when (and (< 18 (- (point-max) (point-min)))
                         (string= (buffer-substring (point-min)
                                                    (+ (point-min) 18))
                                  "<gitter> [Github] "))
                (dolist (regexp '(" \\[Github\\]"
                                  " \\(?:in\\|to\\) [^ /]+/[^ /:]+"
                                  "https?://github\\.com/[^/]+/[^/]+/[^/]+/"
                                  "#issuecomment-[[:digit:]]+"))
                  (goto-char (point-min))
                  (when (re-search-forward regexp (point-max) t)
                    (replace-match "")))
                (goto-char (point-min))
                (when (re-search-forward "[[:digit:]]+$" (point-max) t)
                  (replace-match (format "(#%s)" (match-string 0)))))))

  (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode)
  (setq erc-modules (remove 'track erc-modules))
  (setq erc-modules (remove 'youtube erc-modules))
  (erc-track-mode -1)

  (defun bb/irc ()
    (interactive)
    (erc-tls :server "irc.gitter.im"
             :port "6667"
             :nick "TheBB"
             :password bb/gitter-pwd
             :full-name bb/full-name)
    (erc :server "irc.freenode.net"
         :port "6667"
         :nick "TheBB"
         :full-name bb/full-name))
  (evil-leader/set-key
    "aii" 'bb/irc
    "aiq" 'erc-quit-server)

  ;; Local variables
  (setq safe-local-variable-values
        '((flycheck-checker . python-flake8-py2)))

  ;; Modeline separators
  (setq powerline-default-separator 'alternate)

  ;; Load local
  (when (file-exists-p "~/local.el")
    (load "~/local.el"))
  )
