(defun dotspacemacs/layers ()
  (setq-default

   dotspacemacs-configuration-layer-path '()
   dotspacemacs-delete-orphan-packages t

   dotspacemacs-configuration-layers
   `((auto-completion
      :disabled-for org erc)
     c-c++
     clojure
     csharp
     django
     emacs-lisp
     emoji
     erc
     ess
     extra-langs
     eyebrowse
     games
     git
     github
     haskell
     html
     ibuffer
     javascript
     latex
     markdown
     org
     python
     ranger
     semantic
     shell
     shell-scripts
     smex
     spell-checking
     syntax-checking
     unimpaired
     version-control

     ,@(unless (string= system-type "windows-nt")
         '(fasd
           gtags))

     ;; Non-contrib layers
     encoding
     evil-little-word
     evil-shift-width
     modify-theme
     no-dots

     ;; Personal config layers
     bb-c-styles
     bb-ibuffer)

   dotspacemacs-additional-packages
   '(ag
     helm-flycheck
     help-fns+
     nginx-mode
     lorem-ipsum)

   dotspacemacs-excluded-packages
   '(julia-mode
     hl-anything
     evil-terminal-cursor-changer)))

(defun dotspacemacs/init ()
  (setq-default

   ;; Layers
   auto-completion-return-key-behavior nil
   auto-completion-tab-key-behavior 'cycle
   ibuffer-group-buffers-by nil
   modify-theme-headings-inherit-from-default 'all
   modify-theme-headings-same-size 'all
   modify-theme-headings-bold 'all
   shell-default-shell 'eshell

   ;; Dotfile variables
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading t
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-themes
   '(monokai material spacemacs-dark spacemacs-light solarized-dark leuven zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font
   `("Source Code Pro"
     :size ,(if (string= system-type "windows-nt") 16 13)
     :weight normal :width normal :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize t
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil

   ;; Miscellaneous
   sentence-end-double-space nil
   vc-follow-symlinks t
   ring-bell-function 'ignore
   require-final-newline t
   indent-tabs-mode nil

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
   whitespace-display-mappings
   '((newline-mark 10 [172 10])
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
   org-M-RET-may-split-line
   '((headline . nil) (item . nil) (table . nil))
   org-directory "~/org"
   org-default-notes-file "~/org/capture.org"
   org-capture-templates
   '(("t" "Tasks")
     ("tg" "General" entry (file+headline "" "Tasks")
      "* TODO %?\n%i\n%T"
      :empty-lines 1)
     ("tl" "Location" entry (file+headline "" "Tasks")
      "* TODO %?\n%i\n%T\n%a"
      :empty-lines 1)
     ("n" "Notes")
     ("ng" "General" entry (file+headline "" "Notes")
      "* %?\n%i\n%T"
      :empty-lines 1)
     ("nl" "Location" entry (file+headline "" "Notes")
      "* %?\n%i\n%T\n%a"
      :empty-lines 1))

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
   erc-foolish-content
   '("\\[Github\\].* starred"
     "\\[Github\\].* forked"
     "\\[Github\\].* synchronize a Pull Request"
     "\\[Github\\].* labeled an issue in"
     "\\[Github\\].* unlabeled an issue in")

   ;; Avy
   avy-all-windows 'all-frames

   ;; Theme modifications
   modify-theme-modifications
   '((monokai
      (font-lock-comment-face ((t (:slant italic))))
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
     (t
      (font-latex-slide-title-face
       ((t (:inherit font-lock-type-face :height 1.0 :weight bold)))))
     )))

(defun dotspacemacs/user-config ()

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
  (when (eq 'hybrid dotspacemacs-editing-style)
    (diminish 'hybrid-mode))
  (with-eval-after-load 'emoji-cheat-sheet-plus
    (diminish 'emoji-cheat-sheet-plus-display-mode))

  (setq-default tab-width 8
                evil-move-beyond-eol nil)

  ;; Auto modes
  (setq auto-mode-alist
        (append '(("\\.xml\\'" . web-mode)
                  ("\\.xinp\\'" . web-mode)
                  ("\\.C\\'" . c++-mode)
                  ("\\.h\\'" . c++-mode))
                auto-mode-alist))

  ;; Disable smartparens highlighting
  (with-eval-after-load 'smartparens
    (when show-smartparens-global-mode
      (show-smartparens-global-mode -1)))

  ;; Semantic fucks up scrolling
  (with-eval-after-load 'semantic
    (bb/remove-from-list semantic-submode-list 'global-semantic-stickyfunc-mode))

  ;; Switching buffer
  (evil-leader/set-key
    "TAB" (defun bb/alternate-buffer ()
            (interactive)
            (if (evil-alternate-buffer)
                (switch-to-buffer (car (evil-alternate-buffer)))
              (call-interactively 'spacemacs/alternate-buffer))))

  ;; Keybindings
  (bb/define-key evil-normal-state-map
    (kbd "RET") nil
    "gr" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
    "+" 'evil-numbers/inc-at-pt
    "_" 'evil-numbers/dec-at-pt
    "\\" 'evil-repeat-find-char-reverse
    "gt" 'eyebrowse-next-window-config
    "gT" 'eyebrowse-prev-window-config)
  (bb/define-key evil-motion-state-map
    (kbd "RET") 'smex)
  (evil-leader/set-key
    "oo" 'org-capture
    "os" 'just-one-space
    "ot" 'helm-etags-select
    "os" 'flycheck-select-checker
    "ov" 'evilmi-select-items
    "oh" (defun bb/highlight ()
           (interactive)
           (hlt-highlight-region)
           (keyboard-quit))
    "oH" (defun bb/unhighlight ()
           (interactive)
           (hlt-unhighlight-region)
           (keyboard-quit)))

  ;; Gtags bindings in extra modes
  (dolist (mode '(python-mode emacs-lisp-mode))
    (spacemacs/helm-gtags-define-keys-for-mode mode))
  (with-eval-after-load 'helm-gtags
    (diminish 'helm-gtags-mode))

  ;; Don't quit because of old habits
  (evil-ex-define-cmd "q[uit]" (message "quit disabled"))
  (evil-ex-define-cmd "wq" (message "quit disabled"))

  ;; Helm mode keys
  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (bb/define-key keymap
        (kbd "C-h") nil
        (kbd "C-l") 'helm-execute-persistent-action
        (kbd "C-h") 'helm-find-files-up-one-level)))

  (setq helm-echo-input-in-header-line nil)

  ;; Flyheck
  (use-package helm-flycheck
    :defer t
    :init
    (evil-leader/set-key "eh" 'helm-flycheck))

  (evil-leader/set-key
    "ec" 'flycheck-clear)

  (mapatoms (lambda (atom)
              (when (and (string-suffix-p "-hook" (symbol-name atom))
                         (boundp atom)
                         (listp (eval atom)))
                (remove-hook atom 'flycheck-mode)
                (remove-hook atom 'flyspell-mode))))

  ;; Some fixes for comint-style buffers
  (dolist (mode '(erc-mode comint-mode term-mode eshell-mode inferior-emacs-lisp-mode))
    (bb/remove-from-list evil-insert-state-modes mode))

  (let ((comint-hooks '(eshell-mode-hook
                        term-mode-hook
                        erc-mode-hook
                        messages-buffer-mode-hook
                        inferior-emacs-lisp-mode-hook)))
    (spacemacs/add-to-hooks (defun bb/no-hl-line-mode ()
                              (setq-local global-hl-line-mode nil))
                            comint-hooks)
    (spacemacs/add-to-hooks (defun bb/no-scroll-margin ()
                              (setq-local scroll-margin 0))
                            comint-hooks))
  (add-hook 'inferior-emacs-lisp-mode-hook 'smartparens-mode)

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

  (defun bb/erc-github-filter ()
    "Shortens messages from gitter."
    (interactive)
    (when (and (< 18 (- (point-max) (point-min)))
               (string= (buffer-substring (point-min)
                                          (+ (point-min) 18))
                        "<gitter> [Github] "))
      (dolist (regexp '(" \\[Github\\]"
                        " \\(?:in\\|to\\) [^ /]+/[^ /:]+"))
        (goto-char (point-min))
        (when (re-search-forward regexp (point-max) t)
          (replace-match "")))
      (goto-char (point-min))
      (when (re-search-forward
             "https?://github\\.com/[^/]+/[^/]+/[^/]+/\\([[:digit:]]+\\)\\([^[:space:]]*\\)?"
             (point-max) t)
        (let* ((url (match-string 0))
               (number (match-string 1))
               (start (+ 1 (match-beginning 0)))
               (end (+ 1 (length number) start)))
          (replace-match (format "(#%s)" (match-string 1)))
          (erc-button-add-button start end 'browse-url nil (list url)))
        )))

  (with-eval-after-load 'erc
    (setq erc-insert-modify-hook
          '(erc-controls-highlight
            erc-button-add-buttons
            bb/erc-github-filter
            erc-fill
            erc-match-message
            erc-add-timestamp
            erc-hl-nicks)))

  (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode)
  (dolist (module '(track youtube image))
    (bb/remove-from-list erc-modules module))
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

  ;; Additional packages
  (use-package nginx-mode
    :defer t
    :mode ("nginx\\.conf\\'" "/etc/nginx/.*\\'"))
  (use-package lorem-ipsum
    :defer t)
  (require 'help-fns+)

  ;; Load local
  (when (file-exists-p "~/local.el")
    (load "~/local.el"))
  )
