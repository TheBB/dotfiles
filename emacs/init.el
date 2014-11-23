;; Package initialization
;; =================================================================================

(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(package-initialize)

(require 'use-package)
(require 'cl)


;; Backups
;; =================================================================================

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq-default make-backup-files nil)


;; User interface
;; =================================================================================

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(set-cursor-color "#0a9dff")
(load-theme 'badwolf t)
(global-hl-line-mode t)
(make-variable-buffer-local 'global-hl-line-mode)

(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq linum-format "%4d ")
(add-hook 'prog-mode-hook 'linum-mode)

(visual-line-mode)


;; Whitespace and indentation
;; =================================================================================

(setq require-final-newline t)
(setq-default indent-tabs-mode nil)


;; Varia
;; =================================================================================

(setq vc-follow-symlinks nil
      warning-suppress-styles '((undo discard-info)))
(setq-default fill-column 100)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


;; Whitespace mode
;; =================================================================================

(setq whitespace-style
      '(face
        tabs
        tab-mark))
(setq whitespace-display-mappings
      '((newline-mark 10 [172 10])
        (tab-mark 9 [9655 9])))
(global-whitespace-mode)


;; Powerline
;; =================================================================================

(defun bb/powerline-evil-theme ()
  "My powerline theme."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (separator-left (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (cdr powerline-default-separator-dir))))
             (state-map
              `((normal   . ("NORMAL "
                             powerline-normal-1 powerline-normal-2 powerline-normal-3))
                (insert   . ("INSERT "
                             powerline-insert-1 powerline-insert-2 powerline-insert-3))
                (replace  . ("REPLCE "
                             powerline-replace-1 powerline-insert-2 powerline-insert-3))
                (operator . ("OPRTOR "
                             powerline-normal-1 powerline-normal-2 powerline-normal-3))
                (visual   . ("VISUAL "
                             powerline-visual-1 powerline-visual-2 powerline-visual-3))
                (motion   . ("MOTION "
                             powerline-normal-1 powerline-normal-2 powerline-normal-3))
                (emacs    . ("EMACS  "
                             powerline-emacs-1 powerline-normal-2 powerline-normal-3))))
             (gf (lambda (idx)
                   (if active (funcall sg idx) 'powerline-normal-3)))
             (sg (lambda (idx) (nth idx (cdr (assoc evil-state state-map)))))
             (lhs (list (powerline-raw
                         (if (fboundp 'window-numbering-get-number-string)
                             (window-numbering-get-number-string) "?")
                         (funcall gf 1) 'l)
                        (powerline-raw
                         (if active (funcall sg 0) "------ ")
                         (funcall gf 1) 'l)
                        (funcall separator-left (funcall gf 1) (funcall gf 2))
                        (powerline-buffer-id (funcall gf 2) 'l)
                        (powerline-raw "%* " (funcall gf 2))
                        (funcall separator-left (funcall gf 2) (funcall gf 3))))
             (rhs (list (powerline-minor-modes (funcall gf 3) 'r)
                        (funcall separator-right (funcall gf 3) (funcall gf 2))
                        (powerline-raw " " (funcall gf 2))
                        (powerline-major-mode (funcall gf 2) 'r)
                        (funcall separator-right (funcall gf 2) (funcall gf 1))
                        (powerline-raw " %7p%4l:%3c" (funcall gf 1) 'r))))
        (concat (powerline-render lhs)
                (powerline-fill (funcall gf 3) (powerline-width rhs))
                (powerline-render rhs)))))))

(use-package powerline
  :ensure powerline
  :init
  (progn
    (setq powerline-default-separator 'slant)
    (bb/powerline-evil-theme)))


;; Evil and friends
;; =================================================================================

(use-package evil
  :load-path "sources/evil"
  :pre-load
  (setq evil-want-C-u-scroll t
        evil-want-C-w-in-emacs-state t)
  :init
  (progn
    (use-package evil-leader
      :ensure evil-leader
      :pre-load (setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "org-agenda-mode"))
      :init
      (progn
        (setq evil-leader/in-all-states t
              evil-leader/leader "SPC"
              evil-leader/non-normal-prefix "C-")

        (global-evil-leader-mode t)
        (evil-leader/set-key
          "hi" (lambda () (interactive) (find-file user-init-file))
          "ht" (lambda () (interactive)
                 (find-file (expand-file-name "themes/badwolf-theme.el" user-emacs-directory)))
          "ss" 'just-one-space
          "m" (lambda () (interactive) (message "Mode: %s" major-mode))
          "`" 'other-frame
          "u" 'universal-argument)))

    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :pre-load
      (setq-default evilnc-hotkey-comment-operator "gc")
      :init
      (progn
        (evil-leader/set-key
          "ci" 'evilnc-comment-or-uncomment-lines
          "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
          "cc" 'evilnc-copy-and-comment-lines
          "cp" 'evilnc-comment-or-uncomment-paragraphs
          "cr" 'comment-or-uncomment-region
          "cv" 'evilnc-toggle-invert-comment-line-by-line)))

    (use-package key-chord
      :ensure key-chord
      :init
      (progn
        (key-chord-mode t)
        (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
        (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)))

    (use-package evil-numbers
      :ensure evil-numbers
      :init
      (progn
        (define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
        (define-key evil-normal-state-map "_" 'evil-numbers/dec-at-pt)))

    (use-package evil-args
      :ensure evil-args
      :init
      (progn
        (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
        (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
        (define-key evil-normal-state-map "gs" 'evil-forward-arg)
        (define-key evil-normal-state-map "ga" 'evil-backward-arg)
        (define-key evil-motion-state-map "gs" 'evil-forward-arg)
        (define-key evil-motion-state-map "ga" 'evil-backward-arg)))

    (use-package evil-surround
      :ensure evil-surround
      :commands global-evil-surround-mode
      :init
      (global-evil-surround-mode t))

    (use-package evil-matchit
      :ensure evil-matchit
      :commands global-evil-matchit-mode
      :idle (global-evil-matchit-mode t))

    (use-package evil-visualstar
      :ensure evil-visualstar)

    (use-package evil-little-word
      :load-path "sources/misc")

    (use-package evil-indent-textobject
      :load-path "sources/evil-indent-textobject"))

  :config
  (progn
    (evil-mode t)
    (define-key evil-normal-state-map [backspace]
      (lambda (n) (interactive "p") (save-excursion (move-end-of-line 0) (open-line n))))
    (define-key evil-normal-state-map (kbd "RET")
      (lambda (n) (interactive "p") (save-excursion (move-end-of-line 1) (open-line n))))
    (define-key evil-normal-state-map (kbd "gr")
      (lambda (n) (interactive "p") (dotimes (c n nil) (insert " "))))

    (global-set-key (kbd "RET") 'newline-and-indent)

    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)

    (define-key evil-normal-state-map "]b" 'evil-next-buffer)
    (define-key evil-normal-state-map "[b" 'evil-prev-buffer)
    (define-key evil-normal-state-map "j" 'evil-next-visual-line)
    (define-key evil-normal-state-map "k" 'evil-previous-visual-line)

    (evil-ex-define-cmd "dtw" 'delete-trailing-whitespace)
    (evil-ex-define-cmd "h" 'help)))



;; Use esc to get away from everything, like in vim
(defun bb/minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key minibuffer-local-map [escape] 'bb/minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'bb/minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'bb/minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'bb/minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'bb/minibuffer-keyboard-quit)


;; Ag
;; =================================================================================

(use-package ag
  :ensure ag)


;; Helm
;; =================================================================================

(use-package helm
  :ensure helm
  :init
  (evil-leader/set-key
    "x" 'helm-M-x
    "b" 'helm-mini
    "fd" 'helm-find-files
    "v" 'helm-show-kill-ring
    "fs" 'helm-semantic-or-imenu)
  :config
  (progn
    (use-package helm-config)
    (use-package helm-ag
      :ensure helm-ag
      :init
      (evil-leader/set-key
        "ag" 'helm-ag
        "af" 'helm-ag-this-file
        "ad" 'helm-do-ag))
    (setq helm-buffers-fuzzy-matching t)
    (define-key helm-map (kbd "M-j") 'helm-next-line)
    (define-key helm-map (kbd "M-k") 'helm-previous-line)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "M-z") 'helm-select-action)))


;; IDO
;; =================================================================================

(use-package ido-ubiquitous
  :ensure ido-ubiquitous)

(use-package flx-ido
  :ensure flx-ido)

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;; Dired
;; =================================================================================

(evil-define-key 'normal dired-mode-map
  [delete] 'dired-unmark-backward
  "J" 'dired-goto-file)


;; Magit
;; =================================================================================

(use-package magit
  :ensure magit
  :init
  (evil-leader/set-key
    "gs" 'magit-status
    "gb" 'magit-blame-mode
    "gl" 'magit-log
    "gd" 'magit-diff)
  :config
  (progn
    (evil-make-overriding-map magit-mode-map 'emacs)

    (evil-define-key 'emacs magit-mode-map
      [escape] 'keyboard-quit
      "j" 'magit-goto-next-section
      "k" 'magit-goto-previous-section
      "h" 'magit-goto-parent-section
      "gj" 'magit-goto-next-sibling-section
      "gk" 'magit-goto-previous-sibling-section
      "K" 'magit-discard-item
      "\\" 'magit-git-command
      ":" 'evil-ex)))

(setq vc-handled-backends nil)


;; Projectile
;; =================================================================================

(use-package projectile
  :ensure projectile
  :config
  (progn
    (evil-leader/set-key
      "ap" 'projectile-ag
      "fo" 'helm-projectile-find-other-file)
    (projectile-global-mode)))

(use-package helm-projectile
  :ensure helm-projectile
  :init
  (evil-leader/set-key
    "fp" 'helm-projectile
    "fr" 'helm-projectile-find-file))


;; Company
;; =================================================================================

(use-package company
  :commands company-mode
  :ensure company
  :config
  (progn
    (add-to-list 'completion-styles 'substring)
    (define-key company-active-map [escape] 'company-abort)
    (define-key company-active-map (kbd "M-f") 'company-filter-candidates)
    (define-key company-active-map (kbd "M-j") 'company-select-next)
    (define-key company-active-map (kbd "M-k") 'company-select-previous)))


;; Expand region
;; =================================================================================

(use-package expand-region
  :ensure expand-region
  :config
  (progn
    (define-key evil-normal-state-map (kbd "C-=") 'er/expand-region)
    (define-key evil-visual-state-map (kbd "C-=") 'er/expand-region)))


;; Popwin
;; =================================================================================

(use-package popwin
  :ensure popwin
  :init (popwin-mode t)
  :config
  (progn
    (global-set-key (kbd "C-`") 'popwin:close-popup-window)
    (push '("*helm*" :height 20) popwin:special-display-config)
    (push '("^\\*helm ?[^\\*]+\\*$" :regexp t :height 20) popwin:special-display-config)
    (push '("*compilation*" :height 10 :noselect t) popwin:special-display-config)
    (push '("*haskell-compilation*" :height 10 :noselect t) popwin:special-display-config)))


;; Diminish
;; =================================================================================

(use-package diminish
  :ensure diminish
  :init
  (progn
    (diminish 'undo-tree-mode)
    (diminish 'abbrev-mode)
    (diminish 'global-whitespace-mode)
    (diminish 'magit-auto-revert-mode)
    (diminish 'projectile-mode)
    (eval-after-load "hideshow" '(diminish 'hs-minor-mode))))


;; Recentf
;; =================================================================================

(use-package recentf
  :ensure recentf
  :init
  (setq recentf-max-saved-items 100))


;; Rainbow delimiters
;; =================================================================================

(use-package rainbow-delimiters
  :ensure rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;; Smooth scrolling
;; =================================================================================

(use-package smooth-scrolling
  :ensure smooth-scrolling
  :init (setq smooth-scroll-margin 3))


;; Line numbers
;; =================================================================================

(use-package linum-relative
  :disabled t
  :ensure linum-relative
  :init (setq linum-relative-current-symbol "->"))


;; Window numbering
;; =================================================================================

(use-package window-numbering
  :ensure window-numbering
  :config
  (progn
    (window-numbering-mode t)
    (window-numbering-clear-mode-line)
    (evil-leader/set-key
      "0" 'select-window-0
      "1" 'select-window-1
      "2" 'select-window-2
      "3" 'select-window-3
      "4" 'select-window-4
      "5" 'select-window-5
      "6" 'select-window-6
      "7" 'select-window-7
      "8" 'select-window-8
      "9" 'select-window-9)))

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :init
  (progn
    (evil-leader/set-key
      "." 'ace-jump-word-mode
      "," 'ace-jump-char-mode
      "<SPC>" 'ace-jump-line-mode)))


;; Ace jump
;; =================================================================================

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :init
  (progn
    (evil-leader/set-key
      "." 'ace-jump-word-mode
      "," 'ace-jump-char-mode
      "<SPC>" 'ace-jump-line-mode)))


;; Number font lock
;; =================================================================================

(use-package number-font-lock-mode
  :ensure parent-mode
  :load-path "sources/number-font-lock-mode"
  :init (add-hook 'prog-mode-hook 'number-font-lock-mode))


;; YASnippet
;; =================================================================================

(use-package yasnippet
  :ensure yasnippet
  :init
  (progn
    (yas-global-mode t)
    (setq yas-verbosity 1
          yas-snippet-dir (expand-file-name "snippets" user-emacs-directory)))
  :config
  (progn
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (key-chord-define evil-insert-state-map "}|" 'yas-expand)))


;; Term
;; =================================================================================

(use-package term
  :init
  (progn
    (add-hook 'term-mode-hook
              (lambda ()
                (evil-normal-state)
                (evil-emacs-state)
                (setq global-hl-line-mode nil)))))


;; Shell
;; =================================================================================

(add-hook 'shell-mode-hook
          (lambda ()
            (setq global-hl-line-mode nil)))



;; Eshell
;; =================================================================================

(defun bb/git-branch (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string
                       (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (when (> (length git-output) 0)
        (substring git-output 0 -1)))))

(defun bb/git-status (pwd)
  "Returns the dirty status of the git repo at PWD."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string
                       (concat "cd " pwd " && git status --porcelain"))))
      (> (length git-output) 0))))

(defun bb/eshell-prompt ()
  "Custom EShell prompt."
  (let* ((user (getenv "USER"))
         (pwd (abbreviate-file-name (eshell/pwd)))
         (userface (if (string= user "root") 'prompt-root 'prompt-user))
         (branch (bb/git-branch (eshell/pwd)))
         (dirty (when branch (if (bb/git-status (eshell/pwd))
                               (propertize "✘" 'face 'prompt-root)
                             (propertize "✔" 'face 'prompt-user))))
         (venv (when venv-current-name (concat "‹" venv-current-name "›"))))
    (concat (propertize "╭─" 'face 'default)
            (propertize (concat user "@" system-name) 'face userface)
            "  "
            (propertize pwd 'face 'prompt-pwd)
            (propertize (if branch (concat "   " branch) "") 'face 'prompt-branch)
            (if branch (concat "  " dirty) "")
            (propertize (if venv (concat "  " venv) "") 'face 'prompt-venv)
            (propertize "\n╰─$ " 'face 'default))))


(use-package eshell
  :init
  (progn
    (add-hook 'eshell-mode-hook
              (lambda ()
                (setq global-hl-line-mode nil)))
    (setq eshell-prompt-function 'bb/eshell-prompt
          eshell-prompt-regexp (concat "^╰─" (regexp-quote "$"))
          eshell-highlight-prompt nil)))


;; Virtualenvs
;; =================================================================================

(use-package virtualenvwrapper
  :ensure virtualenvwrapper
  :config
  (venv-initialize-eshell))


;; LaTeX
;; =================================================================================

(use-package latex
  :ensure auctex
  :init
  (progn
    (setq font-latex-fontify-script nil)
    (add-to-list 'LaTeX-verbatim-environments "minted")
    (setq evil-shift-width 2)
    (setq font-latex-match-function-keywords
          '(("definecolor" "{{{")
            ("usetikzlibrary" "{")
            ("includegraphics" "[{")
            ("titlegraphic" "{")
            ("usecolortheme" "{")
            ("usetheme" "{")))
    (setq font-latex-match-reference-keywords
          '(("autoref" "{")
            ("inst" "{")))
    (setq font-latex-match-textual-keywords
          '(("author" "[{")
            ("abstract" "{")
            ("overview" "{")
            ("institute" "[{")
            ("date" "[{")
            ("title" "[{")
            ("url" "{")
            ("and" "")
            ("overview" "{")
            ("doList" "{")
            ("challengeList" "{")))
    (setq font-latex-match-warning-keywords
          '(("maketitle" "")
            ("titlepage" "")
            ("frametitle" "{")
            ("doList" "{")))))


;; Python mode
;; =================================================================================

(use-package elpy
  :ensure elpy
  :config
  (progn
    (setq python-indent 4
          elpy-rpc-backend "jedi")
    (elpy-enable)

    ;; Customize flymake
    ;; If flymake is installed separately at any time, these should be moved
    (custom-set-faces
     '(flymake-errline ((t nil)))
     '(flymake-warnline ((t nil))))
    (setq flymake-error-bitmap '(left-arrow error)
          flymake-warning-bitmap '(left-arrow error)
          flymake-fringe-indicator-position 'right-fringe)))

(setq hs-special-modes-alist
      (assq-delete-all 'python-mode hs-special-modes-alist))
(add-to-list
 'hs-special-modes-alist
 '(python-mode
   "^\\s-*\\(?:def\\|class\\|if\\|else\\|elif\\|try\\|except\\|finally\\|for\\|while\\|with\\)\\>"
   nil "#" #[(arg) "\300 \207" [python-nav-end-of-block] 1] nil))

(add-hook 'python-mode-hook (lambda ()
                              (electric-indent-local-mode -1)))


;; Scala mode
;; =================================================================================

(use-package scala-mode2
  :ensure scala-mode2)


;; Web mode
;; =================================================================================

(use-package web-mode
  :ensure web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.xml\\'" . web-mode)
         ("\\.xinp\\'" . web-mode)
         ("\\.qrc\\'" . web-mode))
  :config
  (progn
    (add-hook 'web-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map "za" 'web-mode-fold-or-unfold)
                (setq evil-shift-width 2)
                (setq-local rainbow-delimiters-highlight-braces-p nil)))
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2)))


;; C++ and C mode
;; =================================================================================

(c-add-style "bb-style"
             '((indent-tabs-mode . nil)
               (c-basic-offset . 4)
               (c-offsets-alist
                (substatement-open . 0)
                (inline-open . 0)
                (statement-cont . c-lineup-assignments))))

(c-add-style "sintef-style"
             '((indent-tabs-mode . nil)
               (c-basic-offset . 2)
               (c-offsets-alist
                (substatement-open . 0)
                (inline-open . 0)
                (statement-cont . c-lineup-assignments))))

(defun bb-style () (interactive) (c-set-style "bb-style"))
(defun sintef-style () (interactive) (c-set-style "sintef-style"))
(evil-ex-define-cmd
 "bbstyle" (lambda () (interactive) (c-set-style "bb-style")))
(evil-ex-define-cmd
 "sintefstyle" (lambda () (interactive) (c-set-style "sintef-style")))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "bb-style")
            (setq c-macro-names-with-semicolon
                  '("Q_OBJECT" "Q_PROPERTY" "Q_DECLARE" "Q_ENUMS" "Q_INTERFACES"))
            (c-make-macro-with-semi-re)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; Coffee mode
;; =================================================================================

(use-package coffee-mode
  :ensure coffee-mode
  :mode "\\.coffee\\'"
  :init (setq coffee-tab-width 4))


;; Lisp mode
;; =================================================================================

(add-hook 'lisp-interaction-mode-hook 'hs-minor-mode)
(add-hook 'lisp-interaction-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)


;; Haskell mode
;; =================================================================================

(use-package haskell-mode
  :ensure haskell-mode
  :mode (("\\.hs" . haskell-mode)
         ("\\.lhs" . haskell-mode))
  :config
  (progn
    (setq haskell-indentation-cycle-warn nil)
    (setq haskell-operator-face 'font-lock-builtin-face)
    (setq haskell-indentation-starter-offset 2)
    (add-hook 'haskell-mode-hook
              (lambda ()
                (setq evil-shift-width 2)
                (turn-on-haskell-indentation)))))


;; ESS
;; =================================================================================

(use-package ess-site
  :load-path "sources/ess/lisp"
  :disabled t
  :config
  (progn
    (load (expand-file-name (concat (file-name-directory load-file-name)
                                    "sources/julia/julia.el")))
    (define-key ess-mode-map (kbd "TAB")
      (lambda (arg) (interactive "*i")
        (if (julia-latexsub)
            (indent-for-tab-command arg))))
    (add-hook 'ess-mode-hook 'linum-mode)
    (add-hook 'inferior-ess-mode-hook
              (lambda ()
                (setq global-hl-line-mode nil)))))


;; Julia
;; =================================================================================

(use-package julia-mode
  :ensure julia-mode)


;; Julia REPL mode
;; =================================================================================

(defvar julia-repl-path "/home/efonn/repos/julia/usr/bin/julia"
  "Path to the Julia executable.")

(defvar julia-repl-arguments '()
  "Command line arguments to pass to Julia.")

(defvar julia-repl-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; (define-key map "\t" (lambda () (interactive) (quoted-insert "\t")))
    (define-key map "\t" (lambda () (interactive)
                           (message "hi there")
                           (insert "\t")
                           (message "calling comint-send-input")
                           (comint-send-input t)
                           (message "called comint-send-input")
                           ))
    map)
  "Basic mode map for Julia REPL.")

(defvar julia-repl-regexp "julia>"
  "Prompt for Julia REPL.")

(defun run-julia-repl ()
  "Run an inferior instance of Julia inside Emacs."
  (interactive)
  (let* ((julia-program julia-repl-path)
         (buffer (comint-check-proc "Julia")))
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'julia-repl-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Julia*"))
       (current-buffer)))
    (unless buffer
      (apply 'make-comint-in-buffer "Julia" buffer
             julia-program julia-repl-arguments)
      (julia-repl-mode))))

(defun julia-repl--initialize ()
  "Helper function to initialize Julia REPL."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode julia-repl-mode comint-mode "Julia REPL"
  "Major mode for Julia REPL.

\\<julia-repl-mode-map>"
  nil "Julia REPL"

  (setq comint-prompt-regexp julia-repl-regexp)
  (setq comint-prompt-read-only t)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) julia-repl-regexp))

(add-hook 'julia-repl-mode-hook 'julia-repl--initialize)
(add-hook 'julia-repl-mode-hook (lambda () (setq global-hl-line-mode nil)))


;; Org
;; =================================================================================

(defun bb/org-eol-call (fun)
  "Go to end of line and call provided function"
  (end-of-line)
  (funcall fun)
  (evil-append nil))

(defun bb/org-insert-item ()
  (interactive)
  (if (not (org-in-item-p))
      (bb/org-eol-call (lambda () (insert "\n- ")))
    (bb/org-eol-call 'org-insert-item)))

(use-package org
  :config
  (progn
    (evil-define-key 'normal org-mode-map

      ;; Movement
      "gJ" 'outline-next-visible-heading
      "gK" 'outline-previous-visible-heading
      "gj" 'org-forward-heading-same-level
      "gk" 'org-backward-heading-same-level
      "gh" 'outline-up-heading
      (kbd "M-n") 'next-error
      (kbd "M-p") 'previous-error
      "$" 'org-end-of-line
      "^" 'org-beginning-of-line

      ;; Insertion of headings and items
      "gO" (lambda () (interactive)
             (bb/org-eol-call 'org-insert-heading-after-current)
             (org-metaright))
      "go" (lambda () (interactive)
             (bb/org-eol-call 'org-insert-heading-after-current))
      "gT" (lambda () (interactive)
             (bb/org-eol-call 'org-insert-heading-after-current)
             (org-metaright)
             (org-todo))
      "gt" (lambda () (interactive)
             (bb/org-eol-call 'org-insert-heading-after-current)
             (org-todo))
      "gI" (lambda () (interactive)
             (bb/org-insert-item)
             (org-metaright))
      "gi" 'bb/org-insert-item

      ;; Common keys
      "-" 'org-ctrl-c-minus
      "gc" 'org-ctrl-c-ctrl-c
      "g*" 'org-ctrl-c-star
      (kbd "g RET") 'org-ctrl-c-ret

      ;; Insertions and setters
      "g-" 'org-table-insert-hline
      "gp" 'org-set-property
      "g." 'org-time-stamp
      "g!" 'org-time-stamp-inactive
      "gf" 'org-footnote-action
      "gl" 'org-insert-link
      "t" 'org-todo

      ;; Clocking
      "gxi" 'org-clock-in
      "gxo" 'org-clock-out
      "gxx" 'org-clock-in-last
      "gxd" 'org-clock-display
      "gxr" 'org-clock-report

      ;; Other
      "gs" 'org-sort
      "g/" 'org-sparse-tree
      (kbd "g SPC") 'org-remove-occur-highlights
      (kbd "TAB") 'org-cycle)

      ;; ";t" 'org-show-todo-tree
      ;; ";a" 'org-agenda

    (mapc (lambda (state)
            (evil-define-key state org-mode-map
              (kbd "M-l") 'org-metaright
              (kbd "M-h") 'org-metaleft
              (kbd "M-k") 'org-metaup
              (kbd "M-j") 'org-metadown
              (kbd "M-L") 'org-shiftmetaright
              (kbd "M-H") 'org-shiftmetaleft
              (kbd "M-K") 'org-shiftmetaup
              (kbd "M-J") 'org-shiftmetadown
              (kbd "M-o") '(lambda () (interactive)
                             (bb/org-eol-call
                              '(lambda()
                                 (org-insert-heading)
                                 (org-metaright))))
              (kbd "M-t") '(lambda () (interactive)
                             (bb/org-eol-call
                              '(lambda()
                                 (org-insert-todo-heading nil)
                                 (org-metaright))))))
          '(normal insert))

    (evil-leader/set-key-for-mode 'org-mode
      "oh" 'helm-org-headlines)
    (evil-leader/set-key
      "hg" (lambda () (interactive) (magit-status "~/org"))
      "hc" (lambda () (interactive) (find-file "~/org/capture.org"))
      "hb" (lambda () (interactive) (find-file "~/org/sandbox.org"))
      "hs" (lambda () (interactive) (find-file "~/org/sintef.org"))
      "hm" (lambda () (interactive) (find-file "~/org/my.org")))
    (add-to-list 'org-agenda-files "~/org/capture.org")
    (add-to-list 'org-agenda-files "~/org/sintef.org")
    (add-to-list 'org-agenda-files "~/org/my.org")
    (setq org-default-notes-file "~/org/capture.org")

    (setq org-log-done 'time)
    (setq org-clock-into-drawer t)

    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)

    (add-hook 'org-mode-hook
              (lambda () (interactive)
                (org-indent-mode)
                (visual-line-mode)
                (evil-leader/set-key
                  "fh" 'helm-org-headlines)))

    (use-package org-agenda
      :config
      (progn
        (setq org-agenda-window-setup 'current-window)
        (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
        (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
        (define-key org-agenda-mode-map "n" 'org-agenda-goto-date)
        (define-key org-agenda-mode-map "p" 'org-agenda-capture)
        (define-key org-agenda-mode-map ":" 'evil-ex)))))


;; Markdown
;; =================================================================================

(use-package markdown-mode
  :ensure markdown-mode
  :mode "\\.md\\'")


;; PO mode
;; =================================================================================

(use-package po-mode
  :load-path "sources/misc"
  :mode "\\.po\\'")


;; CMake
;; =================================================================================

(use-package cmake-mode
  :ensure cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (add-hook 'cmake-mode 'linum-mode))


;; YAML mode
;; =================================================================================

(use-package yaml-mode
  :ensure yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))


;; Source local setup, if it exists
;; =================================================================================

(when (file-exists-p "~/local.el")
  (load-file "~/local.el"))
