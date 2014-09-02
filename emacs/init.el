;; Package initialization
;; =================================================================================

(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")

        ("melpa" . "http://melpa.milkbox.net/packages/")))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(package-initialize)

(require 'use-package)


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


;; Whitespace mode
;; =================================================================================

(setq whitespace-style
      '(face
        tabs
        tab-mark
        newline
        newline-mark))
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
  (bb/powerline-evil-theme))


;; Evil and friends
;; =================================================================================

;; Fix window keys
(defun bb/fix-window (map)
  (define-key map (kbd "C-k") 'evil-window-up)
  (define-key map (kbd "C-j") 'evil-window-down)
  (define-key map (kbd "C-h") 'evil-window-left)
  (define-key map (kbd "C-l") 'evil-window-right)
  (define-key map (kbd "C-M-k") 'evil-window-move-very-top)
  (define-key map (kbd "C-M-j") 'evil-window-move-very-bottom)
  (define-key map (kbd "C-M-h") 'evil-window-move-far-left)
  (define-key map (kbd "C-M-l") 'evil-window-move-far-right))


(use-package evil
  :ensure evil
  :pre-load
  (setq evil-want-C-u-scroll t
        evil-want-C-w-in-emacs-state t)
  :init
  (progn
    (use-package evil-leader
      :ensure evil-leader
      :pre-load (setq evil-leader/no-prefix-mode-rx '("magit-.*-mode"))
      :init
      (progn
        (evil-leader/set-leader ",")
        (global-evil-leader-mode t)
        (evil-leader/set-key
          "hi" (lambda () (interactive) (find-file user-init-file))
          "ht" (lambda () (interactive)
                 (find-file (expand-file-name "themes/badwolf-theme.el" user-emacs-directory)))
          "ss" 'just-one-space
          "m" (lambda () (interactive) (message "Mode: %s" major-mode)))))

    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :init
      (progn
        (evil-leader/set-key
          "ci" 'evilnc-comment-or-uncomment-lines
          "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
          "cc" (lambda (&optional num)
                 (interactive "p")
                 (evilnc-copy-and-comment-lines num))
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

    (use-package surround
      :load-path "sources/misc"
      :commands global-surround-mode
      :idle (global-surround-mode t))

    (use-package evil-matchit
      :ensure evil-matchit
      :commands global-evil-matchit-mode
      :idle (global-evil-matchit-mode t))

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
    (define-key evil-normal-state-map (kbd "g SPC")
      (lambda (n) (interactive "p") (dotimes (c n nil) (insert " "))))

    (global-set-key (kbd "RET") 'newline-and-indent)
    (define-key evil-motion-state-map "\\" 'evil-repeat-find-char-reverse)

    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)

    (bb/fix-window evil-normal-state-map)

    (define-key evil-normal-state-map "]b" 'evil-next-buffer)
    (define-key evil-normal-state-map "[b" 'evil-prev-buffer)

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
    "p" 'helm-show-kill-ring)
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
  :commands (magit-status magit-diff magit-log magit-blame-mode)
  :init
  (evil-leader/set-key
    "gs" 'magit-status
    "gb" 'magit-blame-mode
    "gl" 'magit-log
    "gd" 'magit-diff)
  :config
  (progn
    (evil-make-overriding-map magit-mode-map 'emacs)
    (bb/fix-window magit-mode-map)
    (evil-define-key 'emacs magit-mode-map [escape] 'keyboard-quit)
    (evil-define-key 'emacs magit-mode-map "j" 'magit-goto-next-section)
    (evil-define-key 'emacs magit-mode-map "k" 'magit-goto-previous-section)
    (evil-define-key 'emacs magit-mode-map "K" 'magit-discard-item)
    (evil-define-key 'emacs magit-mode-map "\\" 'magit-git-command)
    (evil-define-key 'emacs magit-mode-map ":" 'evil-ex)))

(setq vc-handled-backends nil)


;; Projectile
;; =================================================================================

(use-package projectile
  :ensure projectile
  :config
  (progn
    (evil-leader/set-key
      "ap" 'projectile-ag)
    (projectile-global-mode)))

(use-package helm-projectile
  :ensure helm-projectile
  :init
  (evil-leader/set-key
    "fp" 'helm-projectile
    "fr" 'projectile-find-file))


;; Company
;; =================================================================================

(use-package company
  :commands company-mode
  :disabled t
  :ensure company
  :init (add-hook 'prog-mode-hook 'company-mode)
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
    (define-key evil-normal-state-map (kbd "C-`") 'popwin:close-popup-window)
    (setq popwin:close-popup-window-timer-interval 0.5) ;; Workaround for Emacs 24.3 issue
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
    (diminish 'projectile-mode "proj")
    (eval-after-load "hideshow" '(diminish 'hs-minor-mode))))


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
    (key-chord-define evil-insert-state-map "\"|" 'yas-expand)))


;; Python mode
;; =================================================================================

(setq hs-special-modes-alist
      (assq-delete-all 'python-mode hs-special-modes-alist))
(add-to-list
 'hs-special-modes-alist
 '(python-mode
   "^\\s-*\\(?:def\\|class\\|if\\|else\\|elif\\|try\\|except\\|finally\\|for\\|while\\|with\\)\\>"
   nil "#" #[(arg) "\300 \207" [python-nav-end-of-block] 1] nil))

(setq python-indent 4)


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
               (c-basic-offset . 2)))

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
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)


;; Haskell mode
;; =================================================================================

(use-package haskell-mode
  :ensure haskell-mode
  :mode (("\\.hs" . haskell-mode)
         ("\\.lhs" . haskell-mode))
  :config
  (progn
    (evil-leader/set-key-for-mode 'haskell-mode "p" 'haskell-process-cabal-build)
    (setq haskell-indentation-cycle-warn nil)
    (setq haskell-operator-face 'font-lock-builtin-face)
    (add-hook 'haskell-mode-hook
              (lambda ()
                (setq evil-shift-width 2)
                (turn-on-haskell-indentation)))))


;; Org
;; =================================================================================" 

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
      (kbd "TAB") 'org-cycle

      ;; ";t" 'org-show-todo-tree
      ;; ";a" 'org-agenda
      )

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

    (evil-leader/set-key
      "og" (lambda () (interactive) (magit-status "~/org"))
      "ot" (lambda () (interactive) (find-file "~/org/sandbox.org"))
      "os" (lambda () (interactive) (find-file "~/org/sintef.org"))
      "om" (lambda () (interactive) (find-file "~/org/my.org")))
    (evil-leader/set-key-for-mode 'org-mode
      "oh" 'helm-org-headlines)
    (add-to-list 'org-agenda-files "~/org/sintef.org")
    (add-to-list 'org-agenda-files "~/org/my.org")
    (setq org-log-done 'time)
    (setq org-clock-into-drawer t)

    (evil-set-initial-state 'org-agenda-mode 'normal)
    (global-set-key (kbd "C-c a") 'org-agenda)

    (add-hook 'org-mode-hook
              (lambda () (interactive)
                (org-indent-mode)
                (visual-line-mode)
                (evil-leader/set-key
                  "oh" 'helm-org-headlines)))))


;; Markdown
;; =================================================================================

(use-package markdown-mode
  :ensure markdown-mode
  :mode "\\.md\\'")


;; CMake
;; =================================================================================

(use-package cmake-mode
  :ensure cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (add-hook 'cmake-mode 'linum-mode))


;; Compilation mode
;; =================================================================================

(bb/fix-window compilation-mode-map)


;; Unimacs
;; =================================================================================

;; (use-package grizzl
;;   :load-path "sources/grizzl")

;; (use-package f
;;   :ensure f)

;; (use-package unimacs
;;   :load-path "sources/unimacs"
;;   :init
;;   (progn
;;     (evil-leader/set-key
;;       "b" 'unimacs/cmd-switch-buffer
;;       "x" 'unimacs/cmd-extended-command
;;       "fd" 'unimacs/cmd-find-file
;;       "hf" 'unimacs/cmd-describe-function
;;       "hv" 'unimacs/cmd-describe-variable
;;       )))
