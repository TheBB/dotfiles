;; Package organization
;; =================================================================================

(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/sources/misc/")
(add-to-list 'load-path "~/.emacs.d/sources/evil-nerd-commenter/")
(add-to-list 'load-path "~/.emacs.d/sources/company-mode/")

;; Require all the packages
;; =================================================================================

(require 'key-chord)

(require 'auto-indent-mode)
(require 'coffee-mode)
(require 'haskell-mode)
(require 'markdown-mode)
(require 'web-mode)

(require 'evil-leader)
(require 'evil)
(require 'evil-nerd-commenter)
(require 'surround)
(require 'evil-org)
(require 'evil-numbers)
(require 'evil-args)
(require 'evil-matchit)
(require 'evil-little-word)

(require 'rainbow-delimiters)
(require 'linum-relative)
(require 'smooth-scrolling)
(require 'powerline)
(require 'number-font-lock-mode)
(require 'ag)

(require 'helm-config)
(require 'ido-ubiquitous)
(require 'yasnippet)

;(require 'company)

;; Helm
;; =================================================================================

(helm-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(global-set-key (kbd "M-x") 'helm-M-x)

;; Ace jump
;; =================================================================================

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(autoload 'ace-jump-pop-mark "ace-jump-mode" "Ace jump back" t)

;; evil, evil-leader and mics keybindings
;; =================================================================================

(key-chord-mode 1)

(global-evil-leader-mode)
(evil-mode t)

;; Exit insert mode by pressing jk
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; Use esc to get away from everything, like in vim
(defun minibuffer-keyboard-quit ()
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Add or remove empty lines and whitespace in normal mode
(defun open-line-above (n)
  (interactive "p")
  (save-excursion
    (move-end-of-line 0)
    (open-line n)))

(defun open-line-below (n)
  (interactive "p")
  (save-excursion
    (move-end-of-line 1)
    (open-line n)))

;; Some whitespace functions
(define-key evil-normal-state-map (kbd "RET") 'open-line-below)
(define-key evil-normal-state-map [backspace] 'open-line-above)
(define-key evil-normal-state-map (kbd "g SPC") (lambda (n) (interactive "p")
                                                  (dotimes (c n nil) (insert " "))))

;; We don't need C-u
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

;; Windows
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-M-k") 'evil-window-move-very-top)
(define-key evil-normal-state-map (kbd "C-M-j") 'evil-window-move-very-bottom)
(define-key evil-normal-state-map (kbd "C-M-h") 'evil-window-move-far-left)
(define-key evil-normal-state-map (kbd "C-M-l") 'evil-window-move-far-right)

;; Ace jump
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "C-SPC") 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "M-SPC") 'ace-jump-line-mode)

;; Use C-a and C-x to manipulate numbers, as in vim
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-s") 'evil-numbers/dec-at-pt)

;; Argument text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-normal-state-map "gs" 'evil-forward-arg)
(define-key evil-normal-state-map "ga" 'evil-backward-arg)
(define-key evil-motion-state-map "gs" 'evil-forward-arg)
(define-key evil-motion-state-map "ga" 'evil-backward-arg)
(define-key evil-normal-state-map "gk" 'evil-jump-out-args)

;; Set the leader to comma, and use backslash for search
(define-key evil-motion-state-map "\\" 'evil-repeat-find-char-reverse)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "i" (lambda () (interactive) (find-file user-init-file))
  "o" (lambda () (interactive) (find-file "~/my.org"))
  "e" 'eval-last-sexp
  "x" 'helm-M-x
  "p" 'helm-show-kill-ring
  "b" 'helm-mini
  "f" 'helm-find-files
  "m" (lambda () (interactive)
        (message "Mode: %s" major-mode))
  (kbd "RET") (lambda () (interactive)
                (open-line 1)
                (move-beginning-of-line 2)
                (evil-insert 1))
  (kbd "\d") (lambda () (interactive)
               (open-line 1)
               (evil-insert 1))
  "s" 'just-one-space
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line)

(defun org-push () (interactive)
  (shell-command "cd $HOME/repos/org && git commit -am \".\" && git push"))
(defun org-pull () (interactive)
  (shell-command "cd $HOME/repos/org && git pull"))
(evil-ex-define-cmd "orgpush" 'org-push)
(evil-ex-define-cmd "orgpull" 'org-pull)

(evil-ex-define-cmd "dtw" 'delete-trailing-whitespace)

;; Turn on various things
;; =================================================================================

(auto-indent-global-mode)
(global-surround-mode t)
(global-evil-matchit-mode t)
(setq linum-relative-current-symbol "")
(setq smooth-scroll-margin 3)
(add-hook 'prog-mode-hook 'number-font-lock-mode)

;; Ag bindings
;; =================================================================================

(setq ag-highlight-search t)
(evil-ex-define-cmd "ag" 'ag)
(evil-ex-define-cmd "agp[roject]" 'ag-project)

;; Yasnippets
;; =================================================================================

(add-to-list 'yas/root-directory
             "~/.emacs.d/sources/yasnippet-snippets/")
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(key-chord-define evil-insert-state-map "\"|" 'yas-expand)

;; Powerline theme
;; =================================================================================

(defun powerline-bb-evil-theme ()
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
                             powerline-normal-1 powerline-normal-2 powerline-normal-3))))
             (gf (lambda (idx)
                   (if active (funcall sg idx) 'powerline-normal-3)))
             (sg (lambda (idx) (nth idx (cdr (assoc evil-state state-map)))))
             (lhs (list (powerline-raw
                         (if active (funcall sg 0) "------ ")
                         (funcall gf 1) 'l)
                        (funcall separator-left (funcall gf 1) (funcall gf 2))
                        (powerline-vc (funcall gf 2))
                        (powerline-raw " " (funcall gf 2))
                        (funcall separator-left (funcall gf 2) (funcall gf 3))
                        (powerline-buffer-id (funcall gf 3) 'l)
                        (powerline-raw "%*" (funcall gf 3) 'l)))
             (rhs (list (powerline-minor-modes (funcall gf 3) 'r)
                        (funcall separator-right (funcall gf 3) (funcall gf 2))
                        (powerline-raw " " (funcall gf 2))
                        (powerline-major-mode (funcall gf 2) 'r)
                        (funcall separator-right (funcall gf 2) (funcall gf 1))
                        (powerline-raw " %7p%4l:%3c" (funcall gf 1) 'r))))
        (concat (powerline-render lhs)
                (powerline-fill (funcall gf 3) (powerline-width rhs))
                (powerline-render rhs)))))))
(powerline-bb-evil-theme)

;; Python mode
;; =================================================================================

(add-hook
 'python-mode-hook
 '(lambda ()
    ;; Delete the built-in python folding function
    (setq hs-special-modes-alist
          (assq-delete-all 'python-mode hs-special-modes-alist))
    ;; Replace it with a better one
    (add-to-list
     'hs-special-modes-alist
     '(python-mode
       "^\\s-*\\(?:def\\|class\\|if\\|else\\|elif\\|try\\|except\\|finally\\|for\\|while\\|with\\)\\>"
       nil "#" #[(arg) "\300 \207" [python-nav-end-of-block] 1] nil))
    ;; Other settings
    (setq python-indent 4)))

(add-to-list 'auto-indent-disabled-modes-list 'python-mode)

;; Web mode
;; =================================================================================

(add-hook 'web-mode-hook
          (lambda ()
            ;; Make evil use the custom web-mode folding function
            (define-key evil-normal-state-local-map "za" 'web-mode-fold-or-unfold)
            ;; Other settings
            (setq evil-shift-width 2)
            (setq-local rainbow-delimiters-highlight-braces-p nil)))

;; Load web-mode on these files
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xinp\\'" . web-mode))

;; Other settings
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; C++ and C mode
;; =================================================================================

(c-add-style "bb-style"
             '((indent-tabs-mode . nil)
               (c-basic-offset . 4)
               (c-offsets-alist
                (substatement-open . 0)
                (inline-open . 0)
                (statement-cont . c-lineup-math))))

(c-add-style "sintef-style"
             '((indent-tabs-mode . nil)
               (c-basic-offset . 2)))

(defun bb-style () (interactive) (c-set-style "bb-style"))
(defun sintef-style () (interactive) (c-set-style "sintef-style"))
(evil-ex-define-cmd "bbstyle" 'bb-style)
(evil-ex-define-cmd "sintefstyle" 'sintef-style)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "bb-style")
            (setq c-macro-names-with-semicolon
                  '("Q_OBJECT" "Q_PROPERTY" "Q_DECLARE" "Q_ENUMS" "Q_INTERFACES"))
            (c-make-macro-with-semi-re)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (add-to-list 'auto-indent-disabled-modes-list 'c++-mode)

;; Coffee mode
;; =================================================================================

(setq coffee-tab-width 4)
(add-to-list 'auto-indent-disabled-modes-list 'coffee-mode)

;; Lisp mode
;; =================================================================================

(add-hook 'lisp-interaction-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

;; Haskell mode
;; =================================================================================

(add-hook 'haskell-mode-hook
          (lambda ()
            (evil-ex-define-cmd "comp" 'haskell-process-cabal-build)
            (setq evil-shift-width 2)))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-indentation-cycle-warn nil)
(add-to-list 'auto-indent-disabled-modes-list 'haskell-mode)

(setq haskell-operator-face 'font-lock-builtin-face)

;; Org mode
;; =================================================================================


(add-hook 'org-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map "gc" 'org-ctrl-c-ctrl-c)
            (linum-mode -1)
            (evil-ex-define-cmd "comp" 'org-push)))
(setq org-log-done 'time)
(add-to-list 'org-agenda-files "~/my.org")

;; CMake mode
;; =================================================================================

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(autoload 'cmake-mode "cmake-mode")

;; Color themes
;; =================================================================================

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(set-cursor-color "#0a9dff")
(provide 'init-themes)
(load-theme 'badwolf t)
(global-hl-line-mode t)

;; IDO mode
;; =================================================================================

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

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

;; Rainbow delimiters
;; =================================================================================

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Backups
;; =================================================================================

(setq backup-directory-alist '(("." . "~/.emacs-saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; GUI elements
;; =================================================================================

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Varia
;; =================================================================================

(defalias 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks nil)
(set-language-environment "UTF-8")
(setq-default indent-tabs-mode nil)
(setq linum-relative-current-symbol "->")
(global-linum-mode t)
(visual-line-mode)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
(setq warning-suppress-types '((undo discard-info)))
