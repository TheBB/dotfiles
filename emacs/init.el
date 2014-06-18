;; MELPA and manual source path
;; =================================================================================

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/sources/misc/")
(add-to-list 'load-path "~/.emacs.d/sources/evil-nerd-commenter/")

(package-initialize)

;; Require all the packages
;; =================================================================================

(require 'key-chord)
(require 'auto-indent-mode)
(require 'coffee-mode)
(require 'evil-leader)
(require 'evil)
(require 'evil-nerd-commenter)
(require 'surround)
(require 'evil-numbers)
(require 'evil-args)
(require 'evil-matchit)
(require 'linum-relative)
(require 'smooth-scrolling)
(require 'powerline)
(require 'number-font-lock-mode)
(require 'ido-ubiquitous)
(require 'web-mode)
(require 'yasnippet)

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
(define-key evil-normal-state-map (kbd "RET") (lambda (n) (interactive "p")
                                                (save-excursion
                                                  (move-end-of-line 1)
                                                  (open-line n))))
(define-key evil-normal-state-map [backspace] (lambda (n) (interactive "p")
                                                (save-excursion
                                                  (move-end-of-line 0)
                                                  (open-line n))))
(define-key evil-normal-state-map (kbd "SPC") (lambda (n) (interactive "p")
                                                (dotimes (c n nil) (insert " "))))

;; Use C-a and C-x to manipulate numbers, as in vim
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

;; Argument text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-normal-state-map "gl" 'evil-forward-arg)
(define-key evil-normal-state-map "gh" 'evil-backward-arg)
(define-key evil-motion-state-map "gl" 'evil-forward-arg)
(define-key evil-motion-state-map "gh" 'evil-backward-arg)
(define-key evil-normal-state-map "gk" 'evil-jump-out-args)

;; Set the leader to comma, and use backslash for search
(define-key evil-motion-state-map "\\" 'evil-repeat-find-char-reverse)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'ido-switch-buffer
  "f" 'ido-find-file
  "e" 'eval-last-sexp
  "x" 'execute-extended-command
  "m" (lambda () (interactive)
        (message "Mode: %s" major-mode))
  (kbd "RET") (lambda () (interactive)
                (open-line 1)
                (move-beginning-of-line 2)
                (evil-insert 1))
  (kbd "\d") (lambda () (interactive)
               (open-line 1)
               (evil-insert 1))
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line)

;; Turn on various things
;; =================================================================================

(auto-indent-global-mode)
(global-surround-mode t)
(global-evil-matchit-mode t)
(setq linum-relative-current-symbol "")
(setq smooth-scroll-margin 3)
(add-hook 'prog-mode-hook 'number-font-lock-mode)

;; Yasnippets
;; =================================================================================

(add-to-list 'yas/root-directory
             "~/.emacs.d/sources/yasnippet-snippets/")
(yas-global-mode 1)

;; Powerline theme
;; =================================================================================
(defun powerline-bb-evil-theme ()
  "My powerline theme."
  (interactive)
  (setq-default mode-line-format
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
                          (state-map `((normal   . ("NORMAL "
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
                                     (powerline-raw "%*" (funcall gf 3))))
                          (rhs (list (powerline-minor-modes (funcall gf 3) 'r)
                                     (funcall separator-right (funcall gf 3) (funcall gf 2))
                                     (powerline-raw " " (funcall gf 2))
                                     (powerline-major-mode (funcall gf 2) 'r)
                                     (powerline-raw (concat
                                                     "["
                                                     (symbol-name buffer-file-coding-system)
                                                     "]") (funcall gf 2) 'r)
                                     (funcall separator-right (funcall gf 2) (funcall gf 1))
                                     (powerline-raw " %6p%4l:%3c" (funcall gf 1) 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill (funcall sg 3) (powerline-width rhs))
                             (powerline-render rhs)))))))
(powerline-bb-evil-theme)

;; Python mode
;; =================================================================================

(add-hook 'python-mode-hook
          '(lambda ()
             ;; Delete the built-in python folding function
             (setq hs-special-modes-alist
                   (assq-delete-all 'python-mode hs-special-modes-alist))
             ;; Replace it with a better one
             (add-to-list 'hs-special-modes-alist
                          '(python-mode
                            "^\\s-*\\(?:def\\|class\\|if\\|else\\|elif\\|try\\|except\\|finally\\|for\\|while\\|with\\)\\>"
                            nil
                            "#"
                            #[(arg) "\300 \207" [python-nav-end-of-block] 1]
                            nil))
             ;; Other settings
             (setq python-indent 4)))

(add-to-list 'auto-indent-disabled-modes-list 'python-mode)

;; (add-to-list 'load-path "~/.emacs.d/sources/python-mode/")
;; (setq py-install-directory "~/.emacs.d/sources/python-mode/")
;; (require 'python-mode)
;; (when (featurep 'python) (unload-feature 'python t))
;; (setq py-hide-show-minor-mode-p t)
;; (add-hook 'python-mode-hook (lambda ()
;;                               (define-key evil-motion-state-local-map "/" 'evil-search-forward)
;;                               (define-key evil-motion-state-local-map "?" 'evil-search-backward)))

;; Web mode
;; =================================================================================

(add-hook 'web-mode-hook (lambda ()
                           ;; Make evil use the custom web-mode folding function
                           (define-key evil-normal-state-local-map "za" 'web-mode-fold-or-unfold)
                           ;; Other settings
                           (setq evil-shift-width 2)))

;; Load web-mode on these files
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Other settings
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; C mode
;; =================================================================================

(setq-default c-basic-offset 4)

;; Coffee mode
;; =================================================================================

(setq coffee-tab-width 4)
(add-to-list 'auto-indent-disabled-modes-list 'coffee-mode)

;; Lisp mode
;; =================================================================================

(add-hook 'lisp-interaction-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

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

(setq-default indent-tabs-mode nil)
(global-linum-mode t)
(visual-line-mode)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(define-key global-map (kbd "RET") 'newline-and-indent)
