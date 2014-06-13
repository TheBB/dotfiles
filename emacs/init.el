; Packages

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'load-path "~/.emacs.d/sources/misc/")

(package-initialize)

(require 'key-chord)
(key-chord-mode 1)

(require 'auto-indent-mode)
(auto-indent-global-mode)
(add-to-list 'auto-indent-disabled-modes-list 'python-mode)

(require 'coffee-mode)

(require 'evil-leader)
(global-evil-leader-mode)

(require 'evil)
(evil-mode t)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
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
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

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

(add-to-list 'load-path "~/.emacs.d/sources/evil-nerd-commenter/")
(require 'evil-nerd-commenter)

(require 'surround)
(global-surround-mode t)

(require 'evil-numbers)

(require 'evil-args)

(require 'evil-matchit)
(global-evil-matchit-mode t)

;; (add-to-list 'load-path "~/.emacs.d/sources/python-mode/")
;; (setq py-install-directory "~/.emacs.d/sources/python-mode/")
;; (require 'python-mode)
;; (when (featurep 'python) (unload-feature 'python t))
;; (setq py-hide-show-minor-mode-p t)
;; (add-hook 'python-mode-hook (lambda ()
;;                               (define-key evil-motion-state-local-map "/" 'evil-search-forward)
;;                               (define-key evil-motion-state-local-map "?" 'evil-search-backward)))

(require 'linum-relative)
(setq linum-relative-current-symbol "")

(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)

(require 'powerline)
(powerline-default-theme)

(require 'number-font-lock-mode)
(add-hook 'prog-mode-hook 'number-font-lock-mode)

(require 'ido-ubiquitous)

(require 'web-mode)
(add-hook 'web-mode-hook (lambda ()
                           (define-key evil-normal-state-local-map "za" 'web-mode-fold-or-unfold)))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

; Folding for python

(add-hook 'python-mode-hook '(lambda ()
                               (setq hs-special-modes-alist
                                     (assq-delete-all 'python-mode hs-special-modes-alist))
                               (add-to-list 'hs-special-modes-alist
                                            '(python-mode
                                              "^\\s-*\\(?:def\\|class\\|if\\|else\\|elif\\|try\\|except\\|finally\\|for\\|while\\|with\\)\\>"
                                              nil
                                              "#"
                                              #[(arg) "\300 \207" [python-nav-end-of-block] 1]
                                              nil))))

; Color themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(set-cursor-color "#0a9dff")
(provide 'init-themes)
(load-theme 'badwolf t)
(global-hl-line-mode t)

; Enable folding in various modes

(add-hook 'lisp-interaction-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

; IDO mode

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

; Tabs

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(add-hook 'python-mode-hook '(lambda ()
                               (setq python-indent 4)))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

; Backups

(setq backup-directory-alist '(("." . "~/.emacs-saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

; GUI elements

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; Varia

(global-linum-mode t)
(visual-line-mode)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(define-key global-map (kbd "RET") 'newline-and-indent)
