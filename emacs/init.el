; Packages

(require 'package)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(require 'key-chord)
(key-chord-mode 1)

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
(evil-leader/set-key
  "b" 'ido-switch-buffer
  "f" 'ido-find-file
  "e" 'eval-last-sexp
  "x" 'execute-extended-command
  (kbd "RET") (lambda () (interactive)
                (open-line 1)
                (move-beginning-of-line 2)
                (evil-insert 1))
  (kbd "\d") (lambda () (interactive)
               (open-line 1)
               (evil-insert 1)))

(require 'linum-relative)
(setq linum-relative-current-symbol "")

(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)

(require 'powerline)
(powerline-default-theme)

(add-hook 'prog-mode-hook 'number-font-lock-mode)

; Color themes

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(set-cursor-color "#0a9dff")
(provide 'init-themes)
(load-theme 'badwolf t)
(global-hl-line-mode t)

; IDO mode

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)

; Tabs

(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

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

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(define-key global-map (kbd "RET") 'newline-and-indent)
