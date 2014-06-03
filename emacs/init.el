; Packages

(require 'package)

(add-to-list
    'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(require 'key-chord)
(key-chord-mode 1)

; {{{
    (require 'evil)
    (evil-mode t)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
; }}}

; Color scheme

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(set-cursor-color "#0a9dff")
(provide 'init-themes)
(load-theme 'badwolf t)

; IDO mode

(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

; Varia

(global-linum-mode t)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
   
(define-key global-map (kbd "RET") 'newline-and-indent)
