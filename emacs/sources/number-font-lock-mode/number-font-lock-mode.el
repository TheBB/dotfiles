;;; number-font-lock-mode.el --- Syntax highlighting of numeric literals -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/number-font-lock-mode
;; Version: 20140618.1210
;; X-Original-Version: 0.1.8
;; Package-Requires: ((emacs "24") (parent-mode "2.0"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2013, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;   * Neither the name of the copyright holder(s) nor the names of any
;;     contributors may be used to endorse or promote products derived from
;;     this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; This minor mode provides syntax highlighting of numeric literals
;; in source code, like what many editors provide by default.
;;
;; To enable: call `number-font-lock-mode'.
;;
;; It tries to be customizable: it should be easy to add or redefine
;; what exactly consitutes a "number" in given major mode. See
;; `number-font-lock-modelist'.

;;; Code:

(require 'parent-mode)

(defgroup number-font-lock nil
  "Highlight numeric literals."
  :prefix "number-font-lock-"
  :group 'faces)

(defface number-font-lock-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight numeric literals."
  :group 'number-font-lock)

(defconst number-font-lock-generic-regexp
  (rx (and
       symbol-start
       digit
       (*? any)
       symbol-end))
  "Generic regexp for number highlighting, used when no
mode-specific one is available.")

(defvar number-font-lock-modelist
  (copy-hash-table
   (eval-when-compile
     (let ((table (make-hash-table :test 'eq)))
       (puthash 'fasm-mode 'do-not-use table)
       (puthash 'c-mode
                (rx (and
                     symbol-start
                     (or (and (+ digit)
                              (opt (and (any "eE")
                                        (opt (any "-+"))
                                        (+ digit))))
                         (and "0"
                              (any "xX")
                              (+ hex-digit)))
                     (opt (or "f" "F"
                              "u" "U"
                              "l" "L"
                              "ll" "lL" "Ll" "LL"
                              "ul" "uL" "Ul" "UL"
                              "lu" "lU" "Lu" "LU"
                              "ull" "ulL" "uLl" "uLL" "Ull" "UlL" "ULl" "ULL"
                              "llu" "llU" "lLu" "lLU" "Llu" "LlU" "LLu" "LLU"))
                     symbol-end))
                table)
       (puthash 'c++-mode
                (rx (and
                     symbol-start
                     (or (and (+ digit)
                              (opt (and (any "eE")
                                        (opt (any "-+"))
                                        (+ digit))))
                         (and "0"
                              (any "xX")
                              (+ hex-digit)))
                     (opt (and (any "_" "A-Z" "a-z")
                               (* (any "_" "A-Z" "a-z" "0-9"))))
                     symbol-end))
                table)
       (puthash 'emacs-lisp-mode
                (rx (and
                     (or (and symbol-start
                              (opt (any "-+"))
                              (+ digit)
                              (opt (or (and (any "eE")
                                            (opt (any "-+"))
                                            (+ digit))
                                       (and "."
                                            (opt (and (+ digit)
                                                      (opt (and
                                                            (any "eE")
                                                            (opt (any "-+"))
                                                            (+ digit)))))))))
                         (and "#"
                              symbol-start
                              (or (and (any "bB")
                                       (opt (any "-+"))
                                       (+ (any "01")))
                                  (and (any "oO")
                                       (opt (any "-+"))
                                       (+ (any "0-7")))
                                  (and (any "xX")
                                       (opt (any "-+"))
                                       (+ hex-digit)))))
                     symbol-end))
                table)
       table)))
  "Hash table storing the mode-specific number highlighting regexps.

The keys are major mode symbols, the values are regexps or symbol
`do-not-use', which prevents `number-font-lock-mode' from doing
anything when the buffer is in the specified major mode.

Parent modes are taken into account, e.g. if there's no
`lisp-interaction-mode' in the modelist, but `emacs-lisp-mode'
is there, the highlighting used for the latter will be used for
the former too.")

(defun number-font-lock--get-from-modelist (modes)
  "Get the regexp for the first matching mode from MODES."
  (catch 'number-font-lock--get-from-modelist-return
    (dolist (mode modes)
      (let ((elt (gethash mode number-font-lock-modelist)))
        (when elt
          (throw 'number-font-lock--get-from-modelist-return elt))))
    nil))

(defun number-font-lock--get-regexp-for-mode (mode)
  "Get the most appropriate regexp for MODE."
  (let* ((modeparents (nreverse (parent-mode-list mode)))
         (elt (number-font-lock--get-from-modelist modeparents))
         (regexp (cond
                  ((null elt) number-font-lock-generic-regexp)
                  ((eq elt 'do-not-use) nil)
                  (t elt))))
    (if regexp
        `((,regexp . 'number-font-lock-face))
      nil)))

(defvar number-font-lock--current-buffer-regexp nil
  "The regexp used when enabling `number-font-lock-mode' in the
current buffer, so that it's possible to unregister the
highlighting even if the regexp in the modelist changed in the
meanwhile.")
(make-variable-buffer-local 'number-font-lock--current-buffer-regexp)

;;;###autoload
(define-minor-mode number-font-lock-mode
  "Minor mode for highlighting numeric literals in source code.

Toggle number font lock mode on or off.

With a prefix argument ARG, enable number font lock mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'."
  :init-value nil
  :lighter ""
  :keymap nil
  ;; Unregister the previous font-locking, if any.
  (when number-font-lock--current-buffer-regexp
    (font-lock-remove-keywords nil number-font-lock--current-buffer-regexp)
    (setq number-font-lock--current-buffer-regexp nil))
  ;; Nothing to do when disabling the mode, the font-locking
  ;; has already been unregistered.
  (when number-font-lock-mode
    (let ((regexp (number-font-lock--get-regexp-for-mode major-mode)))
      (when regexp
        (font-lock-add-keywords nil regexp)
        (setq number-font-lock--current-buffer-regexp regexp))))
  ;; Re-enable font-lock-mode to force rehighlighting.
  (when font-lock-mode
    (font-lock-mode -1)
    (font-lock-mode 1)))

(provide 'number-font-lock-mode)
;;; number-font-lock-mode.el ends here
