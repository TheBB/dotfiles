(defvar non-ascii-pre-extensions '(non-ascii))
(defvar non-ascii-post-extensions '())

(defun non-ascii/init-non-ascii ()
  (defun find-first-non-ascii-char ()
    "Find the first non-ascii character from point onwards."
    (interactive)
    (let (point)
      (save-excursion
        (setq point
              (catch 'non-ascii
                (while (not (eobp))
                  (or (eq (char-charset (following-char))
                          'ascii)
                      (throw 'non-ascii (point)))
                  (forward-char 1)))))
      (if point
          (goto-char point)
        (message "No non-ascii characters."))))
  (evil-leader/set-key "oa" 'find-first-non-ascii-char)
  )
