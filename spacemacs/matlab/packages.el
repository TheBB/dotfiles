(defvar matlab-packages '(matlab-mode))
(defvar matlab-excluded-packages '())

(defun matlab/init-matlab-mode ()
  (use-package matlab-mode :defer t))
