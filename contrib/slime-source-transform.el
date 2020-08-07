(require 'slime)

(define-slime-contrib slime-source-transform
  "Contrib expanding source-transform."
  (:swank-dependencies swank-source-transform))

(defun slime-source-transform-expand-1 (&optional repeatedly)
  "Display the source-transform expansion of sexp starting at point."
  (interactive "P")
  (slime-eval-macroexpand
   (if repeatedly
       'swank-source-transform:swank-source-transform-expand-1 ; FIXME: no repeated expansion
     'swank-source-transform:swank-source-transform-expand-1)))

(defun slime-source-transform-expand-1-inplace (&optional repeatedly)
  "Display the source-transform expansion of sexp starting at point."
  (interactive "P")
  (slime-eval-macroexpand-inplace
   (if repeatedly
       'swank-source-transform:swank-source-transform-expand-1 ; FIXME: no repeated expansion
     'swank-source-transform:swank-source-transform-expand-1)))

(provide 'slime-source-transform)
