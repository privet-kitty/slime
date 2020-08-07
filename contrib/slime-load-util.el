(require 'slime)

(define-slime-contrib slime-load-util
  "Contrib expanding source-transform."
  (:swank-dependencies swank-load-util))

(defun slime-load-util-coerce-name (name)
  (let ((start (or (position-if (lambda (c) (not (member c '(?# ?:)))) name)
                   (length name))))
    (downcase (subseq name start))))

(defvar slime-load-util-beginning-line ";; BEGIN_INSERTED_CONTENTS")
(defvar slime-load-util-use-package-line ";; BEGIN_USE_PACKAGE")
(defvar slime-load-util-package-regexp
  (concat "^(\\(cl:\\|common-lisp:\\)?in-package\\>[ \t']*"
          "\\([^)]+\\)[ \t]*)"))

(defun slime-load-util-collect-packages ()
  (let ((case-fold-search t)
        (regexp slime-load-util-package-regexp))
    (save-excursion
      (or (search-backward slime-load-util-beginning-line nil t)
          (search-forward slime-load-util-beginning-line))
      (cl-loop for pos = (re-search-forward regexp nil t)
               while pos
               collect (match-string-no-properties 2)))))

(defun slime-source-directory (system)
  (slime-eval-async `(swank-load-util:system-directory ,system)
    (lambda (dir) (message "%s" dir))))

(defvar slime-load-util-excluded-names '("cl-user" "cl"))
(defun slime-load-util-call-with-extra-dependency-list (added-system cont)
  (let ((existing-packages
         (delete-if (lambda (name)
                      (member name slime-load-util-excluded-names))
                    (mapcar #'slime-load-util-coerce-name
                            (slime-load-util-collect-packages)))))
    (slime-eval-async
        `(swank-load-util:make-extra-dependency-list
          ',existing-packages
          ,added-system)
      cont)))

(defun slime-load-util-get-extra-dependency-list (added-system)
  (let ((existing-packages
         (delete-if (lambda (name)
                      (member name slime-load-util-excluded-names))
                    (mapcar #'slime-load-util-coerce-name
                            (slime-load-util-collect-packages)))))
    (slime-eval `(swank-load-util:make-extra-dependency-list
                  ',existing-packages
                  ,added-system))))

(defun slime-load-util-make-use-package-line (system-name)
  (format "(use-package :%s)\n" system-name))

(defun slime-load-util-load-system (added-system &optional insert-use-package)
  (let ((pos (point))
        (deps (slime-load-util-get-extra-dependency-list added-system)))
    (save-excursion
      (cl-labels
          ((%move ()
                  (or (search-backward slime-load-util-use-package-line nil t)
                      (search-forward slime-load-util-use-package-line)))
           (%insert-file
            (filename)
            (message "Inserted: %s" filename)
            (let ((file-contents (get-string-from-file filename)))
              (insert file-contents)
              (newline)
              (incf pos (+ (length file-contents) 1)))))
        (%move)
        (beginning-of-line)
        (cl-loop for (system-name . path) in deps
                 do (%insert-file path))
        (%move)
        (forward-line)
        (when insert-use-package
          (let ((line (slime-load-util-make-use-package-line added-system)))
            (insert line)
            (incf pos (length line)))
          (goto-char pos))))))

(provide 'slime-load-util)
