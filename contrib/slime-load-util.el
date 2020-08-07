(require 'slime)

(define-slime-contrib slime-load-util
  "Contrib inserting source files with resolving
dependencies (based on package-inferred-system)"
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

(defun slime-load-util-get-extra-dependency-list (added-filename)
  (let ((existing-packages
         (delete-if (lambda (name)
                      (member name slime-load-util-excluded-names))
                    (mapcar #'slime-load-util-coerce-name
                            (slime-load-util-collect-packages)))))
    (slime-eval `(swank-load-util:make-extra-dependency-list-by-file
                  ',existing-packages
                  ,added-filename
                  t))))

(defun slime-load-util-make-use-package-line (system-name)
  (format "(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :%s :cl-user))
" system-name))

(defun slime-load-util-load-file (added-filename &optional insert-use-package)
  (let ((pos (point)))
    (destructuring-bind (added-system &rest deps)
        (slime-load-util-get-extra-dependency-list added-filename)
      (save-excursion
        (cl-labels
            ((%move ()
                    (or (search-backward slime-load-util-use-package-line nil t)
                        (search-forward slime-load-util-use-package-line)))
             (%insert-file (filename)
                           (message "Inserted: %s" filename)
                           (let ((file-contents (get-string-from-file filename)))
                             (insert file-contents)
                             (newline)
                             (incf pos (+ (length file-contents) 1)))))
          (%move)
          (beginning-of-line)
          (message "%s" deps)
          (cl-loop for (system-name . path) in deps
                   do (%insert-file path))
          (%move)
          (forward-line)
          (when insert-use-package
            (let ((line (slime-load-util-make-use-package-line added-system)))
              (insert line)
              (incf pos (length line))))
          (goto-char pos))))))

(defvar slime-load-util-base-directory "~/common-lisp/code/")
(defun slime-load-util-load-file-interactive (filename &optional arg)
  (interactive (list (read-file-name "Insert file: " slime-load-util-base-directory)
                     current-prefix-arg))
  (unless (file-readable-p filename)
    (error "%s is not readable" filename))
  (let ((create-lockfiles nil))
    (slime-load-util-load-file filename (if (eql 0 arg) nil t))))

(provide 'slime-load-util)
