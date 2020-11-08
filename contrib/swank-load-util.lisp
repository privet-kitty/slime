(defpackage #:swank-load-util
  (:use #:cl #:swank)
  (:import-from #:swank/backend
                #:definterface)
  (:import-from #:swank
                #:defslimefun
                #:apply-macro-expander)
  (:export #:system-directory
           #:system-pathname
           #:system-filename
           #:make-extra-dependency-list
           #:make-extra-dependency-list-by-file))

(in-package #:swank-load-util)

(defun system-directory (system)
  (namestring (uiop:pathname-directory-pathname (asdf:system-source-file system))))

(defun defpackage-form-name (defpackage-form)
  (assert (asdf/package-inferred-system::defpackage-form-p defpackage-form))
  (string-downcase (second defpackage-form)))

(defun canonize-pathstring (pathname-designator)
  (uiop:unix-namestring (uiop:ensure-absolute-pathname pathname-designator)))

(defun system-dependency-list (system-designator)
  (let* ((system (asdf:find-system system-designator))
         res)
    (sb-int:named-let dfs ((name (asdf:component-name system)))
      (unless (member name res :test #'equal)
        (push name res)
        (let ((deps (asdf:system-depends-on (asdf:find-system name))))
          (dolist (dep deps)
            (dfs dep)))))
    res))

(defslimefun system-pathname (system-designator)
  (let* ((system-name (asdf/system:coerce-name system-designator))
         (primary (asdf:primary-system-name system-name)))
    (unless (equal primary system-name)
      (let ((top (asdf:find-system primary nil)))
        (when (typep top 'asdf:package-inferred-system)
          (uiop:if-let (dir (asdf:component-pathname top))
            (let* ((sub (subseq system-name (1+ (length primary))))
                   (component-type (asdf/parse-defsystem:class-for-type
                                    nil
                                    (or (asdf/component:module-default-component-class top)
                                        asdf/parse-defsystem:*default-component-class*)))
                   (file-type (asdf/component:file-type (make-instance component-type)))
                   (f (uiop:probe-file* (uiop:subpathname dir sub :type file-type)
                                        :truename uiop:*resolve-symlinks*)))
              (when (uiop:file-pathname-p f) f))))))))

(defslimefun system-filename (system-designator)
  (let ((pathname (system-pathname system-designator)))
    (and pathname (namestring pathname))))

(defslimefun make-extra-dependency-list (existing-systems added-system &optional eval-defpackage)
  (let* ((new-dependency-list (system-dependency-list added-system))
         (existing-systems (mapcar #'asdf/system:coerce-name existing-systems)))
    (loop
      for system in new-dependency-list
      for pathname = (system-pathname system)
      unless (member system existing-systems :test #'string=)
      collect (prog1 (cons system (uiop:unix-namestring pathname))
                (when eval-defpackage
                  (eval
                   (or (asdf/package-inferred-system::file-defpackage-form pathname)
                       (error "DEFPACKAGE form not found in ~W" pathname))))))))

(defslimefun make-extra-dependency-list-by-file (existing-systems added-pathname &optional eval-defpackage)
  (let ((system-name
          (asdf/system:coerce-name
           (second (or (asdf/package-inferred-system::file-defpackage-form added-pathname)
                       (error "DEFPACKAGE form not found in ~W" added-pathname))))))
    (cons system-name (make-extra-dependency-list existing-systems system-name eval-defpackage))))

(provide :swank-load-util)
