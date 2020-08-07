(defpackage #:swank-load-util
  (:use #:cl #:swank)
  (:import-from #:swank/backend
                #:definterface)
  (:import-from #:swank
                #:defslimefun
                #:apply-macro-expander)
  (:export #:system-directory
           #:system-pathname
           #:make-extra-dependency-list))

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

(defun system-pathname (system-designator)
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

(defun make-extra-dependency-list (existing-systems added-system)
  (let* ((new-dependency-list (system-dependency-list added-system))
         (existing-systems (mapcar #'asdf/system:coerce-name existing-systems)))
    (loop for system in new-dependency-list
          unless (member system existing-systems :test #'string=)
          collect (cons system (uiop:unix-namestring (system-pathname system))))))

(defun system-dependency-list* (system-designator)
  (let ((list (system-dependency-list system-designator)))
    (loop for system-name in list
          collect (cons system-name
                        (uiop:unix-namestring (system-pathname system-name))))))

(defun merge-system-dependency-list (list1 list2)
  (append (loop for a in list1
                unless (member a list2 :test #'string=)
                collect a)
          list2))

;; (defun pathname-to-system-name (pathname)
;;   (let* ((root-string (canonize-pathstring *system-root-directory*))
;;          (target-string (canonize-pathstring pathname))
;;          (pos (mismatch target-string root-string)))
;;     (assert (= pos (length root-string)))
;;     (concatenate 'string
;;                  "/"
;;                  (string-trim '(#\/) (subseq target-string pos)))))

(provide :swank-load-util)
