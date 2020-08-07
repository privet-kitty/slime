(defpackage #:swank-source-transform
  (:use #:cl #:swank)
  (:import-from #:swank/backend
                #:definterface)
  (:import-from #:swank
                #:defslimefun
                #:apply-macro-expander)
  (:export #:swank-source-transform-expand-1
           #:source-transform-expand-1))

(in-package #:swank-source-transform)

(definterface swank/backend::source-transform-expand-1 (form &optional env)
  "Call the source-transform for form.
If FORM is a function call for which a source-transform has been
defined, invoke the expander function and return the results and T.
Otherwise, return the original form and NIL."
  #-sbcl (values form nil)
  #+sbcl
  (if (and (consp form)
           (symbolp (car form))
           (not (special-operator-p (car form)))
           (fboundp (car form)))
      (let* ((env (or env (sb-kernel:make-null-lexenv)))
             (expander (sb-int:info :function :source-transform (car form)))
             (sb-c::*lexenv* env))
        (if expander
            (multiple-value-bind (expansion passed?) (funcall expander form env)
              (if passed?
                  (values form nil)
                  (values expansion t)))
            (values form nil)))
      (values form nil)))

(defslimefun swank-source-transform-expand-1 (string)
  (apply-macro-expander #'swank/backend::source-transform-expand-1 string))

(provide :swank-source-transform)
