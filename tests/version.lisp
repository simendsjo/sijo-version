(defpackage :sijo-version/tests
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (:version :sijo-version)))

(in-package :sijo-version/tests)

(define-test version ()
  ;; Doctests
  (multiple-value-bind (failed passed) (sijo-doctest:test-package :sijo-version)
    (assert-eql 0 failed)
    (assert-true (> passed 0)))
  ;; Default build-metadata
  (let* ((build (version:version :version "" :pre-release nil :root :sijo-version))
         (fields (str:split "." (str:replace-first "0.0.0+" "" build)))
         (branch (first fields))
         (commit (second fields))
         (timestamp (third fields)))
    (assert-true (str:starts-with? "0.0.0+" build))
    (assert-eql 3 (length fields))
    (assert-false (str:empty? branch))
    (assert-false (str:empty? commit))
    (assert-string= "" (string-trim "0123456789abcdef" commit))
    (assert-string= "Z" (string-trim "0123456789" timestamp))
    (assert-true (str:ends-with? "Z" timestamp))))
