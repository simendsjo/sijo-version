(defsystem :sijo-version
  :depends-on ()
  :in-order-to ((test-op (test-op :sijo-version/tests)))
  :version (:read-file-line "VERSION" :at 0)
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "sequence")
               (:file "version")))

(defsystem :sijo-version/tests
  :depends-on (#:sijo-version
               #:str
               #:lisp-unit2
               #:sijo-doctest)
  :perform (test-op (o c)
                    (eval (read-from-string "
                        (lisp-unit2:with-summary ()
                            (lisp-unit2:run-tests
                                :package :sijo-version/tests
                                :name :sijo-version))")))
  :serial t
  :pathname "tests/"
  :components ((:file "version")))
