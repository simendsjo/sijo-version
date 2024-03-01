(defpackage :sijo-version
  (:use #:cl)
  (:export #:version
           ;; Helpers for constructing the semver
           #:version-file-line-0
           #:version-file-line-1
           #:system-version
           #:current-time
           #:git-non-main-branch
           #:git-current-branch
           #:git-current-commit))
