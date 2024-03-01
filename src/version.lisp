(in-package :sijo-version)

(defparameter *root* nil)
(defparameter *system* nil)

(defun %guess-system (root)
  (etypecase root
    (null nil)
    (asdf:system root)
    (pathname
     (if (string-equal (pathname-type root) "asd")
         (handler-case (progn (asdf:load-asd root)
                              (asdf:find-system (string-downcase (pathname-name root))))
           (error ()
             nil))
         (let ((asd-files (uiop:directory-files root "*.asd")))
           (if asd-files
               (%guess-system (car asd-files))
               nil))))
    (package
     ;; System with same name?
     (handler-case (asdf:find-system (string-downcase (package-name root)))
       (error ()
         nil)))
    (string
     ;; System name?
     (handler-case (asdf:find-system (string-downcase root))
       ;; Filename?
       (asdf:load-system-definition-error ()
         (%guess-system (uiop:parse-native-namestring root)))))
    (symbol
     (handler-case (asdf:find-system root)
       (error ()
         nil)))))

(defun %guess-root (root)
  (etypecase root
    (null
     (%guess-root *package*))
    (pathname
     root)
    (asdf:system
     (asdf:system-relative-pathname root ""))
    (package
     (handler-case (%guess-root (asdf:find-system (string-downcase (package-name root))))
       (asdf:missing-component ()
         *default-pathname-defaults*)))
    (string
     (handler-case (asdf:find-system (string-downcase root))
       ;; Treat as path
       (error ()
         (uiop:parse-native-namestring root))))
    (symbol
     (handler-case (%guess-root (asdf:find-system root))
       (error ()
         *default-pathname-defaults*)))))

(defmacro %with-root (root &body body)
  (let ((value (gensym "WITH-ROOT-")))
    `(let* ((,value ,root)
            (*root* (%guess-root ,value))
            (*system* (%guess-system ,value)))
       ,@body)))

(defun %version (version pre-release build-metadata)
  (let ((version (or version "0.0.0"))
        (pre-release (if pre-release
                         (format nil "-~a" pre-release)
                         ""))
        (build-metadata (if build-metadata
                            (format nil "+~a" build-metadata)
                            "")))
    (format nil "~a~a~a" version pre-release build-metadata)))

(defun %filename (filename)
  (merge-pathnames filename (%guess-root *root*)))

(defun %existing-file (filename)
  (probe-file (%filename filename)))

(defun %git-head ()
  (let ((git-head (%existing-file ".git/HEAD")))
    (when git-head
      (uiop:read-file-line git-head))))

(defun %git-ref (ref)
  (let ((git-ref (%existing-file (format nil ".git/~a" ref))))
    (when git-ref
        (uiop:read-file-line git-ref))))

(defun git-current-branch ()
  "Return the current git branch, or nil if not found."
  (let ((git-head (%git-head)))
    (multiple-value-bind (ref ref?) (without-prefix "ref: " git-head)
      (if ref?
          (car (last (split "/" ref)))
          nil))))

(defun git-current-commit ()
  "Return the current git commit, or nil if not found."
  (let ((git-head (%git-head)))
    (multiple-value-bind (ref ref?) (without-prefix "ref: " git-head)
      (if ref?
          (%git-ref ref)
          ref))))

(defun %semver-identifier-char-p (char)
  (find char "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-" :test #'eql))

(defun %eval-spec (spec)
  "See `version' for documentation."
  (etypecase spec
    (null nil)
    (string (let* ((spec (remove-if-not
                          ;; Remove invalid characters. We allow dot as people
                          ;; might pass a dot-separated identifier instead of a
                          ;; list.
                          (lambda (char)
                            (or (eql char #\.)
                                (%semver-identifier-char-p char)))
                          spec))
                   ;; Identifiers shouldn't start or end with separators
                   (spec (string-trim ".-" spec))
                   ;; Avoid empty parts, e.g. "a..b" or "a--b"
                   (spec (replace-all "--" "-" spec))
                   (spec (replace-all ".." "." spec)))
              (if (zerop (length spec))
                  nil
                  spec)))
    (list (cond
            ((eq 'quote (car spec))
             (%eval-spec (prin1-to-string (cdr spec))))
            ((eq 'or (car spec))
             (labels ((run (test rest)
                        (let ((result (%eval-spec test)))
                          (cond
                            (result result)
                            (rest (run (car rest) (cdr rest)))
                            (t nil)))))
               (run (cadr spec) (cddr spec))))
            ((eq 'and (car spec))
             (let ((values (remove-if #'null (mapcar #'%eval-spec (cdr spec)))))
               (if values
                   (%eval-spec (join "-" values))
                   nil)))
            ((eq 'if (car spec))
             (cond
               ((> (length spec) 4) (error "Too many arguments to `if'"))
               ((< (length spec) 3) (error "Too few arguments to `if'")))
             (if (%eval-spec (cadr spec))
                 (%eval-spec (caddr spec))
                 (%eval-spec (cadddr spec))))
            ((eq 'when (car spec))
             (cond
               ((> (length spec) 3) (error "Too many arguments to `when'"))
               ((< (length spec) 2) (error "Too few arguments to `when'")))
             (when (%eval-spec (cadr spec))
               (%eval-spec (caddr spec))))
            ((eq 'unless (car spec))
             (cond
               ((> (length spec) 3) (error "Too many arguments to `unless'"))
               ((< (length spec) 2) (error "Too few arguments to `unless'")))
             (unless (%eval-spec (cadr spec))
               (%eval-spec (caddr spec))))
            (t
             (let ((values (remove-if #'null (mapcar #'%eval-spec spec))))
               (if values
                   (%eval-spec (join "." values))
                   nil)))))
    (function (%eval-spec (funcall spec)))
    (symbol (%eval-spec (cond
                          ((keywordp spec) (symbol-name spec))
                          ((fboundp spec) (funcall spec))
                          ((eq 't spec) "T")
                          ((boundp spec) (symbol-value spec))
                          (t (symbol-name spec)))))
    (atom (%eval-spec (prin1-to-string spec)))))

(defun %version-file ()
  (%existing-file "VERSION"))

(defun version-file-line-0 ()
  "Return the first line of the VERSION file, or nil if it doesn't exist."
  (let ((file (%version-file)))
    (when file
      (first (uiop:read-file-lines file)))))

(defun system-version ()
  "Return the :VERSION from the asdf system `*system*' or nil if it doesn't
exist."
  (and *system* (asdf:component-version *system*)))

(defun version-file-line-1 ()
  "Return the second line of the VERSION file, or nil if it doesn't exist."
  (let ((file (%version-file)))
    (when file
      (second (uiop:read-file-lines file)))))

(defun git-non-main-branch ()
  "Git branch name when not main or master."
  (let ((branch (git-current-branch)))
    (if (member branch '("main" "master") :test #'string-equal)
        nil
        branch)))

(defun current-time ()
  "Return the current time as \"yyyyMMddHHmmssZ\""
  (multiple-value-bind (second minute hour date month year day daylight-p zone) (decode-universal-time (get-universal-time) 0)
    (declare (ignore day daylight-p zone))
    (format nil "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0dZ" year month date hour minute second)))

(defparameter *default-version* '(or version-file-line-0 system-version))
(defparameter *default-pre-release* '(or version-file-line-1 (when git-non-main-branch (and git-current-branch git-current-commit))))
(defparameter *default-build-metadata* '(git-current-branch git-current-commit current-time))

(defun version (&key (version *default-version*)
                  (pre-release *default-pre-release*)
                  (build-metadata *default-build-metadata*)
                  (root *root*))
  "Construct a semantic version (https://semver.org/)

Note that minimal effort is made validating or sanitizing the input, so the user
is able to construct an incorrect semantic version e.g. by supplying too many
components to :VERSION. Some sanitizing is done by dropping invalid characters.

See the package `cl-semver' for a package which parses, validates and compares
semantic version identifiers.

Calling `version' will construct a default semantic version based on the VERSION
file, ./.git/HEAD and the current time.

If no VERSION file exists, the asdf components :VERSION will be used for the
version number.

If no version is found, 0.0.0 is used as the version.

The VERSION file should contain the version number in the first line, and an
optional pre-release tag in the second line. If no pre-release tag exist in the
file, it defaults to the git branch name iff it's not the main/master branch.

VERSION, PRE-RELEASE and BUILD-METADATA is evaluated by `%eval-spec' into a
dotted string, and has the following semantics:
- `string' :: use as-is - it's the canonical form. Some basic sanitizing is done
  by removing some invalid characters.
- `list' ('if as first element) :: call `%eval-spec' on the second element. If
  true, return `%eval-spec' of the third element, otherwise return `%eval-spec'
  of the fourth element.
- `list' ('when as first element) :: call `%eval-spec' on the second element. If
  true, return `%eval-spec' of the third element, otherwise return nil.
- `list' ('unless as first element) :: call `%eval-spec' on the second element. If
  false, return `%eval-spec' of the third element, otherwise return nil.
- `list' ('or as first element) :: call `%eval-spec' on second element. If
  non-nil, return it, otherwise evaluate next element. If there are no more
  elements, return nil.
- `list' ('and as first element) :: call `%eval-spec' on subsequent elements,
  remove nulls, and join with \"-\" as the separator.
- `list' :: call `%eval-spec' on each element, remove nulls, and join with \".\"
  as a separator.
- `function' :: call function pass result to `%eval-spec'
- `symbol' :: if `fboundp', call function and pass result to `%eval-spec'. If
  `boundp' call `%eval-spec' on `symbol-value'. Otherwise call `%eval-spec' on
  the `symbol-name'.
- `atom' :: convert to a string

ROOT is the root folder where the VERSION file and .git directory is located.
The value is evaluated by `%guess-root', and several different forms is
accepted:
- `pathname' :: use as-is - it's the canonical form
- `asdf:system' :: `asdf:system-relative-pathname' is used as the root
- `package' :: try to load a system with the same name as `package-name'. If no
  system is found, `*default-pathname-defaults*' is used.
- `null' :: we guess given `*package*' instead
- `string' :: try to load a system, if missing interpret the input as a `pathname' instead
- `symbol' :: try to guess using the symbol as the system name

Examples:

`(version)'
\"0.1+main.748e8897a233ddbc26a959bd14a97acb0ef5b895.20240301152751Z\"

You can specify the system it should fetch information from directly using `:root'

`(version :root :sijo-version)'
\"0.1+main.748e8897a233ddbc26a959bd14a97acb0ef5b895.20240301152751Z\"

You can remove the use of extra build metadata

`(version :build-metadata nil)'
\"0.1-some-prerelease\"

You can specify the exact version and pre-release tag to use instead of looking in VERSION

>> (version :version \"0.1\" :pre-release nil :build-metadata nil)
\"0.1\"

>> (version :version \"0.1\" :pre-release \"pre\" :build-metadata nil)
\"0.1-pre\"

>> (version :version \"0.1\" :pre-release nil :build-metadata \"build\")
\"0.1+build\"

>> (version :version \"0.1\" :pre-release \"pre\" :build-metadata \"build\")
\"0.1-pre+build\""
  (%with-root root
    (%version (%eval-spec version)
              (%eval-spec pre-release)
              (%eval-spec build-metadata))))
