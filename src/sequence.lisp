;; These functions is to avoid a dependency on the str library.
(in-package :sijo-version)

(defun split (needle haystack &aux result)
  (when (eql 0 (length needle))
    (return-from split (list haystack)))
  (do ((pos (search needle haystack) (search needle haystack)))
      ((null pos) (if haystack
                      (nreverse (cons haystack result))
                      (nreverse result)))
    (setf result (cons (subseq haystack 0 pos) result))
    (setf haystack (subseq haystack (+ pos (length needle))))))

(defun intersperse (separator seq &aux result)
  (do ((rest seq (subseq rest 1)))
      ((or (null rest) (eql 0 (length rest))) (nreverse result))
    (push (elt rest 0) result)
    (when (subseq rest 1)
      (push separator result))))

(defun concat (sequence)
  (let ((first (first sequence)))
    (typecase first
      (string (apply #'concatenate 'string sequence))
      (array
       ;; It might have a specific length, but we want a variable length as the result
       (apply #'concatenate (append (subseq (type-of first) 0 2) '((*))) sequence))
      (t (apply #'concatenate (type-of first) sequence)))))

(defun join (separator sequence)
  (concat (intersperse separator sequence)))

(defun remove-all (needle haystack)
  (concat (split needle haystack)))

(defun replace-all (old new haystack)
  (join new (split old haystack)))

(defun starts-with (prefix sequence &key (test #'eql))
  (unless sequence
    (return-from starts-with nil))
  (let ((prefix-length (length prefix)))
    (if (> prefix-length (length sequence))
        (return-from starts-with nil))
    (eql 0 (search prefix sequence :test test :end2 prefix-length))))

(defun without-prefix (prefix sequence &key (test #'eql))
  (if (starts-with prefix sequence :test test)
      (values (subseq sequence (length prefix)) t)
      (values sequence nil)))
