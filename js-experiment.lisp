;;; js experiment

(declaim (optimize (debug 3)))

(defun camelize (str &optional (acc ""))
  "Helper upper casing letters preceded by '-' and down casing everything else."
  (cond
    ((eq 0 (length str))
     acc)
    ((equal str "-")
     (error "String can not end with -"))
    ((and (>= (length str) 2)
          (equal (subseq str 0 2) "--"))
     (error "String can not contain --"))
    (T
     (let*
         ((letter (not (equal "-" (subseq str 0 1))))
          (letter-start (if letter 0 1))
          (letter-end   (if letter 1 2))
          (string-op    (if letter 'string-downcase 'string-upcase)))
       (camelize (subseq str letter-end)
                 (concatenate 'string acc
                              (funcall string-op
                                       (subseq str letter-start letter-end))))))))

(defun symbol-to-js (symbol)
  "Converts SYMBOL to lowercase or camelCase string"
  (camelize (symbol-name symbol)))

(defun my-sexp (sexp a)
  (if (consp sexp)
      (if (consp (car sexp))
          (progn
            (my-sexp (car sexp) (cons 1 a))
            (my-sexp (cdr sexp) a))
          (progn
            (format t " {~A ~A} " sexp a)
            (my-sexp (cdr sexp)  a)))
      (progn                            ;atom
        (format t " [~a ~a] " sexp a))))

(defun my-test ()
  (progn
    (my-sexp '(((ala) ma) kota) '(1))
    (terpri)
    (my-sexp '(ala ma kota) '(1))
    (terpri)
    (my-sexp '(ala (ma) kota) '(1))
    (terpri)
    (my-sexp '(ala (ma (kota))) '(1))
    (terpri)))
