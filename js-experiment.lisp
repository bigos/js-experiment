;;; js experiment
(in-package #:js-experiment)

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

;;; elements and their depth
(defun my-sexp (sexp a)
  (if (consp sexp)
      (if (consp (car sexp))
          (progn
            (my-sexp (car sexp) (1+ a))
            (my-sexp (cdr sexp) a))
          (progn
            (format t " {~A ~A} " (car sexp) a)
            (my-sexp (cdr sexp) a)))))

(defun my-test ()
  (progn
    (my-sexp '(((ala) ma) kota) 0)
    (terpri)
    (my-sexp '(ala ma kota) 0)
    (terpri)
    (my-sexp '(ala (ma posiada) kota) 0)
    (terpri)
    (my-sexp '(ala (ma (kota))) 0)
    (terpri)))

(defun flatten (x &optional acc)
  (cond ((null x) (concatenate 'list (list #\) ) acc))
        ((atom x) (concatenate 'list
                               (if (or (equal (car acc) #\) )
                                       (equal (car acc) #\( )
                                       (equal x         #\( ))
                                   (list x)
                                   (list x #\,)) ;insert commas
                               acc))
        (t (flatten
            (if (consp (car x))
                (concatenate 'list
                             (list (caar x) #\( )
                             (cdar x))
                (car x))
            (flatten (cdr x) acc)))))

;;; converts sexp into a flat list that can be easily converted to printed
;;; representation of prefix notation
(defun flat (x)
  (butlast (flatten (list x))))
