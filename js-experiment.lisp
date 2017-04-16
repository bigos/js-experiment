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

;; (defun flatten (x &optional acc)
;;   (cond ((null x) acc)
;;         ((atom x) (cons x acc))
;;         (t (flatten (car x) (flatten (cdr x) acc)))))

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
  (cond ((null x) (concatenate 'list (list #\)) acc))
        ((atom x) (concatenate 'list (list  x) acc))
        (t (if (consp (car x))
               (flatten (concatenate 'list
                                     (list (caar x) #\()
                                     (cdar x))
                        (flatten (cdr x)  acc))
               (flatten (car x)
                        (flatten (cdr x) acc))))))

(defun flat (x)
  (butlast (flatten (list x))))
