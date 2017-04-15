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

;;; (fun-to-js '(1 2 (3 (4 (5 )))  6 7) T nil T)
;;; attempt to convert sexp to JS function(arguments)
(defun fun-to-js% (sexp fst snd lst)
  (if (consp sexp)
    (progn
      (if (not (consp (car sexp)))
          (progn
            (if fst
                (format t "~a~a(" (if lst "" ", ") (car sexp)))
            (if (not fst)
                (format t "~a~a"  (if snd "" ", ") (car sexp))))
          (fun-to-js% (car sexp) T fst snd))
      (fun-to-js% (cdr sexp) nil fst snd))
    (format t ")" )))

(defun fun-to-js (sexp)
  (fun-to-js% sexp t nil t))

(defun my-sexp (sexp)
  (if (consp sexp)
      (if (consp (car sexp))
          (progn                        ;consp car
            ;; (format t " <~A> " (car sexp))
            (my-sexp (car sexp))
            (my-sexp (cdr sexp)))
          (progn                        ;just consp
            (format t " {~A} "  sexp)
            (my-sexp (cdr sexp))))
      (progn                            ;atom
        (format t " [~a] " sexp))))
