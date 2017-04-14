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

;;; not working yet
;;; (fun-to-js '(alert "hello"))
;;; attempt to convert sexp to JS function(arguments)
(defun fun-to-js (sexp)
  (if (consp sexp)
      (if (symbolp (car sexp))
          (format nil "~a(~a)" (symbol-to-js (car sexp))
                  (fun-to-js (cdr sexp)))
          (format nil "~A" (car sexp)))
      (format nil "~a" (if (symbolp sexp)
                           (symbol-to-js sexp)
                           sexp
                           ))))
