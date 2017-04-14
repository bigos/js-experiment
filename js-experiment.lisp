;;; js experiment

(defun camelize (str &optional (acc ""))
  (cond
    ((eq 0 (length str))
     acc)
    ((equal str "-")
     (error "String can not end with -"))
    ((and (>=    (length str) 2)
          (equal (subseq str 0 2) "--"))
     (error "String can not contain --"))
    (T
     (let*
         ((dash (equal "-" (subseq str 0 1)))
          (first-start (if dash 1 0))
          (first-end   (if dash 2 1))
          (string-op   (if dash 'string-upcase 'string-downcase)))
       (camelize (subseq str first-end)
                 (concatenate 'string acc
                              (funcall string-op (subseq str first-start first-end))))))))
