(defun move-first-to-third (lst)
  "Moves the first element from a list to the third position."
  (if (>= (length lst) 3)
      (let ((first-elem (car lst))
            (rest (cdr lst)))
        (cons (car rest)
              (cons first-elem (cdr rest))))
      lst))

(defun move-first-to-nth (lst n)
  "Moves the first element from a list to the Nth position."
  (if (>= (length lst) n)
      (let ((first-elem (car lst))
            (rest (cdr lst)))
        (append (subseq rest 0 (1- n))
                (list first-elem)
                (subseq rest (1- n))))
      lst))

