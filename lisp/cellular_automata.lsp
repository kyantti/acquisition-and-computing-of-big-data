; 1-Dimensional cellular automata
; Example: (ca1d 30 10 10)
; 30 is the rule number, 10 is the number of cells, 10 is the number of generations

; First I need a function to convert a number to a binary list
; (decimal-to-binary 77) => (1 0 0 1 1 0 1)
(defun decimal-to-binary (number)
  (if (and (>= number 0) (<= number 255))
      (loop for i from 7 downto 0
            collect (if (>= (logand (ash 1 i) number) 1) 1 0))
      (error "Number must be between 0 and 256")))

; Next I need a function to get the value of a cell for a given rule
; The parameter must be 4: the left cell, the cell itself, the right cell and the rule
; (get-cell-value 1 0 0 77 ) => 1

(defun get-cell-value (left current right rule)
  (nth (reduce #'(lambda (a b) (+ (* a 2) b))
               (list left current right))
       (decimal-to-binary rule)))

; Next I need a function to get the next generation of a given generation
; It will use mapcar with the get-cell-value function to get the value of each cell
; The first and lat cell will be copied from the previous generation
; (get-next-generation '(1 0 0 1 0 1 0 ) 77) => (1 1 1 0 1 0 0)

(defun get-next-generation (generation rule)
  ; Calculate the first element of the next generation
  (let ((next-gen (list (get-cell-value (car (last generation)) (car generation) (cadr generation) rule))))
    ; Apply the get-cell-value function to the next n - 1 elements of the generation list
    (setf next-gen (append next-gen
                           (mapcar #'(lambda (left cell right)
                                       ;(format t "Left: ~A, Current: ~A, Right: ~A~%" left cell right)
                                       (get-cell-value left cell right rule))
                                   generation ; use the whole generation list for left neighbors
                                   (cdr generation) ; start from the second element for current cells
                                   (cddr generation)))) ; start from the third element for right neighbors
    ; Calculate the last element of the next generation
    (setf next-gen (append next-gen (list (get-cell-value (car (last (butlast generation))) (car (last generation)) (car generation) rule))))
    next-gen
  )
)

(defun print-generations (initial-generation rule num-generations)
  (loop :for i :below num-generations :do
    (setq initial-generation (get-next-generation initial-generation rule))
    (loop :for cell :in initial-generation :do
      (princ (if (= cell 1) "*" " "))
      (princ " "))
    (terpri)))

          

