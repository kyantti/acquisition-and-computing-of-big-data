;Dado un elemento y una lista quiero una funcion que diga en que posicion de la lista esta.
;Si el elemento no esta en la lista, la funcion debe devolver -1.
;Se debe usar la funcion member.
;Ejemplo:
;(posicion 3 '(1 2 3 4 5)) => 2
;(posicion 6 '(1 2 3 4 5)) => -1

(defun posicion (x l)
  (if (member x l)
      (- (length l) (length (member x l)))
    'No_esta_en_la_lista)
)

;Otra manera de hacerlo usando variables auxiliares

(defun posicion2 (x l)
   (let ((subl (member x l)))
        (if subl
            (- (length l) (length subl))
        -1)
    )
)

;Contar los nodos de un arbol recursivamente
;Ejemplo: (contar '(1 (2 (3) (4)) (5 (6) (7)))) => 7

(defun contar (arbol)
  (if (null arbol)
      0
      (if (atom arbol)
          1
          (+ (contar (car arbol))
             (contar (cdr arbol))))))


;Aplanar una lista
;Ejemplo: (aplanar '(1 (2 (3) (4)) (5 (6) (7)))) => (1 2 3 4 5 6 7)

(defun aplanar (l)
  (if (null l)
      nil
      (if (atom (car l))
          (cons (car l) (aplanar (cdr l)))
          (append (aplanar (car l)) (aplanar (cdr l))))))


;Esto no se para que sirve