;Rotar la la siguiente lista hacia la derecha: [a, b, c, d, e]

(defun rotate-right (lst)
    (append (last lst 1) (butlast lst 1))
)

(rotate-right '(a b c d e))

;Rotar la la siguiente lista hacia la izquierda: [a, b, c, d, e]
;Debe retornar: [b, c, d, e, a]

(defun rotate-left (lst)
    (append (cdr lst) (list (car lst)))
)

;Rotar hacia la izquiera utilizando la función rotate-right
;Primero se invierte la lista y luego se rota hacia la derecha
;Finalmente se invierte nuevamente la lista

(defun rotate-left (lst)
    (reverse (rotate-right (reverse lst)))
)

;Tengo una "bolsa" de caramelos, la cual contiene caramelos de diferentes sabores.
;En esa bolsa tengo que meter los caramelos del mismo sabor en una bolsa aparte.
;Ejemplo: (group-candies '(a b a c b a b c)) debe retornar ((a a a) (b b b) (c c))

(defun group-candies (lst)
    ;Se crea una lista vacía llamada "candies"
    (let ((candies (list)))
        ;Se recorre la lista de caramelos
        (dolist (c lst)
            ;Se busca si el caramelo ya está en la lista "candies"
            (let ((bag (assoc c candies)))
                ;Si el caramelo ya está en la lista, se agrega a la bolsa
                (if bag
                    (setf (cdr bag) (cons c (cdr bag)))
                    ;Si el caramelo no está en la lista, se agrega a la lista
                    (push (cons c (list c)) candies)
                )
            )
        )
        (mapcar #'cdr candies)
    )
)

;Ahora tengo una bolsa de n caramelos un grupo de n niños, cada niño tiene que recibir un caramelo.
;Ejemplo: (distribute-candies '(cafe naranja limon nata) '(juan antonio pedro pablo)) debe retornar ((cafe juan) (naranja antonio) (limon pedro) (nata pablo))

(defun distribute-candies (candies kids)
    (mapcar 'list candies kids)
)

;Dados dos arrays de números, retornar un array con la suma de los elementos de los dos arrays.
;Ejemplo: (sum-arrays '(1 2 3) '(4 5 6)) debe retornar (5 7 9)

(defun sum-arrays (a b)
    (mapcar '+ a b)
)

;Dados dos listas de arrays de números, retornar un array con la suma de los elementos de los dos arrays utilizando la función sum-arrays.
;Ejemplo: (sum-arrays-lists '((1 2 3) (4 5 6)) '((7 8 9) (10 11 12))) debe retornar ((8 10 12) (14 16 18))

(defun sum-arrays-lists (a b)
    (mapcar 'sum-arrays a b)
)

;Hacer una funcion que diga si un numero es par
;Ejemplo: (is-even 4) debe retornar t

( defun is-even(n)
    (if (= 0 (mod n 2))
          'is_even
       'is_odd
    )
)