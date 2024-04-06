; Dado un numero generar una lista con los numeros desde el mismo hasta 1
; Ejemplo: (generar-lista 5) => (1 2 3 4 5)
; Desarollo: Se puede resolver de manera recursiva
;            si el numero es 1, se retorna una lista con el 1, 
;            si no,
;            se llama a la funcion con el numero - 1 y se le agrega el numero a la lista-
(defun generar-lista (n)
  (if (< n 1)
      '()
      (if (= n 1)
          (list 1)
          (append (generar-lista (- n 1)) (list n)))))

; Hacer lo mismo pero con tale recursion
; TODO


; Volumen cilindro utilizando mapcar y lambda
; Ejemplo: (volumen-cilindro '(1 2 3) '(1 2 3)) => (3.1416 12.5664 28.2744)

(defun volumen-cilindro (r h)
  (mapcar (lambda (r h) (* 3.1416 (* r r) h)) r h))


