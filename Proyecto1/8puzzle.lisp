(defstruct nodo 
    matriz 
    padre 
    valorg 
    valorh
    valorf 
    operador
)
;;; Hola

(defun a_star (abierto)
(cond
    ((null abierto))
    (t (setq actual (pop abierto)) (push actual cerrado)
    (cond
        ((= (nodo-valorh actual) 0) (lista-solucion actual))
        (t (gen_hijos actual)(a_star abierto))))))

(defun a_star (solucion)
(if
    (null abierto) (print 'no_hay_solucion)
    (progn (setq actual (pop abierto)) (push actual cerrado)
    (if (equal (nodo-matriz actual) meta)(lista-solucion actual)
    (progn (gen-hijos actual)(a_star abierto))))))        


(defun 8-puzzle (estado-inicial meta)
(setq nodo_inicial (make-nodo :matriz estado-inicial 
:padre nil
:valorg 0
:valorh (hamm estado-inicial meta)
:valorf 0
:operador 0 ))
(setq abierto (list nodo_inicial))
(setq cerrado '())
(setq solucion '())
(a_star abierto) solucion)

(defun hamm (estado meta) ;;distancia de hamming 
    (setq dis 0)
    (setq i 0)
    (loop for x in estado
        do(progn (if (not(eq (nth i meta) x)) (incf dis)) (incf i))   
)dis)

(defun manhattan (estado meta))

(defun gen-hijos (actual)
(setq estado (nodo-matriz actual))
(setq valg (+ (nodo-valorg actual) 1))
(if (arriba estado)
    (progn  
        (setq nuevo-estado (inv_pos estado (-(tenth estado) 3) (tenth estado)))
        (if (revisa-nodo nuevo-estado (append abierto cerrado))
        (setq abierto (inserta-abierto (crear-hijo actual nuevo-estado (hamm nuevo-estado meta) '^) abierto)))))
(if (abajo estado)
    (progn  
        (setq nuevo-estado (inv_pos estado (+(tenth estado) 3) (tenth estado)))
        (if (revisa-nodo nuevo-estado (append abierto cerrado))
        (setq abierto (inserta-abierto (crear-hijo actual nuevo-estado (hamm nuevo-estado meta) 'v) abierto)))))
(if (derecha estado)
    (progn  
        (setq nuevo-estado (inv_pos estado (+(tenth estado) 1) (tenth estado)))
        (if (revisa-nodo nuevo-estado (append abierto cerrado))
        (setq abierto (inserta-abierto (crear-hijo actual nuevo-estado (hamm nuevo-estado meta) '>) abierto)))))
(if (izquierda estado)
    (progn  
        (setq nuevo-estado (inv_pos estado (-(tenth estado) 1) (tenth estado)))
        (if (revisa-nodo nuevo-estado (append abierto cerrado))
        (setq abierto (inserta-abierto (crear-hijo actual nuevo-estado (hamm nuevo-estado meta) '<) abierto)))))
)

(defun revisa-nodo (estado nodos)
(cond 
    ((null nodos) T)
    ((equal estado (nodo-matriz (car nodos))) NIL)
    (t (revisa-nodo estado (cdr nodos)))))


(defun crear-hijo(actual nuevo-estado dist operador)
(setq hijo (make-nodo :matriz nuevo-estado 
            :padre actual
            :valorg valg
            :valorh dist
            :valorf (+ valg dist)
            :operador operador )) hijo)


(defun inv_pos (estado esp_new esp_old);;
	(setf copia (copy-list estado))
	(rotatef (nth esp_new copia) (nth esp_old copia))
	(fill copia esp_new :start 9 :end 10) ;update the blank posiion in the new list
	copia                                                
)

(defun arriba (estado) 
(setq mov '(0 1 2))
(setq pos (tenth estado))
(null(member pos mov))
)

(defun abajo (estado) 
(setq mov '(6 7 8))
(setq pos (tenth estado))
(null(member pos mov))
)

(defun izquierda (estado) 
(setq mov '(0 3 6))
(setq pos (tenth estado))
(null(member pos mov))
)

(defun derecha (estado) 
(setq mov '(2 5 8))
(setq pos (tenth estado))
(null(member pos mov))
)


(defun inserta-abierto (nodo lista)
(cond
    ((null lista) (list nodo))
	((< (nodo-valorf nodo) (nodo-valorf (car lista))) 
	 (cons nodo lista))
	(t (cons (car lista) (inserta-abierto nodo (cdr lista)))))
)


(defun lista-solucion (node)
(cond ((null node) (print "Empieza la solucion") nil)
(t (push (nodo-operador node) solucion)
(lista-solucion (nodo-padre node))
)))






(setq lst '(1 2 3 4))
(setq lst2 '(3 2 1 4))

(setq nodo1 (make-nodo :matriz '(1 2 3 4 0 6 7 8 5 4) 
:padre nil
:valorg 0
:valorh 1
:valorf 0
:operador 0 ))

(setq nodo2 (make-nodo :matriz '(1 2 3 4 0 6 7 8 5 4) 
:padre nodo1
:valorg 0
:valorh 1
:valorf 0
:operador 0 ))

(setq nodo3 (make-nodo :matriz '(1 2 3 4 5 6 7 8 0 8) 
:padre nodo2
:valorg 0
:valorh 1
:valorf 0
:operador 0 ))

(setq nodo4 (make-nodo :matriz '(1 2 3 4 5 6 7 0 8 7) 
:padre nodo2
:valorg 0
:valorh 1
:valorf 0
:operador 0 ))