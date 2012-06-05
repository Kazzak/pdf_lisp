;; variable global 
(defparameter *cuenta* 1) 

;; creación de las estructuras de almacenamiento 

(defparameter *ht* (make-hash-table))

;; se trabajara con una clase llamada a-pdf (archivo pdf)
;; con los atributos 
	;; nombre -- nombre del archivo
	;; titulo --titulo del archivo
	;; autor --creador del archivo
	;; llaves -- palabras clave que identifican el archivo
	;; creacion -- fecha de creacion
	
(defclass a-pdf () 
	( 
		(nombre :initarg :nombre)
		(titulo :initarg :titulo)
		(autor :initarg :autor)
		(llaves :initarg :llaves)
		(creacion :initarg :creacion)
	)
)

;;; crear el a-pdf
(defun crear-pdf (nombre titulo autor llaves  creacion)
	
	(defparameter *objeto*
		(make-instance 'a-pdf 
			:nombre nombre
			:titulo titulo
			:autor autor
			:llaves llaves
			:creacion creacion
		)
	)
	(setf (gethash *cuenta* *ht*) *objeto*)
	(setf *cuenta* (+ 1 *cuenta*))
)

;; funcion de apoyo split
(defun split (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\/ string :start i)
          collect (subseq string i j)
          while j)
)

(defun acceso (tira)
	(let ((count (length tira)) )
	(return-from acceso (elt tira (- count 1)))
	)
)

;; lectura del archivo: 
;; Esta función se encarga de leer el archivo y extraer los metadatos 
;; para ser instanciados en la clase pdf
(defun archivo (archivo); 
	(archivo-aux archivo))

;;función auxiliar de lectura del archivo para extracción de 
;;los metadatos funciona mediante un recorrido del archivo
(defun archivo-aux (file)
	 (let ((in (open file :direction :input 
	 :element-type 'unsigned-byte)))

			(let ((string (make-string (file-length in))))
			
					(dotimes (i (file-length in))
					(setf (char string i) (code-char (read-byte in))))
					
					;; busqueda de los metadatos
					;; nombre del archivo
					(defparameter *nombre* (namestring (pathname in)))
					(setf *nombre* (split *nombre*))
					(setf *nombre* (acceso *nombre*))
					
					(defparameter titulo 
					(if (NULL (buscarTag "Title" string)) 
					"Ninguno" (buscaCod "Title" "/Author" string)))
					
					(defparameter info 
					(if (NULL (buscarTag "Author" string)) 
					"Ninguno" (buscaCod "Author" "/Creator" string)))
					
					(setf info (split (string info)))
					
					(defparameter fecha 
					(if (NULL (buscarTag "CreationDate" string)) 
					"Ninguno" 
					(buscaCod "CreationDate" "/ModDate" string)))
					
					;; fin de la busqueda
					
					;; para crear el a-pdf
					(format t "cargando..... ~a~%" *nombre*)
					(crear-pdf *nombre* titulo (elt info 0) 
					(elt info 2) fecha)  	
		)
	)
)

;;  esta función se encarga de verificar si la etiqueta del 
;;  metadato esta definida en el archivo, retorna T si esta esta 
;;  presente y Nill en caso contrario
(defun buscarTag (tag string)
	(search tag string))

;; en caso de que la función buscarEtiqueta retorne T esta función se 
;; encargara de extraer el metadato del archivo 
(defun buscaCod (tag tag2 string)
	 (subseq string (+ (buscarTag tag string) 
	 (+ (length tag) 1)) (- (buscarTag tag2 string) 1)))

;; proceso de carga del directorio 
;; llama a la función archivo por cada archivo en el 
;; directorio 
(defun carga(directorio)
	(setf *cuenta* 1)
	(format t "iniciando carga de archivos~%")
	 (loop for f in (directory (concatenate 'string directorio "/*.pdf"))
		collect (archivo f)
	 )
)
;;; consultas 
;; consulta general 
(defgeneric get-all (objeto)
	(:documentation "obtiene todos los slots")
)

(defmethod get-all ((objeto a-pdf))
	(format t (slot-value objeto 'nombre))
	(format t "    ")
	(format t (slot-value objeto 'titulo))
	(format t "    ")
	(format t (slot-value objeto 'autor))
	(format t "       ")
	(format t (slot-value objeto 'creacion))	
	(format t "   ")
	(format t (slot-value objeto 'llaves))		
	(format t "~%")
	
)

(defun consulta-general ()
	(maphash #'(lambda (k v) (get-all v )) *ht*)
)

;; consulta por titulo 
(defgeneric aux-titulo (objeto titulo)
	(:documentation "consulta con respecto a titulo")
)

(defmethod aux-titulo ((objeto a-pdf) titulo)
	;; example 
	;; (search "we" "If we can't be free we can at least be cheap")
	(if (search  titulo  (slot-value objeto 'titulo) )
		(progn 
		(format t "~%")
		(format t (slot-value objeto 'nombre))
		)
	)
)
(defun consulta-titulo (&optional (titulo "")) 
	(format t "Resultados:.....")
	(maphash #'(lambda (k v) (aux-titulo v titulo )) *ht*)
)

;; consulta autor
(defgeneric aux-autor (objeto autor)
	(:documentation "consulta con respecto al autor")
)

(defmethod aux-autor ((objeto a-pdf) autor)
	;; example 
	;; (search "we" "If we can't be free we can at least be cheap")
	(if (search  autor  (slot-value objeto 'autor) )
		(progn 
		(format t "~%")
		(format t (slot-value objeto 'nombre))
		)
	)
)
(defun consulta-autor (&optional (autor "") ) 
	(format t "Resultados:.....")
	(maphash #'(lambda (k v) (aux-autor v autor )) *ht*)
)

;; consulta llave
(defgeneric aux-llave (objeto llave)
	(:documentation "consulta con respecto a la llave")
)

(defmethod aux-llave ((objeto a-pdf) llave)
	;; example 
	;; (search "we" "If we can't be free we can at least be cheap")
	(if (search  llave  (slot-value objeto 'llaves) )
		(progn 
		(format t "~%")
		(format t (slot-value objeto 'nombre))
		)
	)
)
(defun consulta-llave (&optional (llaves "") ) 
	(format t "Resultados:.....")
	(maphash #'(lambda (k v) (aux-llave v llaves )) *ht*)
)

;; consulta creacion
(defgeneric aux-creacion (objeto creacion)
	(:documentation "consulta con respecto a la creacion")
)

(defmethod aux-creacion ((objeto a-pdf) creacion)
	;; example 
	;; (search "we" "If we can't be free we can at least be cheap")
	(if (search  creacion  (slot-value objeto 'creacion) )
		(progn 
		(format t "~%")
		(format t (slot-value objeto 'nombre))
		)
	)
)
(defun consulta-fecha (&optional (creacion "") ) 
	(format t "Resultados:.....")
	(maphash #'(lambda (k v) (aux-creacion v creacion )) *ht*)
)

;; manual de ayuda al usuario
(defun ayuda () 
	(format t " <<<<Manual de ayuda al usuario>>>>")
	(format t "~%")
	(format t "Se mostraran las funciones que puede usar")
	(format t "~%")
	(format t "carga path-carpeta")
	(format t "~%")
	(format t "consulta-general")
	(format t "~%")
	(format t "consulta-titulo titulo")
	(format t "~%")
	(format t "consulta-autor autor")
	(format t "~%")
	(format t "consulta-llave llave")
	(format t "~%")
	(format t "consulta-fecha fecha")
)
