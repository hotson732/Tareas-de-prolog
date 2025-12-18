(defparameter *nodes* '(
    (Edson
     (apellidos (Campos Hernandez))
     (altura (Mediano))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Masculino)))
    
    (Miguel
     (apellidos (Campos Acosta))
     (altura (Mediano))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Masculino)))
    
    (Rosa
     (apellidos (Hernandez Melchor))
     (altura (Baja))
     (ojos (Marrones))
     (cabello (Negro))
     (color_piel (Blanco))
     (genero (Femenino)))
    
    (Jose
     (apellidos (Campos Hernandez))
     (altura (Alto))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Masculino)))
    
    (Ivan
     (apellidos (Campos Hernandez))
     (altura (Mediano))
     (ojos (Marrones))
     (cabello (Castaño_oscuro))
     (color_piel (Blanco))
     (genero (Masculino)))
    
    (Katya
     (apellidos (Campos Hernandez))
     (altura (Media))
     (ojos (Marrones))
     (cabello (Castaño))
     (color_piel (Blanco))
     (genero (Femenino)))
    
    (Jessica
     (apellidos (Campos Hernandez))
     (altura (Baja))
     (ojos (Marrones))
     (cabello (Negro))
     (color_piel (Morena))
     (genero (Femenino)))
    
    (Gael
     (apellidos (Melgoza Campos))
     (altura (Mediano))
     (ojos (Marrones))
     (cabello (Negro))
     (color_piel (Blanco))
     (genero (Masculino)))
    
    (Santiago
     (apellidos (Melgoza Campos))
     (altura (Baja))
     (ojos (Marrones))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Masculino)))
    
    (Eder
     (apellidos (Melgoza Melgarejo))
     (altura (Bajo))
     (ojos (Marrones))
     (cabello (Negro))
     (color_piel (Blanco))
     (genero (Masculino)))
    
    (Guadalupe
     (apellidos (Hernandez Melchor))
     (altura (Mediana))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Femenino)))
    
    (Reyna
     (apellidos (Hernandez Melchor))
     (altura (Mediana))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Femenino)))
    
    (Carmen
     (apellidos (Hernandez Melchor))
     (altura (Mediana))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Femenino)))
    
    (Veronica
     (apellidos (Hernandez Melchor))
     (altura (Mediana))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Femenino)))
    
    (Claudia
     (apellidos (Hernandez Melchor))
     (altura (Mediana))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Femenino)))
    
    (Alejandra
     (apellidos (Hernandez Melchor))
     (altura (Mediana))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Femenino)))
    
    (Josefa
     (apellidos (Melchor Lara))
     (altura (Mediana))
     (ojos (Oscuros))
     (cabello (Negro))
     (color_piel (Moreno))
     (genero (Femenino)))
    
    (Manuel
     (apellidos (Hernandez Hernandez))
     (altura (Mediana))
     (ojos (Negros))
     (cabello (Negro))
     (color_piel (Moreno_claro))
     (genero (Masculino)))
    
    (Macario
     (apellidos (Hernandez Melchor))
     (altura (Mediana))
     (ojos (Negros))
     (cabello (Negro))
     (color_piel (Blanco))
     (genero (Masculino)))
    
    (Martin
     (apellidos (Hernandez Melchor))
     (altura (Mediana))
     (ojos (Cafes_claros))
     (cabello (Negro))
     (color_piel (Blanco))
     (genero (Masculino)))
))


(defparameter *relaciones* '(
    (padre_de Miguel Edson)
    (padre_de Miguel Jose)
    (padre_de Miguel Ivan)
    (padre_de Miguel Katya)
    (padre_de Miguel Jessica)
    
    (madre_de Rosa Edson)
    (madre_de Rosa Jose)
    (madre_de Rosa Ivan)
    (madre_de Rosa Katya)
    (madre_de Rosa Jessica)
    
    (padre_de Eder Gael)
    (padre_de Eder Santiago)
    
    (madre_de Jessica Gael)
    (madre_de Jessica Santiago)
    
    (padre_de Manuel Rosa)
    (padre_de Manuel Guadalupe)
    (padre_de Manuel Reyna)
    (padre_de Manuel Carmen)
    (padre_de Manuel Veronica)
    (padre_de Manuel Claudia)
    (padre_de Manuel Alejandra)
    (padre_de Manuel Macario)
    (padre_de Manuel Martin)
    
    (madre_de Josefa Rosa)
    (madre_de Josefa Guadalupe)
    (madre_de Josefa Reyna)
    (madre_de Josefa Carmen)
    (madre_de Josefa Veronica)
    (madre_de Josefa Claudia)
    (madre_de Josefa Alejandra)
    (madre_de Josefa Macario)
    (madre_de Josefa Martin)
    
    (tio_de Macario Edson)
    (tio_de Macario Jose)
    (tio_de Macario Ivan)
    (tio_de Macario Katya)
    (tio_de Macario Jessica)

    (tio_de Martin Edson)
    (tio_de Martin Jose)
    (tio_de Martin Ivan)
    (tio_de Martin Katya)
    (tio_de Martin Jessica)

    (tia_de Guadalupe Edson)
    (tia_de Guadalupe Jose)
    (tia_de Guadalupe Ivan)
    (tia_de Guadalupe Katya)
    (tia_de Guadalupe Jessica)

    (tia_de Reyna Edson)
    (tia_de Reyna Jose)
    (tia_de Reyna Ivan)
    (tia_de Reyna Katya)
    (tia_de Reyna Jessica)

    (tia_de Carmen Edson)
    (tia_de Carmen Jose)
    (tia_de Carmen Ivan)
    (tia_de Carmen Katya)
    (tia_de Carmen Jessica)

    (tia_de Veronica Edson)
    (tia_de Veronica Jose)
    (tia_de Veronica Ivan)
    (tia_de Veronica Katya)
    (tia_de Veronica Jessica)

    (tia_de Claudia Edson)
    (tia_de Claudia Jose)
    (tia_de Claudia Ivan)
    (tia_de Claudia Katya)
    (tia_de Claudia Jessica)

    (tia_de Alejandra Edson)
    (tia_de Alejandra Jose)
    (tia_de Alejandra Ivan)
    (tia_de Alejandra Katya)
    (tia_de Alejandra Jessica)


    (tio_de Edson Gael)
    (tio_de Edson Santiago)

    (tio_de Jose Gael)
    (tio_de Jose Santiago)
    (tio_de Ivan Gael)
    (tio_de Ivan Santiago)
    (tia_de Katya Gael)
    (tia_de Katya Santiago)
    
))



(defun buscar-persona (nombre)
  (assoc nombre *nodes*))

(defun obtener-atributo (nombre atributo)
  (let ((persona (buscar-persona nombre)))
    (when persona
      (second (assoc atributo (rest persona))))))

(defun obtener-apellidos (nombre)
  (obtener-atributo nombre 'apellidos))

(defun obtener-genero (nombre)
  (first (obtener-atributo nombre 'genero)))

(defun relacion-existe? (relacion persona1 persona2)
  (member (list relacion persona1 persona2) *relaciones* :test #'equal))

(defun padre-de (hijo)
  (let ((padres '()))
    (dolist (rel *relaciones* padres)
      (when (and (equal (first rel) 'padre_de)
                 (equal (third rel) hijo))
        (push (second rel) padres)))
    padres))

(defun madre-de (hijo)
  (let ((madres '()))
    (dolist (rel *relaciones* madres)
      (when (and (equal (first rel) 'madre_de)
                 (equal (third rel) hijo))
        (push (second rel) madres)))
    madres))

(defun padres-de (hijo)
  (append (padre-de hijo) (madre-de hijo)))

(defun hijos-de-padre (padre)
  (let ((hijos '()))
    (dolist (rel *relaciones* hijos)
      (when (and (equal (first rel) 'padre_de)
                 (equal (second rel) padre))
        (push (third rel) hijos)))
    hijos))

(defun hijos-de-madre (madre)
  (let ((hijos '()))
    (dolist (rel *relaciones* hijos)
      (when (and (equal (first rel) 'madre_de)
                 (equal (second rel) madre))
        (push (third rel) hijos)))
    hijos))


(defun hermanos-de (persona)
  (let ((apellidos-persona (obtener-apellidos persona))
        (hermanos '()))
    (dolist (nodo *nodes* hermanos)
      (let ((nombre-hermano (first nodo)))
        (when (and (not (equal nombre-hermano persona))
                   (equal (obtener-apellidos nombre-hermano) apellidos-persona))
          (push nombre-hermano hermanos))))
    hermanos))

(defun hermanas-de (persona)
  (let ((apellidos-persona (obtener-apellidos persona))
        (hermanas '()))
    (dolist (nodo *nodes* hermanas)
      (let ((nombre-hermano (first nodo)))
        (when (and (not (equal nombre-hermano persona))
                   (equal (obtener-apellidos nombre-hermano) apellidos-persona)
                   (equal (obtener-genero nombre-hermano) 'Femenino))
          (push nombre-hermano hermanas))))
    hermanas))

(defun tio-de (persona)
  (let ((tios '()))
    (dolist (rel *relaciones* tios)
      (when (and (equal (first rel) 'tio_de)
                 (equal (third rel) persona))
        (push (second rel) tios)))
    tios))

(defun tia-de (persona)
  (let ((tias '()))
    (dolist (rel *relaciones* tias)
      (when (and (equal (first rel) 'tia_de)
                 (equal (third rel) persona))
        (push (second rel) tias)))
    tias))

(defun tios-de (persona)
  (append (tio-de persona) (tia-de persona)))

(defun abuelos-paternos (nieto)
  (let ((abuelos '()))
    (dolist (padre (padre-de nieto) abuelos)
      (setf abuelos (append abuelos (padres-de padre))))
    abuelos))

(defun abuelos-maternos (nieto)
  (let ((abuelos '()))
    (dolist (madre (madre-de nieto) abuelos)
      (setf abuelos (append abuelos (padres-de madre))))
    abuelos))


(defun abuelos-de (nieto)
  (append (abuelos-paternos nieto) (abuelos-maternos nieto)))

(defun imprimir-persona (nombre)
  (let ((persona (buscar-persona nombre)))
    (when persona
      (format t "~%~a:~%" nombre)
      (dolist (atributo (rest persona))
        (format t "  ~a: ~a~%" (first atributo) (second atributo))))))

(defun imprimir-relaciones (persona)
  (format t "~%Relaciones de ~a:~%" persona)
  (format t "  Padres: ~a~%" (padres-de persona))
  (format t "  Hermanos: ~a~%" (hermanos-de persona))
  (format t "  Hermanas: ~a~%" (hermanas-de persona))
  (format t "  Abuelos: ~a~%" (abuelos-de persona))
  (forma1t t "  Tios: ~a~%" (tios-de persona))
  (when (equal (obtener-genero persona) 'Masculino)
    (format t "  Hijos: ~a~%" (hijos-de-padre persona)))
  (when (equal (obtener-genero persona) 'Femenino)
    (format t "  Hijos: ~a~%" (hijos-de-madre persona))))