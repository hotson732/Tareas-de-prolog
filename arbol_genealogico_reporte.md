<div align="center">
<img src="./imgs/logos/tec_mexico.png" width="290" height="90" alt="Tec Morelia Logo">
<img src="./imgs/logos/tec_morelia.png" width="120" alt="Tec Morelia Logo">


---

# INSTITUTO TECNOLÓGICO DE MORELIA  

## Arbol genealogico

### Ingeniería en Sistemas Computacionales
### Programación Lógica y Funcional


---

### Integrante | N.C.:
**Campo Hernández Edson Ángel** | **22120638**

---

**Profesor:** Alcaraz Chavez Jesus Eduardo

---

MORELIA, MICHOACÁN  
**octubre 2025**

</div>


<div>


---

##  Introducción

El presente reporte describe la implementación de un conjunto de datos que representa información personal y física de varios individuos utilizando dos lenguajes de programación de paradigmas distintos:

- **LISP** → lenguaje funcional y simbólico.  
- **Prolog** → lenguaje lógico basado en hechos y reglas.

Ambos programas buscan representar las **características personales** de un grupo de personas, incluyendo su nombre, apellidos, altura, complexión, color de ojos, cabello y tono de piel.  

---

## Código en Lisp

###  Definición de la base de datos

En Lisp, se utiliza la función `defparameter` para definir una variable global llamada `*nodes*`, la cual contiene una lista de listas.  
Cada sublista representa a una persona con sus respectivos atributos.

```lisp
(defparameter *nodes* '(
  (Edson
   (apellidos (Campos Hernandez))
   (altura (Mediano))
   (complexion (Atletico))
   (ojos (Oscuros))
   (cabello (Negro))
   (color_piel (moreno)))
  ...
))
```


```scss
(Nombre
  (apellidos (...))
  (altura (...))
  (complexion (...))
  (ojos (...))
  (cabello (...))
  (color_piel (...)))
```
Ejemplo:
```lisp

(Rosa
 (apellidos (Hernandez Melchor))
 (altura (baja))
 (complexion (llenita))
 (ojos (Marrones))
 (cabello (negro))
 (color_piel (blanco)))

```

---
Código en Prolog
 Definición de hechos

En Prolog, la misma información se traduce a una base de hechos, utilizando predicados para cada atributo.

```prolog
apellidos(edson, [campos, hernandez]).
altura(edson, mediano).
complexion(edson, atletico).
ojos(edson, oscuros).
cabello(edson, negro).
color_piel(edson, moreno).
```

Clasificación por género

```prolog

male(edson).
male(miguel).
female(rosa).
female(katya).
```

#Estructura general

Cada persona está definida mediante una serie de hechos de la forma:
``` prolog
atributo(persona, valor).
```

### Practica
```prolog
?- color_piel(Persona, blanco).
Persona = rosa ;
Persona = ivan ;
Persona = katya ;
Persona = gael ;
Persona = eder.
```

| Aspecto                 | Lisp                         | Prolog                                |
| ----------------------- | ------------------------------ | --------------------------------------- |
| Paradigma               | Funcional / simbólico          | Lógico / declarativo                    |
| Representación de datos | Listas anidadas                | Hechos y reglas                         |
| Consultas de datos      | Manuales o mediante funciones  | Automáticas mediante inferencia lógica  |
| Tipo de estructura      | Jerárquica                     | Relacional                              |
| Facilidad de extensión  | Alta (puede incluir funciones) | Alta (puede incluir reglas y consultas) |


## Conclusiones

Ambos lenguajes representan eficazmente una base de conocimiento sobre características humanas.

Lisp ofrece una estructura más simbólica y adecuada para procesamiento semántico o IA basada en listas.

Prolog permite realizar consultas lógicas y razonamiento automático, siendo ideal para sistemas expertos.

En conjunto, estos ejemplos muestran dos enfoques distintos para modelar conocimiento:
uno estructural (Lisp) y otro relacional e inferencial (Prolog).

