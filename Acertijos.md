<div align="center">
<img src="./imgs/logos/tec_mexico.png" width="290" height="90" alt="Tec Morelia Logo">
<img src="./imgs/logos/tec_morelia.png" width="120" alt="Tec Morelia Logo">


---

# INSTITUTO TECNOLÓGICO DE MORELIA  

## Acertijos

### Ingeniería en Sistemas Computacionales
### Programación Lógica y Funcional


---

### Integrante | N.C.:
**Campo Hernández Edson Ángel** | **22120638**

---

**Profesor:** Alcaraz Chavez Jesus Eduardo

---

## Acertijo 1

```text

Cuatro personas necesitan cruzar un puente de noche con un sola
linterna. El puente es frágil y solo puede soportar a dos personas a
la vez. Cada persona tarda diferentes tiempos en cruzar (1, 2 5 y 10
minutos).  Cuando dos personas cruzan, lo hacen al ritmo del más lento
¿Cómo pueden todos cruzar el puente en 17 minutos?
```


##### Solucion propia 

Para este acertijo tenemos encuenta que solo contamos con una linterna y que cada dos personas cruzan a la velocidad del mas lento asi que podemos expresarlo de la sisguiente manera como x las personas
>1X1 +2X2 + 5X3 +10X4=17 

donde el numero que esta multiplicando es el tiempo minimo que tardar al cruzar el puete hay dos metodos de solución uno es asignandole valores a los x1,x2 ... o resolvidendo el sistema de ecuaciones
Solucion de orden
para el sistema de ecuaciones debemos tener encuenta que solo pueden pasar a la vez asi que podriamos simplifcarlo en 3 formaulas
>x1+x4=10
x1+x3=5
x1+x2=2

>x2+x4=10
x2+x3=5
x2+x1=2

>x3 +x4=10
x3+x2=5
x3+x1=2

>x4+x3=10
x4+x2=10
x4+x1=10
```
[1 + 0 + 0 +10 |10
 1 + 0 + 1 +0  |5-1
 1 + 1 + 0 +0  |2
  1 +0 + 0 + 0 +0  |1-1
]
[1 + 0 + 0 +10 |10
 0 + -1 + 0 +-1  |4
 1 + 1 + 0 +0  |2-1
  1 +0 + 0 + 0 +0  |1-1
]

[1 + 0 + 0 +10    |10
 0 + -1 + 0 + -1  |4
 0 + 0 + -1 + -1  |1
 1 +0 + 0 + 0 +0  |1-1
]
[1 + 0 + 0 +10 |10
 0 + 0 + 1 +0  |4
 0 + 0 + 0 +0  |1
 0 +-1 +-1 + -1|0
]
```
1. Bajo esto podemos asumir que el numero x1 pasa junto al x2 y se regresa el x1 acumulando un valor de 3 min
2. Ahora los mas lentos pasan juntos el X3 con x5, con un tiempo total de 13 min
3. El x2 regresa con la linterna  (15 min )y pasa con  el x1
dando el total de 17 minutos exaxtos



## Acertijo 2

```text

En una calle hay cinco casas, cada una de un color distinto.  En cada
casa vive una persona de distinta nacionalidad.  Cada dueño bebe un
único tipo de bebida, fuma una sola marca de cigarrillos y tiene una
mascota diferente a sus vecinos.  A partir de las 15 pistas
presentadas a continuación, la consigna que hay que responder es:
"¿Quién es el dueño del pez?".

*El británico vive en la casa roja.
*El sueco tiene un perro como mascota.
*El danés toma té.
*El noruego vive en la primera casa.
*El alemán fuma Prince.
*La casa verde está inmediatamente a la izquierda de la blanca.
*El dueño de la casa verde bebe café.
*El propietario que fuma Pall Mall cría pájaros.
*El dueño de la casa amarilla fuma Dunhill.
*El hombre que vive en la casa del centro bebe leche.
El vecino que fuma Blends vive al lado del que tiene un gato.
*El hombre que tiene un caballo vive al lado del que fuma Dunhill.
*El propietario que fuma Bluemaster toma cerveza.
*El vecino que fuma Blends viveal lado del que toma agua.
*El noruego vive al lado de la casa azul.
```


|objeto| Casa 1 | Casa 2 | Casa 3 | Casa 4| Casa 5 |
|---| ---    | ----   | ----   |  ---- | ------ |
|Nacionalidad| Noruego | Danés|Britanico|Aleman|Sueco
|Bebida|Agua | Té| Leche | Café| Cerveza |
|caja de cigarrillos|Dunhill|Blends | Pall Mall| Price| Blumaster| 
|mascota|Gato|Caballo|Pajaros|Pez|Perro|
|color de casa|Amarrila | Azul|Roja | Verde| Blanca|

El metodo de resolucion consto de tres fases
1. La primera fase de poner las consas que nos indica como que el noruego esta e la posicion numero 1
2. La segunda fase se basa en hacer metodo de descarte por ejemplo el que toma agua esta a la izquierda del que fuma blens ahora de ese punto vemos que solo esta una casilla disponible 
3. Para el ultimo metodo de ordenamiento es hacer que los datos que estan puestos consida con el orden de las cosas si tenemos que en la casa 4 toma cafe y falta el que toma cerveza entonces deducimos que es el sueco y rellenamos lo que falta
## Conclucion

El Aleman es el que cria peces

</div>