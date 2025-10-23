<div align="center">
<img src="./imgs/logos/tec_mexico.png" width="290" height="90" alt="Tec Morelia Logo">
<img src="./imgs/logos/tec_morelia.png" width="120" alt="Tec Morelia Logo">


---

# INSTITUTO TECNOLÓGICO DE MORELIA  

## Factorial
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

##  Descripción
El siguiente programa calcula el **factorial de un número** utilizando **recursión** en Prolog.  
El factorial de un número `n` (denotado como `n!`) se define como el producto de todos los enteros positivos desde `1` hasta `n`.

---

##  Código

```prolog
% Caso base
factorial(0, 1).

factorial(X, F) :-
    X > 0,
    X1 is X - 1,
    factorial(X1, F1),
    F is X * F1.
```
### Consulta:

``` prolog 
?- factorial(5, F).
```
```ini
F = 120.
```


## Conclusión

El programa utiliza definición recursiva para resolver el factorial, donde el caso base detiene la recursión y el caso general multiplica el valor actual por el factorial del número anterior.