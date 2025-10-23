<div align="center">
<img src="./imgs/logos/tec_mexico.png" width="290" height="90" alt="Tec Morelia Logo">
<img src="./imgs/logos/tec_morelia.png" width="120" alt="Tec Morelia Logo">


---

# INSTITUTO TECNOLÓGICO DE MORELIA  

## BIBLIOTECA Y LIBROS

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

# Reporte Completo del Sistema de Biblioteca en Prolog

## 1. Base de Conocimiento

### 1.1 Bibliotecas
```prolog
biblioteca(biblioteca_nacional).
biblioteca(biblioteca_central).
biblioteca(biblioteca_universitaria).

```
### 1.2 Libros
```prolog
libro('don_quijote_de_la_mancha').
libro('cien_anos_de_soledad').
libro('la_vida_es_sueno').
libro('principia_matematica').
libro('el_origen_de_las_especies').
libro('cuentos_infantiles').
libro('atlas_de_ciencias').
```
### 1.3 Autores
```prolog
autor(cervantes).
autor(garcia_marquez).
autor(calderon_de_la_barca).
autor(newton).
autor(darwin).
autor(autor_infantil).
```

## 2. Relaciones Principales
### 2.1 Relación Libro-Autor
```prolog
escrito_por('don_quijote_de_la_mancha', cervantes).
escrito_por('cien_anos_de_soledad', garcia_marquez).
escrito_por('la_vida_es_sueno', calderon_de_la_barca).
escrito_por('principia_matematica', newton).
escrito_por('el_origen_de_las_especies', darwin).
escrito_por('cuentos_infantiles', autor_infantil).
escrito_por('atlas_de_ciencias', newton).
```
### 2.2 Relación Biblioteca-Libro
```prolog
tiene(biblioteca_nacional, 'don_quijote_de_la_mancha').
tiene(biblioteca_nacional, 'cien_anos_de_la_soledad').
tiene(biblioteca_central, 'principia_matematica').
tiene(biblioteca_central, 'el_origen_de_las_especies').
tiene(biblioteca_central, 'don_quijote_de_la_mancha').
tiene(biblioteca_universitaria, 'atlas_de_ciencias').
tiene(biblioteca_universitaria, 'cuentos_infantiles').
```
### 2.3 Estado de Libros
```prolog
buen_estado('don_quijote_de_la_mancha').
buen_estado('principia_matematica').
buen_estado('cuentos_infantiles').
```
### 2.4 Temas de Libros
```prolog
tema('principia_matematica', ciencias).
tema('atlas_de_ciencias', ciencias).
tema('el_origen_de_las_especies', ciencias).
tema('don_quijote_de_la_mancha', literatura).
tema('cien_anos_de_soledad', literatura).
tema('cuentos_infantiles', infantil).
```
### 2.5 Secciones de Libros
```prolog
seccion('don_quijote_de_la_mancha', literatura).
seccion('cien_anos_de_soledad', literatura).
seccion('la_vida_es_sueno', literatura).
seccion('principia_matematica', ciencias).
seccion('atlas_de_ciencias', ciencias).
seccion('el_origen_de_las_especies', ciencias).
seccion('cuentos_infantiles', infantil).
```
## 3. Gestión de Ejemplares y Usuarios
### 3.1 Ejemplares por Libro
```prolog
ejemplares('don_quijote_de_la_mancha', 3).
ejemplares('cien_anos_de_soledad', 1).
ejemplares('principia_matematica', 1).
ejemplares('cuentos_infantiles', 2).
```
### 3.2 Usuario
```prolog
usuario(juan).
usuario(maria).
usuario(luis).
usuario(ana).

usuario_registrado(juan).
usuario_registrado(maria).
usuario_registrado(luis).
usuario_registrado(ana).
```
### 3.3 Multas y Préstamos
```prolog
tiene_multa(luis).

prestado_por(juan, 'don_quijote_de_la_mancha').
prestado_por(maria, 'cuentos_infantiles').
prestado_por(ana, 'principia_matematica').

prestado('don_quijote_de_la_mancha').
prestado('cuentos_infantiles').
prestado('principia_matematica').

prestamos(juan, 2).
prestamos(maria, 4).
prestamos(ana, 1).
prestamos(luis, 0).
```
### 3.4 Páginas por Libro
```prolog
paginas('don_quijote_de_la_mancha', 863).
paginas('cien_anos_de_soledad', 470).
paginas('cuentos_infantiles', 85).
paginas('principia_matematica', 350).
paginas('atlas_de_ciencias', 200).
```
## 4. Reglas y Consultas
## 4.1 Consultas de Catalogación
```prolog
catalogado(Libro) :- tiene(biblioteca_nacional, Libro).
```
### 4.2 Libros en Múltiples Bibliotecas
```prolog
esta_en_mas_de_una_biblioteca(Libro) :-
    tiene(Biblio1, Libro),
    tiene(Biblio2, Libro),
    Biblio1 \= Biblio2.
```
### 4.3 Restricciones de Préstamo
```prolog
no_puede_prestarse(Libro) :- raro(Libro).

puede_prestarse(Libro) :- buen_estado(Libro).

solo_consulta(Libro) :-
    libro(Libro),
    \+ puede_prestarse(Libro).
```
### 4.4 Gestión de Cantidades
```prolog
cantidad_libros(biblioteca_central, 10000+).

libro_con_varios_ejemplares(Libro) :-
    libro(Libro),
    ejemplares(Libro, N),
    N > 1.
```
### 4.5 Consultas de Autores
```prolog
tiene_libro_en_biblioteca(Autor) :-
    autor(Autor),
    libro(Libro),
    escrito_por(Libro, Autor),
    tiene(_, Libro).

autor_con_mas_de_5_libros(Autor) :-
    autor(Autor),
    findall(L, escrito_por(L, Autor), Lista),
    length(Lista, N),
    N > 5.

```
### 4.6 Reglas de Estado
```prolog
existe_libro_no_buen_estado :-
    libro(L),
    tiene(_, L),
    \+ buen_estado(L).
```
### 4.7 Reglas de Usuarios
```prolog
puede_prestar(Usuario, Libro) :-
    usuario_registrado(Usuario),
    libro(Libro).

no_puede_prestar(Usuario) :- tiene_multa(Usuario).

debe_devolver(Usuario) :-
    prestamos(Usuario, N),
    N > 3.
```
### 4.8 Reglas de Secciones
```prolog
misma_seccion(Autor) :-
    escrito_por(L1, Autor),
    escrito_por(L2, Autor),
    seccion(L1, S),
    seccion(L2, S).

seccion_ciencias(S) :-
    seccion(_, S),
    forall(seccion(L, S), tema(L, ciencias)).
```
### 4.9 Reglas de Préstamos y Devoluciones
```prolog
devolver_en(Libro, 15) :- prestado(Libro).

no_solicitado(Libro) :-
    libro(Libro),
    \+ prestado(Libro).
```
### 4.10 Consultas Especiales
```prolog
existe_libro_menos_100_paginas :-
    libro(L),
    paginas(L, N),
    N =< 100.

usuario_todos_infantil(Usuario) :-
    usuario(Usuario),
    forall((seccion(L, infantil), libro(L)), prestado_por(Usuario, L)).
```
## 5. Resumen del Sistema
### 5.1 Estadísticas

- Bibliotecas: 3

- Libros: 7

- Autores: 6

- Usuarios registrados: 4

- Libros prestados actualmente: 3


### 5.2 Características Clave
- Sistema de gestión de bibliotecas múltiples

- Control de estado de libros

- Gestión de préstamos y multas

- Organización por secciones temáticas

- Control de ejemplares disponibles

- Restricciones de préstamo basadas en estado y multas