:- discontiguous altura/2.
:- discontiguous ojos/2.
:- discontiguous cabello/2.
:- discontiguous color_piel/2.

% ==========================================================
% PERSONAS
% ==========================================================
person(edson).
person(miguel).
person(rosa).
person(jose).
person(ivan).
person(katya).
person(jessica).
person(gael).
person(santiago).
person(eder).
person(guadalupe).
person(reyna).
person(carmen).
person(veronica).
person(claudia).
person(alejandra).
person(josefa).
person(manuel).
person(macario).
person(martin).



% ==========================================================
% RELACIONES
% ==========================================================
padre_de(miguel, edson).
padre_de(miguel, jose).
padre_de(miguel, ivan).
padre_de(miguel, katya).
padre_de(miguel, jessica).

madre_de(rosa, edson).
madre_de(rosa, jose).
madre_de(rosa, ivan).
madre_de(rosa, katya).
madre_de(rosa, jessica).

padre_de(eder,gael).
padre_de(eder,santiago).

madre_de(jessica, gael).
madre_de(jessica, santiago).

padre_de(manuel,rosa).
padre_de(manuel,guadalupe).
padre_de(manuel,reyna).
padre_de(manuel,carmen).
padre_de(manuel,veronica).
padre_de(manuel,claudia).
padre_de(manuel,alejandra).
padre_de(manuel,macario).
padre_de(manuel,martin).

madre_de(josefa,rosa).
madre_de(josefa,guadalupe).
madre_de(josefa,reyna).
madre_de(josefa,carmen).
madre_de(josefa,veronica).
madre_de(josefa,claudia).
madre_de(josefa,alejandra).
madre_de(josefa,macario).
madre_de(josefa,martin).

% ==========================================================
% APELLIDOS
% ==========================================================
apellidos(edson,     [campos, hernandez]).
apellidos(jose,      [campos, hernandez]).
apellidos(ivan,      [campos, hernandez]).
apellidos(katya,     [campos, hernandez]).
apellidos(jessica,   [campos, hernandez]).

apellidos(miguel,    [campos, acosta]).
apellidos(rosa,      [hernandez, melchor]).
apellidos(gael,      [melgoza, campos]).
apellidos(santiago,  [melgoza, campos]).
apellidos(eder,      [melgoza, melgarejo]).

apellidos(guadalupe, [hernandez, melchor]).
apellidos(reyna,     [hernandez, melchor]).
apellidos(carmen,    [hernandez, melchor]).
apellidos(veronica,  [hernandez, melchor]).
apellidos(claudia,   [hernandez, melchor]).
apellidos(alejandra, [hernandez, melchor]).
apellidos(macario,   [hernandez, melchor]).
apellidos(martin,    [hernandez, melchor]).
apellidos(manuel,    [hernandez, hernandez]).
apellidos(josefa,    [melchor, lara]).

% ==========================================================
% ATRIBUTOS FÍSICOS
% ==========================================================
altura(edson, mediano).
ojos(edson, oscuros).
cabello(edson, negro).
color_piel(edson, moreno).

altura(jose, alto).
ojos(jose, oscuros).
cabello(jose, negro).
color_piel(jose, moreno).

altura(ivan, mediano).
ojos(ivan, marrones).
cabello(ivan, castano_oscuro).
color_piel(ivan, blanco).

altura(katya, media).
ojos(katya, marrones).
cabello(katya, castano).
color_piel(katya, blanco).

altura(jessica, baja).
ojos(jessica, marrones).
cabello(jessica, negro).
color_piel(jessica, morena).

altura(manuel, mediana).
ojos(manuel, negros).
cabello(manuel, negro).
color_piel(manuel, moreno_claro).

altura(macario, mediana).
ojos(macario, negros).
cabello(macario, negro).
color_piel(macario, blanco).

altura(martin, mediana).
ojos(martin, cafes_claros).
cabello(martin, negro).
color_piel(martin, blanco).

% ==========================================================
% GÉNERO
% ==========================================================
male(edson).
male(jose).
male(ivan).
male(miguel).
male(gael).
male(santiago).
male(eder).
male(macario).
male(martin).
male(manuel).

female(rosa).
female(katya).
female(jessica).
female(guadalupe).
female(reyna).
female(carmen).
female(veronica).
female(claudia).
female(alejandra).
female(josefa).

% ==========================================================
% Reglas
% ==========================================================


hermanos(X, Y) :-
    apellidos(X, A),
    apellidos(Y, A),
    X \= Y.

hermano(X, Y) :-
    hermanos(X, Y),
    male(X).

hermana(X, Y) :-
    hermanos(X, Y),
    female(X).

padre(Padre, Hijo) :-
    male(Padre),
    apellidos(Padre, [ApPadre, _]),
    apellidos(Hijo, [ApHijo, _]),
    ApPadre == ApHijo,  % Primer apellido coincide
    Padre \= Hijo.

madre(Madre, Hijo) :-
    female(Madre),
    apellidos(Madre, [ApMadre, _]),
    apellidos(Hijo, [_, ApHijo]),
    ApMadre == ApHijo, 
    Madre \= Hijo.

abuelo_paterno(Abuelo, Nieto) :-
    padre(Abuelo, Padre),
    padre(Padre, Nieto).

abuelo_materno(Abuelo, Nieto) :-
    padre(Abuelo, Madre),
    madre(Madre, Nieto).

% Abuela paterna: madre del padre  
abuela_paterna(Abuela, Nieto) :-
    madre(Abuela, Padre),
    padre(Padre, Nieto).

abuela_materna(Abuela, Nieto) :-
    madre(Abuela, Madre),
    madre(Madre, Nieto).

abuelo(Abuelo, Nieto) :-
    male(Abuelo),
    (abuelo_paterno(Abuelo, Nieto); abuelo_materno(Abuelo, Nieto)).

abuela(Abuela, Nieto) :-
    female(Abuela),
    (abuela_paterna(Abuela, Nieto); abuela_materna(Abuela, Nieto)).


tio_paterno(Tio, Sobrino) :-
    padre(Padre, Sobrino),
    hermanos(Tio, Padre),
    male(Tio).

tio_materno(Tio, Sobrino) :-
    madre(Madre, Sobrino),
    hermanos(Tio, Madre),
    male(Tio).

tia_paterna(Tia, Sobrino) :-
    padre(Padre, Sobrino),
    hermanos(Tia, Padre),
    female(Tia).

tia_materna(Tia, Sobrino) :-
    madre(Madre, Sobrino),
    hermanos(Tia, Madre),
    female(Tia).

tio(Tio, Sobrino) :-
    male(Tio),
    (tio_paterno(Tio, Sobrino); tio_materno(Tio, Sobrino)).

tia(Tia, Sobrino) :-
    female(Tia),
    (tia_paterna(Tia, Sobrino); tia_materna(Tia, Sobrino)).

primos(X, Y) :-
    padre(P1, X),
    padre(P2, Y),
    hermanos(P1, P2),
    X \= Y.


todos_hermanos(P, L) :-
    findall(H, hermanos(P, H), R),
    sort(R, L).

todos_hermanas(P, L) :-
    findall(H, hermana(H, P), R),
    sort(R, L).

todos_padres(P, L) :-
    findall(Pa, padre(Pa, P), R1),
    findall(Ma, madre(Ma, P), R2),
    append(R1, R2, R),
    sort(R, L).

todos_tios(P, L) :-
    findall(T, tio(T, P), R),
    sort(R, L).

todas_tias(P, L) :-
    findall(T, tia(T, P), R),
    sort(R, L).

todos_primos(P, L) :-
    findall(X, primos(P, X), R),
    sort(R, L).

todos_abuelos(P, L) :-
    findall(A, abuelo(A, P), R1),
    findall(B, abuela(B, P), R2),
    append(R1, R2, R),
    sort(R, L).

% Hijos de una persona
hijos(Padre, Hijos) :-
    findall(H, padre(Padre, H), HijosP),
    findall(H, madre(Padre, H), HijosM),
    append(HijosP, HijosM, HijosTodos),
    sort(HijosTodos, Hijos).

pareja(Hombre, Mujer) :-
    male(Hombre),
    female(Mujer),
    findall(H, padre(Hombre, H), HijosH),
    findall(H, madre(Mujer, H), HijosM),
    HijosH == HijosM,
    HijosH \= [].