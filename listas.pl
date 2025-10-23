% Ejercicio 1: Cabeza y cola de una lista
cabeza_y_cola([Cabeza|Cola], Cabeza, Cola).

% Ejercicio 2: Verificar si un elemento pertenece a una lista
pertenece(Elemento, [Elemento|_]).
pertenece(Elemento, [_|Cola]) :- pertenece(Elemento, Cola).

% Ejercicio 3: Calcular la longitud de una lista
longitud([], 0).
longitud([_|Cola], N) :- longitud(Cola, N1), N is N1 + 1.

% Ejercicio 4: Concatenar dos listas
concatenar([], Lista, Lista).
concatenar([Cabeza|Cola1], Lista2, [Cabeza|Resultado]) :-
    concatenar(Cola1, Lista2, Resultado).

% Ejercicio 5: Invertir una lista
invertir(Lista, Invertida) :- invertir(Lista, [], Invertida).
invertir([], Acumulador, Acumulador).
invertir([Cabeza|Cola], Acumulador, Resultado) :-
    invertir(Cola, [Cabeza|Acumulador], Resultado).

% Ejercicio 6: Obtener el último elemento
ultimo([Ultimo], Ultimo).
ultimo([_|Cola], Ultimo) :- ultimo(Cola, Ultimo).

% Ejercicio 7: Sumar los elementos de una lista numérica
suma_lista([], 0).
suma_lista([Cabeza|Cola], Suma) :-
    suma_lista(Cola, SumaCola),
    Suma is Cabeza + SumaCola.

% Ejercicio 8: Eliminar un elemento de una lista
eliminar(Elemento, [Elemento|Cola], Cola).
eliminar(Elemento, [Cabeza|Cola], [Cabeza|Resultado]) :-
    eliminar(Elemento, Cola, Resultado).

% Ejercicio 9: Duplicar los elementos de una lista
duplicar([], []).
duplicar([Cabeza|Cola], [Cabeza, Cabeza|Resultado]) :-
    duplicar(Cola, Resultado).

% Ejercicio 10: Intercalar dos listas
intercalar([], Lista, Lista).
intercalar(Lista, [], Lista).
intercalar([Cabeza1|Cola1], [Cabeza2|Cola2], [Cabeza1, Cabeza2|Resultado]) :-
    intercalar(Cola1, Cola2, Resultado).