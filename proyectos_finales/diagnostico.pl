% ==========================================================
% HECHOS: ENFERMEDADES Y SÍNTOMAS
% ==========================================================

% --- Anorexia nerviosa
tiene_sintoma(anorexia_nerviosa, desmayos).
tiene_sintoma(anorexia_nerviosa, debilidad).
tiene_sintoma(anorexia_nerviosa, cansancio_severo).

% --- Botulismo 
tiene_sintoma(botulismo, sequedad_boca).
tiene_sintoma(botulismo, vision_borrosa).
tiene_sintoma(botulismo, dificultad_respirar).

% --- Cálculo renal
tiene_sintoma(calculo_renal, dolor_lumbar).
tiene_sintoma(calculo_renal, sangre_orina).
tiene_sintoma(calculo_renal, dificultad_orinar).

% --- lepra
tiene_sintoma(lepra, manchas).
tiene_sintoma(lepra, sensibilidad).

% --- poliomielitis
tiene_sintoma(poliomielitis, fiebre).
tiene_sintoma(poliomielitis, paralisis).
% --- tosferina
tiene_sintoma(tosferina, tos).
tiene_sintoma(tosferina, silbido).

tiene_sintoma(gripe, cuerpo_cortado).

% --- gripe
tiene_sintoma(gripe, fiebre).
tiene_sintoma(gripe, dolor_cabeza).
tiene_sintoma(gripe, congestion).

% --- alergia

tiene_sintoma(alergia, estornudos).
tiene_sintoma(alergia, picazon_ojos).
tiene_sintoma(alergia, congestion).


% --- migraña
tiene_sintoma(migrana, dolor_cabeza_severo).
tiene_sintoma(migrana, sensibilidad_luz).
tiene_sintoma(migrana, nauseas).

% --- resfriado
tiene_sintoma(resfriado, estornudos).
tiene_sintoma(resfriado, congestion).
tiene_sintoma(resfriado, dolor_garganta).



interpretar_texto(P, Texto) :-
    sub_atom(Texto, _, _, _, 'debilidad'),
    assertz(sintoma(P, debilidad)).

interpretar_texto(P, Texto) :-
    sub_atom(Texto, _, _, _, 'desmayo'),
    assertz(sintoma(P, desmayos)).

interpretar_texto(P, Texto) :-
    sub_atom(Texto, _, _, _, 'cansancio'),
    assertz(sintoma(P, cansancio_severo)).

interpretar_texto(P, Texto) :-
    sub_atom(Texto, _, _, _, 'vision borrosa'),
    assertz(sintoma(P, vision_borrosa)).

interpretar_texto(P, Texto) :-
    sub_atom(Texto, _, _, _, 'dificultad para respirar'),
    assertz(sintoma(P, dificultad_respirar)).

interpretar_texto(P, Texto) :-
    sub_atom(Texto, _, _, _, 'dolor lumbar'),
    assertz(sintoma(P, dolor_lumbar)).



tratamiento(anorexia_nerviosa, 'Reeducación nutricional y psicoterapia.').
tratamiento(botulismo, 'Antitoxina inmediata y hospitalización.').
tratamiento(calculo_renal, 'Hidratación abundante y analgésicos.').

tratamiento(gastritis, 'Omeprazol en ayunas y evitar picante/grasa').
tratamiento(infeccion_estomacal, 'Suero oral, dieta blanda y probioticos').
tratamiento(apendicitis, 'Cirugia de emergencia (Apendicectomia)').
tratamiento(dengue, 'Reposo, hidratacion y NO tomar aspirina (peligro de sangrado)').
tratamiento(varicela, 'Locion de calamina, banos coloidales y no rascarse').
tratamiento(lepra, 'Tratamiento largo con antibioticos (rifampicina)').
tratamiento(poliomielitis, 'Fisioterapia para recuperar movilidad').
tratamiento(migrana, 'Dormir a oscuras y ketorolaco/cafeina').
tratamiento(conjuntivitis, 'Gotas antibioticas (cloranfenicol) y lavar ojos').
tratamiento(diabetes, 'Insulina o metformina, y dieta sin azucar').
tratamiento(ansiedad, 'Terapia cognitivo-conductual y ejercicios de respiracion').

% ENFERMEDADES ESTOMACALES
tiene_sintoma(gastritis, ardor_estomago).
tiene_sintoma(gastritis, reflujo).
tiene_sintoma(gastritis, inflamacion).
tiene_sintoma(infeccion_estomacal, diarrea).
tiene_sintoma(infeccion_estomacal, vomito).
tiene_sintoma(infeccion_estomacal, colicos).
tiene_sintoma(apendicitis, dolor_lado_derecho).
tiene_sintoma(apendicitis, fiebre_alta).
tiene_sintoma(apendicitis, ombligo_doloroso).

% ENFERMEDADES VIRALES o PIEL 
tiene_sintoma(dengue, dolor_huesos).
tiene_sintoma(dengue, dolor_ojos).
tiene_sintoma(dengue, sarpullido).
tiene_sintoma(varicela, granitos).
tiene_sintoma(varicela, comezon_intensa).
tiene_sintoma(varicela, ampollas).
tiene_sintoma(lepra, manchas_blancas).
tiene_sintoma(lepra, falta_sensibilidad).
tiene_sintoma(lepra, entumecimiento).
tiene_sintoma(poliomielitis, paralisis_piernas).
tiene_sintoma(poliomielitis, atrofia_muscular).

% OTRAS ENFERMEDADEESS
tiene_sintoma(migrana, dolor_cabeza_fuerte).
tiene_sintoma(migrana, vision_borrosa).
tiene_sintoma(migrana, asco_luz).
tiene_sintoma(conjuntivitis, ojos_rojos).
tiene_sintoma(conjuntivitis, laganas).
tiene_sintoma(diabetes, mucha_sed).
tiene_sintoma(diabetes, orina_frecuente).
tiene_sintoma(diabetes, perdida_peso).
tiene_sintoma(ansiedad, taquicardia).
tiene_sintoma(ansiedad, nerviosismo).
tiene_sintoma(ansiedad, falta_aire_nerviosa).


:- dynamic sintoma/2.

reset_paciente(P) :- retractall(sintoma(P,_)).



pregunta(P, S) :- sintoma(P, S), !.
pregunta(P, S) :-
    write('¿El paciente tiene '), write(S), write('? (si/no): '),
    read(R),
    (R = si -> assertz(sintoma(P,S)) ; fail).



diagnostico_basico(P, E) :-
    tiene_sintoma(E, S),
    pregunta(P, S).



diagnostico_exclusivo(P, E) :-
    tiene_sintoma(E, S),
    sintoma(P, S),
    \+ (tiene_sintoma(Otra, S), Otra \= E).


probabilidad(P, E, Porcentaje) :-
    findall(S, tiene_sintoma(E,S), Todos),
    findall(S, (tiene_sintoma(E,S), sintoma(P,S)), Confirmados),
    length(Todos, T),
    length(Confirmados, C),
    T > 0,
    Porcentaje is (C * 100) / T.


diagnostico_preventivo(P, E) :-
    findall(S, tiene_sintoma(E,S), Todos),
    findall(S, (tiene_sintoma(E,S), sintoma(P,S)), Confirmados),
    length(Confirmados, C),
    length(Todos, T),
    C >= 1,
    C < T.


enfermedades_similares(E1, E2) :-
    E1 \= E2,
    findall(S, (tiene_sintoma(E1,S), tiene_sintoma(E2,S)), L),
    length(L, C),
    C >= 2.


contradictorio(fiebre, picazon_ojos).
contradictorio(nauseas, estornudos).

sintomas_contradictorios(P) :-
    sintoma(P, S1),
    sintoma(P, S2),
    contradictorio(S1, S2).

arbol_diagnostico(P, botulismo) :-
    pregunta(P, vision_borrosa),
    pregunta(P, dificultad_respirar).

arbol_diagnostico(P, calculo_renal) :-
    pregunta(P, dolor_lumbar),
    pregunta(P, sangre_orina).

arbol_diagnostico(P, anorexia_nerviosa) :-
    pregunta(P, desmayos),
    pregunta(P, cansancio_severo).


riesgo(P, botulismo, alto) :-
    sintoma(P, dificultad_respirar).

riesgo(_, calculo_renal, medio).
riesgo(_, anorexia_nerviosa, bajo).


tratamiento_combinado(P, Lista) :-
    findall(T,
        (diagnostico_basico(P,E), tratamiento(E,T)),
        Lista).


contar_sintomas_confirmados(P, E, C) :-
    findall(S, (tiene_sintoma(E,S), sintoma(P,S)), L),
    length(L, C).

severidad(P, E, severa) :- contar_sintomas_confirmados(P,E,C), C >= 3.
severidad(P, E, moderada) :- contar_sintomas_confirmados(P,E,2).
severidad(P, E, leve) :- contar_sintomas_confirmados(P,E,1).


recomendacion(P,E,'Acudir de inmediato a un hospital.') :-
    severidad(P,E,severa).

recomendacion(P,E,'Consultar médico y guardar reposo.') :-
    severidad(P,E,moderada).

recomendacion(P,E,'Observación y cuidados básicos.') :-
    severidad(P,E,leve).



diagnosticar_y_tratar(P, E, T) :-
    diagnostico_basico(P,E),
    tratamiento(E,T).


reporte(P) :-
    write('--- REPORTE MÉDICO ---'), nl,
    write('Síntomas confirmados:'), nl,
    forall(sintoma(P,S),(write('- '),write(S),nl)),
    write('Enfermedades posibles:'), nl,
    forall(diagnostico_basico(P,E),(write('- '),write(E),nl)),
    write('Probabilidades:'), nl,
    forall(probabilidad(P,E,Pc),
        (write(E),write(': '),write(Pc),write('%'),nl)),
    diagnostico_basico(P,E),
    severidad(P,E,Sev),
    tratamiento(E,T),
    recomendacion(P,E,R),
    write('Diagnóstico final: '), write(E), nl,
    write('Severidad: '), write(Sev), nl,
    write('Tratamiento: '), write(T), nl,
    write('Recomendación: '), write(R), nl.



diagnostico_desde_texto(P, Texto, Enfermedad) :-
    reset_paciente(P),
    interpretar_texto(P, Texto),
    diagnostico_basico(P, Enfermedad).
