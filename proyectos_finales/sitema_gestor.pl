:- dynamic sintoma/2.
:- use_module(library(readutil)).

% ==========================================================
% ================== SISTEMA EXPERTO INTEGRADO =============
% ==========================================================

% ==================== INICIALIZACIÓN ====================
iniciar :-
    writeln('==============================================='),
    writeln('   SISTEMA EXPERTO INTEGRADO PROLOG v3.0'),
    writeln('==============================================='),
    nl,
    reset_paciente(paciente),
    mostrar_menu_principal.

mostrar_menu_principal :-
    writeln('MENÚ PRINCIPAL:'),
    writeln('1. Sistema Médico '),
    writeln('2. Árbol Genealógico'),
    writeln('3. Base Pokémon -'),
    
    writeln('5. Utilidades del Sistema'),
    writeln('6. Salir'),
    nl,
    write('Seleccione una opción (1-6): '),
    read_line_to_string(user_input, OpcionStr),
    atom_number(OpcionStr, Opcion),
    procesar_menu(Opcion).

procesar_menu(1) :- sistema_medico.
procesar_menu(2) :- sistema_familia.
procesar_menu(3) :- sistema_pokemon.
procesar_menu(4) :- chat_mejorado.
procesar_menu(5) :- sistema_utilidades.
procesar_menu(6) :- writeln('¡Gracias por usar el sistema! 👋'), halt.
procesar_menu(_) :- writeln('Opción inválida'), mostrar_menu_principal.

% ==========================================================
% ================== SISTEMA MÉDICO COMPLETO ===============
% ==========================================================

sistema_medico :-
    writeln('==============================================='),
    writeln('            SISTEMA MÉDICO EXPERTO'),
    writeln('==============================================='),
    nl,
    writeln('COMANDOS:'),
    writeln('- "sintomas" - Mostrar síntomas actuales'),
    writeln('- "diagnostico" - Obtener diagnóstico'),
    writeln('- "limpiar" - Borrar todos los síntomas'),
    writeln('- "urgencia" - Verificar casos urgentes'),
    writeln('- "reporte" - Generar reporte médico'),
    writeln('- "ayuda" - Mostrar esta ayuda'),
    writeln('- "salir" - Volver al menú principal'),
    nl,
    loop_medico.

loop_medico :-
    write('Médico> '),
    read_line_to_string(user_input, Input),
    string_lower(Input, Texto),
    ( Texto = "salir" -> mostrar_menu_principal
    ; Texto = "ayuda" -> ayuda_medico
    ; Texto = "sintomas" -> mostrar_sintomas_actuales
    ; Texto = "diagnostico" -> realizar_diagnostico_completo
    ; Texto = "limpiar" -> limpiar_sistema_medico
    ; Texto = "urgencia" -> verificar_urgencias
    ; Texto = "reporte" -> generar_reporte_medico
    ; procesar_sintomas_medico(Texto)
    ),
    loop_medico.

tiene_sintoma(anorexia_nerviosa, desmayos).
tiene_sintoma(anorexia_nerviosa, debilidad).
tiene_sintoma(anorexia_nerviosa, cansancio_severo).
tiene_sintoma(anorexia_nerviosa, perdida_peso).
tiene_sintoma(anorexia_nerviosa, miedo_a_engordar).

tiene_sintoma(botulismo, sequedad_boca).
tiene_sintoma(botulismo, vision_borrosa).
tiene_sintoma(botulismo, dificultad_respirar).
tiene_sintoma(botulismo, debilidad_muscular).
tiene_sintoma(botulismo, paralisis).

tiene_sintoma(calculo_renal, dolor_lumbar).
tiene_sintoma(calculo_renal, sangre_orina).
tiene_sintoma(calculo_renal, dificultad_orinar).
tiene_sintoma(calculo_renal, nauseas).
tiene_sintoma(calculo_renal, vomitos).

tiene_sintoma(gripe, fiebre).
tiene_sintoma(gripe, dolor_cabeza).
tiene_sintoma(gripe, congestion).
tiene_sintoma(gripe, dolor_garganta).
tiene_sintoma(gripe, tos).
tiene_sintoma(gripe, cansancio).

tiene_sintoma(alergia, estornudos).
tiene_sintoma(alergia, picazon_ojos).
tiene_sintoma(alergia, congestion).
tiene_sintoma(alergia, picazon_nasal).
tiene_sintoma(alergia, lagrimeo).

tiene_sintoma(migrana, dolor_cabeza_severo).
tiene_sintoma(migrana, vision_borrosa).
tiene_sintoma(migrana, nauseas).
tiene_sintoma(migrana, sensibilidad_luz).
tiene_sintoma(migrana, sensibilidad_ruido).

tiene_sintoma(resfriado, estornudos).
tiene_sintoma(resfriado, congestion).
tiene_sintoma(resfriado, dolor_garganta).
tiene_sintoma(resfriado, tos_leve).
tiene_sintoma(resfriado, malestar_general).

tiene_sintoma(neumonia, fiebre_alta).
tiene_sintoma(neumonia, tos_con_flema).
tiene_sintoma(neumonia, dificultad_respirar).
tiene_sintoma(neumonia, dolor_pecho).
tiene_sintoma(neumonia, escalofrios).

tiene_sintoma(diabetes, sed_excesiva).
tiene_sintoma(diabetes, hambre_constante).
tiene_sintoma(diabetes, perdida_peso).
tiene_sintoma(diabetes, vision_borrosa).
tiene_sintoma(diabetes, fatiga).
tiene_sintoma(diabetes, miccion_frecuente).

tiene_sintoma(hipertension, dolor_cabeza).
tiene_sintoma(hipertension, mareos).
tiene_sintoma(hipertension, sangrado_nasal).
tiene_sintoma(hipertension, cansancio).
tiene_sintoma(hipertension, vision_borrosa).

tiene_sintoma(apendicitis, dolor_abdominal).
tiene_sintoma(apendicitis, nauseas).
tiene_sintoma(apendicitis, vomitos).
tiene_sintoma(apendicitis, fiebre_baja).
tiene_sintoma(apendicitis, perdida_apetito).

tiene_sintoma(gastritis, dolor_estomago).
tiene_sintoma(gastritis, ardor_estomacal).
tiene_sintoma(gastritis, nauseas).
tiene_sintoma(gastritis, indigestion).
tiene_sintoma(gastritis, hinchazon).

tiene_sintoma(bronquitis, tos_persistente).
tiene_sintoma(bronquitis, flema).
tiene_sintoma(bronquitis, dificultad_respirar).
tiene_sintoma(bronquitis, fatiga).
tiene_sintoma(bronquitis, fiebre_leve).

tiene_sintoma(asma, sibilancias).
tiene_sintoma(asma, dificultad_respirar).
tiene_sintoma(asma, opresion_pecho).
tiene_sintoma(asma, tos_nocturna).

tiene_sintoma(ansiedad, nerviosismo).
tiene_sintoma(ansiedad, preocupacion_excesiva).
tiene_sintoma(ansiedad, taquicardia).
tiene_sintoma(ansiedad, sudoracion).
tiene_sintoma(ansiedad, insomnio).

% Tratamientos
tratamiento(anorexia_nerviosa, 'Reeducación nutricional, psicoterapia y seguimiento médico constante').
tratamiento(botulismo, 'ANTITOXINA INMEDIATA Y HOSPITALIZACIÓN URGENTE').
tratamiento(calculo_renal, 'Hidratación abundante, analgésicos y posible litotricia').
tratamiento(gripe, 'Reposo, hidratación, antipiréticos y antivirales si es necesario').
tratamiento(alergia, 'Antihistamínicos, descongestionantes y evitar alérgenos').
tratamiento(migrana, 'Reposo en lugar oscuro y silencioso, analgésicos específicos').
tratamiento(resfriado, 'Líquidos, reposo y medicamentos para síntomas específicos').
tratamiento(neumonia, 'ANTIBIÓTICOS, reposo y hospitalización en casos graves').
tratamiento(diabetes, 'Control de glucosa, dieta balanceada, ejercicio y posible medicación').
tratamiento(hipertension, 'Medicación antihipertensiva, dieta baja en sal y ejercicio').
tratamiento(apendicitis, 'CIRUGÍA DE EMERGENCIA (apendicectomía)').
tratamiento(gastritis, 'Antiácidos, inhibidores de bomba de protones y dieta blanda').
tratamiento(bronquitis, 'Expectorantes, broncodilatadores y reposo').
tratamiento(asma, 'Broncodilatadores, corticosteroides inhalados y evitar desencadenantes').
tratamiento(ansiedad, 'Terapia psicológica, técnicas de relajación y posible medicación').

% Mapeo de síntomas desde texto natural
mapa_sintoma("fiebre", fiebre).
mapa_sintoma("fiebre alta", fiebre_alta).
mapa_sintoma("fiebre baja", fiebre_baja).
mapa_sintoma("fiebre leve", fiebre_leve).
mapa_sintoma("dolor de cabeza", dolor_cabeza).
mapa_sintoma("dolor cabeza", dolor_cabeza).
mapa_sintoma("dolor cabeza severo", dolor_cabeza_severo).
mapa_sintoma("migraña", dolor_cabeza_severo).
mapa_sintoma("jaqueca", dolor_cabeza_severo).
mapa_sintoma("congestión", congestion).
mapa_sintoma("congestion nasal", congestion).
mapa_sintoma("nariz tapada", congestion).
mapa_sintoma("estornudos", estornudos).
mapa_sintoma("estornudar", estornudos).
mapa_sintoma("visión borrosa", vision_borrosa).
mapa_sintoma("ver borroso", vision_borrosa).
mapa_sintoma("náuseas", nauseas).
mapa_sintoma("ganas de vomitar", nauseas).
mapa_sintoma("cansancio", cansancio_severo).
mapa_sintoma("fatiga", cansancio_severo).
mapa_sintoma("agotamiento", cansancio_severo).
mapa_sintoma("debilidad", debilidad).
mapa_sintoma("falta de fuerza", debilidad).
mapa_sintoma("desmayo", desmayos).
mapa_sintoma("desmayos", desmayos).
mapa_sintoma("pérdida conocimiento", desmayos).
mapa_sintoma("sequedad boca", sequedad_boca).
mapa_sintoma("boca seca", sequedad_boca).
mapa_sintoma("dificultad respirar", dificultad_respirar).
mapa_sintoma("falta de aire", dificultad_respirar).
mapa_sintoma("ahogo", dificultad_respirar).
mapa_sintoma("dificultad para respirar", dificultad_respirar).
mapa_sintoma("dolor lumbar", dolor_lumbar).
mapa_sintoma("dolor espalda baja", dolor_lumbar).
mapa_sintoma("dolor riñones", dolor_lumbar).
mapa_sintoma("sangre orina", sangre_orina).
mapa_sintoma("orina con sangre", sangre_orina).
mapa_sintoma("hematuria", sangre_orina).
mapa_sintoma("dificultad orinar", dificultad_orinar).
mapa_sintoma("problemas orinar", dificultad_orinar).
mapa_sintoma("disuria", dificultad_orinar).
mapa_sintoma("picazón ojos", picazon_ojos).
mapa_sintoma("ojos que pican", picazon_ojos).
mapa_sintoma("prurito ocular", picazon_ojos).
mapa_sintoma("dolor garganta", dolor_garganta).
mapa_sintoma("garganta irritada", dolor_garganta).
mapa_sintoma("faringitis", dolor_garganta).
mapa_sintoma("tos", tos).
mapa_sintoma("tos leve", tos_leve).
mapa_sintoma("tos persistente", tos_persistente).
mapa_sintoma("tos con flema", tos_con_flema).
mapa_sintoma("expectoración", tos_con_flema).
mapa_sintoma("dolor pecho", dolor_pecho).
mapa_sintoma("opresión pecho", opresion_pecho).
mapa_sintoma("dolor torácico", dolor_pecho).
mapa_sintoma("escalofríos", escalofrios).
mapa_sintoma("tiritona", escalofrios).
mapa_sintoma("sed excesiva", sed_excesiva).
mapa_sintoma("mucha sed", sed_excesiva).
mapa_sintoma("polidipsia", sed_excesiva).
mapa_sintoma("hambre constante", hambre_constante).
mapa_sintoma("mucho hambre", hambre_constante).
mapa_sintoma("polifagia", hambre_constante).
mapa_sintoma("pérdida peso", perdida_peso).
mapa_sintoma("adelgazar", perdida_peso).
mapa_sintoma("bajar de peso", perdida_peso).
mapa_sintoma("mareos", mareos).
mapa_sintoma("vértigo", mareos).
mapa_sintoma("inestabilidad", mareos).
mapa_sintoma("sangrado nasal", sangrado_nasal).
mapa_sintoma("sangre nariz", sangrado_nasal).
mapa_sintoma("epistaxis", sangrado_nasal).
mapa_sintoma("dolor abdominal", dolor_abdominal).
mapa_sintoma("dolor estómago", dolor_estomago).
mapa_sintoma("dolor vientre", dolor_abdominal).
mapa_sintoma("vómitos", vomitos).
mapa_sintoma("vomitar", vomitos).
mapa_sintoma("emesis", vomitos).
mapa_sintoma("ardor estomacal", ardor_estomacal).
mapa_sintoma("acidez", ardor_estomacal).
mapa_sintoma("pirosis", ardor_estomacal).
mapa_sintoma("indigestión", indigestion).
mapa_sintoma("mala digestión", indigestion).
mapa_sintoma("dispepsia", indigestion).
mapa_sintoma("hinchazón", hinchazon).
mapa_sintoma("distensión abdominal", hinchazon).
mapa_sintoma("sibilancias", sibilancias).
mapa_sintoma("silbido pecho", sibilancias).
mapa_sintoma("tos nocturna", tos_nocturna).
mapa_sintoma("nerviosismo", nerviosismo).
mapa_sintoma("ansiedad", nerviosismo).
mapa_sintoma("preocupación excesiva", preocupacion_excesiva).
mapa_sintoma("taquicardia", taquicardia).
mapa_sintoma("palpitaciones", taquicardia).
mapa_sintoma("sudoración", sudoracion).
mapa_sintoma("sudor", sudoracion).
mapa_sintoma("insomnio", insomnio).
mapa_sintoma("dificultad dormir", insomnio).
mapa_sintoma("micción frecuente", miccion_frecuente).
mapa_sintoma("orinar mucho", miccion_frecuente).
mapa_sintoma("poliuria", miccion_frecuente).
mapa_sintoma("sensibilidad luz", sensibilidad_luz).
mapa_sintoma("fotofobia", sensibilidad_luz).
mapa_sintoma("sensibilidad ruido", sensibilidad_ruido).
mapa_sintoma("fonofobia", sensibilidad_ruido).
mapa_sintoma("picazón nasal", picazon_nasal).
mapa_sintoma("lagrimeo", lagrimeo).
mapa_sintoma("epífora", lagrimeo).
mapa_sintoma("malestar general", malestar_general).
mapa_sintoma("debilidad muscular", debilidad_muscular).
mapa_sintoma("parálisis", paralisis).
mapa_sintoma("pérdida apetito", perdida_apetito).
mapa_sintoma("anorexia", perdida_apetito).
mapa_sintoma("miedo a engordar", miedo_a_engordar).
mapa_sintoma("flema", flema).
mapa_sintoma("esputo", flema).

ayuda_medico :-
    writeln('AYUDA SISTEMA MÉDICO:'),
    writeln('Escribe los síntomas que experimentas, por ejemplo:'),
    writeln('  "tengo fiebre y dolor de cabeza"'),
    writeln('  "me duele la garganta y tengo tos"'),
    writeln('  "tengo visión borrosa y mucha sed"'),
    nl,
    writeln('Comandos especiales:'),
    writeln('  sintomas    - Ver síntomas registrados'),
    writeln('  diagnostico - Obtener diagnóstico'),
    writeln('  limpiar     - Borrar todos los síntomas'),
    writeln('  urgencia    - Verificar casos urgentes'),
    writeln('  reporte     - Generar reporte médico'),
    writeln('  ayuda       - Mostrar esta ayuda'),
    writeln('  salir       - Volver al menú principal').

mostrar_sintomas_actuales :-
    findall(S, sintoma(paciente, S), Sintomas),
    ( Sintomas = [] ->
        writeln('No hay síntomas registrados.')
    ;
        writeln('Síntomas registrados:'),
        forall(member(S, Sintomas), format('  • ~w~n', [S]))
    ).

procesar_sintomas_medico(Texto) :-
    ( sub_string(Texto, 0, 5, _, "tengo") ->
        string_concat("", Texto, Texto2),
        procesar_frase_sintomas(Texto2)
    ; detectar_sintoma_directo(Texto) ->
        true
    ;
        format('No entendí: "~w". Escribe "tengo [síntoma]" o "ayuda".~n', [Texto])
    ).

procesar_frase_sintomas(Frase) :-
    split_string(Frase, " ", "", Palabras),
    procesar_palabras_sintomas(Palabras).

procesar_palabras_sintomas([]).
procesar_palabras_sintomas([Palabra|Resto]) :-
    ( mapa_sintoma(Palabra, Sintoma) ->
        ( sintoma(paciente, Sintoma) ->
            format('El síntoma "~w" ya estaba registrado.~n', [Sintoma])
        ;
            assertz(sintoma(paciente, Sintoma)),
            format('✓ Síntoma registrado: ~w~n', [Sintoma])
        )
    ;
        true
    ),
    procesar_palabras_sintomas(Resto).

detectar_sintoma_directo(Texto) :-
    mapa_sintoma(Texto, Sintoma),
    ( sintoma(paciente, Sintoma) ->
        format('El síntoma "~w" ya estaba registrado.~n', [Sintoma])
    ;
        assertz(sintoma(paciente, Sintoma)),
        format('✓ Síntoma registrado: ~w~n', [Sintoma])
    ).

% Cálculo de probabilidad
probabilidad(P, E, Porcentaje) :-
    findall(S, tiene_sintoma(E,S), Todos),
    findall(S, (tiene_sintoma(E,S), sintoma(P,S)), Confirmados),
    length(Todos, T),
    length(Confirmados, C),
    T > 0,
    Porcentaje is (C*100)/T.

% Diagnóstico posible
diagnostico_posible(P,E) :-
    probabilidad(P,E,Pc),
    Pc >= 40.  % Umbral más bajo para captar más posibilidades

realizar_diagnostico_completo :-
    findall(E, diagnostico_posible(paciente,E), Diagnosticos),
    ( Diagnosticos = [] ->
        writeln('Anorexia nervisa.'),
        writeln('Por favor, describe más síntomas.')
    ;
        writeln('🔍 DIAGNÓSTICOS POSIBLES:'),
        writeln('========================='),
        sort(Diagnosticos, DiagnosticosUnicos),
        forall(member(E, DiagnosticosUnicos), (
            probabilidad(paciente,E,Pc),
            tratamiento(E,Trat),
            format('~n📌 ~w~n', [E]),
            format('   Probabilidad: ~2f%%~n', [Pc]),
            format('   💊 Tratamiento: ~w~n', [Trat]),
            writeln('   📋 Síntomas coincidentes:'),
            forall((tiene_sintoma(E,S), sintoma(paciente,S)),
                format('      • ~w~n', [S])),
            ( Pc >= 70 -> writeln('   ⚠️  Alta probabilidad')
            ; Pc >= 50 -> writeln('   ⚠️  Probabilidad media')
            ; true
            )
        )),
        verificar_urgencias,
        sugerir_sintomas_faltantes
    ).

sugerir_sintomas_faltantes :-
    findall(E, diagnostico_posible(paciente,E), Diagnosticos),
    findall(S, (
        member(E, Diagnosticos),
        tiene_sintoma(E,S),
        \+ sintoma(paciente,S)
    ), SintomasFaltantes),
    sort(SintomasFaltantes, Unicos),
    ( Unicos \= [] ->
        writeln('~n🔍 Para mejorar el diagnóstico, ¿presentas alguno de estos síntomas?'),
        forall(member(S, Unicos), format('   • ~w~n', [S]))
    ;
        true
    ).

enfermedad_grave(E) :-
    member(E, [botulismo, apendicitis, neumonia, calculo_renal, meningitis]).

verificar_urgencias :-
    findall(E, (diagnostico_posible(paciente,E), enfermedad_grave(E)), Graves),
    ( Graves \= [] ->
        writeln('~n🚨 ¡ADVERTENCIA! POSIBLES CONDICIONES GRAVES DETECTADAS:'),
        writeln('====================================================='),
        forall(member(E, Graves), (
            probabilidad(paciente,E,Pc),
            format('~n⚠️  ~w (~2f%% probabilidad)~n', [E, Pc]),
            ( E = botulismo -> writeln('   ❗ Requiere ANTITOXINA INMEDIATA')
            ; E = apendicitis -> writeln('   ❗ Requiere CIRUGÍA DE EMERGENCIA')
            ; E = neumonia -> writeln('   ❗ Requiere ATENCIÓN HOSPITALARIA')
            ; E = calculo_renal -> writeln('   ❗ Podría requerir intervención urgente')
            ; true
            )
        )),
        writeln('~n❗ Consulte a un médico INMEDIATAMENTE.')
    ;
        writeln('~n✅ No se detectaron condiciones de emergencia.')
    ).

limpiar_sistema_medico :-
    retractall(sintoma(paciente,_)),
    writeln('✅ Todos los síntomas han sido eliminados.').

generar_reporte_medico :-
    get_time(Timestamp),
    format_time(atom(Fecha), '%Y-%m-%d %H:%M:%S', Timestamp),
    atomic_list_concat(['reporte_medico_', Fecha, '.txt'], '', Archivo),
    open(Archivo, write, Stream),
    
    format(Stream, 'REPORTE MÉDICO~n', []),
    format(Stream, '===============~n~n', []),
    format(Stream, 'Fecha: ~w~n~n', [Fecha]),
    
    format(Stream, 'SÍNTOMAS REPORTADOS:~n', []),
    findall(S, sintoma(paciente, S), Sintomas),
    ( Sintomas = [] ->
        format(Stream, 'No hay síntomas registrados.~n~n', [])
    ;
        forall(member(S, Sintomas), format(Stream, '• ~w~n', [S])),
        format(Stream, '~n', [])
    ),
    
    format(Stream, 'DIAGNÓSTICOS POSIBLES:~n', []),
    findall(E, diagnostico_posible(paciente,E), Diagnosticos),
    ( Diagnosticos = [] ->
        format(Stream, 'No se pudo establecer diagnóstico con la información actual.~n', [])
    ;
        sort(Diagnosticos, DiagnosticosUnicos),
        forall(member(E, DiagnosticosUnicos), (
            probabilidad(paciente,E,Pc),
            tratamiento(E,Trat),
            format(Stream, '~n~w (~2f%% probabilidad)~n', [E, Pc]),
            format(Stream, 'Tratamiento recomendado: ~w~n', [Trat])
        ))
    ),
    
    format(Stream, '~nRECOMENDACIONES GENERALES:~n', []),
    format(Stream, '1. Siempre consulte con un profesional de la salud~n', []),
    format(Stream, '2. No se automedique~n', []),
    format(Stream, '3. En caso de emergencia, acuda al centro médico más cercano~n', []),
    
    close(Stream),
    format('✅ Reporte generado: ~w~n', [Archivo]).

% ==========================================================
% ================== ÁRBOL GENEALÓGICO =====================
% ==========================================================

sistema_familia :-
    writeln('==============================================='),
    writeln('           ÁRBOL GENEALÓGICO'),
    writeln('==============================================='),
    nl,
    writeln('COMANDOS:'),
    writeln('- "padre [nombre]" - Padre de una persona'),
    writeln('- "madre [nombre]" - Madre de una persona'),
    writeln('- "hermanos [nombre]" - Hermanos de una persona'),
    writeln('- "hijos [nombre]" - Hijos de una persona'),
    writeln('- "abuelos [nombre]" - Abuelos de una persona'),
    writeln('- "tios [nombre]" - Tíos de una persona'),
    writeln('- "primos [nombre]" - Primos de una persona'),
    writeln('- "antepasados [nombre]" - Antepasados de una persona'),
    writeln('- "descendientes [nombre]" - Descendientes de una persona'),
    writeln('- "arbol" - Mostrar árbol completo'),
    writeln('- "buscar [nombre]" - Buscar persona'),
    writeln('- "listar" - Listar todas las personas'),
    writeln('- "ayuda" - Mostrar esta ayuda'),
    writeln('- "salir" - Volver al menú principal'),
    nl,
    loop_familia.

loop_familia :-
    write('Familia> '),
    read_line_to_string(user_input, Input),
    string_lower(Input, Texto),
    ( Texto = "salir" -> mostrar_menu_principal
    ; Texto = "ayuda" -> ayuda_familia
    ; Texto = "arbol" -> mostrar_arbol_completo
    ; Texto = "listar" -> listar_personas
    ; procesar_comando_familia(Texto)
    ),
    loop_familia.

% Base de conocimiento del árbol genealógico
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
person(lucia).
person(ricardo).
person(fernanda).
person(rodrigo).

% Relaciones de parentesco
padre_de(miguel, edson).
padre_de(miguel, jose).
padre_de(miguel, ivan).
padre_de(miguel, katya).
padre_de(miguel, jessica).
padre_de(macario, miguel).
padre_de(manuel, rosa).
padre_de(eder, gael).
padre_de(eder, santiago).
padre_de(ricardo, lucia).
padre_de(rodrigo, fernanda).

madre_de(rosa, edson).
madre_de(rosa, jose).
madre_de(rosa, ivan).
madre_de(rosa, katya).
madre_de(rosa, jessica).
madre_de(carmen, miguel).
madre_de(josefa, rosa).
madre_de(jessica, gael).
madre_de(jessica, santiago).
madre_de(veronica, lucia).
madre_de(claudia, fernanda).

% Géneros
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
male(ricardo).
male(rodrigo).

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
female(lucia).
female(fernanda).

% Funciones básicas
padre(X, Y) :- padre_de(X, Y).
madre(X, Y) :- madre_de(X, Y).
hijo(X, Y) :- (padre_de(Y, X); madre_de(Y, X)).
hermano(X, Y) :- padre(P, X), padre(P, Y), X \= Y.
abuelo(A, N) :- (padre(A, P), padre(P, N)); (madre(A, P), madre(P, N)).

% Funciones de consulta
todos_padres(P, L) :- findall(X, (padre(X, P); madre(X, P)), L).
todos_hijos(P, L) :- findall(X, hijo(X, P), L).
todos_hermanos(P, L) :- findall(X, hermano(P, X), L).
todos_abuelos(P, L) :- findall(A, abuelo(A, P), L).
todos_tios(P, L) :- findall(T, (padre(Pa, P), hermano(Pa, T); madre(Ma, P), hermano(Ma, T)), L).
todos_primos(P, L) :- findall(Pr, (padre(Pa, P), hermano(Pa, T), hijo(Pr, T); madre(Ma, P), hermano(Ma, T), hijo(Pr, T)), L).

todos_antepasados(P, L) :-
    findall(A, antepasado(P, A), L).

antepasado(P, A) :- padre(A, P); madre(A, P).
antepasado(P, A) :- padre(X, P), antepasado(X, A); madre(X, P), antepasado(X, A).

todos_descendientes(P, L) :-
    findall(D, descendiente(P, D), L).

descendiente(P, D) :- hijo(D, P).
descendiente(P, D) :- hijo(H, P), descendiente(H, D).

ayuda_familia :-
    writeln('AYUDA ÁRBOL GENEALÓGICO:'),
    writeln('Escribe comandos como:'),
    writeln('  "padre edson"'),
    writeln('  "hermanos rosa"'),
    writeln('  "hijos miguel"'),
    writeln('  "abuelos gael"'),
    writeln('  "tios jessica"'),
    writeln('  "primos ivan"'),
    writeln('  "antepasados santiago"'),
    writeln('  "descendientes macario"'),
    writeln('  "buscar rosa"'),
    nl,
    writeln('Comandos especiales:'),
    writeln('  arbol    - Mostrar árbol completo'),
    writeln('  listar   - Listar todas las personas'),
    writeln('  ayuda    - Mostrar esta ayuda'),
    writeln('  salir    - Volver al menú principal').

procesar_comando_familia(Texto) :-
    split_string(Texto, " ", "", [Comando|Args]),
    ( Args = [NombreStr] ->
        atom_string(Nombre, NombreStr),
        ( person(Nombre) ->
            ejecutar_comando_familia(Comando, Nombre)
        ;
            format('No se encontró a "~w" en el árbol genealógico.~n', [Nombre])
        )
    ;
        format('Uso: "~w [nombre]"~n', [Comando])
    ).

ejecutar_comando_familia("padre", Persona) :-
    findall(P, padre(P, Persona), Padres),
    ( Padres = [] -> format('~w no tiene padre registrado.~n', [Persona])
    ; forall(member(P, Padres), format('Padre de ~w: ~w~n', [Persona, P]))
    ).

ejecutar_comando_familia("madre", Persona) :-
    findall(M, madre(M, Persona), Madres),
    ( Madres = [] -> format('~w no tiene madre registrada.~n', [Persona])
    ; forall(member(M, Madres), format('Madre de ~w: ~w~n', [Persona, M]))
    ).

ejecutar_comando_familia("hermanos", Persona) :-
    todos_hermanos(Persona, Hermanos),
    ( Hermanos = [] -> format('~w no tiene hermanos registrados.~n', [Persona])
    ; format('Hermanos de ~w: ~w~n', [Persona, Hermanos])
    ).

ejecutar_comando_familia("hijos", Persona) :-
    todos_hijos(Persona, Hijos),
    ( Hijos = [] -> format('~w no tiene hijos registrados.~n', [Persona])
    ; format('Hijos de ~w: ~w~n', [Persona, Hijos])
    ).

ejecutar_comando_familia("abuelos", Persona) :-
    todos_abuelos(Persona, Abuelos),
    ( Abuelos = [] -> format('~w no tiene abuelos registrados.~n', [Persona])
    ; format('Abuelos de ~w: ~w~n', [Persona, Abuelos])
    ).

ejecutar_comando_familia("tios", Persona) :-
    todos_tios(Persona, Tios),
    ( Tios = [] -> format('~w no tiene tíos registrados.~n', [Persona])
    ; format('Tíos de ~w: ~w~n', [Persona, Tios])
    ).

ejecutar_comando_familia("primos", Persona) :-
    todos_primos(Persona, Primos),
    ( Primos = [] -> format('~w no tiene primos registrados.~n', [Persona])
    ; format('Primos de ~w: ~w~n', [Persona, Primos])
    ).

ejecutar_comando_familia("antepasados", Persona) :-
    todos_antepasados(Persona, Antepasados),
    ( Antepasados = [] -> format('~w no tiene antepasados registrados.~n', [Persona])
    ; format('Antepasados de ~w: ~w~n', [Persona, Antepasados])
    ).

ejecutar_comando_familia("descendientes", Persona) :-
    todos_descendientes(Persona, Descendientes),
    ( Descendientes = [] -> format('~w no tiene descendientes registrados.~n', [Persona])
    ; format('Descendientes de ~w: ~w~n', [Persona, Descendientes])
    ).

ejecutar_comando_familia("buscar", Persona) :-
    format('Información de ~w:~n', [Persona]),
    ( male(Persona) -> write('  Género: Masculino~n')
    ; female(Persona) -> write('  Género: Femenino~n')
    ),
    todos_padres(Persona, Padres),
    ( Padres \= [] -> format('  Padres: ~w~n', [Padres]) ; true ),
    todos_hijos(Persona, Hijos),
    ( Hijos \= [] -> format('  Hijos: ~w~n', [Hijos]) ; true ),
    todos_hermanos(Persona, Hermanos),
    ( Hermanos \= [] -> format('  Hermanos: ~w~n', [Hermanos]) ; true ).

mostrar_arbol_completo :-
    writeln('ÁRBOL GENEALÓGICO COMPLETO:'),
    writeln('============================'),
    nl,
    forall(person(P), mostrar_info_persona(P)).

mostrar_info_persona(P) :-
    format('~w ', [P]),
    ( male(P) -> write('(M)')
    ; female(P) -> write('(F)')
    ),
    todos_padres(P, Padres),
    ( Padres \= [] -> format(' - Padres: ~w', [Padres]) ; true ),
    todos_hijos(P, Hijos),
    ( Hijos \= [] -> format(' - Hijos: ~w', [Hijos]) ; true ),
    nl.

listar_personas :-
    findall(P, person(P), Personas),
    writeln('Personas en el árbol genealógico:'),
    forall(member(P, Personas), format('  • ~w~n', [P])).

% ==========================================================
% ================== BASE POKÉMON ==========================
% ==========================================================

sistema_pokemon :-
    writeln('==============================================='),
    writeln('           BASE DE DATOS POKÉMON'),
    writeln('==============================================='),
    nl,
    writeln('COMANDOS:'),
    writeln('- "info [nombre]" - Información de un Pokémon'),
    writeln('- "tipo [tipo]" - Pokémon por tipo'),
    writeln('- "generacion [num]" - Pokémon por generación'),
    writeln('- "legendarios" - Pokémon legendarios'),
    writeln('- "miticos" - Pokémon míticos'),
    writeln('- "comparar [pok1] [pok2]" - Comparar dos Pokémon'),
    writeln('- "evoluciones [nombre]" - Evoluciones de un Pokémon'),
    writeln('- "buscar [caracteristica]" - Buscar por característica'),
    writeln('- "listar" - Listar todos los Pokémon'),
    writeln('- "estadisticas" - Estadísticas de la base'),
    writeln('- "ayuda" - Mostrar esta ayuda'),
    writeln('- "salir" - Volver al menú principal'),
    nl,
    loop_pokemon.

loop_pokemon :-
    write('Pokémon> '),
    read_line_to_string(user_input, Input),
    string_lower(Input, Texto),
    ( Texto = "salir" -> mostrar_menu_principal
    ; Texto = "ayuda" -> ayuda_pokemon
    ; Texto = "listar" -> listar_todos_pokemon
    ; Texto = "legendarios" -> mostrar_legendarios
    ; Texto = "miticos" -> mostrar_miticos
    ; Texto = "estadisticas" -> estadisticas_pokemon
    ; procesar_comando_pokemon(Texto)
    ),
    loop_pokemon.

% Base de conocimiento de Pokémon
pokemon(pikachu, [
    tipo(electrico),
    color(amarillo),
    generacion(1),
    evoluciones('Pichu -> Pikachu -> Raichu'),
    habitat(bosque),
    tamaño(pequeño),
    forma(bipedo),
    legendario(no),
    mitico(no),
    peso(6.0),
    altura(0.4),
    categoria(raton),
    habilidad(electricidad_estatica),
    especie('Pokémon Ratón')
]).

pokemon(charizard, [
    tipo(fuego),
    tipo(volador),
    color(rojo),
    generacion(1),
    evoluciones('Charmander -> Charmeleon -> Charizard'),
    habitat(montana),
    tamaño(grande),
    forma(bipedo),
    legendario(no),
    mitico(no),
    peso(90.5),
    altura(1.7),
    categoria(llama),
    habilidad(mar_llamas),
    especie('Pokémon Llama')
]).

pokemon(bulbasaur, [
    tipo(planta),
    tipo(veneno),
    color(verde),
    generacion(1),
    evoluciones('Bulbasaur -> Ivysaur -> Venusaur'),
    habitat(pradera),
    tamaño(pequeño),
    forma(cuadrupedo),
    legendario(no),
    mitico(no),
    peso(6.9),
    altura(0.7),
    categoria(semilla),
    habilidad(espesura),
    especie('Pokémon Semilla')
]).

pokemon(squirtle, [
    tipo(agua),
    color(azul),
    generacion(1),
    evoluciones('Squirtle -> Wartortle -> Blastoise'),
    habitat(agua_dulce),
    tamaño(pequeño),
    forma(bipedo),
    legendario(no),
    mitico(no),
    peso(9.0),
    altura(0.5),
    categoria(tortuga),
    habilidad(torrente),
    especie('Pokémon Tortuga')
]).

pokemon(eevee, [
    tipo(normal),
    color(marron),
    generacion(1),
    evoluciones('Eevee -> Vaporeon/Jolteon/Flareon/Espeon/Umbreon/Leafeon/Glaceon/Sylveon'),
    habitat(ciudad),
    tamaño(pequeño),
    forma(cuadrupedo),
    legendario(no),
    mitico(no),
    peso(6.5),
    altura(0.3),
    categoria(evolucion),
    habilidad(adaptabilidad),
    especie('Pokémon Evolución')
]).

pokemon(mewtwo, [
    tipo(psiquico),
    color(blanco),
    generacion(1),
    evoluciones(ninguna),
    habitat(laboratorio),
    tamaño(grande),
    forma(bipedo),
    legendario(si),
    mitico(no),
    peso(122.0),
    altura(2.0),
    categoria(genetico),
    habilidad(presion),
    especie('Pokémon Genético')
]).

pokemon(mew, [
    tipo(psiquico),
    color(rosa),
    generacion(1),
    evoluciones(ninguna),
    habitat(desconocido),
    tamaño(pequeño),
    forma(bipedo),
    legendario(no),
    mitico(si),
    peso(4.0),
    altura(0.4),
    categoria(nueva_especie),
    habilidad(sincronia),
    especie('Pokémon Nueva Especie')
]).

pokemon(lugia, [
    tipo(psiquico),
    tipo(volador),
    color(blanco),
    generacion(2),
    evoluciones(ninguna),
    habitat(mar_profundo),
    tamaño(grande),
    forma(bipedo),
    legendario(si),
    mitico(no),
    peso(216.0),
    altura(5.2),
    categoria(inmersión),
    habilidad(presion),
    especie('Pokémon Inmersión')
]).

pokemon(lucario, [
    tipo(lucha),
    tipo(acero),
    color(azul),
    generacion(4),
    evoluciones('Riolu -> Lucario'),
    habitat(montana),
    tamaño(mediano),
    forma(bipedo),
    legendario(no),
    mitico(no),
    peso(54.0),
    altura(1.2),
    categoria(aura),
    habilidad(impasible),
    especie('Pokémon Aura')
]).

pokemon(greninja, [
    tipo(agua),
    tipo(siniestro),
    color(azul),
    generacion(6),
    evoluciones('Froakie -> Frogadier -> Greninja'),
    habitat(pantano),
    tamaño(mediano),
    forma(bipedo),
    legendario(no),
    mitico(no),
    peso(40.0),
    altura(1.5),
    categoria(ninja),
    habilidad(ligereza),
    especie('Pokémon Ninja')
]).

ayuda_pokemon :-
    writeln('AYUDA BASE POKÉMON:'),
    writeln('Escribe comandos como:'),
    writeln('  "info pikachu"'),
    writeln('  "tipo fuego"'),
    writeln('  "generacion 1"'),
    writeln('  "comparar charizard blastoise"'),
    writeln('  "evoluciones eevee"'),
    writeln('  "buscar legendario"'),
    writeln('  "buscar tipo agua"'),
    nl,
    writeln('Comandos especiales:'),
    writeln('  listar      - Listar todos los Pokémon'),
    writeln('  legendarios - Mostrar Pokémon legendarios'),
    writeln('  miticos     - Mostrar Pokémon míticos'),
    writeln('  estadisticas - Estadísticas de la base'),
    writeln('  ayuda       - Mostrar esta ayuda'),
    writeln('  salir       - Volver al menú principal').

procesar_comando_pokemon(Texto) :-
    split_string(Texto, " ", "", Partes),
    ( Partes = ["info", NombreStr] ->
        atom_string(Nombre, NombreStr),
        mostrar_info_pokemon(Nombre)
    ; Partes = ["tipo", TipoStr] ->
        atom_string(Tipo, TipoStr),
        buscar_por_tipo(Tipo)
    ; Partes = ["generacion", NumStr] ->
        atom_number(NumStr, Num),
        buscar_por_generacion(Num)
    ; Partes = ["comparar", Nom1Str, Nom2Str] ->
        atom_string(Nom1, Nom1Str),
        atom_string(Nom2, Nom2Str),
        comparar_pokemon(Nom1, Nom2)
    ; Partes = ["evoluciones", NombreStr] ->
        atom_string(Nombre, NombreStr),
        mostrar_evoluciones(Nombre)
    ; Partes = ["buscar", BusquedaStr] ->
        atom_string(Busqueda, BusquedaStr),
        buscar_pokemon(Busqueda)
    ;
        format('Comando no reconocido: "~w"~n', [Texto])
    ).

mostrar_info_pokemon(Nombre) :-
    ( pokemon(Nombre, Atributos) ->
        format('Información de ~w:~n', [Nombre]),
        writeln('════════════════════════════════════════'),
        forall(member(Atributo, Atributos),
            ( write('  • '), writeln(Atributo) )
        )
    ;
        format('No se encontró el Pokémon "~w"~n', [Nombre])
    ).

buscar_por_tipo(Tipo) :-
    findall(P, (pokemon(P, Attrs), member(tipo(Tipo), Attrs)), Lista),
    ( Lista = [] ->
        format('No hay Pokémon del tipo "~w"~n', [Tipo])
    ;
        format('Pokémon del tipo ~w:~n', [Tipo]),
        forall(member(P, Lista), format('  • ~w~n', [P]))
    ).

buscar_por_generacion(Gen) :-
    findall(P, (pokemon(P, Attrs), member(generacion(Gen), Attrs)), Lista),
    ( Lista = [] ->
        format('No hay Pokémon de la generación ~w~n', [Gen])
    ;
        format('Pokémon de la generación ~w:~n', [Gen]),
        forall(member(P, Lista), format('  • ~w~n', [P]))
    ).

comparar_pokemon(Pok1, Pok2) :-
    ( pokemon(Pok1, Attrs1), pokemon(Pok2, Attrs2) ->
        format('Comparando ~w vs ~w:~n', [Pok1, Pok2]),
        writeln('════════════════════════════════════════'),
        comparar_atributo(tipo, Attrs1, Attrs2),
        comparar_atributo(generacion, Attrs1, Attrs2),
        comparar_atributo(tamaño, Attrs1, Attrs2),
        comparar_atributo(peso, Attrs1, Attrs2),
        comparar_atributo(altura, Attrs1, Attrs2),
        comparar_atributo(legendario, Attrs1, Attrs2),
        comparar_atributo(mitico, Attrs1, Attrs2)
    ;
        writeln('No se pudieron comparar los Pokémon especificados.')
    ).

comparar_atributo(Atributo, Attrs1, Attrs2) :-
    ( member(Atributo(V1), Attrs1) -> true ; V1 = 'desconocido' ),
    ( member(Atributo(V2), Attrs2) -> true ; V2 = 'desconocido' ),
    format('  ~w: ~w | ~w~n', [Atributo, V1, V2]).

mostrar_evoluciones(Nombre) :-
    ( pokemon(Nombre, Attrs) ->
        ( member(evoluciones(Evols), Attrs) ->
            format('Evoluciones de ~w: ~w~n', [Nombre, Evols])
        ;
            format('~w no tiene evoluciones registradas.~n', [Nombre])
        )
    ;
        format('No se encontró el Pokémon "~w"~n', [Nombre])
    ).

buscar_pokemon(Busqueda) :-
    findall(P, (
        pokemon(P, Attrs),
        (
            member(tipo(Busqueda), Attrs);
            member(color(Busqueda), Attrs);
            member(habitat(Busqueda), Attrs);
            member(tamaño(Busqueda), Attrs);
            member(forma(Busqueda), Attrs);
            member(categoria(Busqueda), Attrs);
            member(habilidad(Busqueda), Attrs);
            member(especie(Busqueda), Attrs)
        )
    ), Resultados),
    ( Resultados = [] ->
        format('No se encontraron Pokémon con la característica "~w"~n', [Busqueda])
    ;
        format('Pokémon con característica "~w":~n', [Busqueda]),
        forall(member(P, Resultados), format('  • ~w~n', [P]))
    ).

listar_todos_pokemon :-
    findall(P, pokemon(P, _), Lista),
    writeln('Pokémon en la base de datos:'),
    writeln('════════════════════════════════════════'),
    forall(member(P, Lista), format('  • ~w~n', [P])).

mostrar_legendarios :-
    findall(P, (pokemon(P, Attrs), member(legendario(si), Attrs)), Legendarios),
    ( Legendarios = [] ->
        writeln('No hay Pokémon legendarios registrados.')
    ;
        writeln('Pokémon legendarios:'),
        forall(member(P, Legendarios), format('  • ~w~n', [P]))
    ).

mostrar_miticos :-
    findall(P, (pokemon(P, Attrs), member(mitico(si), Attrs)), Miticos),
    ( Miticos = [] ->
        writeln('No hay Pokémon míticos registrados.')
    ;
        writeln('Pokémon míticos:'),
        forall(member(P, Miticos), format('  • ~w~n', [P]))
    ).

estadisticas_pokemon :-
    findall(_, pokemon(_, _), Todos), length(Todos, Total),
    findall(T, (pokemon(_, Attrs), member(tipo(T), Attrs)), Tipos),
    sort(Tipos, TiposUnicos), length(TiposUnicos, NTipos),
    findall(_, (pokemon(_, Attrs), member(legendario(si), Attrs)), L), length(L, NLegendarios),
    findall(_, (pokemon(_, Attrs), member(mitico(si), Attrs)), M), length(M, NMiticos),
    
    writeln('📊 ESTADÍSTICAS POKÉMON:'),
    writeln('════════════════════════════════════════'),
    format('Total de Pokémon: ~w~n', [Total]),
    format('Tipos diferentes: ~w~n', [NTipos]),
    format('Pokémon legendarios: ~w~n', [NLegendarios]),
    format('Pokémon míticos: ~w~n', [NMiticos]),
    format('Tipos disponibles: ~w~n', [TiposUnicos]).

% ==========================================================
% ================== CHATBOT INTEGRADO =====================
% ==========================================================

chat_mejorado :-
    writeln('==============================================='),
    writeln('           CHATBOT INTELIGENTE'),
    writeln('==============================================='),
    nl,
    writeln('¡Hola! Soy un asistente inteligente.'),
    writeln('Puedo ayudarte con:'),
    writeln('  • Diagnóstico médico (ej: "tengo fiebre")'),
    writeln('  • Consultas familiares (ej: "padre de edson")'),
    writeln('  • Información Pokémon (ej: "info pikachu")'),
    writeln('  • Preguntas generales'),
    nl,
    writeln('Escribe "menu" para volver al menú principal'),
    writeln('Escribe "ayuda" para ver comandos disponibles'),
    nl,
    reset_paciente(chat_paciente),
    loop_chat.

loop_chat :-
    write('Tú> '),
    read_line_to_string(user_input, Input),
    string_lower(Input, Texto),
    ( Texto = "menu" -> mostrar_menu_principal
    ; Texto = "ayuda" -> ayuda_chat
    ; procesar_chat(Texto)
    ),
    loop_chat.

procesar_chat(Texto) :-
    ( es_saludo(Texto) -> responder_saludo(Texto)
    ; es_despedida(Texto) -> responder_despedida(Texto)
    ; contiene_palabra(Texto, ["tengo", "siento", "dolor", "me duele"]) -> procesar_sintoma_chat(Texto)
    ; contiene_palabra(Texto, ["padre", "madre", "hermano", "hijo", "familia"]) -> procesar_familia_chat(Texto)
    ; contiene_palabra(Texto, ["pokemon", "pikachu", "charizard", "tipo pokemon"]) -> procesar_pokemon_chat(Texto)
    ; es_pregunta(Texto) -> responder_pregunta(Texto)
    ; responder_generico(Texto)
    ).

es_saludo(Texto) :-
    sub_string(Texto, _, _, _, "hola");
    sub_string(Texto, _, _, _, "buenos");
    sub_string(Texto, _, _, _, "buenas");
    sub_string(Texto, _, _, _, "saludos").

es_despedida(Texto) :-
    sub_string(Texto, _, _, _, "adiós");
    sub_string(Texto, _, _, _, "adios");
    sub_string(Texto, _, _, _, "chao");
    sub_string(Texto, _, _, _, "hasta luego").

contiene_palabra(Texto, Palabras) :-
    member(Palabra, Palabras),
    sub_string(Texto, _, _, _, Palabra).

es_pregunta(Texto) :-
    sub_string(Texto, _, _, _, "qué");
    sub_string(Texto, _, _, _, "cómo");
    sub_string(Texto, _, _, _, "dónde");
    sub_string(Texto, _, _, _, "cuándo");
    sub_string(Texto, _, _, _, "por qué");
    sub_string(Texto, _, _, _, "quién").

responder_saludo(_) :-
    writeln('Asistente> ¡Hola! ¿En qué puedo ayudarte hoy?'),
    writeln('          Puedes preguntarme sobre salud, familia o Pokémon.').

responder_despedida(_) :-
    writeln('Asistente> ¡Hasta luego! Que tengas un buen día. 👋').

procesar_sintoma_chat(Texto) :-
    writeln('Asistente> Entiendo que mencionas algún síntoma. Déjame analizarlo...'),
    ( detectar_y_registrar_sintomas(Texto) ->
        writeln('Asistente> He registrado tus síntomas.'),
        writeln('          ¿Quieres que te dé un diagnóstico? (responde sí/no)'),
        read_line_to_string(user_input, Respuesta),
        ( Respuesta = "sí"; Respuesta = "si" ->
            paciente_diagnostico_chat
        ;
            writeln('Asistente> De acuerdo. Puedes seguir describiendo síntomas si lo deseas.')
        )
    ;
        writeln('Asistente> No pude identificar síntomas específicos.'),
        writeln('          Por favor, descríbelos más claramente, por ejemplo:')
        writeln('          "tengo fiebre y dolor de cabeza"')
    ).

detectar_y_registrar_sintomas(Texto) :-
    findall(S, (mapa_sintoma(Palabra, S), sub_string(Texto, _, _, _, Palabra)), Sintomas),
    Sintomas \= [],
    forall(member(S, Sintomas),
        ( \+ sintoma(chat_paciente, S) ->
            assertz(sintoma(chat_paciente, S))
        ; true
        )).

paciente_diagnostico_chat :-
    findall(E, (diagnostico_posible(chat_paciente,E), probabilidad(chat_paciente,E,P), P >= 50), Diagnosticos),
    ( Diagnosticos = [] ->
        writeln('Asistente> No tengo suficiente información para un diagnóstico preciso.'),
        writeln('          Por favor, describe más síntomas.')
    ;
        writeln('Asistente> Basado en tus síntomas, estos son los posibles diagnósticos:'),
        forall(member(E, Diagnosticos), (
            probabilidad(chat_paciente,E,Pc),
            tratamiento(E,Trat),
            format('  • ~w (~2f%% probabilidad)~n', [E, Pc]),
            format('    Tratamiento sugerido: ~w~n', [Trat])
        )),
        ( findall(E, (member(E, Diagnosticos), enfermedad_grave(E)), Graves), Graves \= [] ->
            writeln('⚠️  ADVERTENCIA: Algunas condiciones detectadas pueden ser graves.'),
            writeln('   Te recomiendo consultar con un profesional de la salud.')
        ;
            true
        )
    ).

procesar_familia_chat(Texto) :-
    ( extraer_nombre_chat(Texto, Nombre) ->
        ( person(Nombre) ->
            writeln('Asistente> Información sobre la familia de '),
            format('          ~w:~n', [Nombre]),
            todos_padres(Nombre, Padres),
            ( Padres \= [] -> format('          Padres: ~w~n', [Padres]) ; true ),
            todos_hermanos(Nombre, Hermanos),
            ( Hermanos \= [] -> format('          Hermanos: ~w~n', [Hermanos]) ; true ),
            todos_hijos(Nombre, Hijos),
            ( Hijos \= [] -> format('          Hijos: ~w~n', [Hijos]) ; true )
        ;
            format('Asistente> No encontré a "~w" en el árbol genealógico.~n', [Nombre])
        )
    ;
        writeln('Asistente> ¿De quién te gustaría saber información familiar?'),
        writeln('          Por ejemplo: "padre de edson" o "hermanos de rosa"')
    ).

extraer_nombre_chat(Texto, Nombre) :-
    split_string(Texto, " ", "", Palabras),
    member("de", Palabras),
    append(_, ["de", NombreStr|_], Palabras),
    atom_string(Nombre, NombreStr).

procesar_pokemon_chat(Texto) :-
    ( extraer_nombre_pokemon(Texto, Pokemon) ->
        ( pokemon(Pokemon, Atributos) ->
            writeln('Asistente> Información sobre '),
            format('          ~w:~n', [Pokemon]),
            forall(member(tipo(T), Atributos), format('          Tipo: ~w~n', [T])),
            forall(member(generacion(G), Atributos), format('          Generación: ~w~n', [G])),
            forall(member(especie(E), Atributos), format('          Especie: ~w~n', [E]))
        ;
            format('Asistente> No tengo información sobre el Pokémon "~w".~n', [Pokemon])
        )
    ; sub_string(Texto, _, _, _, "tipo pokemon") ->
        writeln('Asistente> Los tipos Pokémon más comunes son:'),
        writeln('          Agua, Fuego, Planta, Eléctrico, Psíquico, Normal'),
        writeln('          ¿Te interesa algún tipo en particular?')
    ;
        writeln('Asistente> ¿Sobre qué Pokémon te gustaría saber?'),
        writeln('          Por ejemplo: "info pikachu" o "pokemon tipo fuego"')
    ).

extraer_nombre_pokemon(Texto, Pokemon) :-
    split_string(Texto, " ", "", Palabras),
    ( member("pokemon", Palabras) ->
        append(_, ["pokemon", NombreStr|_], Palabras),
        atom_string(Pokemon, NombreStr)
    ; member("info", Palabras) ->
        append(_, ["info", NombreStr|_], Palabras),
        atom_string(Pokemon, NombreStr)
    ;
        member(Palabra, Palabras),
        pokemon(Pokemon, _),
        atom_string(Pokemon, Palabra)
    ).

responder_pregunta(Texto) :-
    ( sub_string(Texto, _, _, _, "qué puedes hacer") ->
        writeln('Asistente> Puedo ayudarte con:'),
        writeln('          1. Diagnóstico médico basado en síntomas'),
        writeln('          2. Consultas sobre relaciones familiares'),
        writeln('          3. Información detallada sobre Pokémon'),
        writeln('          4. Responder preguntas generales')
    ; sub_string(Texto, _, _, _, "cómo funciona") ->
        writeln('Asistente> Uso inteligencia artificial basada en reglas.'),
        writeln('          Analizo lo que escribes y busco en mis bases de conocimiento.')
    ; sub_string(Texto, _, _, _, "quién eres") ->
        writeln('Asistente> Soy un sistema experto integrado en Prolog.'),
        writeln('          Fui creado para demostrar capacidades de IA.')
    ;
        responder_generico(Texto)
    ).

responder_generico(Texto) :-
    random_member(Respuesta, [
        'Interesante, ¿puedes contarme más?',
        'No estoy seguro de entender completamente. ¿Podrías reformular?',
        'Eso es algo sobre lo que puedo ayudarte. ¿Te gustaría saber más?',
        'Tengo información sobre varios temas. ¿En qué área te puedo ayudar?',
        'Puedo ayudarte mejor si especificas tu pregunta.'
    ]),
    format('Asistente> ~w~n', [Respuesta]).

ayuda_chat :-
    writeln('Asistente> COMANDOS Y EJEMPLOS:'),
    writeln('════════════════════════════════════════'),
    writeln('Salud:'),
    writeln('  • "Tengo fiebre y dolor de cabeza"'),
    writeln('  • "Me duele el estómago y tengo náuseas"'),
    writeln('  • "¿Qué diagnóstico tengo?"'),
    nl,
    writeln('Familia:'),
    writeln('  • "Padre de edson"'),
    writeln('  • "Hermanos de rosa"'),
    writeln('  • "Hijos de miguel"'),
    nl,
    writeln('Pokémon:'),
    writeln('  • "Info pikachu"'),
    writeln('  • "Pokemon tipo fuego"'),
    writeln('  • "Comparar charizard y blastoise"'),
    nl,
    writeln('General:'),
    writeln('  • "¿Qué puedes hacer?"'),
    writeln('  • "¿Cómo funcionas?"'),
    writeln('  • "Hola / Adiós"'),
    nl,
    writeln('Escribe "menu" para volver al menú principal').

% ==========================================================
% ================== UTILIDADES DEL SISTEMA ================
% ==========================================================

sistema_utilidades :-
    writeln('==============================================='),
    writeln('           UTILIDADES DEL SISTEMA'),
    writeln('==============================================='),
    nl,
    writeln('1. Estadísticas generales'),
    writeln('2. Exportar datos'),
    writeln('3. Limpiar datos temporales'),
    writeln('4. Ver información del sistema'),
    writeln('5. Volver al menú principal'),
    nl,
    write('Seleccione una opción (1-5): '),
    read_line_to_string(user_input, OpcionStr),
    atom_number(OpcionStr, Opcion),
    procesar_utilidad(Opcion).

procesar_utilidad(1) :-
    estadisticas_generales,
    sistema_utilidades.

procesar_utilidad(2) :-
    writeln('Seleccione qué datos exportar:'),
    writeln('1. Datos médicos del paciente actual'),
    writeln('2. Árbol genealógico completo'),
    writeln('3. Base de datos Pokémon'),
    writeln('4. Volver'),
    nl,
    write('Opción: '),
    read_line_to_string(user_input, ExpStr),
    atom_number(ExpStr, Exp),
    ( Exp = 1 -> exportar_datos_medicos
    ; Exp = 2 -> exportar_arbol_genealogico
    ; Exp = 3 -> exportar_base_pokemon
    ; sistema_utilidades
    ),
    sistema_utilidades.

procesar_utilidad(3) :-
    limpiar_datos_temporales,
    sistema_utilidades.

procesar_utilidad(4) :-
    informacion_sistema,
    sistema_utilidades.

procesar_utilidad(5) :-
    mostrar_menu_principal.

procesar_utilidad(_) :-
    writeln('Opción inválida'),
    sistema_utilidades.

estadisticas_generales :-
    findall(E, tiene_sintoma(E,_), Enfermedades),
    sort(Enfermedades, EnfermedadesUnicas),
    length(EnfermedadesUnicas, NEnfermedades),
    
    % Estadísticas familiares
    findall(_, person(_), Personas), length(Personas, NPersonas),
    findall(_, padre_de(_,_), NPadres),
    findall(_, madre_de(_,_), NMadres),
    
    % Estadísticas Pokémon
    findall(_, pokemon(_,_), NPokemon),
    findall(T, (pokemon(_, Attrs), member(tipo(T), Attrs)), Tipos),
    sort(Tipos, TiposUnicos), length(TiposUnicos, NTipos),
    
    writeln('📊 ESTADÍSTICAS GENERALES DEL SISTEMA:'),
    writeln('════════════════════════════════════════'),
    writeln('SISTEMA MÉDICO:'),
    format('  • Enfermedades registradas: ~w~n', [NEnfermedades]),
    format('  • Síntomas en base: ~w~n', [contar_sintomas]),
    nl,
    writeln('ÁRBOL GENEALÓGICO:'),
    format('  • Personas registradas: ~w~n', [NPersonas]),
    format('  • Relaciones paternas: ~w~n', [NPadres]),
    format('  • Relaciones maternas: ~w~n', [NMadres]),
    nl,
    writeln('BASE POKÉMON:'),
    format('  • Pokémon registrados: ~w~n', [NPokemon]),
    format('  • Tipos diferentes: ~w~n', [NTipos]),
    format('  • Tipos disponibles: ~w~n', [TiposUnicos]).

contar_sintomas :-
    findall(S, tiene_sintoma(_,S), Sintomas),
    sort(Sintomas, Unicos),
    length(Unicos, N),
    N.

exportar_datos_medicos :-
    get_time(Timestamp),
    format_time(atom(Fecha), '%Y%m%d_%H%M%S', Timestamp),
    atomic_list_concat(['export_medico_', Fecha, '.txt'], '', Archivo),
    open(Archivo, write, Stream),
    
    format(Stream, 'EXPORTACIÓN DATOS MÉDICOS~n', []),
    format(Stream, 'Fecha: ~w~n~n', [Fecha]),
    
    format(Stream, 'ENFERMEDADES Y SÍNTOMAS:~n', []),
    forall(tiene_sintoma(E,S), format(Stream, '~w: ~w~n', [E,S])),
    
    format(Stream, '~nTRATAMIENTOS:~n', []),
    forall(tratamiento(E,T), format(Stream, '~w: ~w~n', [E,T])),
    
    close(Stream),
    format('✅ Datos médicos exportados a: ~w~n', [Archivo]).

exportar_arbol_genealogico :-
    get_time(Timestamp),
    format_time(atom(Fecha), '%Y%m%d_%H%M%S', Timestamp),
    atomic_list_concat(['export_familia_', Fecha, '.txt'], '', Archivo),
    open(Archivo, write, Stream),
    
    format(Stream, 'EXPORTACIÓN ÁRBOL GENEALÓGICO~n', []),
    format(Stream, 'Fecha: ~w~n~n', [Fecha]),
    
    format(Stream, 'PERSONAS:~n', []),
    forall(person(P), (
        format(Stream, '~w ', [P]),
        ( male(P) -> format(Stream, '(M)~n', [])
        ; female(P) -> format(Stream, '(F)~n', [])
        )
    )),
    
    format(Stream, '~nRELACIONES PATERNAS:~n', []),
    forall(padre_de(P,H), format(Stream, '~w es padre de ~w~n', [P,H])),
    
    format(Stream, '~nRELACIONES MATERNAS:~n', []),
    forall(madre_de(M,H), format(Stream, '~w es madre de ~w~n', [M,H])),
    
    close(Stream),
    format('✅ Árbol genealógico exportado a: ~w~n', [Archivo]).

exportar_base_pokemon :-
    get_time(Timestamp),
    format_time(atom(Fecha), '%Y%m%d_%H%M%S', Timestamp),
    atomic_list_concat(['export_pokemon_', Fecha, '.txt'], '', Archivo),
    open(Archivo, write, Stream),
    
    format(Stream, 'EXPORTACIÓN BASE POKÉMON~n', []),
    format(Stream, 'Fecha: ~w~n~n', [Fecha]),
    
    forall(pokemon(P, Atributos), (
        format(Stream, 'POKÉMON: ~w~n', [P]),
        forall(member(Atrib, Atributos), format(Stream, '  ~w~n', [Atrib])),
        format(Stream, '~n', [])
    )),
    
    close(Stream),
    format('✅ Base Pokémon exportada a: ~w~n', [Archivo]).

limpiar_datos_temporales :-
    retractall(sintoma(_,_)),
    writeln('✅ Todos los datos temporales han sido limpiados.').

informacion_sistema :-
    writeln('ℹ️  INFORMACIÓN DEL SISTEMA:'),
    writeln('════════════════════════════════════════'),
    writeln('Nombre: Sistema Experto Integrado Prolog v3.0'),
    writeln('Descripción: Sistema experto con múltiples módulos'),
    writeln('Módulos incluidos:'),
    writeln('  1. Sistema Médico - Diagnóstico basado en síntomas'),
    writeln('  2. Árbol Genealógico - Consultas de parentesco'),
    writeln('  3. Base Pokémon - Información de criaturas'),
    writeln('  4. Chatbot Inteligente - Interacción natural'),
    nl,
    writeln('Características:'),
    writeln('  • Procesamiento de lenguaje natural básico'),
    writeln('  • Base de conocimiento extensible'),
    writeln('  • Diagnóstico probabilístico'),
    writeln('  • Exportación de datos'),
    writeln('  • Interfaz interactiva por consola'),
    nl,
    writeln('Desarrollado como demostración de capacidades de Prolog').

% ==========================================================
% ================== INICIO DEL SISTEMA ====================
% ==========================================================

:- writeln('Cargando Sistema Experto Integrado...'),
   writeln('Sistema listo.'),
   mostrar_menu_principal.