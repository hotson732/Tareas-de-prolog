

% Entidades
% ═════════

  % **Investigadores**
  % 1. Ana
  % 2. Bruno
  % 3. Carlos
  % 4. Diana
  % 5. Elisa

  % **Especialidades**
  % • Genética
  % • Microbiología
  % • Bioquímica
  % • Inmunología
  %• Neurociencia

  % **Horarios**
  % • 6am
  % • 8am
  % • 10am
  % • 12pm
  % • 14pm

% **Bebidas**
% • Café
% • Té
% • Jugo
% • Mate
% • Agua
% **Equipos**
% • Microscopio
% • Centrífuga
% • PCR
% • Espectrómetro
% • Incubadora

% **Países**
% • México
% • Chile
% • España
% • Argentina
% • Perú


% Reglas del Acertijo
% ═══════════════════

% 1. El investigador de Genética llega a las 6am.
% 2. Ana no trabaja ni en Genética ni en Neurociencia.
% 3. La persona que usa la Centrífuga bebe Té.
% 4. La investigadora de Perú llega a las 10am.
% 5. Carlos usa el Espectrómetro.
% 6. Quien bebe Café llega dos horas antes que quien bebe Jugo.
% 7. La persona que trabaja en Inmunología usa PCR.
% 8. La especialista en Bioquímica es de Chile.
% 9. La Incubadora es utilizada por alguien que llega a las 14pm.
% 10. El investigador de Argentina bebe Mate.
% 11. Elisa no bebe Té ni Café.
% 12. Diana trabaja en Microbiología.
% 13. El que llega a las 8am usa el Microscopio.
% 14. Bruno no es de México.
% 15. La persona de España trabaja en Neurociencia.
% 16. El usuario del equipo PCR llega después que el especialista en Microbiología.
% 17. El de México usa el equipo que NO es Microscopio ni Incubadora.
% 18. La persona que bebe Agua no usa ni PCR ni Espectrómetro.
% 19. El especialista en Neurociencia llega después de la persona que bebe Jugo.
% 20. El de Perú NO bebe Agua.


% Preguntas para resolver con Prolog
% ══════════════════════════════════

% 1. ¿A qué hora llega cada investigador?
% 2. ¿Qué especialidad tiene cada uno?
%  3. ¿Qué bebida prefiere cada investigador?
%  4. ¿Qué equipo usa cada quien?
%  5. ¿De qué país es cada uno?

investigadores([ana,bruno, carlos, diana, elisa]).
especialidades([genetica, microbiologia, bioquimica, inmunologia, neurociencia]).
horarios(['6am','8am','10am','12pm','14pm']).
bebidas([cafe, te, jugo, mate, agua]).
equipos([microscopio, centrifuga, pcr, espectrometro, incubadora]).
paises([mexico, chile, espana, argentina, peru]).



% reglas del acertijo
% reglas del acertijo

hora_index('6am', 1).
hora_index('8am', 2).
hora_index('10am', 3).
hora_index('12pm', 4).
hora_index('14pm', 5).
