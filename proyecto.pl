:- dynamic corral/1, niño/1, robot/1, sucio/1, obstaculo/1, 
    tablero/1, no_niños/1, carga_niño/0.


mi_write([]):-nl.
mi_write([X|Y]):- write(X), mi_write(Y).

mi_write_output([]).
mi_write_output([X|Y]):- write(output,X), mi_write_output(Y).

tablero(N,M,Tablero):-     
    numlist(1,N,Filas),numlist(1,M,Columnas),findall((X,Y),(member(X,Filas),member(Y,Columnas)),Tablero).

poner_corral(X):- corral(X) ; assert(corral(X)).
poner_niño(X):- niño(X) ; (not(obstaculo(X)), assert(niño(X))).
poner_robot(X):- robot(X) ;  (not(obstaculo(X)), not(niño(X)), assert(robot(X))).
poner_suciedad(X):- sucio(X) ; assert(sucio(X)).
poner_obstaculo(X) :- obstaculo(X) ; assert(obstaculo(X)).

elegir_random_lista([],_):- 
    writeln('elegir_random_lista espera una lista con al menos un elemento'), fail,!.
elegir_random_lista(Lista,X):-
    length(Lista,Listalen), Lr is random(Listalen), nth0(Lr,Lista,X).

vecina_directa((X,Y),(X,W)):- Y is W+1; Y is W-1.
vecina_directa((X,Y),(W,Y)):- X is W+1; X is W-1. 
vecina_diagonal((X,Y),(V,W)) :- 1=:=abs(V-X), 1=:=abs(W-Y).

casilla_interior((X,Y),N,M):- X =< N, Y=< M, X>0, Y>0.
colinda(X,Corral,N,M):- member(Y,Corral), vecina_directa(X,Y),casilla_interior(X,N,M).
frontera_corral(Corral,Frontera,N,M):-findall((X,Y),colinda((X,Y),Corral,N,M),Frontera).

vacia(X):-
    tablero(Tablero), member(X,Tablero),not(corral(X);niño(X);robot(X);sucio(X);obstaculo(X)).

poner_K_niños(0,_):- !.
poner_K_niños(K,Vacias):- 
    elegir_random_lista(Vacias,Cniño),poner_niño(Cniño),
    K1 is K-1, select(Cniño,Vacias,V2), poner_K_niños(K1,V2).

do_corral([]).
do_corral([X|Cor_rest]):-do_corral(Cor_rest),poner_corral(X).

aux_corral([],_,_,_,X,_,_):- X = nil,!.
aux_corral(Posibles,Frontera,Frontera2,Cor_rest,X,N,M):-
    elegir_random_lista(Posibles,X),select(X,Frontera2,F3), frontera_corral([X],Fron_x,N,M),
    findall(C,(member(C,Fron_x),not(member(C,Cor_rest);member(C,Frontera2))),Nuevas),append(Nuevas,F3,Frontera).

corral(Disponibles,Frontera,[X],1,N,M):-
    elegir_random_lista(Disponibles,X),frontera_corral([X],Frontera,N,M),!.
corral(Disponibles,Frontera,[X|Cor_rest],Size,N,M):-
    Size > 1,S is Size-1, corral(Disponibles,Frontera2,Cor_rest,S,N,M),
    intersection(Frontera2,Disponibles,Posibles), aux_corral(Posibles,Frontera,Frontera2,Cor_rest,X,N,M).

lograr_K_corrales(Disponibles,K,N,M,Logrados):- 
    (length(Disponibles,0); K=:=0), tablero(Tablero), poner_K_niños(Logrados,Tablero),!.
lograr_K_corrales(Disponibles,K,N,M,Logrados):-
    length(Disponibles,Dlen), Mm is round(Dlen/K), Mm1 is Mm+1,Mr is random(Mm1), Size is Mr+1, 
    (corral(Disponibles,Frontera,Pre_corral,Size,N,M);true),
    delete(Pre_corral,nil,Corral), mi_write(['nuevo corral: ',Corral]), do_corral(Corral),!,
    union(Corral,Frontera,No_disp),findall(C,(member(C,Disponibles),not(member(C,No_disp))),New_disp),
    L is Logrados+1, K1 is K-1, lograr_K_corrales(New_disp,K1,N,M,L).


distancia((X,Y),(V,W), K) :- 
    Cf is (V-X)**2,
    Cc is (W-Y)**2,
    K is sqrt(Cf+Cc).

elegir(D,_,Dd,Dc,B):- Dd<Dc, B=D,!.
elegir(_,C,_,_,B):- B=C.

cuadricula(Pos,Cuadro) :- 
    tablero(T), findall(X,( member(X,T), (vecina_directa(X,Pos) ; vecina_diagonal(X,Pos))), Cuadro).

direccion((A1,A2),(B1,B2),(S1,S2)) :- S1 is sign(B1-A1), S2 is sign(B2-A2).

dirigirse((A1,A2),(D1,D2),1,_,(S1,S2)) :- 
    S1 is (A1+D1), S2 is (A2+D2), not(obstaculo((S1,S2))), tablero(T), member((S1,S2),T).
dirigirse((A1,A2),(D1,D2),2,Posibles, F) :- 
    dirigirse((A1,A2),(D1,D2),1,Posibles, Final), member(Final,Posibles), F=Final,!.
dirigirse((A1,A2),(D1,D2),2,Posibles,(S1,S2)) :- 
    dirigirse((A1,A2),(D1,D2),1,Posibles,_), S1 is (A1+2*D1), S2 is (A2+2*D2), not(obstaculo((S1,S2))),
    tablero(T), member((S1,S2),T).

mover_robot(X,Y) :- 
    tablero(T), member(Y,T), retractall(robot(X)), poner_robot(Y).

mas_cercana(A,[B],B).
mas_cercana(A,[D|L],B) :-
    mas_cercana(A,L,C),
    distancia(A,D,Dd), distancia(A,C,Dc), 
    elegir(D,C,Dd,Dc,B).
    
avanzar(X,D,Pasos,Buscadas,_) :- 
    dirigirse(X,D,Pasos,Buscadas, Final), mover_robot(X,Final), !.
avanzar(X,D,Pasos,Buscadas,Q) :- 
    cuadricula(X,Cuadro), findall(G,(member(G,Cuadro),not(niño(G);obstaculo(G))),[L|Lr]),
    mas_cercana(Q,[L|Lr],Segura), mover_robot(X,Segura),writeln('\navanzar caso 2').

mover_robot_mas_cercano(_,[],_) :- writeln('fallo mover_robot_mas_cercano'), fail,!.
mover_robot_mas_cercano(X,Buscadas,Pasos) :-
    mas_cercana(X,Buscadas,Q), direccion(X,Q,D), 
    avanzar(X,D,Pasos,Buscadas,Q).
        

actua_robot_proactivo(Robot_pos,Cuadro) :- 
    carga_niño, findall(X,( member(X,Cuadro), corral(X), not(niño(X))), [P|R]),
    poner_niño(P), retractall(carga_niño), mi_write(['robot proactivo deja niño en ',P]), !. 
actua_robot_proactivo(Robot_pos,_) :-
    carga_niño, findall(X,(corral(X),not(niño(X))),Corrales), mover_robot_mas_cercano(Robot_pos,Corrales,2),
    mi_write(['robot proactivo lleva un niño cargado y se dirige al corral mas cercano ']),!.
actua_robot_proactivo(Robot_pos,Cuadro) :-
    findall(X,( member(X,Cuadro), niño(X), not(corral(X)) ), [P|R]), 
    retractall(niño(P)), mover_robot(Robot_pos,P), assert(carga_niño), 
    mi_write(['robot proactivo se mueve a ',P,' y carga el niño']),!.
actua_robot_proactivo(Robot_pos,_) :-
    findall(X,( niño(X), not(corral(X)) ),[N|R]), mover_robot_mas_cercano(Robot_pos,[N|R],1),
    mi_write(['robot proactivo se dirige a buscar niños ']),!.
actua_robot_proactivo(Robot_pos,_) :-
    sucio(Robot_pos), retractall(sucio(Robot_pos)),
    mi_write(['robot proactivo limpia ',Robot_pos]),!.
actua_robot_proactivo(Robot_pos,_) :-
    findall(X,sucio(X),[N|R]), mover_robot_mas_cercano(Robot_pos,[N|R],1),
    mi_write(['robot proactivo se dirige a las casillas sucias ']),!.


porciento_suciedad(P) :- 
    findall(X,sucio(X),J), length(J,Jlen), tablero(T), length(T,Tlen), P is (Jlen*100)/Tlen. 


actua_robot_reactivo(Robot_pos,Cuadro) :-
    carga_niño, findall(X,( member(X,Cuadro), corral(X), not(niño(X))), [P|R]),
    poner_niño(P), retractall(carga_niño), 
    mi_write(['robot reactivo deja niño en ',P]), !. 
actua_robot_reactivo(Robot_pos,Cuadro) :-
    not(carga_niño), findall(X,( member(X,Cuadro), niño(X), not(corral(X)) ), [P|R]), 
    retractall(niño(P)), mover_robot(Robot_pos,P), assert(carga_niño), 
    mi_write(['robot reactivo se mueve a ',P,' y carga el niño']),!.
actua_robot_reactivo(Robot_pos,_) :-
    sucio(Robot_pos), retractall(sucio(Robot_pos)),
    mi_write(['robot reactivo limpia ',Robot_pos]),!.
actua_robot_reactivo(Robot_pos,_) :-
    carga_niño, porciento_suciedad(P), P< 40,
    findall(X,(corral(X),not(niño(X))),Corrales), mover_robot_mas_cercano(Robot_pos,Corrales,2),
    mi_write(['robot reactivo lleva un niño cargado y se dirige al corral mas cercano ']),!.
actua_robot_reactivo(Robot_pos,_) :-
    porciento_suciedad(P), (P>40 ; findall(X,( niño(X), not(corral(X)) ),[])) , 
    findall(X,sucio(X),[N|R]), mover_robot_mas_cercano(Robot_pos,[N|R],1),
    mi_write(['robot reactivo se dirige a las casillas sucias ']),!.
actua_robot_reactivo(Robot_pos,_) :-
    findall(X,( niño(X), not(corral(X)) ),[N|R]), mover_robot_mas_cercano(Robot_pos,[N|R],1),
    mi_write(['robot reactivo se dirige a buscar niños ']),!.


limpiar_todo:- 
    write('Hola!!!!\n'),
    retractall(niño(X)), retractall(corral(X)), retractall(robot(X)), 
    retractall(sucio(X)), retractall(obstaculo(X)), retractall(carga_niño),
    write('Todo esta limpio!!!\n').


ver_tablero :- 
    writeln('\nTABLERO'), porciento_suciedad(P), mi_write(['porciento suciedad: ',P]),
    findall(X,niño(X),N), mi_write(['niños: ',N]),
    findall(X,(niño(X),not(corral(X))),Nfc), length(Nfc,Nfclen), mi_write(['hay fuera del corral: ',Nfclen]),
    findall(X,robot(X),R), mi_write(['robot: ',R]),
    findall(X,carga_niño,Cn), mi_write(['carga niño ',Cn]).

action_random(_,[],_):-!.
action_random(V,_,_):- V<1,!.
action_random(Veces,Posibles,Action):-
    elegir_random_lista(Posibles,X), T =.. [Action,X], (call(T);true), 
    V is Veces-1, select(X,Posibles,P), action_random(V,P,Action).


/*
N: cantidad de filas del tablero a generar
M: cantidad de columnas del tablero a generar
Ns: cantidad de casillas sucias iniciales
No: cantidad de obstaculos a ubicar
*/
generar_tablero(N,M,Ns,No):-
    limpiar_todo, tablero(Tablero), no_niños(Nc),
    mi_write(['Corrales: ', Nc,' Sucias: ', Ns,' Obstaculos: ', No]),
    lograr_K_corrales(Tablero, Nc, N, M, 0),
    findall(C,(member(C,Tablero),not(niño(C))),Noboy),
    action_random(1,Noboy,poner_robot),
    findall(C,(member(C,Tablero), not(corral(C))),Nocorral), 
    action_random(Ns,Nocorral,poner_suciedad),
    findall(G,vacia(G),Para_obs), 
    action_random(No,Para_obs,poner_obstaculo).

parte(Todo,Porciento,Parte) :- Parte is Porciento*Todo/100.

try_change_enviroment(M,_,_) :- M =\= 0, !.
try_change_enviroment(0,N,M) :- 
    numlist(1,5,Porc), Mult is N*M,
    porciento_suciedad(Ps), parte(Mult,Ps,Ns),
    Gr2 is random(5), nth0(Gr2,Porc,G2), Po is G2*10, parte(Mult,Po,No),
    generar_tablero(N,M,Ns,No), 
    writeln('CAMBIO EN EL TABLERO'),
    findall(X,corral(X),C), mi_write(['corrales: ',C]).



niñoAgente(Pos):-
               niñoPuedeMoverse(Pos),
               casillas_Posibles(Pos, Posiciones),
               selectMovSuc(Action),!,
               ejecutarAction(Pos, Action, Posiciones).


casillas_Posibles((X, Y), Posiciones):-     
                Fi is X - 1,
                Ff is X + 1,
                numlist(Fi,Ff,Filas),
                Ci is Y - 1,
                Cf is Y + 1,
                numlist(Ci,Cf,Columnas),
                findall((F,C),(member(F,Filas),member(C,Columnas)),Pos),
                select((X,Y), Pos, Pos2),
                tablero(Tablero),
                intersection(Pos2, Tablero, Posiciones).


selectMovSuc(Num):-
                R is random(2),
                Num is R + 1. 


ejecutarAction(Pos, Action, Posiciones):-
                Action =:= 1,
                mi_write(['el niño de ',Pos,' va a ensuciar']),
                reglas_ParaEnsuciar(Posiciones), !.

ejecutarAction(Pos, Action, Posiciones):-
                Action =:= 2,
                selectPositionniño(Posiciones, NewPos),         
                intentaMoverse(Pos, NewPos).



selectPositionniño(Posiciones, Pos):-
                length(Posiciones, Ind),
                P is random(Ind),
                Ps is P + 1,
                nth1(Ps, Posiciones, Pos).


intentaMoverse(ActPos, NewPos):-
               obstaculo(NewPos),
               mover_Obstaculo(ActPos, NewPos),
               mi_write(['el niño de ',ActPos,' se mueve a ',NewPos]),!.
intentaMoverse(ActPos, NewPos):-
               not(obstaculo(NewPos)),
               not(robot(NewPos)),
               not(niño(NewPos)),
               retractall(niño(ActPos)),
               assert(niño(NewPos)), 
               mi_write(['el niño de ',ActPos,' se mueve a ',NewPos]).


mover_Obstaculo((X, Y), (X1, Y1)):-
                recorre_obs((X1, Y1), T),
                retractall(niño(X, Y)),
                retractall(obstaculo((X1, Y1))),
                assert(niño((X1, Y1))),
                assert(obstaculo(T)).



recorre_obs((X, Y), Pos):-
                obstaculo(Pos),
                T is X + 1,
                append([], (T, Y), Pos),
                recorre_obs((T, Y), Pos).

recorre_obs((X, Y), Pos):- 
                vacia(Pos), !.



niñoPuedeMoverse((X, Y)):-
                not(corral((X, Y))), not(robot((X, Y))).


reglas_ParaEnsuciar(List):-
                cantniños(List, Cant),
                casos(Cant, EnsRan),
                buscarRandom(List, EnsRan, Aens),
                ensuciar_Casillas(Aens).
               
                



buscarRandom(_, 0, []):- !.
buscarRandom(List, Count, Result):-
            Count > 0,
            C1 is Count - 1,
            length(List, Ind),
            P is random(Ind),
            Ps is P + 1,
            nth1(Ps, List, Pos),
            Result = [Pos| T],
            buscarRandom(List, C1, T).


cantniños(List, Cant):-
        findall(X, (member(X, List), niño(X)), Temp),
        length(Temp, Cant).


casos(Cant, EnsRan):- 
        Cant =:= 1, 
        EnsRan = 1, !.
casos(Cant, EnsRan):-
        Cant =:= 2,
        EnsRan = 2, !.
casos(Cant, EnsRan):-
        Cant >= 3,
        EnsRan = 6, !.



ensuciarCasilla(Pos):-
               not(sucio(Pos)),
               not(obstaculo(Pos)),
               assert(sucio(Pos)).
               


ensuciar_Casillas([]).

ensuciar_Casillas([X|Y]):-
       ensuciarCasilla(X), !,
       ensuciar_Casillas(Y).

ensuciar_Casillas([X|Y]):-
       not(ensuciarCasilla(X)),
       ensuciar_Casillas(Y).

mover_niños([]):- !.
mover_niños([X|Y]):-
            (niñoAgente(X) ; true),!,
            mover_niños(Y).


simulacion(Tiempo,_,N,M) :- 
    porciento_suciedad(P), 60=<P,
    mi_write(['la suciedad a alcanzado el ',P,' pociento.']),
    writeln('El robot queda despedido, termina la simulacion'),
    informe(N,M,Tiempo,'Despedido'),!.
simulacion(Tiempo,_,_,_) :-
    findall(X,sucio(X),[]), findall(X,(niño(X),not(corral(X))),[]),
    write('todo esta limpio y ordenado, termina la simulacion'),
    informe(N,M,Tiempo,'OK'), !.
simulacion(Tiempo,Interval,_,_) :-
    Tiempo=:=100*Interval, write('se ha alcanzado 100 veces t, termina la simulacion'), 
    informe(N,M,Tiempo,'100 veces t'), !.
simulacion(Tiempo,Interval,N,M) :-
    mi_write(['\n\n\nESTAMOS EN EL MINUTO ',Tiempo]),
    robot(Robot_pos), cuadricula(Robot_pos,Cuadro),
    writeln('ACTUACION DEL ROBOT'),!,
    actua_robot_reactivo(Robot_pos,Cuadro),
    ((findall(X,  niño(X), Listniños), mover_niños(Listniños)) ; true),   
    Modulo is Tiempo mod Interval, try_change_enviroment(Modulo,N,M),
    T is Tiempo+1, ver_tablero,
    simulacion(T,Interval,N,M).


informe(N,M,Minuto, Mensaje) :-
    open('./informe.txt', write, Fd, [alias(output)]),
    porciento_suciedad(P),
    mi_write_output(['suciedad:',P]),
    mi_write_output(['minuto:',Minuto]),
    mi_write_output([Mensaje,'\n']),
    close(output).
    

/*
N: cantidad de filas del tablero a generar
M: cantidad de columnas del tablero a generar
Ps: porciento de suciedad inicial
Po: porciento de obstaculos a ubicar
niños: cantidad de niños
T: periodo de cambio del ambiente
*/
main(N,M,Ps,Po,niños,T) :-
    retractall(tablero(X)), retractall(no_niños(X)),
    tablero(N,M,Tablero), assert(tablero(Tablero)), assert(no_niños(niños)),
    Mult is N*M,
    parte(Mult,Ps,Cs),
    parte(Mult,Po,Co),
    generar_tablero(N,M,Cs,Co),
    simulacion(1,T,N,M).