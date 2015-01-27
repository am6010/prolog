
move([Stanes, Stoiva, Pos, N, M,_], move,[Stanes, Stoiva, NPos ,N, move,non], 3) :-
    M \= move,
    between(1,N,NPos), 
    NPos \= Pos.
        

move([Stanes, Stoiva, Pos, N,M,Ex], load,[NStanes, [lamp(Pos,S)|Stoiva], Pos ,N,load,non], 1) :-
    M = unload, 
    Ex = non, 
    Stanes = [_|T],
    member(lamp(Pos,S),T),
    select(lamp(Pos,S),Stanes,NStanes).

move([Stanes, Stoiva, Pos, N,M,Ex], load,[NStanes, [lamp(Pos,S)|Stoiva], Pos ,N,load,non], 1) :-
    (M = unload,Ex = yes ; M\=unload),
    member(lamp(Pos,S),Stanes),
    select(lamp(Pos,S),Stanes,NStanes).
         
move([Stanes, [lamp(_,Pos)|Stoiva], Pos, N,_,_], unload,[Stanes, Stoiva, Pos ,N,unload,yes] ,1).

move([Stanes, [lamp(_,E)|Stoiva], Pos, N,M,_], unload,[[lamp(Pos,E)|Stanes], Stoiva, Pos ,N,unload,non], 1):-
    E \= Pos,
    M \= load.

solution([[],[],_,_,_,_], [],RE,RE).    

solution(C,[Move|Moves],ACC,RE) :-
    move(C,Move,NC, Cost),
    NACC is ACC + Cost,
    solution(NC,Moves,NACC,RE).

provata(File,Rs,C) :- 
    read_and_return(File, S, L),
    length(Rs,_),
    solution([L,[],0,S,start,non],Rs,0,C),!.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_and_return(File,Stanes, Lamps) :-
    open(File, read, Stream),
    read_line(Stream, [Stanes, N]),
    read_gates(Stream, N, Lamps),
    close(Stream).

read_gates(Stream, N, Lamps) :-
    ( N > 0 ->
	Lamps = [G|Gs],
    read_line(Stream, [Start, End]),
	G = lamp(Start,End),
    N1 is N - 1,
    read_gates(Stream, N1, Gs)
    ; N =:= 0 ->
	Lamps = []
    ).
    
read_line(Stream, List) :-
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, List).    