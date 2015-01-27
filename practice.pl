hanoi(0,_Source,_Destination,_Aux,[]).

hanoi(N,Sour,Dest,Aux,Moves):-
    N > 0,
    M is N-1,
    hanoi(M,Sour,Aux,Dest,Moves1),
    hanoi(M,Aux,Dest,Sour,Moves2),
    append(Moves1,[move(Sour,Dest)| Moves2], Moves).
    
    
is_wining([x,x,x]).
is_wining([x,x,b]).
is_wining([x,b,x]).
is_wining([b,x,x]).

triple(L,T) :- member(T,L).
triple(L,T) :- transpose(L,LT), member(T,LT).
triple([[A,_,_],[_,B,_],[_,_,C]], [A,B,C]).
triple([[_,_,A],[_,B,_],[C,_,_]], [A,B,C]).

transpose([[A,B,C],[D,E,F],[G,H,I]], [[A,D,G],[B,E,H],[C,F,I]]).

winner(L) :- L = [_,_,_], triple(L,T), is_wining(T).


zebra(Houses) :- 
    Houses = [house(norwegian,_,_,_,_),
              house(_,blue,_,_,_),
              house(_,_,_,milk,_),_,_],
    member(house(english,red,_,_,_),Houses),
    member(house(spanish,_,dog,_,_),Houses),
    member(house(_,green,_,coffee,_),Houses),
    member(house(ukranian,_,_,tea,_),Houses),
    right_of(house(_,wight,_,_,_), house(_,green,_,_,_), Houses),
    member(house(_,_,snails,_,old_Gold),Houses),
    member(house(_,yellow,_,_,kools),Houses),
    next_to(house(_,_,_,_,chesterﬁelds), house(_,_,fox,_,_),Houses),
    next_to(house(_,_,_,_,kools), house(_,_,horse,_,_),Houses),
    member(house(_,_,_,orange_jouce,lucky_strike),Houses),
    member(house(japanise,_,_,_,parliaments),Houses).
    
    
    
right_of(X,Y,[X,Y|_]).
right_of(X,Y,[_,X,Y|_]).
right_of(X,Y,[_,_,X,Y|_]).
right_of(X,Y,[_,_,_,X,Y]).
    
    
next_to(X,Y,Houses) :- right_of(X,Y,Houses).
next_to(X,Y,Houses) :- right_of(Y,X,Houses).

water(X) :- 
    zebra(Houses),
    member(house(X,_,_,water,_),Houses).

zevra(X) :-
    zebra(Houses),
    member(house(X,_,zebra,_,_),Houses).
  
change(e,w).
change(w,e).

move([X,X,Goat,Cabbage], wolf, [NX,NX,Goat,Cabbage]) :- change(X,NX).
move([X,Wolf,X,Cabbage], goat, [NX,Wolf,NX,Cabbage]) :- change(X,NX).
move([X,Wolf,Goat,X], cabbage, [NX,Wolf,Goat,NX]) :- change(X,NX).
move([X,Wolf,Goat,Cabbage], nothing, [NX,Wolf,Goat,Cabbage]) :- change(X,NX).


guarded_or_seperated(X,X,X).
guarded_or_seperated(_,X,Y) :-  X\=Y.

safe([Man,Wolf,Goat,Cabbage]) :- 
    guarded_or_seperated(Man,Wolf,Goat),
    guarded_or_seperated(Man,Goat,Cabbage).
    
solution([e,e,e,e],[]).

solution(C, [Move| Moves]) :- 
    move(C, Move, NC),
    safe(NC),
    solution(NC, Moves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

move([Stanes, Stoiva, Pos, N], move,[Stanes, Stoiva, NPos ,N], 3) :-
    between(1,N,NPos),
    NPos \= Pos.
    
move([Stanes, Stoiva, Pos, N], load,[NStanes, [lamp(Pos,S)|Stoiva], Pos ,N], 1) :-
    member(lamp(Pos,S),Stanes),
    select(lamp(Pos,S),Stanes,NStanes).
    
move([Stanes, [lamp(_,Pos)|Stoiva], Pos, N], unload,[Stanes, Stoiva, Pos ,N] ,1).
move([Stanes, [lamp(_,E)|Stoiva], Pos, N], unload,[[lamp(Pos,E)|Stanes], Stoiva, Pos ,N], 1).



solution([[],[],_,_], [],_,0).    
solution(C,[Move|Moves],Visited,NTCost) :-
    move(C,Move,NC, Cost),
    \+member(NC,Visited),
    solution(NC,Moves,[NC|Visited] ,OTCost),
    NTCost is Cost + OTCost.

provata(File,C) :- 
    read_and_return(File, S, L),
    length(Rs,_),
    solution([L,[],0,S],Rs,[],C),!.
    









 