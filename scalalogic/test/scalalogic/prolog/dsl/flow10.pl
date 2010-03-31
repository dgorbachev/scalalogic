% time(findall(P,path(11,110,[11],P),L)),length(L,LL).

path(X,Y) :- path(X,Y,[X],_).

path(X,X,A,A).
path(X,Y,A,R) :- 
        X \== Y,
        edge(X,Z),
        absent(Z,A),
        path(Z,Y,[Z|A],R).

edge(X,Y) :- dir_edge(Y,X).
edge(X,Y) :- dir_edge(X,Y).

absent(_,[]).
absent(X,[Y|Z]) :- X \== Y, absent(X,Z).

dir_edge(11,21).
dir_edge(11,22).
dir_edge(11,12).
dir_edge(12,22).
dir_edge(12,23).
dir_edge(12,13).
dir_edge(13,23).
dir_edge(13,24).
dir_edge(13,14).
dir_edge(14,24).
dir_edge(14,25).
dir_edge(14,15).
dir_edge(15,25).
dir_edge(15,26).
dir_edge(15,16).
dir_edge(16,26).
dir_edge(16,27).
dir_edge(16,17).
dir_edge(17,27).
dir_edge(17,28).
dir_edge(17,18).
dir_edge(18,28).
dir_edge(18,29).
dir_edge(18,19).
dir_edge(19,29).
dir_edge(19,210).
dir_edge(19,110).
dir_edge(110,210).
dir_edge(21,31).
dir_edge(21,32).
dir_edge(21,22).
dir_edge(22,32).
dir_edge(22,33).
dir_edge(22,23).
dir_edge(23,33).
dir_edge(23,34).
dir_edge(23,24).
dir_edge(24,34).
dir_edge(24,35).