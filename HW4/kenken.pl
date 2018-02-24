
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% kenken predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
Memory               limit         in use            free

   trail  stack      16383 Kb            0 Kb        16383 Kb
   cstr   stack      16384 Kb            0 Kb        16384 Kb
   global stack      32767 Kb            4 Kb        32763 Kb
   local  stack      16383 Kb            0 Kb        16383 Kb
   atom   table      32768 atoms      1789 atoms     30979 atoms

The following stat is the time for the 6 by 6 kenken puzzle

   Times            since start      since last
   user   time       0.007 sec       0.007 sec
   system time       0.003 sec       0.003 sec
   cpu    time       0.010 sec       0.010 sec
   real   time      44.118 sec      44.118 sec
The following stat is the time for the 4 by 4 kenken puzzle ( notice that the 4 by 4 puzzle in this case is way 
    faster than the plain_kenken)

Times              since start      since last
   user   time       0.009 sec       0.002 sec
   system time       0.007 sec       0.004 sec
   cpu    time       0.016 sec       0.006 sec
   real   time     272.545 sec     228.427 sec
*/

%Helper functions for the regular kenken 

% This function will check the length of a column.
col_len(Len, Something) :- length(Something, Len).

% setting up the row domain for the maplist/2
row_domain(Max, Row) :- fd_domain(Row, 1, Max).

% Extract element from the list of list.
matrix_element([I|J], T, Val) :- nth(I, T, Row),
			       nth(J, Row, Val).

% transposing the column to row to allow us to check if the column has the unique element 
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

% end of the helper functions 

% define constraints in C 
arithm_constraints(T, C) :- constraint(T,C).
constraint(T, +(S, L)) :- add(T, S, L, 0).
constraint(T, *(P, L)) :- mult(T, P, L, 1).
constraint(T, -(D, J, K)) :- subtract(T, D, J, K).
constraint(T, /(Q, J, K)) :- division(T, Q, J, K). 

% add/4 
% add(puzzle, sum, list of elements, accumulator)
% this predicate will take the sum of a list of element, 
% recursively fill the squares until accumulator meets the sum 
add(_, S, [], S).
add(T, S, [Hd|Tl], Acc) :-
    matrix_element(Hd, T, Val),
    Rec_acc #= Acc+Val,
    add(T, S, Tl, Rec_acc).

% mult/4 
% add(puzzle, sum, list of elements, accumulator)
% this predicate will take the product of a list of element, 
% recursively fill the squares until accumulator meets the sum 
mult(_, P, [], P).
mult(T, P, [Hd|Tl], Acc) :-
    matrix_element(Hd, T, Val),
    Rec_acc #= Acc * Val,
    mult(T, P, Tl, Rec_acc).


% subtract/4 
% add(puzzle, sum, list of elements, accumulator)
% this predicate will take subtraction  of a list of element, 
% recursively fill the squares until accumulator meets the sum 
subtract(_, D, _, _, D).
% J - K
subtract(T, D, J, K) :-
    matrix_element(J, T, Val_1),
    matrix_element(K, T, Val_2),
    Rec_acc #= Val_1 - Val_2,
    subtract(T, D, J, K, Rec_acc).
% K - J 
subtract(T, D, J, K) :-
    matrix_element(J, T, Val_1),
    matrix_element(K, T, Val_2),
    Rec_acc #= Val_2 - Val_1,
    subtract(T, D, J, K, Rec_acc).

% division/4 
% add(puzzle, sum, list of elements, accumulator)
% this predicate will take division of a list of element, 
% recursively fill the squares until accumulator meets the sum 
division(_, Q, _, _, Q).
% J - K
division(T, Q, J, K) :-
    matrix_element(J, T, Val_1),
    matrix_element(K, T, Val_2),
    Rec_acc #= Val_1 / Val_2,
    division(T, Q, J, K, Rec_acc).
% K - J
division(T, Q, J, K) :-
    matrix_element(J, T, Val_1),
    matrix_element(K, T, Val_2),
    Rec_acc #= Val_2 / Val_1,
    division(T, Q, J, K, Rec_acc).
%% end of the helper functions 

% The idea here is we want to make sure we want to use maplist/2 to check the col and row length
% and we can also use maplist to check if the row and col contains the unique elmement 
% For prolog, we need to tranpose col to row so we can check element in the list 
% http://eclipseclp.org/doc/bips/lib/lists/maplist-2.html

kenken(N,C,T):-
    length(T,N), maplist(col_len(N), T), % check the length of the puzzle make sure we have N by N 
    maplist(row_domain(N), T),
    maplist(fd_all_different, T),   % this ensure that each row of T has a unique values        
    transpose(T, Transpose),  maplist(fd_all_different, Transpose), % this ensure that each col of T has a unique values 
    maplist(arithm_constraints(T), C),   
    maplist(fd_labeling, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% End of the kenken predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% plain_kenken predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
Memory               limit         in use            free

   trail  stack      16383 Kb            0 Kb        16383 Kb
   cstr   stack      16384 Kb            0 Kb        16384 Kb
   global stack      32767 Kb            4 Kb        32763 Kb
   local  stack      16383 Kb            0 Kb        16383 Kb
   atom   table      32768 atoms      1789 atoms     30979 atoms

Times              since start      since last

   user   time       1.338 sec       1.329 sec
   system time       0.021 sec       0.014 sec
   cpu    time       1.359 sec       1.343 sec
   real   time    4765.229 sec    4492.684 sec

the out of the result using the plain_kenken with 4 by 4 kenken puzzle is 1329 ms, which is way slower than the kenken
*/

set_domain_plain(N,L) :-
    findall(Num, between(1,N,Num), L).

plain_different([]).
plain_different([Hd|Tl]) :-
    \+(member(Hd, Tl)),
    plain_different(Tl).

arithmetic_constraints_plain(T, C) :- constraint_plain(T, C).
constraint_plain(T, +(S, L)) :- add_plain(T, S, L, 0).
constraint_plain(T, *(P, L)) :- mult_plain(T, P, L, 1).
constraint_plain(T, -(D, J, K)) :- subtract_plain(T, D, J, K).
constraint_plain(T, /(Q, J, K)) :- division_plain(T, Q, J, K). 

add_plain(_, S, [], S).
add_plain(T, S, [Hd|Tl], Acc) :-
    matrix_element(Hd, T, Val),
    Rec_acc is Acc + Val,
    add_plain(T, S, Tl, Rec_acc).

mult_plain(_, P, [], P).
mult_plain(T, P, [Hd|Tl], Acc) :-
    matrix_element(Hd, T, Val),
    Rec_acc is Acc * Val,
    mult_plain(T, P, Tl, Rec_acc).

subtract_plain(_, D, _, _, D). 
% J - K rule
subtract_plain(T, D, J, K) :-
    matrix_element(J, T, Val_1),
    matrix_element(K, T, Val_2),
    Rec_acc is Val_1 - Val_2,
    subtract_plain(T, D, J, K, Rec_acc).
% K - J rule
subtract_plain(T, D, J, K) :-
    matrix_element(J, T, Val_1),
    matrix_element(K, T, Val_2),
    Rec_acc is Val_2 - Val_1,
    subtract_plain(T, D, J, K, Rec_acc).
    
division_plain(_, Q, _, _, Q). %Accumulator base fact
% J - K rule
division_plain(T, Q, J, K) :-
    matrix_element(J, T, Val_1),
    matrix_element(K, T, Val_2),
    Val_1 rem Val_2 =:= 0,
    Rec_acc is Val_1 // Val_2,
    division_plain(T, Q, J, K, Rec_acc).
% K - J rule
division_plain(T, Q, J, K) :-
    matrix_element(J, T, Val_1),
    matrix_element(K, T, Val_2),
    Val_2 rem Val_1 =:= 0,
    Rec_acc is Val_2 // Val_1,
    division_plain(T, Q, J, K, Rec_acc).

% This plain_kenken has a very similar fashion in compare with the kenken 
% except now, we need to define a different predicate for domain, instead of using 
% fd_all_different to check the row in T we now need to use permutation 
% http://kti.ms.mff.cuni.cz/~bartak/prolog/combinatorics.html

plain_kenken(N,C,T) :-
    length(T, N), maplist(col_len(N), T),
    set_domain_plain(N, L), maplist(permutation(L), T),
    transpose(T, Transpose), maplist(plain_different, Transpose),
    maplist(arithmetic_constraints_plain(T), C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% End of the plain_kenken predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% noop_kenken predicate %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
noop_kenken/4 
(N, C , Oplist,T)
the logic is very similar to the kenken/3, except now we have a operation list that have all the solutions
along with the T and cell that do not have the operator will still include in it. 
http://math2.uncc.edu/~hbreiter/DavidsonInstitute/D2Day3.pdf

the idea here is that our noop_kenken predicate will do the similar things as the kenken 
it will make sure we have N by N and then each row and col contains the unique element.
each constraint will try every possible result. since it disjoions from the rest of the 
matrix_element, we off to the race.


let do a noop_kenken example call here:

noop_kenken(3,
     [(5, [1|1, 1|2, 2|2]),
      (1, [3|3, 2|1]),
      (8, [1|2, 1|3, 2|3]),
      (1, [1|1, 2|2, 3|3])],
      O,
     T).
  ____________________
  
 O = [+,-,/,*]
 T = [[1,3,2],
      [2,1,3],
      [3,2,1]]
 
 no

*/





