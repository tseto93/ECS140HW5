/*******************************************/
/**    Your solution goes in this file    **/ 
/*******************************************/

/*Part 1*/

%a %done
fc_course(A) :-
    course(A, _, Units),
    member(Units, [3, 4]).

%b %done
prereq_110(A) :-
    course(A, Prereq, _),
    member(ecs110, Prereq).
    
%c %done
ecs140a_students(A) :-
    student(A, Classes),
    member(ecs140a, Classes).
    
%d 
instructor_names(I) :-
    instructor(I, _),
    teaches_john(I).

teaches_john(I) :-
    student(john, StudentClasses),
    instructor(I, InstructorClasses),
    member(InstructorClass, InstructorClasses),
    member(InstructorClass, StudentClasses), 
    !.
    
%e 
students(S) :-
    student(S, _),
    jims_student(S).

jims_student(S) :-
    instructor(jim, InstructorClasses),
    student(S, StudentClasses),
    member(StudentClass, StudentClasses),
    member(StudentClass, InstructorClasses), 
    !.

%f

allprereq([], []) :- !.
allprereq([H|T], X) :-
    course(H, Y, _),
    allprereq(Y, Z),
    append(Z, Y, HPrereq),
    allprereq(T, TPrereq),
    append(TPrereq, HPrereq, X), 
    !.
allprereq(C, X) :-
    allprereq([C], X).
    
    
/*Part 2*/

/*all_length*/ %done
all_length([], 0).
all_length([H|T], L) :-
    atom(H),
    all_length(T, X),
    L is X + 1,
    !.
all_length([H|T], L) :-
    all_length(H, X),
    all_length(T, Y),
    L is X + Y,
    !.
    
/*equal_a_b*/ 

equal_a_b(L) :-
    equal_helper(L, 0, 0).
equal_helper([], Anum, Bnum) :-
    Anum = Bnum.
equal_helper([X|T], Anum, Bnum) :-
    X = a -> (A is Anum + 1, equal_helper(T, A, Bnum)).
equal_helper([X|T], Anum, Bnum) :-
    X = b -> (B is Bnum + 1, equal_helper(T, Anum, B)).
equal_helper([X|T], Anum, Bnum) :-
    X \== a, X \== b -> (equal_helper(T, Anum, Bnum)).
        
/*swap_prefix_suffix*/ %done
swap_prefix_suffix(K, L, S) :-
    append3(Prefix, K, Suffix, L),
    append3(Suffix, K, Prefix, S).

append3(L1, L2, L3, L) :-
    append(L1, LL, L),
    append(L2, L3, LL).
    
/*palin*/ %change
palin(A) :-
    my_reverse(A, A).
my_reverse([], []).
my_reverse([H|T], R) :-
    my_reverse(T, TR),
    append(TR, [H], R).

/*good*/ %done
good([0]).
good([1|T]) :-
    append(X, Y, T),
    good(X),
    good(Y).

/*Part 3*/

%Puzzle

/*opposite(left, right).
opposite(right, left).

solve :-
    state(_, _, _, _).

unsafe(state(A, B, B, _)) :- opposite(A, B).
unsafe(state(A, _, C, C)) :- opposite(A, C).

safe(A) :- \+ unsafe(A).

path(A, A, Stack):-
    nl, write('solution:'), nl,
    print_stack(Stack).

path(A, C, Current):-
    move(A, B),
    \+ member(B, Current),
    stack(B, Current, Next),
    path(B, C, Next).

move(A, B):-
    A == B -> fail; %no possible safe movements from last move (so it didnt do anything)
    arc(A, B),
    safe(B).

%possible arcs you can take
arc(state(A, A, G, C), state(B, B, G, C)):- opposite(A, B). %move wolf
arc(state(A, W, A, C), state(B, W, B, C)):- opposite(A, B). %move goat
arc(state(A, W, G, A), state(B, W, G, B)):- opposite(A, B). %move cabbage
arc(state(A, W, G, C), state(B, W, G, C)):- opposite(A, B). %move farmer

%util methods
stack(H, T, [H|T]).

print_stack([]).
print_stack(S):-
    stack(H, T, S),
    print_stack(T),
    write(H), nl.*/
    
% solution is: let X either be wolf or cabbage, and Y be the other one
% move sheep, move farmer, move X, move sheep, move Y, move farmer, move sheep

opposite(left, right).
opposite(right, left).

unsafe(state(A, B, B, _)) :- opposite(A, B).
unsafe(state(A, _, C, C)) :- opposite(A, C).

safe(A) :- \+ unsafe(A).

solve:-
    write('KEY: state(farmer, wolf, goat, cabbage)'), nl,
    go(state(left, left, left, left), state(right, right, right, right)).

go(A, C):-
    stack(A, [], Stack),
    path(A, C, Stack).

%success!
path(A, A, Stack):-
    nl, write('solution:'), nl,
    printSol(Sol).

path(A, C, Current):-
    move(A, B),
    \+ member(B, Current),
    stack(B, Current, Next),
    path(B, C, Next).

move(A, B):-
    A == B -> fail; %no possible safe movements from last move (so it didnt do anything)
    arc(A, B),
    safe(B).

%possible arcs you can take
arc(state(A, A, G, C), state(B, B, G, C)):- opposite(A, B). %move wolf
arc(state(A, W, A, C), state(B, W, B, C)):- opposite(A, B). %move goat
arc(state(A, W, G, A), state(B, W, G, B)):- opposite(A, B). %move cabbage
arc(state(A, W, G, C), state(B, W, G, C)):- opposite(A, B). %move farmer

%util methods
stack(H, T, [H|T]).

printSol([H,HT|T]) :-
    printE(H,HT),
    printSol([HT|T]).

/*printE(state(A,A,E,D),state(B,B,E,D)):-
	print('take'),
	print('('),print('wolf,'),
	print(A),print(','),
	print(B),print(')'),nl.

printE(state(A,C,A,D),state(B,C,B,D)):-
	print('take'),
	print('('),print('goat,'),
	print(A),print(','),
	print(B),print(')'),nl.

printE(state(A,C,E,A),state(B,C,E,B)):-
	print('take'),
	print('('),print('cabbage,'),
	print(A),print(','),
	print(B),print(')'),nl.

printE(state(A,C,E,D),state(B,C,E,D)):-
	print('take'),
	print('('),print('none,'),
	print(A),print(','),
	print(B),print(')'),nl.*/

    




    
    
    
