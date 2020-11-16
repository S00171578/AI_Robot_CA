% Mark Smith S00171578 query ?- deliverItem(coffee). to kick it off.
% Video link: https://drive.google.com/open?id=0B96ypynDfbV9c3RhcnRlcl9maWxl. 

:- dynamic robot/3.
:- dynamic connected/3.
:- dynamic item/2.

connected(c0, cs1, open).
connected(cs1, c101, open).
connected(c101, c103, open).
connected(c103, c105, open).
connected(c105, c107, open).
connected(c107, c109, open).
connected(c109, c111, open).
connected(c109, c113, open).
connected(c113, c115, open).
connected(c115, c117, open).
connected(c117, c118, open).
connected(c118, c119, open).
connected(c119, c121, open).
connected(c121, c123, open).
connected(c123, c125, open).
connected(c125, c127, open).
connected(c127, c129, open).
connected(c129, c131, open).
connected(c131, c132, open).
connected(c132, c133, open).
connected(c133, c0, open).
connected(c118, cc118, open).
connected(cs1, lab1, locked).
connected(c101, lab1, locked).
connected(c103, lab2, locked).
connected(c107, lab2, locked).
connected(c123, lab3, locked).
connected(c125, lab4, locked).
connected(c129, lab4, locked).
connected(lab1, lab2, locked).
connected(lab1, lab4, locked).
connected(lab2, lab3, locked).
connected(c101, r101, open).
connected(c103, r103, locked).
connected(c105, r105, locked).
connected(c107, r107, locked).
connected(c109, r109, locked).
connected(c111, r111, locked).
connected(c113, r113, locked).
connected(c115, r115, locked).
connected(c117, r117, open).
connected(c119, r119, locked).
connected(c121, r121, locked).
connected(c123, r123, locked).
connected(c125, r125, locked).
connected(c127, r127, locked).
connected(c129, r129, locked).
connected(c131, r131, locked).
connected(cc118, canteen, locked).

connected_to(Pos1, Pos2, DoorPos) :- 
    connected(Pos1, Pos2, DoorPos).

connected_to(Pos1, Pos2, DoorPos) :- 
    connected(Pos2, Pos1, DoorPos).

%items item name and location
item(key,r117).
item(coffee,canteen).
paul(r101).
%robot(location,Current Power,items its holding)
robot(c0, 150, []).

unlock_door(Pos1,Pos2):-
    robot(Pos1,_,Items),
    member(key,Items),   
    retract(connected(Pos1, Pos2, locked)),
    assert(connected(Pos1,Pos2,open)),
    retract(robot(Pos1,CE,Items)),
    Cost is CE-2,
    assert(robot(Pos1,Cost,Items)),
    format("Unlocked ~a \n",[Pos2]).
    
is_locked(Pos1,Pos2):-
    connected(Pos1,Pos2,locked).

is_locked(Pos1,Pos2):-
    connected(Pos2,Pos1,locked).

processPath([]).

processPath([Node|Tail]):-
    robot(Location,_,_),
    is_locked(Location,Node),
    unlock_door(Location,Node),
    moveRobot(Node),
    processPath(Tail).

processPath([Node|Tail]):-
    robot(Location,_,_),
    \+is_locked(Location,Node),
    moveRobot(Node),
    processPath(Tail).

preparePath([_|T]):-
    processPath(T).

getItem(Name):-
    robot(CurrentPos,_,_),
    item(Name,Location),
    format("Finding path to ~a \n",[Location]),
    uni_cost([[CurrentPos]],Location,Path,ExploredNodes),!,
    format("Explorered ~a and resolved \n",[ExploredNodes]),
    reverse(Path,RevList),
    preparePath(RevList),
    addItem(Name).

addItem(Name):-
    format("Adding ~a to inventory \n",[Name]),
    robot(CP,CE,CI),
    retract(robot(CP,CE,CI)),
    assert(robot(CP,CE,[Name|CI])),
    write_ln(robot(CP,CE,[Name|CI])).

deliverItem(Name):-
    getItem(key),
    getItem(Name),
    robot(L,_,_),
    format("Finding path to ~a \n",[r101]),
    uni_cost([[L]],r101,Path,ExploredNodes),
    format("Explorered ~a and resolved \n",[ExploredNodes]),
    reverse(Path,RevList),
    preparePath(RevList),
    removeItem(Name).

removeItem(Name):-
    format("Removing ~a from inventory and giving to paul \n",[Name]),
    retract(robot(CL,CE,CI)),
    delete(CI, Name, NewItems),
    assert(robot(CL,CE,NewItems)),
    write_ln(robot(CL,CE,NewItems)).
    
hasItem(Name):-
    robot(_,_,CurrentItems),
    member(Name,CurrentItems).

moveRobot(Node):-
    robot(CurrentPos,_,_),
    format("Moving to ~a \n",[Node]),
    robot(CurrentPos,CE,CI),
    retract(robot(CurrentPos,CE,CI)),
    Cost is CE-3,
    assert(robot(Node,Cost,CI)).    

extend([Node|Path],NewPaths) :-
    findall([NewNode,Node|Path],
            (connected_to(Node,NewNode,open),
            \+ member(NewNode,Path)), % for avoiding loops
                NewPaths).

extend([Node|Path],NewPaths) :-
    hasItem(key),
    findall([NewNode,Node|Path],
    (connected_to(Node,NewNode,_),
    \+ member(NewNode,Path)), % for avoiding loops
    NewPaths).

cost(open,1).
cost(locked,3).

path_cost([A,B],Cost) :-
    connected(A,B,DoorPos),
    cost(DoorPos,Cost).

path_cost([A,B|T],Cost) :-
    connected(A,B,DoorPos),
    cost(DoorPos,Cost1),
    path_cost([B|T],Cost2),
    Cost is Cost1+Cost2.

reverse_path_cost([A,B],Cost) :-
    connected(A,B,DoorPos),
    cost(DoorPos,Cost).
reverse_path_cost([A,B|T],Cost) :-
    connected(A,B,DoorPos),
    cost(DoorPos,Cost1),
    reverse_path_cost([B|T],Cost2),
    Cost is Cost1+Cost2.
    
    %   Uniform Cost Search                                        %
    %   call: uni_cost(+[[Start]],+Goal,-Path,-ExploredNodes).     %
uni_cost([[Goal|Path]|_],Goal,[Goal|Path],0).

uni_cost([Path|Queue],Goal,FinalPath,N) :- 
    extend(Path,NewPaths),
    append(Queue,NewPaths,Queue1), 
    sort_queue(Queue1,NewQueue),
    uni_cost(NewQueue,Goal,FinalPath,M),
    N is M+1.
    
sort_queue(L,L2) :-
    swap(L,L1), !,
    sort_queue(L1,L2).

sort_queue(L,L).
    
swap([X,Y|T],[Y,X|T]) :-
    reverse_path_cost(X,CX),
    reverse_path_cost(Y,CY),
    CX>CY.

swap([X|T],[X|V]) :-
    swap(T,V).
    