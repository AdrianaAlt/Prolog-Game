:- dynamic(whereami/1).
:- dynamic(inventory/1).
:- dynamic(position_item/3).

:-assert(position_item(_,_,_)).
:-retractall(position_item(_,_,_)).

% ------------- Initial statements ----------------
whereami(position(0, 0)).
inventory([]).


% 1. Environment 
environment(forest).
environment(sea).
environment(mountains).
environment(rocks).
environment(meadow).
environment(dune).

% 2. Directions
direction(north, n, position(0, 1)).
direction(east, e, position(1, 0)).
direction(south,s, position(0, -1)).
direction(west, w, position(-1, 0)).

% 3. Map
map_cell(position(0, 0), meadow). % hatchet
map_cell(position(0, -1), forest). % tree and apple
map_cell(position(1, 0), forest).
map_cell(position(0, 1), forest).
map_cell(position(-1, 0), forest).
map_cell(position(-1, 1), forest).
map_cell(position(1, 1), forest).
map_cell(position(-1, -1), forest).
map_cell(position(1, -1), forest).

map_cell(position(-2, 0), forest).
map_cell(position(-2, 1), rocks).
map_cell(position(-2, 2), forest).
map_cell(position(-1, 2), meadow).
map_cell(position(0, 2), meadow).
map_cell(position(1, 2), forest).
map_cell(position(2, 2), mountains).
map_cell(position(2, 1), mountains).
map_cell(position(2, -2), forest).
map_cell(position(1, -2), forest).
map_cell(position(0, -2), forest).
map_cell(position(-1, -2), forest).
map_cell(position(-2, -2), sea).

map_cell(position(-3, 0), forest).
map_cell(position(-3, -1), rocks).
map_cell(position(-3, -2), sea).
map_cell(position(-3, -3), rocks).
map_cell(position(-2, -3), rocks).
map_cell(position(-1, -3), meadow).
map_cell(position(0, -3), forest).
map_cell(position(1, -3), forest).
map_cell(position(2, -3), forest).  % ladder
map_cell(position(3, -3), mountains).
map_cell(position(3, -2), mountains). % key
map_cell(position(3, -1), mountains).
map_cell(position(3, 0), mountains).
map_cell(position(3, 1), mountains).
map_cell(position(3, 2), mountains).
map_cell(position(3, 3), mountains).
map_cell(position(2, 3), mountains).
map_cell(position(1, 3), forest).
map_cell(position(0, 3), meadow).
map_cell(position(-1, 3), forest). % shovel
map_cell(position(-2, 3), forest).
map_cell(position(-3, 3), rocks). 
map_cell(position(-3, 2), forest).
map_cell(position(-3, 2), sea).


map_cell(position(-4, 4), rocks). % compass
map_cell(position(-3, 4), rocks).
map_cell(position(-2, 4), meadow).
map_cell(position(-1, 4), meadow).
map_cell(position(0, 4), meadow).
map_cell(position(1, 4), dune).
map_cell(position(2, 4), mountains).
map_cell(position(3, 4), forest).
map_cell(position(4, 4), forest).
map_cell(position(4, 3), rocks).
map_cell(position(4, 2), forest).
map_cell(position(4, 1), forest).
map_cell(position(4, 0), meadow).
map_cell(position(4, -1), meadow).
map_cell(position(4, -2), meadow).
map_cell(position(4, -3), mountains).
map_cell(position(4, -4), mountains).
map_cell(position(3, -4), mountains).
map_cell(position(2, -4), forest).
map_cell(position(1, -4), meadow).
map_cell(position(0, -4), forest).
map_cell(position(-1, -4), forest).
map_cell(position(-2, -4), rocks).
map_cell(position(-3, -4), sea).
map_cell(position(-4, -4), sea).
map_cell(position(-4, -3), rocks).
map_cell(position(-4, -2), sea).
map_cell(position(-4, -1), sea).
map_cell(position(-4, 0), sea).
map_cell(position(-4, 1), sea).
map_cell(position(-4, 2), sea).
map_cell(position(-4, 3), sea).

map_cell(position(-4, 5), sea).
map_cell(position(-3, 5), rock).
map_cell(position(-2, 5), rock).
map_cell(position(-1, 5), sea).
map_cell(position(0, 5), dune).
map_cell(position(1, 5), dune).
map_cell(position(2, 5), dune). % chest
map_cell(position(3, 5), dune).
map_cell(position(4, 5), forest).

% 4. Items
item(tree, 'a tall TREE').
item(hatchet, 'a hewing HATCHET').
item(apple, 'a red APPLE').
item(ladder, 'a wooden LADDER').
item(key, 'a golden KEY').
item(shovel,'a steel SHOVEL').
item(compass, 'an old mariner"s COMPASS').
item(chest, 'a treasure CHEST').
item(treasures, 'a lot of TREASURES').
item(pit, 'a digged PIT').

% 5. Items placed on map
position_item(position(0, 0), hatchet, visible).
position_item(position(0, -1), apple, invisible).
position_item(position(0, -1), tree, visible).
position_item(position(2, -3), ladder, visible).
position_item(position(2, 5), chest, invisible).
position_item(position(3, -2), key, visible).
position_item(position(-4, 4), compass, visible).
position_item(position(-1, 3), shovel, visible).


capable([apple, ladder, key, compass, shovel, hatchet]).
locked_items(position(2, 5), [chest]).

interaction(chest, key).
interaction(tree, hatchet).


% ------------- Inventory Manager ---------------------
show_list([Head|Tail]) :- write(Head), nl, show_list(Tail).
show_inventory :- inventory(X), show_list(X).
add_inventory(Y) :- inventory(X), append(X, [Y], Z), retractall(inventory(_)), assert(inventory(Z)).
remove_inventory(Y) :- inventory(X), delete(X, Y, Z), retractall(inventory(_)), assert(inventory(Z)).
has(X):- inventory(List), not(member(X, List)), write('There is no '), write(X), write(' in your inventory.'),nl, fail.
has(X):- inventory(List), member(X, List).


% ------------- Move ---------------------
current_disposition(Pos):-
    whereami(Pos),
    map_cell(Pos, Loc),
    write('You are in the '), write(Loc), nl.

not_movable_directions:-
    whereami(position(X, Y)),
    direction(Dir_name, _, _), 
    desired_move_position(X, Y, Dir_name, _, NewPos),
    not(map_cell(NewPos, _)),
    write('You can not go '), write(Dir_name), nl, fail.
not_movable_directions.

where_to_go:-
    whereami(position(X, Y)),
    direction(Dir_name, _, _),
    desired_move_position(X, Y, Dir_name, _, NewPos),
    map_cell(NewPos, _),
    write('You can go '),
    write(Dir_name), nl, fail.
where_to_go.
    
desired_move_position(X, Y, Dir_name, Dir_code, position(NewX, NewY)):- 
    direction(Dir_name, Dir_code, position(DtX, DtY)),
    NewY is Y + DtY, NewX is X + DtX.

can_go_to(Pos):- map_cell(Pos, _).

goto(Dir_code):-
    whereami(position(X, Y)),
    desired_move_position(X, Y, Dir_name, Dir_code, Pos),
    can_go_to(Pos), 
    move(Pos),
    write('You go to '), write(Dir_name), nl,
    look_around.

move(Pos):- retract(whereami(_)), asserta(whereami(Pos)).

% ----------------- Look around -----------------
is_there_any_item(Pos):- position_item(Pos, _, visible).

look_for_objects_around(Pos):-
    ( is_there_any_item(Pos) -> write('You can see: '); write('There are no items'),fail),
    position_item(Pos, Item, visible),
    item(Item, Item_Info),
    write(Item_Info), write(', '), fail.
look_for_objects_around(_).

look_around:-
    current_disposition(Pos),
    not_movable_directions,
    look_for_objects_around(Pos),
    fail.


% -------------- Take / Put --------------------------
put(Item):-
    inventory(List),
    member(Item, List),
    whereami(Pos),
    remove_inventory(Item),
    assert(position_item(Pos, Item, visible)),
    write('You put '), write(Item), nl,
    fail.

can_take(Item):- 
    whereami(Pos),
    ( not(position_item(Pos, Item, visible)) -> write('There is no '), write(Item), write(' here.'),nl,fail; true),
    capable(List),
    (not(member(Item, List)) -> write('You can not put '),write(Item),write(' into your inventory.'), fail;write('You can take '), write(Item), write('.'), nl, true).


take_object(Item):-
    whereami(Pos),
    position_item(Pos,Item, visible),
    add_inventory(Item), 
    retractall(position_item(Pos,Item, visible)),
    write(Item), write(' was taken'), nl.

take(Item):- can_take(Item), take_object(Item).
take:-  
    whereami(Pos), 
    ( is_there_any_item(Pos) -> true; write('There are no visible items.'),nl,fail),
    grab_all_items,
    fail.

grab_all_items:- 
    whereami(Pos),
    position_item(Pos,Item,visible),
    can_take(Item),
    add_inventory(Item), retractall(position_item(Pos,Item,visible)); 
    nl,write('You took all capable items.'),nl.


% -------------- Pit digging  -----------------------

dig:-
    whereami(Pos),
    (not(has(shovel)) -> write('You can not dig a hole. Let''s try to find a shovel and come back to it later.'), fail; true), 
    (not(map_cell(Pos, dune)) -> write('You can not dig a pit here.'), fail; true),
    (not(position_item(Pos, pit, _)) -> true; write('You already digged a hole here'), fail),
    write('You dig a hole.'), nl,
    assert(position_item(Pos, pit, visible)),
    dig_up_item(Pos),
    fail.

dig_up_item(Pos):-
    ( position_item(Pos, Item, invisible) -> write('At the bottom of the pit you found: '), item(Item, Item_Info), write(Item_Info), nl; write('Nothing was found at the bottom of the hole'),fail),
    retract(position_item(Pos, Item, invisible)),
    assert(position_item(Pos, Item, visible)).


% -------------- Additional Actions -----------------
chest_was_opened:- 
    write('Your key broke.'), nl, write('Treasure chest was opened!'),nl,
    add_inventory(treasures),
    item(treasures, Item_Info),
    write('You got '), write(Item_Info), nl.

open(X):-
    whereami(Pos), 
    has(key),
    position_item(Pos, X, visible),
    (not(interaction(X, key)) -> write(Item), write(' is not lockable!'),nl, fail; true),
    locked_items(Pos, Locked_List),
    (not(member(X, Locked_List)) -> write(Item), write(' was already opened'), nl, fail; true),
    remove_inventory(key),
    chest_was_opened,
    fail.

apple_falling(Pos):-
    write('An apple fall from a tree'),
    retract(position_item(Pos, apple, invisible)),
    assert(position_item(Pos, apple, visible)).

cut_down(X):-
    whereami(Pos), 
    has(hatchet),
    position_item(Pos, X, visible),
    (not(interaction(X, hatchet)) -> write('You can not cut '), write(X), nl, fail; write('You cut '), write(X),nl,true),
    retract(position_item(Pos, X, visible)),
    X == tree,
    position_item(Pos, apple, invisible),
    apple_falling(Pos),
    fail.


% ------------------- Menu -------------------------
title :- write('Puszka Pandory').
game_over :- write('Game Over'), nl.

regule(1). regule(2). regule(3). regule(4). regule(5). regule(6). regule(7). regule(8). regule(9). regule(10). regule(11).

regule_text(1, 'To show game name: title.').
regule_text(2, 'To show current position: whereami(X)').
regule_text(3, 'To show places to go: where_to_go()').
regule_text(4, 'To look around: look_around()').
regule_text(5, 'To go into place: goto(Direction). (Direction = {n, s, w, e})').
regule_text(6, 'To show the inventory: show_inventory().').
regule_text(7, 'To open an object by key: open(Object).').
regule_text(8, 'To put item into your inventory: grab_items() or take(Item)').
regule_text(9, 'To put item from your inventory: put(Item)').
regule_text(10, 'To dig a pit: dig(Item)').
regule_text(11, 'To cut down an object: cut_down(Object)').

iterate_regules(X,Y) :- regule(X), !, regule(Y).

start:- title,nl, iterate_regules(X, Y), regule_text(Y, Z), write(X-Y), write('. '), write(Z), nl, fail.

quit :-game_over, title, halt.