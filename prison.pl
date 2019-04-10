/*

    Module 531: Laboratory (Prolog)
    Exercise No.4  (prison)

*/


% May be helpful for testing

% generate_integer(+Min, +Max, -I)
%   I is an integer in the range Min <= I <= Max

generate_integer(Min,Max,Min):-
  Min =< Max.
generate_integer(Min,Max,I) :-
  Min < Max,
  NewMin is Min + 1,
  generate_integer(NewMin,Max,I).


% Uncomment this line to use the provided database for Problem 2.
% You MUST recomment or remove it from your submitted solution.
%:- include(prisonDb).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%     Problem 1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prison_game(+Cells, +Warders, -Escaped)
%   Escaped is a list of cell numbers for prisoners who will escape
%   once all warders have completed their runs.

prison_game(Cells, Warders, Escaped) :-
  integer(Cells), Cells > 0,
  integer(Warders), Warders > 0,
  make_list(Cells, unlocked, Initial),
  run_warders(2, Warders, Initial, Final),
  extract_indices(Final, unlocked, Escaped).


% Write your program here.

% Task 1.1 make_list(+N, +Item, -List).
%   Given a (non-negative) integer N and item Item constructs list List of N
%   elements each of which is Item

make_list(N, Item, List)  :-
    construct_list(N, Item, [], List).

% as long as N is greater than 0 we add Item to the Acc list
construct_list(N, Item, Acc, List)  :-
    N > 0,!,
    NewN is N - 1,
    construct_list(NewN, Item, [Item|Acc], List).

% Unify Acc and List
construct_list(_, _, Acc, Acc).


% Task 1.2 extract_indices(+List, +Item, -Indices).
%   Given list List and item Item computes the list Indices of integers N such
%   that Item is the Nth item of the list List

extract_indices(List, Item, Indices) :-
    construct_indices_list(List, Item, 1, [], Indices).

% if the head of the List of items (=X) == Item then we add the current index to the list of indices,
% increment the current index and call construct_indices_list again with the Tail of the list of items
construct_indices_list([X|Tail], Item, Index, Acc, Indices) :-
    X == Item,!,
    NewIndex is Index + 1,
    construct_indices_list(Tail, Item, NewIndex, [Index|Acc], Indices).

% if X != Item then we simply skip the current item, increment the current index
% and call construct_indices_list again with the Tail of the list of items
construct_indices_list([_|Tail], Item, Index, Acc, Indices) :-
    NewIndex is Index + 1,
    construct_indices_list(Tail, Item, NewIndex, Acc, Indices).

% if there are no more items left in the list of items then we reverse the order of items in the Acc list
% (as the order has been reversed by appending to the Head of the list) and unify Acc and Indices
construct_indices_list([], _, _, Acc, Indices) :-
    reverse(Acc, Indices).


% Task 1.3 run_warders(+N, +W, +Initial, -Final).
%   Given next warder N and total warders W (both positive integers), and
%   current door states Initial (a list of the constants locked and unlocked)
%   returns Final, the list of door states after all warders have completed
%   their runs.

% if N > W then we do not make any more warders run
% Run warder N starting from the first index in the current list of door states and reverse the order of the
% current door states as appending to the head of a list reverses the list's order. Then call run_warders again
% with the next warder and the current list of door states
run_warders(N, W, Initial, Final) :-
  N =< W,!,
  run_warder(N, 1, Initial, [], CurrentDoorStates),
  reverse(CurrentDoorStates, NewCurrentDoorStates),
  NewN is N + 1,
  run_warders(NewN, W, NewCurrentDoorStates, Final).

% unfiy CurrentDoorStates and Final
run_warders(_, _, CurrentDoorStates, CurrentDoorStates).

% run the current warder N and check if the current index is divisible by N (remainder = 0) and then flip_locked;
% Increment the current index and call run_warder again with the new index and the tail of the list of current
% door states. The Accumulator adds the flipped lock value.
run_warder(N, Index, [X|Tail], Acc, MidFinal) :-
  0 is mod(Index, N),!,
  flip_locked(X, ReturnedVal),
  NewIndex is Index + 1,
  run_warder(N, NewIndex, Tail, [ReturnedVal|Acc], MidFinal).

% if the remainder is not 0 then just keep the current door state and continue with the next item in the list of
% current door states
run_warder(N, Index, [X|Tail], Acc, MidFinal) :-
  NewIndex is Index + 1,
  run_warder(N, NewIndex, Tail, [X|Acc], MidFinal).

% unify Acc and MidFinal when there are no more items to check
run_warder(_, _, [], Acc, Acc).

% flip the door state
flip_locked(unlocked, locked).
flip_locked(locked, unlocked).

% reverses the sequence as adding elements to the head of the sequence keeps the last element at the front
reverse(Sequence, NewSequence) :-
  reverse_sequence(Sequence, [], NewSequence).

reverse_sequence([], NewSequence, NewSequence).

reverse_sequence([X|Tail], Acc, NewSequence) :-
  reverse_sequence(Tail, [X|Acc], NewSequence).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%     Problem 2
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Write your program here.

% Task 2.1 cell_status(+Cell, +N, ?Status)
%   Succeeds if the status of cell Cell is Status after N warders have made
%   their runs. If Status is a variable, then the correct value should be
%   returned.


% if no warder is going then the cell will stay locked
cell_status(_, 0, locked).

% if cell <= N then add 1 extra flip as we are otherwise missing the flip where CurrentWarder = Cell
cell_status(Cell, N, Status) :-
  (Cell =< N -> change_cell_status(Cell, 1, N, unlocked, Status);
  change_cell_status(Cell, 1, N, locked, Status)).

% stop when CurrentWard > Cell/2 - no more warders will unlock/lock the cell door unless N >= Cell
% then there is one additional flip (which has already been added)
change_cell_status(Cell, CurrentWarder, N, CurrentStatus, FinalStatus) :-
  CurrentWarder > Cell/2, !,
  CurrentStatus = FinalStatus.

% stop when all warders have run
change_cell_status(Cell, CurrentWarder, N, CurrentStatus, FinalStatus) :-
  CurrentWarder > N, !,
  CurrentStatus = FinalStatus.

% if remainder is 0 then this door key will be turned
change_cell_status(Cell, CurrentWarder, N, CurrentStatus, FinalStatus) :-
  0 is mod(Cell, CurrentWarder), !,
  NewCurrentWarder is CurrentWarder + 1,
  flip_locked_if_not_psychopath(Cell, CurrentStatus, NewStatus),
  change_cell_status(Cell, NewCurrentWarder, N, NewStatus, FinalStatus).

% if remainder is not 0 skip the current warder and do not turn the current cell key and recall change_cell_status
change_cell_status(Cell, CurrentWarder, N, CurrentStatus, FinalStatus) :-
  NewCurrentWarder is CurrentWarder + 1,
  change_cell_status(Cell, NewCurrentWarder, N, CurrentStatus, FinalStatus).

% only turn key if prisoner is not a psychopath
flip_locked_if_not_psychopath(Cell, unlocked, locked) :-
  \+ (prisoner(Surname, Firstname, Cell, _, _, _), psychopath(Surname, Firstname)),!.

flip_locked_if_not_psychopath(_, unlocked, unlocked).

% only turn key if prisoner is not a psychopath
flip_locked_if_not_psychopath(Cell, locked, unlocked) :-
  \+ (prisoner(Surname, Firstname, Cell, _, _, _), psychopath(Surname, Firstname)),!.

flip_locked_if_not_psychopath(_, locked, locked).


% Task 2.2

% escaped(?Surname, ?FirstName)
%   holds when the prisoner with that name escapes (i.e., occupies a cell which
%   is unlocked after the last warder has made his run, but bearing in mind that
%   prisoners with a year or less left to serve will not escape).
escaped(Surname, FirstName) :-
  prisoner(Surname, FirstName, Cell, _, _, ToServe),
  warders(NoOfWarders),
  cell_status(Cell, NoOfWarders, unlocked),
  ToServe > 1.

% escapers(-List)
%   List is the list of escaped prisoners. List is a list of terms of the form
%   (Surname, FirstName), sorted in ascending alphabetical order according to
%   Surname.
escapers(List) :-
  setof((Surname, FirstName), escaped(Surname, FirstName), List).
