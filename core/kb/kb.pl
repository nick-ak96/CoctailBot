% include all modules
:- use_module(knowledge).
:- use_module(queries).
:- use_module(interpreter).


% create answer from list
process_answer([], Result) :-
	unknown_response(_, Result).
process_answer([noresult], Result) :-
	unknown_response(_, Result).
process_answer([H|T], Result) :-
	H \== noresult,
	list_to_string([H|T], Result).


% aux function to convert ist of items to a string.
list_to_string([], "").
list_to_string([H|T], Result) :-
	atom_string(H, Item),
	string_concat(Item, "\n", ItemWithNewLine),
	list_to_string(T, TempResult),
	string_concat(ItemWithNewLine, TempResult, Result).



ask(StringMessage) :-
	ask(StringMessage, Result),
	write(Result).
ask(StringMessage, Result) :-
	findall(X, create_result(StringMessage, X), ResultList),
	process_answer(ResultList, Result).
