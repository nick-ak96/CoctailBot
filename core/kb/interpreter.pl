% ***********************************************************************
% Input message processings
% ***********************************************************************

:- module(interpreter, 
	[
		create_result/2
	]
).
% include modules
:- use_module(queries).
:- use_module(knowledge).

create_result(StringMessage, Result) :-
	create_command(StringMessage, CommandList),
	evaluate_predicates(CommandList, Result).

% get list of keywords from string
create_command(StringMessage, CommandList) :-
	string_lower(StringMessage, StringMessageLower),
	generate_keywords(StringMessageLower, KeywordsList),
	command_from_keywords(KeywordsList, CommandList).


generate_keywords(String, KeywordsList) :-
	split_string(String, " ", "", TokensList),
	KeywordsList = [ String | TokensList ].


command_from_keywords([], []).
command_from_keywords([H|T], CommandList) :-
	\+ keyword(H, _, _),
	command_from_keywords(T, CommandList).
command_from_keywords([H|T], CommandList) :-
	keyword(H, Predicate, Argument),
	command_from_keywords(T, TempCommandsList),
	CommandList = [ [ Predicate, Argument ] | TempCommandsList ].

% evaluating composite queries
evaluate_predicates([], noresult).
evaluate_predicates([[Predicate, Parameter]], Result) :-
	call(Predicate, Parameter, Result).
evaluate_predicates([[Predicate, Parameter]|T], Result) :-
	call(Predicate, Parameter, Result),
	evaluate_predicates(T, Result).
