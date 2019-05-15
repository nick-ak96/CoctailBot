% ***********************************************************************
% Communication
% ***********************************************************************

% welcoming
hello :- hello(_,Result), write(Result).
hello(_,Result) :-
	Result = 'Hello, my name is Jack. Would you like something?'.

% what can Jack do
abilities. % TODO





% ***********************************************************************
% Coctails knowledge base
% https://www.cocktailflow.com
% other sources ...
% ***********************************************************************

% cocktails
% name, base ingredient, list of ingredients
% -----------------------------------------------------------------------
cocktail(mojito, rum, [mint, rum, lime, ice, soda]).



% cocktail types: short, long, shooter, etc
% -----------------------------------------------------------------------
% mojito
cocktail_type(long, mojito).
% cuba libre
% ...



% coctail seasons: winter, spring, summer, fall, all_seasons
% -----------------------------------------------------------------------
% mojito
cocktail_season(summer, mojito).



% cocktail moods / tastes (adjective):
% -----------------------------------------------------------------------
% mojito
cocktail_adj(refreshing, mojito).



% cocktail strength: nonalkoholic, weak, light, medium, strong, extremly_strong
% -----------------------------------------------------------------------
cocktail_strength(light, mojito).





% ***********************************************************************
% Queries
% ***********************************************************************

% get all coctails that are based on 'BaseSpirit'
get_by_base(BaseSpirit, Result) :-
	cocktail(Result, BaseSpirit, _).

% get all coctails that can be made using the list of ingredients 'IngredientsList'
get_by_ingredients(IngredientsList, Result). % TODO


% get all coctails that are of type 'Type'
get_by_type(Type, Result) :-
	cocktail(Result, _, _),
	cocktail_type(Type, Result).


% get all coctails that relevant for the season 'Season'
get_by_season(Season, Result) :-
	cocktail(Result, _, _),
	cocktail_season(Season, Result).


% get all coctails that have mood / taste 'Adj'
get_by_adj(Adj, Result) :- 
	cocktail(Result, _, _),
	cocktail_adj(Adj, Result).


% get all coctails that have strength 'Strength'
get_by_strength(Strength, Result) :-
	cocktail(Result, _, _),
	cocktail_strength(Strength, Result).


% get cocktails from user input 'StringMessage'
get_cocktails(StringMessage, Result) :-
	get_keywords(StringMessage, KeywordsList),
	map_keywords_to_predicates(KeywordsList, PredicatesList),
	evaluate_predicates(PredicatesList, Result).
% evaluating composite queries
evaluate_predicates([], _).
evaluate_predicates([[Predicate, Parameter]|T], Result) :-
	call(Predicate, Parameter, Result),
	evaluate_predicates(T, Result).





% ***********************************************************************
% Input message processings
% ***********************************************************************

% get list of keywords from string
get_keywords(StringMessage, KeywordsList) :-
	% TODO: find keywords in a message, simply store known words
	% that can be used in a query as a parameter.
	KeywordsList = ["long"].

% aux keyward mapper
% keyword string, predicate that is associated with the keyword, keyword predicate
% Note: keywords should be unique.
keyword("long", get_by_type, long).
% TODO: add more keywords

% map keywords to predicates
map_keywords_to_predicates(KeywordsList, PredicatesList) :-
	% TODO: map known keywords to key-value pairs: predicate-parameter
	PredicatesList = [[get_by_type, long]].
