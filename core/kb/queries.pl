% ***********************************************************************
% Queries
% ***********************************************************************

:- module(queries, 
	[
		get_cocktail_recepie/2,
		get_by_base/2,
		get_by_ingredients/2,
		get_by_type/2,
		get_by_season/2,
		get_by_adj/2,
		get_by_strength/2,
		get_cocktail_description/2,
		get_all_cocktail_base_spirits/2,
		get_all_cocktail_types/2,
		get_all_cocktail_seasons/2,
		get_all_cocktail_adjs/2,
		get_all_cocktail_strengths/2
	]
).

:- use_module(knowledge).

% describe cocktail
get_cocktail_description(Name, Result) :-
	cocktail(Name, _, Ingredients),
	cocktail_type(Type, Name),
	findall(S, cocktail_season(S, Name), SeasonsList),
	findall(A, cocktail_adj(A, Name), AdjList),
	cocktail_strength(Strength, Name),
	cocktail_photo(Name, PhotoLink),
	string_upper(Name, NameUpper),
	string_concat("COCKTAIL: ", NameUpper, Title),
	Result = [
		Title, 
		"\n",	
		'_type:_', 
		Type, 
		"\n",	
		'_strength:_', 
		Strength, 
		"\n",	
		'_suitable during:_', 
		SeasonsList, 
		"\n",	
		'_features:_', 
		AdjList,
		"\n",	
		'_ingredients:_', 
		Ingredients,
		PhotoLink
	].


% get cocktail recepie
get_cocktail_recepie(Name, Result) :-
	cocktail_recepie(Name, Result).

% get all base spirits
get_all_cocktail_base_spirits(_, Result) :-
	cocktail(_, Result, _).


% get all coctails that are based on 'BaseSpirit'
get_by_base(BaseSpirit, Result) :-
	cocktail(Result, BaseSpirit, _).


% get all coctails that can be made using the list of ingredients 'IngredientsList'
get_by_ingredients(IngredientsList, Result) :-
	IngredientsList \== [],
	cocktail(Result, _, CocktailIngredients),
	subset(CocktailIngredients, IngredientsList).


% get all cocktail types
get_all_cocktail_types(_, Result) :-
	cocktail_type(Result, _).


% get all coctails that are of type 'Type'
get_by_type(Type, Result) :-
	cocktail(Result, _, _),
	cocktail_type(Type, Result).


% get all cocktail seasons
get_all_cocktail_seasons(_, Result) :-
	cocktail_season(Result, _).


% get all coctails that relevant for the season 'Season'
get_by_season(Season, Result) :-
	cocktail(Result, _, _),
	cocktail_season(Season, Result).


% get all cocktail mood / tastes
get_all_cocktail_adjs(_, Result) :-
	cocktail_adj(Result, _).


% get all coctails that have mood / taste 'Adj'
get_by_adj(Adj, Result) :- 
	cocktail(Result, _, _),
	cocktail_adj(Adj, Result).


% get all cocktail strengths
get_all_cocktail_strengths(_, Result) :-
	cocktail_strength(Result, _).


% get all coctails that have strength 'Strength'
get_by_strength(Strength, Result) :-
	cocktail(Result, _, _),
	cocktail_strength(Strength, Result).
