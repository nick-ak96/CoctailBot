% ***********************************************************************
% Communication
% ***********************************************************************

% welcoming
hello :- hello(_,Result), write(Result).
hello(_,Result) :-
	Result = 'Hello, my name is Jack. Would you like something to drink?'.

% what can Jack do
Abilities = 'Okay, then! I can help you prepare some cocktails, you can write 
me ingredients that you already have and I can show you all cocktails that you 
can prepere with these ingredients that I know of. But you can  also write what 
type of drink you wish long, short, medium, shooter or strong, week, light, 
non-alcoholic, you can choose the season of the cocktail or we can eaven try to 
search by the cocktail mood / taste. Or you can just search for ingredients for 
your coctail by it's name.'.



% ***********************************************************************
% Coctails knowledge base
% https://www.cocktailflow.com
% https://www.thespruceeats.com/best-recipes-for-shots-and-shooters-4151053
% other sources ...
% **********************************************************************

% cocktails
% name, base ingredient, list of ingredients
% -----------------------------------------------------------------------
cocktail(mojito, rum, [mint, rum, lime, ice, soda]).
cocktail(margo, malvazija, [malvazija, mint, limon, ice, radenska, elder_syrup]).
cocktail(hugo, prosecco, [prosecco, mint, lime, ice, soda, elder_syrup]).
cocktail(spritz, white_wine, [white_wine, soda, ice]).
cocktail(spritz_veneziano, prosecco, [prosecco, aperol, soda, ice]).
cocktail(greentini, vodka, [vodka, midori, ice]).
cocktail(apocalypse_now, tequila, [tequila, irish_cream, dry_vermounth]).
cocktail(b-52, grand_mariner, [grand_mariner, baileys, irish_cream, kahula]).
cocktail(cuba_libre, bacardi, [bacardi, lime, cola, ice]).
cocktail(beton, beer, [beer, rakija]).



% cocktail types: short, long, shooter, midle, etc
% -----------------------------------------------------------------------
% mojito
cocktail_type(long, mojito).
cocktail_type(long, margo).
cocktail_type(long, hugo).
cocktail_type(long, spritz).
cocktail_type(long, spritz_veneziano).
cocktail_type(midle, greentini).
cocktail_type(short, apocalypse_now).
cocktail_type(shooter, b-52).
cocktail_type(long, cuba_libre).
cocktail_type(long, beton).



% coctail seasons: winter, spring, summer, fall, all_seasons
% -----------------------------------------------------------------------
% mojito
cocktail_season(summer, mojito).
cocktail_season(summer, margo).
cocktail_season(summer, hugo).
cocktail_season(summer, spritz).
cocktail_season(summer, spritz_veneziano).
cocktail_season(summer, greentini).
cocktail_season(winter, apocalypse_now).
cocktail_season(winter, b-52).
cocktail_season(summer, cuba_libre).
cocktail_season(all_seasson, beton).



% cocktail moods / tastes (adjective):
% -----------------------------------------------------------------------
% mojito
cocktail_adj(refreshing, mojito).
cocktail_adj(sweet_istra, margo).
cocktail_adj(sweet, hugo).
cocktail_adj(refreshing, spritz).
cocktail_adj(relaxing, spritz_veneziano).
cocktail_adj(plain, greentini).
cocktail_adj(powerful, apocalypse_now).
cocktail_adj(worming, b-52).
cocktail_adj(refreshing, cuba_libre).
cocktail_adj(beerish, beton).



% cocktail strength: nonalkoholic, weak, light, medium, strong, extremly_strong
% -----------------------------------------------------------------------
cocktail_strength(light, mojito).
cocktail_strength(weak, margo).
cocktail_strength(weak, hugo).
cocktail_strength(light, spritz).
cocktail_strength(medium, spritz_veneziano).
cocktail_strength(medium, greentini).
cocktail_strength(strong, apocalypse_now).
cocktail_strength(strong, b-52).
cocktail_strength(light, cuba_libre).
cocktail_strength(medium, beton).




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

%the known keywords manually updated list containing also other keywords like search name , to search by name ecc...
knownKeywords(["long","refreshing","short","middle","summer"]).

%find keyowrds by intersecting the known keywords with the message
findKeywords(A,L):-knownKeywords(B),
    intersect(A,B,L).

intersect([],_,[]).
intersect([X|R],Y,[X|Z]) :- member(X,Y),!,intersect(R,Y,Z).
intersect([X|R],Y,Z) :- non_member(X,Y),!,intersect(R,Y,Z).
   
non_member(X,[Y|T]) :- X \== Y, non_member(X,T).
non_member(_,[]).




% get list of keywords from string
get_keywords(StringMessage, KeywordsList) :-
	string_lower(StringMessage,StringMessageLower),
	split_string(StringMessageLower," ","",ListMessage),
	findKeywords(ListMessage,KeywordsList).



% aux keyward mapper
% keyword string, predicate that is associated with the keyword, keyword predicate
% Note: keywords should be unique.
keyword("long", get_by_type, long).
% TODO: add more keywords

% map keywords to predicates
map_keywords_to_predicates(KeywordsList, PredicatesList) :-
	% TODO: map known keywords to key-value pairs: predicate-parameter
	PredicatesList = [[get_by_type, long]].
