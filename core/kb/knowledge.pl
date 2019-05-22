% ***********************************************************************
% Coctails knowledge base
% https://www.cocktailflow.com
% https://www.thespruceeats.com/best-recipes-for-shots-and-shooters-4151053
% other sources ...
% **********************************************************************

:- module(knowledge, 
	[
		cocktail/3,
		keyword/3, from_string_to_atom_list/2,
		cocktail_recepie/2,
		cocktail_type/2,
		cocktail_season/2,
		cocktail_adj/2,
		cocktail_strength/2,
		cocktail_strength/2,
		hello/0, hello/2,
		%abilities/0, abilities/2,
		unknown_response/0, unknown_response/2
	]
).

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
cocktail(b52, grand_mariner, [grand_mariner, baileys, irish_cream, kahula]).
cocktail(cuba_libre, bacardi, [bacardi, lime, cola, ice]).
cocktail(beton, beer, [beer, rakija]).


% recepies
% -----------------------------------------------------------------------
cocktail_recepie(mojito, "TODO recepie").
cocktail_recepie(margo, "TODO recepie").
cocktail_recepie(hugo, "TODO recepie").
cocktail_recepie(sprit, "TODO recepie").
cocktail_recepie(spritz_veneziano, "TODO recepie").
cocktail_recepie(greentini, "TODO recepie").
cocktail_recepie(apocalypse_now, "TODO recepie").
cocktail_recepie(b52, "TODO recepie").
cocktail_recepie(cuba_libre, "TODO recepie").
cocktail_recepie(beton, "TODO recepie").



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
cocktail_type(shooter, b52).
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
cocktail_season(winter, b52).
cocktail_season(summer, cuba_libre).
cocktail_season(all_seasons, beton).



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
cocktail_adj(warming, b52).
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
cocktail_strength(strong, b52).
cocktail_strength(light, cuba_libre).
cocktail_strength(medium, beton).




% ***********************************************************************
% Keywords
% ***********************************************************************

% keyword(keyword string, predicate that is associated with the keyword, keyword predicate)
% Note: keywords should be unique.

keyword(String, get_cocktail_description, CocktailName) :-
	sub_string(String, Before, 8, After, "describe"),
	After > 0,
	Start is Before + 8 + 1,
	sub_string(String, Start, _, 0, Temp),
	atom_string(CocktailName, Temp).

keyword(String, get_cocktail_recepie, CocktailName) :-
	sub_string(String, Before, 10, After, "recepie of"),
	After > 0,
	Start is Before + 10 + 1,
	sub_string(String, Start, _, 0, Temp),
	atom_string(CocktailName, Temp).

keyword(String, get_by_ingredients, Ingredients) :-
	sub_string(String, Before, 27, After, "cocktails from ingredients:"),
	After > 0,
	Start is Before + 27 + 1,
	sub_string(String, Start, _, 0, Temp),
	split_string(Temp, " ", "", Temp1),
	from_string_to_atom_list(Temp1, Ingredients).

from_string_to_atom_list([], []).
from_string_to_atom_list([H|T], AtomsList) :-
	atom_string(A, H),
	from_string_to_atom_list(T, Temp),
	AtomsList = [A | Temp].


% type keywords
keyword("long", get_by_type, long).
keyword("middle", get_by_type, middle).
keyword("short", get_by_type, short).
keyword("shooter", get_by_type, shooter).

% season keywords
keyword("winter", get_by_season, winter).
keyword("spring", get_by_season, spring).
keyword("summer", get_by_season, summer).
keyword("fall", get_by_season, fall).
keyword("all-seasons", get_by_season, all_seasons).

% taste / mood keywords
keyword("refreshing", get_by_adj, refreshing).
keyword("sweet istra", get_by_adj, sweet_istra).
keyword("sweet", get_by_adj, sweet).
keyword("relaxing", get_by_adj, relaxing).
keyword("plain", get_by_adj, plain).
keyword("powerful", get_by_adj, powerful).
keyword("warming", get_by_adj, warming).
keyword("beerish", get_by_adj, beerish).

% strength keywords
keyword("nonalkoholic", get_by_strength, nonalkoholic).
keyword("weak", get_by_strength, weak).
keyword("light", get_by_strength, light).
keyword("medium", get_by_strength, medium).
keyword("strong", get_by_strength, strong).
keyword("extremly-strong", get_by_strength, extremly_strong).

% informational
keyword("spirits", get_all_cocktail_base_spirits, _).
keyword("types", get_all_cocktail_types, _).
keyword("seasons", get_all_cocktail_seasons, _).
keyword("tastes/moods", get_all_cocktail_adjs, _).
keyword("strengths", get_all_cocktail_strengths, _).


% greetings
keyword("hi", hello, _).
keyword("hello", hello, _).
keyword("yay", hello, _).

% abilities
%keyword("abilities", abilities, _).




% ***********************************************************************
% Basic communication
% ***********************************************************************

% welcoming
hello :- hello(_,Result), write(Result).
hello(_,Result) :-
	Result = "Hello, my name is Jack. I am the bartender here.\n
	I can help you discover or prepare some cocktails.\n
	Use the following keywords to find out what kind of cocktails we have in the bar: \n\tbase spirits, types, seasons, tastes/moods and strengths.\n
	If you already decided what kind of cocktail you want, just describe it for me and I will help you with the choice.\n
	If you want to learn how to prepare some cocktails, simply type: recepie of cocktail_name.\n
	You can also find out what you can make from the list of ingredients by asking: cocktails from ingredients: list_of_space_separated_ingredients.\n
	Hope you will enjoy our bar ;-) ".


% respone to unknown queries
unknown_response :-
	unknown_response(_, Result),
	write(Result).
unknown_response(_, Result) :-
	Result = "Sorry, I could not find anything suitable for you.".
