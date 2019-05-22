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
		cocktail_recipe/2,
		cocktail_type/2,
		cocktail_season/2,
		cocktail_adj/2,
		cocktail_strength/2,
		cocktail_strength/2,
		cocktail_photo/2,
		hello/0, hello/2,
		unknown_response/0, unknown_response/2
	]
).

% cocktails
% name, base ingredient, list of ingredients
% -----------------------------------------------------------------------
cocktail(mojito, rum, [mint, rum, lime, ice, soda]).
cocktail(margo, malvazija, [malvazija, mint, limon, ice, radenska, 'elder syrup']).
cocktail(hugo, prosecco, [prosecco, mint, lime, ice, soda, 'elder syrup']).
cocktail(spritz, 'white wine', ['white wine', soda, ice]).
cocktail('spritz veneziano', prosecco, [prosecco, aperol, soda, ice]).
cocktail(greentini, vodka, [vodka, midori, ice]).
cocktail('apocalypse now', tequila, [tequila, 'irish cream', 'dry vermounth']).
cocktail(b52, 'grand mariner', ['grand mariner', baileys, 'irish cream', kahula]).
cocktail('cuba libre', bacardi, [bacardi, lime, cola, ice]).
cocktail(beton, beer, [beer, rakija]).


% recipes
% -----------------------------------------------------------------------
cocktail_recipe(mojito, "TODO recipe").
cocktail_recipe(margo, "TODO recipe").
cocktail_recipe(hugo, "TODO recipe").
cocktail_recipe(spritz, "TODO recipe").
cocktail_recipe('spritz veneziano', "TODO recipe").
cocktail_recipe(greentini, "TODO recipe").
cocktail_recipe('apocalypse now', "TODO recipe").
cocktail_recipe(b52, "TODO recipe").
cocktail_recipe('cuba libre', "TODO recipe").
cocktail_recipe(beton, "TODO recipe").


% photos
% -----------------------------------------------------------------------
cocktail_photo(mojito, "[photo](https://cdn.diffords.com/contrib/stock-images/2016/1/01/20163e856fbeb76b298eb064a15897d2b5d6.jpg)").
cocktail_photo(margo, "[photo](https://i.pinimg.com/originals/58/ff/a6/58ffa6a35fcd0bc4aa43997020fa77b9.png)").
cocktail_photo(hugo, "[photo](https://gbc-cdn-public-media.azureedge.net/img70164.768x512.jpg)").
cocktail_photo(spritz, "[photo](https://www.loveandoliveoil.com/wp-content/uploads/2018/07/spritz-cocktail-FEAT1.jpg)").
cocktail_photo('spritz veneziano', "[photo](https://secure.getgoout.com/goout/resource?entity=ITALY_BASE&shortRef=9GzgdJxkEc)").
cocktail_photo(greentini, "[photo](https://images.cocktailflow.com/v1/cocktail/w_300,h_540/cocktail_greentini-1.png)").
cocktail_photo('apocalypse now', "[photo](https://media-cdn.tripadvisor.com/media/photo-s/15/f5/15/9a/photo0jpg.jpg)").
cocktail_photo(b52, "[photo](https://i.pinimg.com/originals/10/ac/14/10ac14d18f819ff9b9d7605d81c96662.jpg)").
cocktail_photo('cuba libre', "[photo](https://www.thebacklabel.com/wp-content/uploads/2017/07/cubelibre-1050x519.jpg)").
cocktail_photo(beton, "[photo](https://cdn.liquor.com/wp-content/uploads/2016/09/23074244/20th-century-720x720-recipe.jpg)").


% cocktail types: short, long, shooter, midle, etc
% -----------------------------------------------------------------------
% mojito
cocktail_type(long, mojito).
cocktail_type(long, margo).
cocktail_type(long, hugo).
cocktail_type(long, spritz).
cocktail_type(long, 'spritz veneziano').
cocktail_type(midle, greentini).
cocktail_type(short, 'apocalypse now').
cocktail_type(shooter, b52).
cocktail_type(long, 'cuba libre').
cocktail_type(long, beton).



% coctail seasons: winter, spring, summer, fall, all_seasons
% -----------------------------------------------------------------------
% mojito
cocktail_season(summer, mojito).
cocktail_season(summer, margo).
cocktail_season(summer, hugo).
cocktail_season(summer, spritz).
cocktail_season(summer, 'spritz veneziano').
cocktail_season(summer, greentini).
cocktail_season(winter, 'apocalypse now').
cocktail_season(winter, b52).
cocktail_season(summer, 'cuba libre').
cocktail_season('all seasons', beton).



% cocktail moods / tastes (adjective):
% -----------------------------------------------------------------------
% mojito
cocktail_adj(refreshing, mojito).
cocktail_adj('sweet istra', margo).
cocktail_adj(sweet, hugo).
cocktail_adj(refreshing, spritz).
cocktail_adj(relaxing, 'spritz veneziano').
cocktail_adj(plain, greentini).
cocktail_adj(powerful, 'apocalypse now').
cocktail_adj(warming, b52).
cocktail_adj(refreshing, 'cuba libre').
cocktail_adj(beerish, beton).



% cocktail strength: nonalkoholic, weak, light, medium, strong, extremly_strong
% -----------------------------------------------------------------------
cocktail_strength(light, mojito).
cocktail_strength(weak, margo).
cocktail_strength(weak, hugo).
cocktail_strength(light, spritz).
cocktail_strength(medium, 'spritz veneziano').
cocktail_strength(medium, greentini).
cocktail_strength(strong, 'apocalypse now').
cocktail_strength(strong, b52).
cocktail_strength(light, 'cuba libre').
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

keyword(String, get_cocktail_recipe, CocktailName) :-
	sub_string(String, Before, 9, After, "recipe of"),
	After > 0,
	Start is Before + 9 + 1,
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
keyword("all-seasons", get_by_season, 'all seasons').

% taste / mood keywords
keyword("refreshing", get_by_adj, refreshing).
keyword("sweet-istra", get_by_adj, 'sweet istra').
keyword("sweet", get_by_adj, sweet).
keyword("relaxing", get_by_adj, relaxing).
keyword("plain", get_by_adj, plain).
keyword("powerful", get_by_adj, powerful).
keyword("warming", get_by_adj, warming).
keyword("beerish", get_by_adj, beerish).

% strength keywords
keyword("nonalkoholic", get_by_strength, 'non alkoholic').
keyword("weak", get_by_strength, weak).
keyword("light", get_by_strength, light).
keyword("medium", get_by_strength, medium).
keyword("strong", get_by_strength, strong).
keyword("extremly-strong", get_by_strength, 'extremly strong').

% informational
keyword("spirits", get_all_cocktail_base_spirits, _).
keyword("types", get_all_cocktail_types, _).
keyword("seasons", get_all_cocktail_seasons, _).
keyword("tastes/moods", get_all_cocktail_adjs, _).
keyword("strengths", get_all_cocktail_strengths, _).


% greetings
keyword("hi", hello, _).
keyword("hello", hello, _).
keyword("help", hello, _).


% ***********************************************************************
% Basic communication
% ***********************************************************************

% welcoming
hello :- hello(_,Result), write(Result).
hello(_,Result) :-
	Result = 
"Hello, my name is Jack. I am the bartender here.\n
I can help you discover or prepare some cocktails.\n

I would be glad to telll you all about them. You can discover what `spirits` they are based on,\
the `types` of cocktails,suitable `seasons`, some features like `tastes/moods` and of cource the `strengths`.\n

If you already decided what kind of cocktail you want, just describe it for me and I will help you with the right choice.\n

If you want to know more about a particular cocktail, simply ask me to `describe cocktail_name`.\n

Sure, you would be interested to make one ;) Let me help you by telling you about the `recipe of cocktail_name`.\n

Jack is your friend, so, if you already bought some stuff you can ask me what `cocktails from ingredients: list_of_space_separated_ingredients` you can make.\n

Hope you will enjoy our bar!".


% respone to unknown queries
unknown_response :-
	unknown_response(_, Result),
	write(Result).
unknown_response(_, Result) :-
	Result = "Sorry, I could not find anything suitable for you.".
