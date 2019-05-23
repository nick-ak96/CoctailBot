% ***********************************************************************
% Coctails knowledge base
% https://www.cocktailflow.com
% https://www.thespruceeats.com/best-recipes-for-shots-and-shooters-4151053
% https://uk.thebar.com/
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
cocktail(spricer, 'white wine', ['white wine', soda, ice]).
cocktail('spritz veneziano', prosecco, [prosecco, aperol, soda, ice]).
cocktail(greentini, vodka, [vodka, midori, ice]).
cocktail('apocalypse now', tequila, [tequila, 'irish cream', 'dry vermounth']).
cocktail(b52, 'grand mariner', ['grand mariner', baileys, 'irish cream', kahula]).
cocktail('cuba libre', bacardi, [bacardi, lime, cola, ice]).
cocktail(submarine, beer, [beer, rakija]).


% recipes
% -----------------------------------------------------------------------
cocktail_recipe(mojito, "
    1. Muddle sugar and lime wedges together in a glass.
    Press down on 2 wedges of lime and 2 teaspoons of caster sugar in a tankard or jar using a large spoon or pestle to extract flavour and aroma.
    
    2. Muddle mint.
    Pick 12 leaves from a sprig of mint and place in the glass. Press down gently on the mint, together with the sugar and lime.
    
    3. Add ice to a glass.
    Add crushed ice so the glass is ¾ full.
    
    4. Add Captain Morgan White Rum® and soda.
    Pour in Captain Morgan® White Rum and a dash of soda water.
    
    5. Stir with a spoon.
    Stir the mixture thoroughly using a bar spoon until well combined.
    
    6. Add ice and a sprig of mint.
    Top up with more crushed ice and garnish with a sprig of mint.").

cocktail_recipe(margo, "
    1. Scoop a handful of ice into a large wine glass along with some torn mint leaves.
    
    2. Add the elderflower cordial with a splash of Radenska then top up with Malvazija.
    
    3. Give a quick stir and garnish with a wheel of limon.").
    
cocktail_recipe(hugo, "
    1. Scoop a handful of ice into a large wine glass along with some torn mint leaves.
    
    2. Add the elderflower cordial with a splash of soda water then top up with Prosecco.
    
    3. Give a quick stir and garnish with a wheel of lime.").
    
cocktail_recipe(spricer, "
    1. Pour cold white wine in large wine glass until is half full.
    
    2. Fill up with cold soda.").

cocktail_recipe('spritz veneziano', "
    1. Add ice to the old fashioned glass
    
    2. Add prosecco and aperol to the glass then stir together
    
    3. Fill up the glass with club soda
    
    4. Garnish with orange wheel.").
    
cocktail_recipe(greentini, "
    1. Fill up the shaker with ice.
    
    2. Pour Midorivodka into the shaker.
    
    3. Shake well.
    
    4. Strain into the cocktail glass.
    
    5. Garnish with lime wedge.").
    
cocktail_recipe('apocalypse now', "
    1. Pour the dry vermouth and tequila into a cocktail shaker filled with ice.

    2. Shake well.

    3. Strain into a shot glass.

    4. Float the Irish cream liqueur on top by slowly pouring it over the back of a bar spoon.").

cocktail_recipe(b52, "
    1. Pour the coffee liqueur into a shot glass.

    2. Float the Irish cream liqueur on top.

    3.Float the Grand Marnier on top of the second layer.").

cocktail_recipe('cuba libre', "
    1. Fill a tall glass with Ice.

    2. Pour in the Captain Morgan Original Rum and cola.

    3. To serve, squeeze the lime over the drink and drop in.").

cocktail_recipe(submarine, "
    1. Pour a half of liter or a pint of lager beer.
    
    2. Drop the glass of shooter filled up with rakia or tequila and drop it in a glass of beer.").


% photos
% -----------------------------------------------------------------------
cocktail_photo(mojito, "[photo](https://cdn.diffords.com/contrib/stock-images/2016/1/01/20163e856fbeb76b298eb064a15897d2b5d6.jpg)").
cocktail_photo(margo, "[photo](https://i.pinimg.com/originals/58/ff/a6/58ffa6a35fcd0bc4aa43997020fa77b9.png)").
cocktail_photo(hugo, "[photo](https://gbc-cdn-public-media.azureedge.net/img70164.768x512.jpg)").
cocktail_photo(spricer, "[photo](http://www.sodasherpa.com/wp-content/uploads/2015/05/Soda-Sherpa-Sparkling-Wine-SodaStream.png)").
cocktail_photo('spritz veneziano', "[photo](https://secure.getgoout.com/goout/resource?entity=ITALY_BASE&shortRef=9GzgdJxkEc)").
cocktail_photo(greentini, "[photo](https://images.cocktailflow.com/v1/cocktail/w_300,h_540/cocktail_greentini-1.png)").
cocktail_photo('apocalypse now', "[photo](https://www.thespruceeats.com/thmb/ZT8na51Zknx8YXlMLPA5oOaYAtI=/960x0/filters:no_upscale():max_bytes(150000):strip_icc()/ApocalypseNow-Shot-f1d319ebb30d4da793843703116a5642.jpg)").
cocktail_photo(b52, "[photo](https://i.pinimg.com/originals/10/ac/14/10ac14d18f819ff9b9d7605d81c96662.jpg)").
cocktail_photo('cuba libre', "[photo](https://www.thebacklabel.com/wp-content/uploads/2017/07/cubelibre-1050x519.jpg)").
cocktail_photo(submarine, "[photo](https://urlzs.com/VDitE)").


% cocktail types: short, long, shooter, midle, etc
% -----------------------------------------------------------------------
% mojito
cocktail_type(long, mojito).
cocktail_type(long, margo).
cocktail_type(long, hugo).
cocktail_type(long, spricer).
cocktail_type(long, 'spritz veneziano').
cocktail_type(midle, greentini).
cocktail_type(short, 'apocalypse now').
cocktail_type(shooter, b52).
cocktail_type(long, 'cuba libre').
cocktail_type(long, submarine).



% coctail seasons: winter, spring, summer, fall, all_seasons
% -----------------------------------------------------------------------
% mojito
cocktail_season(summer, mojito).
cocktail_season(summer, margo).
cocktail_season(summer, hugo).
cocktail_season(summer, spricer).
cocktail_season(summer, 'spritz veneziano').
cocktail_season(summer, greentini).
cocktail_season(winter, 'apocalypse now').
cocktail_season(winter, b52).
cocktail_season(summer, 'cuba libre').
cocktail_season('all seasons', submarine).



% cocktail moods / tastes (adjective):
% -----------------------------------------------------------------------
% mojito
cocktail_adj(refreshing, mojito).
cocktail_adj('sweet istra', margo).
cocktail_adj(sweet, hugo).
cocktail_adj(refreshing, spricer).
cocktail_adj(relaxing, 'spritz veneziano').
cocktail_adj(plain, greentini).
cocktail_adj(powerful, 'apocalypse now').
cocktail_adj(warming, b52).
cocktail_adj(refreshing, 'cuba libre').
cocktail_adj(beerish, submarine).



% cocktail strength: nonalkoholic, weak, light, medium, strong, extremly_strong
% -----------------------------------------------------------------------
cocktail_strength(light, mojito).
cocktail_strength(weak, margo).
cocktail_strength(weak, hugo).
cocktail_strength(light, spricer).
cocktail_strength(medium, 'spritz veneziano').
cocktail_strength(medium, greentini).
cocktail_strength(strong, 'apocalypse now').
cocktail_strength(strong, b52).
cocktail_strength(light, 'cuba libre').
cocktail_strength(medium, submarine).




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
