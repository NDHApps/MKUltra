%%%
%%% EECS 395: Knowledge Representation and Reasoning for Game Characters
%%% Ian Horswill
%%% Spring 2015
%%% Final Project
%%% Juliusz Choinski, Nicholas Hall, Marc Malinowski, Tim Sullivan
%%%

Chinese(X) :- Asian(X).
Thai(X) :- Asian(X).
Vietnamese(X) :- Asian(X).
Japanese(X) :- Asian(X).
Sushi(X) :- Japanese(X).
Pizza(X) :- Italian(X).

%% restaurant(?Name, ?Cuisine, ?StarRating, ?PriceLevel, ?BYOB, ?Chain)
%  Knowledge base of restaurants
restaurant("Tsim Sha Tsui Cafe", [Chinese], 4, 2, No, No).
restaurant("Boltwood", [American], 3.5, 3, No, No).
restaurant("Found", [American, Lounges], 3.5, 2, No, No).
restaurant("Chipotle", [Mexican, FastFood], 3, 1, No, Yes).
restaurant("Blaze", [Pizza, FastFood], 4.5, 1, No, Yes).
restaurant("Flat Top", [Asian, Vegan], 3, 2, No, Yes).
restaurant("Cozy", [Thai], 3.5, 1, Yes, No).
restaurant("Todoroki", [Sushi], 3, 2, Yes, No).
restaurant("Zoba", [Thai], 3.5, 2, Yes, No).
restaurant("Olive Mountain", [MiddleEastern], 4, 2, Yes, No).


	   