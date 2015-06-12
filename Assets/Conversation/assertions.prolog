%%
%% Responding to assertions
%%

strategy(respond_to_dialog_act(assertion(Speaker,_, LF, Tense, Aspect)),
	 respond_to_assertion(Speaker, Modalized, Truth)) :-
   modalized(LF, Tense, Aspect, Modalized),
   admitted_truth_value(Speaker, Modalized, Truth).

default_strategy(respond_to_assertion(_Speaker, _ModalLF, true),
		 say_string("Yes, I know.")).
default_strategy(respond_to_assertion(_Speaker, _ModalLF, false),
		 say_string("I don't think so.")).
default_strategy(respond_to_assertion(Speaker, ModalLF, unknown),
		 (say_string(Response), assert(/hearsay/Speaker/ModalLF))) :-
   heard_hearsay(ModalLF) -> Response="I've head that." ; Response="Really?".

strategy(respond_to_assertion(Speaker, restaurant_const(X,Y), _),
	 begin(assert(/hearsay/Speaker/restaurant_const(X,Y)),
	       say_string("Okay!"))).

heard_hearsay(ModalLF) :-
   /hearsay/_/Assertion, Assertion=ModalLF.

% Get all of constraints:
i_should_eat(Y) :-
   all(X,
       (/hearsay/_/X, X = restaurant_const(_,_)),
       List),
   satisfies_const(Y, List).

cheap(X) :-
   property_value(X, price, "cheap").

expensive(X) :-
   property_value(X, price, "expensive").

serves(X, Y) :-
   related(X, serves, Y).

american(X) :- serves(X, american).

mexican(X) :- serves(X, mexican).

fast_food(X) :- serves(X, fast_food).

vegan(X) :- serves(X, vegan).

vegetarian(X) :- serves(X, vegetarian).

sandwiches(X) :- serves(X, sandwiches).

soup(X) :- serves(X, soup).

salad(X) :- serves(X, salad).

breakfast(X) :- serves(X, breakfast).

brunch(X) :- serves(X, brunch).

cajun(X) :- serves(X, cajun).

southern(X) :- serves(X, southern).

tapas(X) :- serves(X, tapas).

spanish(X) :- serves(X, spanish).

middle_eastern(X) :- serves(X, middle_eastern).

chinese(X) :- serves(X, chinese).

thai(X) :- serves(X, thai).

sushi(X) :- serves(X, sushi).

japanese(X) :- serves(X, japanese).
japanese(X) :- sushi(X).
sushi(X) :- japanese(X).

asian(X) :- serves(X, asian).
asian(X) :- chinese(X).
asian(X) :- thai(X).
asian(X) :- japanese(X).

pizza(X) :- serves(X, pizza).

italian(X) :- serves(X, italian).
italian(X) :- pizza(X).
pizza(X) :- italian(X).

two_five(X) :-
   property_value(X, rating, Y),
   number(Y) >= 2.5.

three(X) :-
   property_value(X, rating, Y),
   number(Y) >= 3.

three_five(X) :-
   property_value(X, rating, Y),
   number(Y) >= 3.5.

four(X) :-
   property_value(X, rating, Y),
   number(Y) >= 4.

four_five(X) :-
   property_value(X, rating, Y),
   number(Y) >= 4.5.

:- external restaurant_const/2.
satisfies_const(_X, []).
satisfies_const(X, [restaurant_const(X,P) | Tail]) :-
     P,
     satisfies_const(X,Tail).

% ^^^ THAT'LL CHECK ALL CONSTR AND RETURN A RESTAURANT

strategy(respond_to_dialog_act(question_answer(Speaker,_, LF)),
	 assert(/hearsay/Speaker/LF)).


%%
%% Kluges to keep characters from inappropriately contradicting other characters answers
%%

be(X,X).
be(Character, Name) :-
   property_value(Character, given_name, Name).

strategy(respond_to_assertion(Speaker, okay(Speaker), _),
	 null).


