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
strategy(restaurant_chooser,
	 say_string(Y)) :-
   all(X,
       (/hearsay/_/X, X = restaurant_const(_,_)),
       List),
   satisfies_const(Y, List).

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


