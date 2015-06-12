% To do:
% Vocative: "TOM, ..."

:- indexical anaphore_context=[ ].

:- randomizable utterance//1, stock_phrase//1.

utterance(DialogAct) --> stock_phrase(DialogAct).
utterance(question(Speaker, Addressee, LF, T, A)) -->
   sentence(LF, interrogative, affirmative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(assertion(Speaker, Addressee, LF, T, A)) -->
   sentence(LF, indicative, affirmative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(question_answer(Speaker, Addressee, LF, T, A)) -->
   sentence(LF, indicative, affirmative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(assertion(Speaker, Addressee, not(LF), T, A)) -->
   sentence(LF, indicative, negative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(command(Speaker, Addressee, LF)) -->
   sentence(LF, imperative, affirmative, _, _),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(injunction(Speaker, Addressee, LF)) -->
   sentence(LF, imperative, negative, _, _),
   { current_dialog_pair(Speaker, Addressee) }.
utterance(agree(Speaker, Addressee, _LF)) -->
   [ yes ],
   { current_dialog_pair(Speaker, Addressee) }.
utterance(disagree(Speaker, Addressee, _LF)) -->
   [ no ],
   { current_dialog_pair(Speaker, Addressee) }.
utterance(hypno_command(Speaker, Addressee, LF, T, A)) -->
   [ fnord ],
   s(LF, indicative, affirmative, T, A),
   { current_dialog_pair(Speaker, Addressee) }.


current_dialog_pair($speaker, $addressee).

%
% Stock phrases
%

stock_phrase(do_not_understand($speaker, $addressee, _)) --> [ huh, '?'].
stock_phrase(prompt_player($me, $me)) --> [type, something].

:-register_lexical_items([huh, type, something]).

stock_phrase(greet($speaker, Who)) -->
   [Salutation, ','],
   { member(Salutation, ['Hey', 'Hello', 'Hi']) },
   proper_name(Who, singular).
stock_phrase(greet($speaker, $addressee)) --> ['Hi', there].

:- register_lexical_items(['Hey', 'Hello', 'Hi']).

stock_phrase(apology($speaker, $addressee)) --> ['Sorry'].
stock_phrase(excuse_self($speaker, $addressee)) --> ['Excuse', me].

:- register_lexical_items(['Sorry', 'Excuse']).

stock_phrase(parting($speaker, $addressee)) --> [X], { member(X, [bye, byebye, goodbye]) }.
stock_phrase(parting($speaker, $addressee)) --> [see, you].
stock_phrase(parting($speaker, $addressee)) --> [be, seeing, you].

stock_phrase(command($speaker, $addressee, end_game($addressee, $addressee))) --> [ end, game ].

:- register_lexical_items([end, game]).

stock_phrase(assertion($speaker, $addressee,
		       restaurant_const(X,Predication),
		       present, simple)) -->
   ['I', want, a],
   rating_number(X^Predication), [star, restaurant].

stock_phrase(assertion($speaker, $addressee,
		       restaurant_const(X,Predication),
		       present, simple)) -->
   ['I', want, someplace, rated],
   rating_number(X^Predication), [stars, or, better].

stock_phrase(assertion($speaker, $addressee,
		       restaurant_const(X,Predication),
		       present, simple)) -->
   ['I', want, someplace, rated],
   rating_number(X^Predication), [stars, or, higher].

stock_phrase(assertion($speaker, $addressee,
		       restaurant_const(X,Predication),
		       present, simple)) -->
   ['I', want, something, rated],
   rating_number(X^Predication), [stars, or, better].

stock_phrase(assertion($speaker, $addressee,
		       restaurant_const(X,Predication),
		       present, simple)) -->
   ['I', want, something, rated],
   rating_number(X^Predication), [stars, or, higher].

rating_number(X^Predication) -->
   [Number],
   { rating_predicate(Number, X^Predication) }.

% rating_predicate('1', X^one(X)).
% rating_predicate('1.5', X^one_five(X)).
% rating_predicate('2', X^two(X)).
rating_predicate('2.5', X^two_five(X)).
rating_predicate('3', X^three(X)).
rating_predicate(three, X^three(X)).
rating_predicate('3.5', X^three_five(X)).
rating_predicate('4', X^four(X)).
rating_predicate(four, X^four(X)).
rating_predicate('4.5', X^four_five(X)).
% rating_predicate('5', X^five(X)).

:- register_lexical_items(['I', something, someplace, star, stars, restaurant, '2.5', '3', three, '3.5', '4', four, '4.5', or, better, higher]).

stock_phrase(assertion($speaker, $addressee,
		       restaurant_const(X,Predication),
		       present, simple)) -->
   ['I', want, something],
   cuisine_type(X^Predication).

stock_phrase(assertion($speaker, $addressee,
		       restaurant_const(X,Predication),
		       present, simple)) -->
   ['I', want, someplace],
   cuisine_type(X^Predication).

stock_phrase(assertion($speaker, $addressee,
		       restaurant_const(X,Predication),
		       present, simple)) -->
   ['I', want, some],
   cuisine_type_noun(X^Predication).

stock_phrase(assertion($speaker, $addressee,
		       restaurant_const(X,Predication),
		       present, simple)) -->
   ['I', want],
   cuisine_type_noun(X^Predication).

cuisine_type(X^Predication) -->
   [Word],
   { cuisine_predicate(Word, X^Predication) }.

cuisine_type_noun(X^Predication) -->
   [Word],
   { cuisine_predicate_noun(Word, X^Predication) }.

cuisine_predicate(cheap, X^cheap(X)).
cuisine_predicate(expensive, X^expensive(X)).
cuisine_predicate('American', X^american(X)).
cuisine_predicate_noun('American', X^american(X)).
cuisine_predicate('Spanish', X^spanish(X)).
cuisine_predicate_noun('Spanish', X^spanish(X)).
cuisine_predicate('Chinese', X^chinese(X)).
cuisine_predicate_noun('Chinese', X^chinese(X)).
cuisine_predicate('Thai', X^thai(X)).
cuisine_predicate_noun('Thai', X^thai(X)).
cuisine_predicate('Mexican', X^mexican(X)).
cuisine_predicate_noun('Mexican', X^mexican(X)).
cuisine_predicate(fast, X^fast_food(X)).
cuisine_predicate_noun(sushi, X^sushi(X)).
% cuisine_predicate_noun('fast food', X^fast_food(X)).
stock_phrase(assertion($speaker, $addressee, restaurant_const(X,fast_food(X)), present, simple)) --> ['I', want, fast, food].
stock_phrase(assertion($speaker, $addressee, restaurant_const(X,fast_food(X)), present, simple)) --> ['I', want, some, fast, food].
cuisine_predicate(vegan, X^vegan(X)).
cuisine_predicate_noun(sandwiches, X^sandwiches(X)).
cuisine_predicate_noun(soup, X^soup(X)).
cuisine_predicate_noun(salad, X^salad(X)).
cuisine_predicate_noun(breakfast, X^breakfast(X)).
cuisine_predicate_noun(brunch, X^brunch(X)).
% cuisine_predicate('Middle Eastern', X^middle_eastern(X)).
stock_phrase(assertion($speaker, $addressee, restaurant_const(X,middle_eastern(X)), present, simple)) --> ['I', want, something, 'Middle', 'Eastern'].
stock_phrase(assertion($speaker, $addressee, restaurant_const(X,middle_eastern(X)), present, simple)) --> ['I', want, someplace, 'Middle', 'Eastern'].
% cuisine_predicate_noun('Middle Eastern', X^middle_eastern(X)).
stock_phrase(assertion($speaker, $addressee, restaurant_const(X,middle_eastern(X)), present, simple)) --> ['I', want, 'Middle', 'Eastern'].
stock_phrase(assertion($speaker, $addressee, restaurant_const(X,middle_eastern(X)), present, simple)) --> ['I', want, some, 'Middle', 'Eastern'].
cuisine_predicate('Cajun', X^cajun(X)).
cuisine_predicate_noun('Cajun', X^cajun(X)).
cuisine_predicate('Southern', X^southern(X)).
cuisine_predicate_noun('Southern', X^southern(X)).
cuisine_predicate_noun(tapas, X^tapas(X)).
cuisine_predicate('Asian', X^asian(X)).
cuisine_predicate_noun('Asian', X^asian(X)).
cuisine_predicate('Japanase', X^japanase(X)).
cuisine_predicate_noun('Japanase', X^japanase(X)).
cuisine_predicate_noun(pizza, X^pizza(X)).
cuisine_predicate('Italian', X^italian(X)).
cuisine_predicate_noun('Italian', X^italian(X)).

:- register_lexical_items([cheap, expensive, some, 'American', 'Chinese', 'Thai', 'Mexican', fast, food, vegan, vegetarian, sandwiches, soup, salad, breakfast, brunch, 'Cajun', southern, tapas, 'Spanish', 'Middle', 'Eastern', 'Asian', sushi, 'Japanese', pizza, 'Italian']).

stock_phrase(question($speaker, $addressee, X:i_should_eat(X), present, simple)) -->
   [where, should, 'I', eat, '?'].

:- register_lexical_items([where, should, 'I', eat]).

%
% Help queries from the player
%

stock_phrase(general_help(player, $me)) -->
   [help].

:- register_lexical_item(help).

stock_phrase(general_help(player, $me)) -->
   [what, do, 'I', do, '?'].
stock_phrase(how_do_i(player, $me, Q)) -->
   [how, do, Us],
   { member(Us, ['I', you, we]) },
   player_question(Q),
   ['?'].

stock_phrase(objective_query(player, $me)) -->
   [what, am, 'I', trying, to, do, '?'].
stock_phrase(objective_query(player, $me)) -->
   [what, are, Us],
   { member(Us, [we, you]) },
   [trying, to, do, '?'].

stock_phrase(color_query(player, $me, red)) -->
   [what, does, red, text, mean, '?'].
stock_phrase(color_query(player, $me, red)) -->
   [why, does, my, text, turn, red, '?'].

:- register_lexical_items([red, green, yellow, white, turn, mean]).

stock_phrase(color_query(player, $me, yellow)) -->
   [what, does, yellow, text, mean, '?'].
stock_phrase(color_query(player, $me, yellow)) -->
   [why, does, my, text, turn, yellow, '?'].

stock_phrase(color_query(player, $me, green)) -->
   [what, does, green, text, mean, '?'].
stock_phrase(color_query(player, $me, green)) -->
   [why, does, my, text, turn, green, '?'].

stock_phrase(color_query(player, $me, white)) -->
   [what, does, white, text, mean, '?'].
stock_phrase(color_query(player, $me, white)) -->
   [why, does, my, text, turn, white, '?'].

%
% Increments produced by the discourse generator
%

utterance(discourse_increment(Speaker, Addressee, Fragments)) -->
   { generating_nl,               % Only valid for character output, not player input.
     bind(speaker, Speaker),
     bind(addressee, Addressee) },
   discourse_fragments(Fragments).

discourse_fragments([]) -->
   [ ].
discourse_fragments([F | Fs]) -->
   discourse_fragment(F),
   discourse_fragments(Fs).

discourse_fragment(question_answer(X)) -->
   {!}, sentence(X, indicative, affirmative, present, simple).
discourse_fragment(s(X)) -->
   {!}, sentence(X, indicative, affirmative, present, simple).

discourse_fragment(np(X)) -->
   {kind(X), !}, [a, X].

discourse_fragment(np(X)) -->
   {!}, np((X^S)^S, subject, third:singular, nogap, nogap).

discourse_fragment(X) -->
   { string(X), ! },
   [ X ].

%
% Interface to action and conversation systems
% This adds rules at load time for the different utterances
% They declare utterances to be actions (i.e. atomically executable)
% and to be events that conversations should respond to.
%

:- public register_utterance_types/0.

register_utterance_types :-
   forall(( clause(utterance(A, _, _), _),
	    nonvar(A) ),
	  assert_action_functor(A)),
   forall(clause(stock_phrase(A, _, _), _),
	  assert_action_functor(A)).

assert_action_functor(Structure) :-
   functor(Structure, Functor, Arity),
   ( action_functor(Functor, Arity) -> true
     ;
     ( assert(action_functor(Functor, Arity)),
       assert(precondition(Structure, /perception/nobody_speaking)),
       add_conversation_dispatch_clause(Structure) ) ).

add_conversation_dispatch_clause(Structure) :-
   functor(Structure, Functor, Arity),
   indexical_named(me, Me),
   EventArgs = [Partner, Me | _],
   length(EventArgs, Arity),
   Event =.. [Functor | EventArgs],
   assert( ( on_event(Event, conversation, C,
		      conversation_handler_task(C, respond_to_dialog_act(Event))) :-
	        C/partner/Partner
	   ) ).

:- register_utterance_types.
