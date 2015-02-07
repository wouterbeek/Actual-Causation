:- module(
  test_models,
  [
    load_test_model/1, % +Name:atom
    load_test_model/2 % +Name:atom
                      % -Model:atom
  ]
).

/** <module> Test models

Causal models that can be used to test the Actual-Causation project.

@author Wouter Beek
@version 2014/12, 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(clpfd)). % Syntax?
:- use_module(library(plunit)).
:- use_module(library(semweb/rdfs)).

:- use_module(ac(ac_build)).
:- use_module(ac(ac_build_sim)).
:- use_module(ac(ac_models)).
:- use_module(ac(ac_read_sim)).





load_test_model(Name):-
  load_test_model(Name, _).

load_test_model(forest_fire, M):-
  assert_model(
    forest_fire,
    'A forest fire can either be caused by lightning or an arsonist \c
     dropping a lit match.',
    [fire in 0..1, lightning in 0..1, match in 0..1],
    [fire #= max(lightning,match)],
    M
  ),
  assert_default_causal_formula(M, fire-1).
load_test_model(trumping, M):-
  assert_model(
    trumping,
    'Suppose that a group of soldiers is very well trained, so that they \c
     will obey any order given by a superior officer; in the case of \c
     conflicting orders, they obey the highest-ranking officer. \c
     Both a sergeant and a major issue the order to march, and the soldiers \c
     march. \c
     In the case of conflicting orders, the soldiers obey the major.',
    [major in 0..2, sergeant in 0..2, soldiers in 0..1],
    (   sergeant =:= 0
    ->  soldiers #= 0
    ;   sergeant =:= 2,
        major =:= 0
    ->  soldiers #= 0
    ;   soldiers #= 1
    ),
    M
  ),
  assert_default_causal_formula(M, p-1).
load_test_model(suze_and_billy, M):-
  assert_model(
    suzy_and_billy,
    'Suzy and Billy both pick up rocks and throw them at a bottle. \c
     Suzy’s rock gets there first, shattering the bottle. \c
     Since both throws are perfectly accurate, Billy’s would have shattered \c
     the bottle had Suzy not thrown.',
    [bh in 0..1, bs in 0..1, bt in 0..1, sh in 0..1, st in 0..1],
    [bh #= min(bt,1-sh), sh #= st, bs #= max(bh,sh)],
    M
  ),
  assert_default_causal_formula(M, bs-1).
load_test_model(careless_camper, M):-
  assert_model(
    careless_camper,
    'Suppose that the Careless Camper (CC for short) has plans to go \c
     camping on the first weekend in June.
     He will go camping unless there is a fire in the forest in May. \c
     If he goes camping, he will leave a campfire unattended, and there will \c
     be a forest fire. \c
     Let the variable C take the value 1 if CC goes camping, and 0 otherwise.',
    [cc in 0..1, f1 in 0..1, f2 in 0..1],
    [cc #= 1 - f1, f2 #= cc * (1 - f1)],
    M
  ),
  assert_default_causal_formula(M, f2-1).





:- begin_tests(test_models).

ac_models_test(forest_fire, [[lightning],[match]], true).

test(ac_models, [forall(ac_models_test(Name,Causes,true))]):-
  load_test_model(Name, M),
  calculate_models(M, Us, _, Phi, Xs),
  aggregate_all(
    set(Cause),
    (
      models(M, Us, Phi, Xs, _, _),
      maplist(rdfs_label, Xs, Labels),
      sort(Labels, Cause)
    ),
    Causes0
  ),
  Causes = Causes0.

:- end_tests(test_models).
