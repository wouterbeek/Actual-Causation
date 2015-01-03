:- module(
  ac_test_models,
  [
    test_model/2 % +Name:atom
                 % -Model:iri
  ]
).

/** <module> Actual Causation: Test models

@author Wouter Beek
@version 2015/01
*/

:- use_module(library(clpfd)).

:- use_module(ac(ac_build)).





test_model(forest_fire, M):-
  assert_model(
    forest_fire,
    aap,
    [fire-[0,1],lightning-[0,1],match-[0,1]],
    [fire #= max(lightning,match)],
    M
  ).
