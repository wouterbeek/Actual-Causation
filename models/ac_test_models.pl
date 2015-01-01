:- module(
  ac_test_models,
  [
    test_model/1 % +Name:atom
  ]
).

/** <module> Actual Causation: Test models

@author Wouter Beek
@version 2015/01
*/

:- use_module(library(clpfd)).

:- use_module(ac(ac_build_model)).





test_model(forest_fire):-
  assert_model(
    forest_fire,
    aap,
    [fire #= max(lightning,match)],
    fire-1
  ).
