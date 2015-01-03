% Debug file for the Actual-Causation project.


:- use_module(library(debug)).
:- debug(ac).


:- [load].


:- use_module(debug_project).
%%%%:- debug_all_files.


:- use_module(ac(ac_build)).
:- use_module(library(clpfd)).

:- initialization(load_test_models).

load_test_models:-
  assert_model(
    forest_fire,
    'A forest fire can either be caused by lightning or an arsonist \c
     dropping a lit match.',
    [fire-[0,1],lightning-[0,1],match-[0,1]],
    [fire #= max(lightning,match)]
  ).
