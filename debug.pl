% Debug file for the Actual-Causation project.


:- use_module(library(debug)).
:- debug(ac).


:- [load].


:- use_module(debug_project).
%%%%:- debug_all_files.


:- use_module(library(apply)).

:- use_module(ac(debug/test_models)).

%:- initialization(debug_init).

debug_init:-
  maplist(load_test_model, [forest_fire,suze_and_billy,careless_camper]).
