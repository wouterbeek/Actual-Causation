% Debug file for the Actual-Causation project.


:- use_module(library(debug)).
:- debug(ac).


:- [load].


:- use_module(debug_project).
%%%%:- debug_all_files.



:- use_module(ac(models/ac_test_models)).
:- test_model(forest_fire).
