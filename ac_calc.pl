:- module(
  ac_calc,
  [
    calculate_all_values/2, % +Model:iri
                            % -Solution:list(pair(iri,integer))
    satisfy_formula/3 % +Model:iri
                      % +Assignment:list(pair(iri,integer))
                      % +Phi:compound
  ]
).

/** <module> Actual Causation: Calculate values

@author Wouter Beek
@version 2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(ac(ac_build)).
:- use_module(ac(ac_read)).





%! calculate_all_values(+Model:iri, -Solution:list(pair(iri,integer))) is det.
% Use a database snapshot for a specific counterfactual with a contingency.
% If Assignment is the empty list then Solution represents what is the case,
% i.e., it describes the real world.

calculate_all_values(M, Solution):-
  rdf_transaction(
    calculate_all_values0(M, Solution),
    _,
    [snapshot(true)]
  ).

calculate_all_values0(M, Solution):-
  determined_variable(M, Var),
  determine_value(M, Var, Val),
  assign_value(Var-Val),
  calculate_all_values0(M, Solution).
calculate_all_values0(M, Solution):-
  aggregate_all(
    set(Var-Val),
    (
      rdf_has(M, aco:endogenous_variable, Var),
      actual_value(Var, Val)
    ),
    Solution
  ).



%! calculate_some_value_under_assignment0(
%!   +Model:iri,
%!   +Variable:iri,
%!   -Value:integer
%! ) is det.

calculate_some_value_under_assignment0(M, Var, Val):-
  forall(
    rdf_has(Var0, aco:causes, Var),
    calculate_some_value_under_assignment0(M, Var0, _)
  ),
  determine_value(M, Var, Val),
  assign_value(Var-Val).



%! satisfy_formula(
%!   +Model:iri,
%!   +Assignment:list(pair(iri,integer)),
%!   +Phi:compound
%! ) is det.

satisfy_formula(M, As, Phi):-
  rdf_transaction(
    satisfy_formula0(M, As, Phi),
    _,
    [snapshot(true)]
  ).

satisfy_formula0(M, As, Phi):-
  maplist(assign_value, As),
  %debug_model(M), % DEB
  satisfy_formula_under_assignment(M, Phi).

satisfy_formula_under_assignment(M, and(Phi,Psi)):- !,
  satisfy_formula_under_assignment(M, Phi),
  satisfy_formula_under_assignment(M, Psi).
satisfy_formula_under_assignment(M, not(Phi)):- !,
  \+ satisfy_formula_under_assignment(M, Phi).
satisfy_formula_under_assignment(M, Var-Val):-
  calculate_some_value_under_assignment0(M, Var, Val).
