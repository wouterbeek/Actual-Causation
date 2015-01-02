:- module(
  ac_calculate_values,
  [
    calculate_values/4 % +Model:iri
                       % +Context:list(pair(iri,integer))
                       % +Assignment:list(pair(iri,integer))
                       % -Solution:list(pair(iri,integer))
  ]
).

/** <module> Actual Causation: Calculate values

@author Wouter Beek
@version 2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(api/rdf_read)).

:- use_module(ac(ac_build)).
:- use_module(ac(ac_read)).





%! calculate_values(
%!   +Model:iri,
%!   +Context:list(pair(iri,integer)),
%!   +Assignment:list(pair(iri,integer)),
%!   -Solution:list(pair(iri,integer))
%! ) is det.
% Use a database snapshot for a specific counterfactual with a contingency.

calculate_values(Model, Context, A, Solution):-
  rdf_transaction(
    calculate_values0(Model, Context, A, Solution),
    _,
    [snapshot(true)]
  ).

calculate_values0(Model, Context, A, Solution):-
  maplist(assign_value, Context),
  maplist(assign_value, A),
  calculate_endogenous_values(Model, Solution).

calculate_endogenous_values(Model, Solution):-
  determined_variable(Model, Var),
  determine_value(Model, Var, Val),
  assign_value(Var-Val),
  calculate_endogenous_values(Model, Solution).
calculate_endogenous_values(Model, Solution):-
  aggregate_all(
    set(Var-Val),
    (
      rdf_has(Model, ac:endogenous_variable, Var),
      rdf_typed_literal(Var, ac:value, Val, xsd:integer, _)
    ),
    Solution
  ).
