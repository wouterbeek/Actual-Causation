:- module(
  ac_read,
  [
    causal_formula/2, % +Model:iri
                      % -CausalFormula:list(pair(iri,integer))
    determine_value/3, % +Model:iri
                       % +Variable:iri
                       % -Value:integer
    determined_variable/2 % +Model:iri
                          % -Variable:iri
  ]
).

/** <module> Actual Causation: Read predicates

@author Wouter Beek
@version 2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_read)).





%! causal_formula(+Model:iri, -CausalFormula:list(pair(iri,integer))) is det.

causal_formula(Model, Pairs):-
  aggregate_all(
    set(Var-Val),
    (
      rdf_has(Model, ac:endogenous_variable, Var),
      rdf_typed_literal(Var, ac:causal_formula, Val, xsd:integer, _)
    ),
    Pairs
  ).



%! determine_value(+Model:iri, +Variable:iri, -Value:integer) is det.
% Succeeds for the determined value of the given variable.

determine_value(Model, Var, Val):-
  rdf_simple_literal(Var, ac:structural_equation, Eq0),
  read_term_from_atom(Eq0, Eq),
  Eq = #=(_,Right0),
  instantiate_term(Right0, Right),
  Val is Right.



%! determined_variable(+Model:iri, -Variable:iri) is nondet.
% Succeeds for endogenous variables that have not yet been calculated
% but whose value is completely determined by values of variables
% that have been calculated.

determined_variable(Model, Var):-
  rdfs_individual(Var, ac:'EndogenousVariable'),
  \+ rdf_has(Var, ac:value, _),
  forall(
    rdf_has(Var0, ac:causes, Var),
    rdf_has(Var0, ac:value, _)
  ).



%! instantiate_term(+Term, -InstantiatedTerm) is det.

instantiate_term(Name, Val):-
  atom(Name), !,
  once(rdfs_label_value(Var, Name)),
  rdf_typed_literal(Var, ac:value, Val, xsd:integer, _).
instantiate_term(Expr1, Expr2):-
  Expr1 =.. [Pred|Args1],
  maplist(instantiate_term, Args1, Args2),
  Expr2 =.. [Pred|Args2].
