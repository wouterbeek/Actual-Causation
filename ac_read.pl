:- module(
  ac_read,
  [
    causal_formula/2, % +Model:iri
                      % -CausalFormula:list(pair(iri,integer))
    determine_value/3, % +Model:iri
                       % +Variable:iri
                       % -Value:integer
    determined_variable/2, % +Model:iri
                           % -Variable:iri
    endogenous_variables/2, % +Model:iri
                            % -Endogenous:ordset(iri)
    outer_variable/2, % +Model:iri
                      % -Variable:iri
    value/2, % +Variable:iri
             % ?Value:integer
    variable/3 % +Model:iri
               % +Name:atom
               % -Variable:iri
  ]
).

/** <module> Actual Causation: Read predicates

@author Wouter Beek
@version 2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_read)).





%! causal_formula(+Model:iri, -CausalFormula:list(pair(iri,integer))) is det.

causal_formula(Model, Pairs):-
  aggregate_all(
    set(Var-Val),
    (
      rdf_has(Model, aco:endogenous_variable, Var),
      rdf_typed_literal(Var, aco:causal_formula, Val, xsd:integer, _)
    ),
    Pairs
  ).



%! determine_value(+Model:iri, +Variable:iri, -Value:integer) is det.
% Succeeds for the determined value of the given variable.

determine_value(_, Var, Val):-
  rdf_typed_literal(Var, aco:value, Val, xsd:integer), !.
determine_value(M, Var, Val):-
  rdf_simple_literal(Var, aco:structural_equation, Eq0),
  read_term_from_atom(Eq0, Eq, []),
  Eq = #=(_,Right0),
  instantiate_term(M, Right0, Right),
  Val is Right.



%! determined_variable(+Model:iri, +Variable:iri) is semidet.
%! determined_variable(+Model:iri, -Variable:iri) is nondet.
% Succeeds for endogenous variables that have not yet been calculated
% but whose value is completely determined by values of variables
% that have been calculated.

determined_variable(M, Var):-
  rdf_has(M, aco:endogenous_variable, Var),
  \+ rdf_has(Var, aco:value, _),
  forall(
    rdf_has(Var0, aco:causes, Var),
    rdf_has(Var0, aco:value, _)
  ), !.



%! endogenous_variables(+Model:iri, -Endogenous:ordset(iri)) is det.
% Extracts the endogenous variables from the model.

endogenous_variables(M, Vs):-
  aggregate_all(
    set(V),
    rdf_has(M, aco:endogenous_variable, V),
    Vs
  ).



%! outer_variable(+Model:iri, +Variable:iri) is semidet.
%! outer_variable(+Model:iri, -Variable:iri) is nondet.

outer_variable(M, Var):-
  rdf_has(M, aco:endogenous_variable, Var),
  \+ rdf_has(_, aco:causes, Var).



%! value(+Variable:iri, +Value:integer) is semidet.
%! value(+Variable:iri, -Value:integer) is semidet.

value(Var, Val):-
  once(rdf_has(Var, aco:value, Val)).



%! variable(+Model:iri, +Name:atom, -Variable:iri) is semidet.

variable(M, Name, Var):-
  rdf_has(M, aco:endogenous_variable, Var),
  rdfs_label_value(Var, Name).





% HELPERS %

%! instantiate_term(+Module:iri, +Term, -InstantiatedTerm) is det.

instantiate_term(M, Name, Val):-
  atom(Name), !,
  variable(M, Name, Var),
  rdf_typed_literal(Var, aco:value, Val, xsd:integer, _).
instantiate_term(M, Expr1, Expr2):-
  Expr1 =.. [Pred|Args1],
  maplist(instantiate_term(M), Args1, Args2),
  Expr2 =.. [Pred|Args2].
