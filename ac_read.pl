:- module(
  ac_read,
  [
    actual_value/2, % +Variable:iri
                    % ?Value:integer
    causal_formula/2, % +Model:iri
                      % -CausalFormula:compound
    determine_value/3, % +Model:iri
                       % +Variable:iri
                       % -Value:integer
    determined_variable/2, % +Model:iri
                           % -Variable:iri
    endogenous_variables/2, % +Model:iri
                            % -Endogenous:ordset(iri)
    outer_variable/2, % +Model:iri
                      % -Variable:iri
    potential_value/2, % +Variable:iri
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

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_read)).





%! actual_value(+Variable:iri, +Value:integer) is semidet.
%! actual_value(+Variable:iri, -Value:integer) is semidet.

actual_value(Var, Val):-
  once(rdf_typed_literal(Var, aco:value, Val, xsd:integer)).



%! causal_formula(+Model:iri, -CausalFormula:compound) is det.

causal_formula(M, Phi):-
  rdf_simple_literal(M, aco:causal_formula, Phi_atom),
  read_term_from_atom(Phi_atom, Phi_term, []),
  instantiate_term(M, var, Phi_term, Phi).



%! determine_value(+Model:iri, +Variable:iri, -Value:integer) is det.
% Succeeds for the determined value of the given variable.

determine_value(_, Var, Val):-
	actual_value(Var, Val), !.
determine_value(M, Var, Val):-
  rdf_simple_literal(Var, aco:structural_equation, Eq0),
  read_term_from_atom(Eq0, Eq, []),
  Eq = #=(_,Right0),
  instantiate_term(M, val, Right0, Right),
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



%! potential_value(+Variable:iri, +Value:integer) is semidet.
%! potential_value(+Variable:iri, -Value:integer) is multi.

potential_value(Var, Val):-
  once(rdf_has(Var, aco:range, Range)),
  rdf_typed_literal(Range, aco:low, Low, xsd:integer),
  rdf_typed_literal(Range, aco:high, High, xsd:integer),
  between(Low, High, Val).



%! variable(+Model:iri, +Name:atom, -Variable:iri) is semidet.

variable(M, Name, Var):-
  rdf_has(M, aco:endogenous_variable, Var),
  rdfs_label_value(Var, Name).





% HELPERS %

%! instantiate_term(
%!   +Module:iri,
%!   +Mode:oneof([val,var]),
%!   +Term:compound,
%!   -InstantiatedTerm:compound
%! ) is det.

instantiate_term(M, Mode, Name, X):-
  atom(Name), !,
  variable(M, Name, Var),
  (   Mode == val
  ->  rdf_typed_literal(Var, aco:value, X, xsd:integer)
  ;   X = Var
  ).
instantiate_term(M, Mode, Expr1, Expr2):-
  Expr1 =.. [Pred|Args1],
  maplist(instantiate_term(M, Mode), Args1, Args2),
  Expr2 =.. [Pred|Args2].
