:- module(
  ac_trans,
  [
    instantiate_term/4 % +Module:iri,
                       % +Mode:oneof([val,var]),
                       % +Term:compound,
                       % -InstantiatedTerm:compound
  ]
).

/** <module> Actual Causation: Translation

Translation from natural language input to an RDF representation.

@author Wouter Beek
@version 2015/01
*/

:- use_module(library(apply)).

:- use_module(ac(ac_read)).





%! instantiate_term(
%!   +Module:iri,
%!   +Mode:oneof([val,var]),
%!   +Term:compound,
%!   -InstantiatedTerm:compound
%! ) is det.
% Simple instantiation for RDF-represented AC terms.

instantiate_term(_, _, N, N):-
	integer(N), !.
instantiate_term(M, Mode, Name, X):-
  atom(Name), !,
  variable(M, Name, Var),
  (   Mode == val
  ->  actual_value(Var, X)
  ;   X = Var
  ).
instantiate_term(M, Mode, Expr1, Expr2):-
  Expr1 =.. [Pred|Args1],
  maplist(instantiate_term(M, Mode), Args1, Args2),
  Expr2 =.. [Pred|Args2].
