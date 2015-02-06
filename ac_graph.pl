:- module(
  ac_graph,
  [
    causal_path/4 % +Model:atom
                  % +Phi:atom
                  % -Cause:ordset(atom)
                  % -CausalPath:ordset(atom)
  ]
).

/** <module> Actual Causation: Graph theory

Graph-theoretic predicates used for calculating actual causation.

A causal path must adhere to the following conditions:
  1. It must end in all and only caused variables.
  2. It must start in all and only cause variables.
  3. No superfluous or non-traversed variables are included.

---

@author Wouter Beek
@version 2014/12-2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).





%! causal_path(
%!   +Model:atom,
%!   +Phi:atom,
%!   -Cause:ordset(atom),
%!   -CausalPath:ordset(atom)
%! ) is nondet.

causal_path(_, Phi, Xs, Zs):-
  formula_to_variables(Phi, PhiVars),
  maplist(causal_subpath, PhiVars, Pairs),
  maplist(causal_combi_choice, Pairs, Paths, Xs),
  maplist(list_to_ord_set, Paths, Sets),
  ord_union(Sets, Zs).


%! formula_to_variables(+Phi:compound, -Variables:ordset(iri)) is det.
% Boolean combinations of primitive events.

formula_to_variables(not(Phi), Vars):- !,
  formula_to_variables(Phi, Vars).
formula_to_variables(and(Phi,Psi), Vars):- !,
  maplist(formula_to_variables, [Phi,Psi], [Vars1,Vars2]),
  ord_union(Vars1, Vars2, Vars).
formula_to_variables(or(Phi,Psi), Vars):- !,
  maplist(formula_to_variables, [Phi,Psi], [Vars1,Vars2]),
  ord_union(Vars1, Vars2, Vars).
formula_to_variables(Var-_, [Var]).


causal_subpath(Y, Y-Paths):-
  aggregate_all(
    set(path(X,P,Y)),
    path(Y, P, X),
    Paths
  ).

path(Y, P, X):-
  path(Y, [Y], P, X).

path(Y, _, [Y], Y):-
  \+ rdf_has(_, aco:causes, Y), !.
path(Y, T1, [Y|T2], X):-
  rdf_has(Z, aco:causes, Y),
  \+ memberchk(Z, T1),
  path(Z, [Z|T1], T2, X).


causal_combi_choice(Y-Paths, P, X):-
  member(path(X,P,Y), Paths).
