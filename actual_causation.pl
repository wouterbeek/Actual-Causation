:- module(
  actual_causation,
  [
    models/4 % +Model:iri
             % ?Context:ordset(pair(iri,integer))
             % +CausalFormula:compound
             % ?Cause:ordset(pair(iri,integer))
  ]
).

/** <module> Actual causation

@author Wouter Beek
@see [http://arxiv.org/abs/1106.2652](Actual causation and the art of modeling)
@tbs Causal path restricts possible partitions.
@version 2014/12-2015/01
*/

:- use_module(library(apply)).
:- use_module(library(aggregate)).
:- use_module(library(clpfd)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(list_ext)).
:- use_module(generics(pair_ext)).

:- use_module(plSet(set_theory)).

:- use_module(plRdf(api/rdf_read)).

:- use_module(ac(ac_build)).
:- use_module(ac(ac_calculate_values)).
:- use_module(ac(ac_debug)).
:- use_module(ac(ac_read)).

:- dynamic(cause0/3).





%! models(
%!   +Model:iri,
%!   +Context:ordset(pair(iri,integer)),
%!   +CausalFormula:compound,
%!   +Cause:ordset(iri)
%! ) is semidet.
%! models(
%!   +Model:iri,
%!   ?Context:ordset(pair(iri,integer)),
%!   +CausalFormula:compound,
%!   ?Cause:ordset(iri)
%! ) is nondet.
%
% @arg CausalFormula A formula composed out of primitive events
%      using conjunction and negation.
% @arg Cause The variables that make up the cause.

models(M, Us, Phi, Xs):-
gtrace,
  % NONDET.
  context(M, Us),

  % Reset cause memoization on a per-context basis.
  retractall(cause0(M, Us, _)),

  % Make a snapshot of the database.
  rdf_transaction(
    models0(M, Us, Phi, Xs),
    _,
    [snapshot(true)]
  ).

%! models0(
%!   +Model:iri,
%!   +Context:ordset(pair(iri,integer)),
%!   +CausalFormula:compound,
%!   +Cause:ordset(iri)
%! ) is semidet.
%! models0(
%!   +Model:iri,
%!   +Context:ordset(pair(iri,integer)),
%!   +CausalFormula:compound,
%!   ?Cause:ordset(iri)
%! ) is nondet.

models0(M, Us, Phi, Xs):-
  % Set the context in the current database snapshot.
  maplist(assign_value, Us),

  % Collect all endogenous variables.
  endogenous_variables(M, Vs),

  % It does not make sense for cause and caused to be the same,
  % so do not consider variables that occur in the causal formula.
  formula_to_variables(Phi, PhiVars),
  ord_subtract(Vs, PhiVars, Vs0),

  % NONDET.
  % Split the endogenous variables into those constituting a causal path
  % and those that are "off to the side" (condition 2).
  partition(Vs0, [A,B]),

  % Notice that the order in which partition members occur is arbitrary.
  % Therefore for each binary partition we have two causal paths to try out.
  (   A = Zs,
      B = Ws
  ;   A = Ws,
      B = Zs
  ),

  % NONDET.
  % Find a potential cause.
  % The cause must belong to the causal path.
  %
  % @tbd A cause must not only be part of the causel path;
  %      it must be the onset of the causal path.
  %      ```prolog
  %      \+ (member(X, Xs), Model:causal_link(_-X)),
  %      has_causal_path(Model, Xs, PhiVars, Zs),
  %      ```
  subset(Xs, Zs),

  % No subcause of a cause should be considered a cause
  % (condition 3: minimality).
  % Notice that smaller causes are considered first.
  \+ ((
    cause0(M, Us, Xs0),
    subset(Xs0, Xs)
  )),

  % A cause must be non-empty.
  Xs \== [],

  % The cause must be the case (Condition 1).
  % This means that it must consist entirely of values from the real world.
  calculate_all_values(M, [], AVs),

  % The caused must be the case (Condition 1).
  satisfy_formula(M, [], Phi),
  debug_models(M, Us, [], Phi), % DEB

  % Construct a contingency under which the counterfactual
  % can be satisfied (Condition 2).
  % NONDET.
  assign_variables(Xs, AXs_contingent),
  % NONDET.
  assign_variables(Ws, AWs_contingent),

  % 2A:
  ord_union(AXs_contingent, AWs_contingent, Contingency1),
  satisfy_formula(M, Contingency1, not(Phi)),

  % 2B:
  ord_subtract(Zs, Xs, ZsMinusXs),
  forall(
    (
      sublist(Ws_subset, Ws),
      sublist(ZsMinusXs_subset, ZsMinusXs)
    ),
    (
      subpairs(AWs_contingent, Ws_subset, Continency2a),
      subpairs(AVs, ZsMinusXs_subset, Continency2b),
      ord_union(Continency2a, Continency2b, Continency2),
      satisfy_formula(M, Continency2, Phi)
    )
  ),

  % Store this result to ensure minimality of future results.
  assert(cause0(M, Us, Xs)).





% HELPERS %

%! assign_variables(
%!   +Variables:list(iri),
%!   -Assignment:list(pair(iri,integer))
%! ) is nondet.

assign_variables(Vars, As):-
  findall(
    Var-Val,
    (
      member(Var, Vars),
      rdf_typed_literal(Var, ac:possible_value, Val, xsd:integer)
    ),
    As
  ).



%! context(+Model:iri, +Context:list(pair(iri,integer))) is semidet.
%! context(+Model:iri, -Context:list(pair(iri,integer))) is nondet.

context(M, Us):-
  nonvar(Us), !,
  context1(M, Us), !.
context(M, Us):-
  context1(M, Us).

context1(M, Us):-
  aggregate_all(
    set(Var),
    outer_variable(M, Var),
    Vars
  ),
  context2(Vars, Us).

context2([], []).
context2([Var|T1], [Var-Val|T2]):-
  rdf_typed_literal(Var, ac:possible_value, Val, xsd:integer),
  context2(T1, T2).



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



%! list_conjunction(+List:list, -Conjunction:compound) is det.
%! list_conjunction(-List:list, +Conjunction:compound) is det.

list_conjunction([H], H).
list_conjunction([H|T1], and(H,T2)):-
  list_conjunction(T1, T2).
