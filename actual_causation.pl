:- module(
  actual_causation,
  [
    cause/2, % +Model:atom
             % ?Cause:list(pair(atom,nonneg))
    has_cause/1 % +Model:atom
  ]
).

/** <module> Actual causation

@author Wouter Beek
@see [http://arxiv.org/abs/1106.2652](Actual causation and the art of modeling)
@version 2014/12
*/

:- use_module(library(apply)).
:- use_module(library(aggregate)).
:- use_module(library(clpfd)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).

:- use_module(generics(list_ext)).
:- use_module(generics(pair_ext)).

:- use_module(plSet(set_theory)).

:- use_module(ac(ac_debug)).

% Memoization.
:- dynamic(cause/2).





%! cause(+Model:atom, ?Cause:list(pair)) is nondet.

cause(Model, AXs):-
  retractall(cause(Model, _)),
  Model:causal_formula(Phi),
  endogenous_variables(Model, Vs),
  
  % Condition 1: Facticity.
  % "The cause must be the case."
  % Especially for the generative case:
  % It does not make sense for cause and caused to be the same.
  Model:determine_values([], AVs),
  formula_to_variables(Phi, PhiVars),
  remove_pairs(AVs, PhiVars, AVs0),
  sublist(AXs, AVs0),
  (   debugging(ac)
  ->  list_conjunction(AXs, AXs0),
      debug_models([], AXs0)
  ;   true
  ),
  AXs \== [],
  % "The caused must be the case."
  % "The case" is what occurs under the empty assignment.
  sat0(AVs, Phi),
  debug_models([], Phi),
  
  % Condition 2: Counterfactual under contingency.
  % It does not make sense to involve the caused part in the construction of
  % a contingent counterfactual.
  ord_subtract(Vs, PhiVars, Vs0),
  partition(Vs0, [A,B]),
  % In the partition, the order is arbitrary.
  % Thus for each binary partition we have two causal paths to try out.
  (   A = Zs,
      B = Ws
  ;   A = Ws,
      B = Zs
  ),
  pairs_keys(AXs, Xs),
  % The cause must be part of the causal path.
  % @tbd Not just part of it, but at the onsets of it?
  ord_subset(Xs, Zs),
  assign_variables(Model, Xs, AXs_alt),
  assign_variables(Model, Ws, AWs_alt),
  
  % 2A:
  ord_union(AXs_alt, AWs_alt, Alts1),
  sat(Model, Alts1, not(Phi)),
  
  % 2B:
  %ord_minus(Zs, Xs, ZsMinusXs),
  forall(
    %(
      sublist(Ws_sub, Ws),
      %sublist(ZsMinusXs_sub, ZsMinusXs)
    %),
    (
      subpairs(AWs_alt, Ws_sub, Alts2),
      sat(Model, Alts2, Phi)
    )
  ),
  
  % Condition 3: Minimality.
  \+ ((
    cause(Model, AXs_smaller),
    sublist(AXs_smaller, AXs)
  )),
  assert(cause(Model, AXs)).



%! has_cause(+Model:atom) is semidet.

has_cause(Model):-
  once(cause(Model, _)).





% HELPERS %

%! assign_variables(
%!   +Model:atom,
%!   +Variables:list,
%!   -Assignment:list(pair)
%! ) is det.

assign_variables(Model, Xs, AXs):-
  maplist(assignment0(Model), Xs, AXs).

assignment0(Model, X, X-Val):-
  Model:endogenous_variable(X, _, Low, High),
  between(Low, High, Val).



%! endogenous_variables(+Model:atom, -Endogenous:ordset) is det.
% Extracts the endogenous variables from the model.

endogenous_variables(Model, Vs):-
  aggregate_all(
    set(V),
    Model:endogenous_variable(V, _, _, _),
    Vs
  ).



%! list_conjunction(+List:list, -Conjunction:compound) is det.
%! list_conjunction(-List:list, +Conjunction:compound) is det.

list_conjunction([H], H).
list_conjunction([H|T1], and(H,T2)):-
  list_conjunction(T1, T2).



%! formula_to_variables(+Phi:compound, -Vars:ordset) is det.

formula_to_variables(not(Phi), Vars):- !,
  formula_to_variables(Phi, Vars).
formula_to_variables(and(Phi,Psi), Vars):- !,
  maplist(formula_to_variables, [Phi,Psi], [Vars1,Vars2]),
  ord_union(Vars1, Vars2, Vars).
formula_to_variables(or(Phi,Psi), Vars):- !,
  maplist(formula_to_variables, [Phi,Psi], [Vars1,Vars2]),
  ord_union(Vars1, Vars2, Vars).
formula_to_variables(Var-_, [Var]).



%! sat(+Model:atom, +Alts:ordset(pair), +Phi:compound) is semidet.

sat(Model, Alts, Phi):-
  Model:determine_values(Alts, Assignment),
  sat0(Assignment, Phi),
  debug_models(Alts, Phi).

%! sat0(+Domain:list(pair), +Phi:compound) is semidet.
% Satisfaction of a formula in a domain of pairs representing assignments.

sat0(Dom, not(Phi)):- !,
  \+ sat0(Dom, Phi).
sat0(Dom, and(Phi,Psi)):- !,
  sat0(Dom, Phi),
  sat0(Dom, Psi).
sat0(Dom, or(Phi,Psi)):- !,
  sat0(Dom, Phi);
  sat0(Dom, Psi).
sat0(Dom, Phi):-
  memberchk(Phi, Dom).
