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

% Memoization.
:- dynamic(cause/2).





%! cause(+Model:atom, ?Cause:list(pair)) is nondet.

cause(Mod, AXs):-
  retractall(cause(Mod, _)),
  Mod:causal_formula(Phi),
  endogenous(Mod, Vs),
  
  % Condition 1: Facticity.
  % "The cause must be the case."
  % Especially for the generative case:
  % It does not make sense for cause and caused to be the same.
  Mod:determine_values([], AVs),
  proposition_to_variables(Phi, PhiVars),
  remove_pairs(AVs, PhiVars, AVs0),
  sublist(AXs, AVs0),
  AXs \== [],
  % "The caused must be the case."
  % "The case" is what occurs under the empty assignment.
  sat(AVs, Phi),

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
  ord_subset(Xs, Zs),
  assign_variables(Mod, Xs, AXs_alt),
  assign_variables(Mod, Ws, AWs_alt),
  
  % 2A:
  ord_union(AXs_alt, AWs_alt, Alts1),
  Mod:determine_values(Alts1, AVs_alt1),
  sat(AVs_alt1, not(Phi)),
  
  % 2B:
  %ord_minus(Zs, Xs, ZsMinusXs),
  forall(
    %(
      sublist(Ws_sub, Ws),
      %sublist(ZsMinusXs_sub, ZsMinusXs)
    %),
    (
      subpairs(AWs_alt, Ws_sub, Alts2),
      Mod:determine_values(Alts2, AVs_alt2),
      sat(AVs_alt2, Phi)
    )
  ),
  
  % Condition 3: Minimality.
  \+ ((
    cause(Mod, AXs_smaller),
    sublist(AXs_smaller, AXs)
  )),
  assert(cause(Mod, AXs)).



%! has_cause(+Model:atom) is semidet.

has_cause(Mod):-
  once(cause(Mod, _)).





% HELPERS %

%! assign_variables(
%!   +Model:atom,
%!   +Variables:list,
%!   -Assignment:list(pair)
%! ) is det.

assign_variables(Mod, Xs, AXs):-
  maplist(assignment0(Mod), Xs, AXs).

assignment0(Mod, X, X-Val):-
  Mod:range(X, Low, High),
  between(Low, High, Val).



%! endogenous(+Model:atom, -Endogenous:ordset) is det.
% Extracts the endogenous variables from the model.

endogenous(Mod, Vs):-
  aggregate_all(
    set(V),
    Mod:range(V, _, _),
    Vs
  ).



%! proposition_to_variables(+Phi:compound, -Vars:list(pair)) is det.

proposition_to_variables(not(Phi), Vars):- !,
  proposition_to_variables(Phi, Vars).
proposition_to_variables(or(Phi,Psi), Vars):- !,
  maplist(proposition_to_variables, [Phi,Psi], [Vars1,Vars2]),
  ord_union(Vars1, Vars2, Vars).
proposition_to_variables(Phi, Vars):-
  pairs_keys(Phi, Vars).



%! sat(+Domain:list(pair), +Phi:compound) is semidet.
% Satisfaction of a formula in a domain of pairs representing assignments.
% Notice that conjunction is represented by the use of lists.

sat(Dom, not(Phi)):- !,
  \+ sat(Dom, Phi).
sat(Dom, or(Phi,Psi)):- !,
  sat(Dom, Phi);
  sat(Dom, Psi).
sat(Dom, Phi):-
  sublist(Phi, Dom).
