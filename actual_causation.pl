:- module(
  actual_causation,
  [
    cause/3, % +Model:atom
             % ?Cause:list(pair(atom,nonneg))
             % ?CausalPath:ordset
    has_cause/1 % +Model:atom
  ]
).

/** <module> Actual causation

@author Wouter Beek
@see [http://arxiv.org/abs/1106.2652](Actual causation and the art of modeling)
@tbs Causal path restricts possible partitions.
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

:- use_module(plGraph(graph_walk)).

:- use_module(ac(ac_debug)).

%! cause0(?Model:atom, ?Cause:list(pair), ?CausalPath:ordset) is nondet.
% Memoization of causes.

:- dynamic(cause0/3).





%! cause(+Model:atom, ?Cause:list(pair), ?CausalPath:ordset) is nondet.

cause(Model, AXs, Zs):-
  retractall(cause0(Model, _, _)),
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
  AXs \== [],
  (   debugging(ac)
  ->  list_conjunction(AXs, AXs0),
      debug_models(AXs, [], AXs0)
  ;   true
  ),
  % "The caused must be the case."
  % "The case" is what occurs under the empty assignment.
  sat0(AVs, Phi),
  debug_models(AXs, [], Phi),

  % Condition 2: Counterfactual under contingency.
  % It does not make sense to involve the caused part in the construction of
  % a contingent counterfactual.
  ord_subtract(Vs, PhiVars, Vs0),
  %%%%\+ (member(X, Xs), Model:causal_link(_-X)),
  %%%%% The cause must be at the onsets of the causal path.
  %%%%has_causal_path(Model, Xs, PhiVars, Zs),
  partition(Vs0, [A,B]),
  % In the partition, the order is arbitrary.
  % Thus for each binary partition we have two causal paths to try out.
  (   A = Zs,
      B = Ws
  ;   A = Ws,
      B = Zs
  ),
  pairs_keys(AXs, Xs),
  ord_subset(Xs, Zs),

  assign_variables(Model, Xs, AXs_alt),
  assign_variables(Model, Ws, AWs_alt),

  % 2A:
  ord_union(AXs_alt, AWs_alt, Alts1),
  sat(AXs, Model, Alts1, not(Phi)),

  % 2B:
  ord_subtract(Zs, Xs, ZsMinusXs),
  forall(
    (
      sublist(Ws_sub, Ws),
      sublist(ZsMinusXs_sub, ZsMinusXs)
    ),
    (
      subpairs(AWs_alt, Ws_sub, Alts2a),
      subpairs(AVs, ZsMinusXs_sub, Alts2b),
      ord_union(Alts2a, Alts2b, Alts2),
      sat(AXs, Model, Alts2, Phi)
    )
  ),

  % Condition 3: Minimality.
  \+ ((
    cause0(Model, AXs_smaller, _),
    sublist(AXs_smaller, AXs)
  )),
  assert(cause0(Model, AXs, Zs)).



%! has_cause(+Model:atom) is semidet.

has_cause(Model):-
  once(cause(Model, _, _)).



%! has_causal_path(
%!   +Model:atom,
%!   +CauseVars:ordset,
%!   +CausedVars:ordset,
%!   -PathVars:ordset
%! ) is det.
% A causal path must adhere to the following conditions:
%   1. It must end in all and only caused variables.
%   2. It must start in all and only cause variables.
%   3. No superfluous or non-traversed variables are included.

has_causal_path(Model, Xs, PhiVars, Zs):-
  aggregate_all(
    set(Path),
    (
      member(X, Xs),
      member(Y, PhiVars),
      path(
        ca_vertex,
        ca_edge,
        ca_neighbor,
        Model,
        Y,
        X,
        Path
      )
    ),
    Paths
  ),
  maplist(path_to_vertices, Paths, Vss),
  ord_union(Vss, Zs).

path_to_vertices([_,_|T], Vs):-
  path_to_vertices0(T, Vs0),
  list_to_ord_set(Vs0, Vs).

path_to_vertices0([], []).
path_to_vertices0([H], [H]):- !.
path_to_vertices0([V,_|T1], [V|T2]):-
  path_to_vertices0(T1, T2).

ca_vertex(Model, V):-
  Model:causal_link(V-_).
ca_vertex(Model, V):-
  Model:causal_link(_-V).

ca_edge(Model, V-W):-
  Model:causal_link(W-V).

ca_neighbor(Model, V, W):-
  ca_edge(Model, V-W).





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



%! sat(
%!   +Cause:list(pair),
%!   +Model:atom,
%!   +AlternativeAssignments:ordset(pair),
%!   +Phi:compound
%! ) is semidet.

sat(AXs, Model, Alts, Phi):-
  Model:determine_values(Alts, Assignment),
  sat0(Assignment, Phi),
  debug_models(AXs, Alts, Phi).

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
