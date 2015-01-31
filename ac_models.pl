:- module(
  ac_models,
  [
    calculate_models/6 % +Model:iri
                       % ?Context:ordset(pair(iri,integer))
                       % ?CausalFormula:atom
                       % ?Cause:ordset(pair(iri,integer))
                       % -CausalPath:ordset(iri)
                       % -Models:iri
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
:- use_module(ac(ac_build_sim)).
:- use_module(ac(ac_calc)).
:- use_module(ac(ac_debug)).
:- use_module(ac(ac_read)).
:- use_module(ac(ac_trans)).

:- dynamic(cause0/4).





%! calculate_models(
%!   +Model:iri,
%!   ?Context:ordset(pair(iri,integer)),
%!   ?CausalFormula:atom,
%!   ?Cause:ordset(iri),
%!   -CausalPath:ordset(iri),
%!   -Models:iri
%! ) is nondet.

calculate_models(M, Us, Phi_atom, Xs, Zs, Models):-
  var(Phi_atom), !,
  once(rdf_simple_literal(M, aco:default_causal_formula, Phi_atom)),
  calculate_models(M, Us, Phi_atom, Xs, Zs, Models).
calculate_models(M, Us, Phi_atom, Xs, Zs, Models):-
  % @tbd Store causal formulas explicitly in RDF.
  read_term_from_atom(Phi_atom, Phi_term, []),
  % Replace the causal variable names with Prolog variables.
  instantiate_term(M, var, Phi_term, Phi),

  % NONDET.
  % Generate contexts for the given causal model.
  context(M, Us),

  % Reset cause memoization on a per-context basis.
  retractall(cause0(M, Us, Phi, _)),

  rdf_transaction(
    forall(
      % Set the context in the current database snapshot.
      calculate_models(M, Us, Phi, Xs, Zs),
      (
        % Store this result to ensure minimality of future results.
        assert(cause0(M, Us, Phi, Xs)),
        assert_models(M, Us, Phi_term, Xs, Zs, Models)
      )
    ),
    _,
    [snapshot(true)]
  ).

%! calculate_models(
%!   +Model:iri,
%!   +Context:ordset(pair(iri,integer)),
%!   +CausalFormula:compound,
%!   +Cause:ordset(iri),
%!   -CausalPath:ordset(iri)
%! ) is semidet.
%! calculate_models(
%!   +Model:iri,
%!   +Context:ordset(pair(iri,integer)),
%!   +CausalFormula:compound,
%!   -Cause:ordset(iri),
%!   -CausalPath:ordset(iri)
%! ) is nondet.

calculate_models(M, Us, Phi, Xs, Zs):-
  % Since we are now within an RDF transaction, we can assert the context.
  maplist(assign_value, Us),

  % The caused must be the case (Condition 1).
forall(rdf(X, aco:value, Y), format(user_output, '~w = ~w\n', [X,Y])),
  satisfy_formula(M, [], Phi),
gtrace,
forall(rdf(X, aco:value, Y), format(user_output, '~w = ~w\n', [X,Y])),
  debug_models(M, Us, [], Phi), % DEB

  % Calculate the value of the endogenous variables.
  calculate_all_values(M, AVs),
  % Collect the endogenous variables.
  pairs_keys(AVs, Vs),

  % It does not make sense for cause and caused to be the same,
  % so do not consider variables that occur in the causal formula.
  formula_to_variables(Phi, PhiVars),
  ord_subtract(Vs, PhiVars, Vs0),

  % NONDET.
  % Split the endogenous variables into:
  %   1. Those that constituting the causal path and
  %   2. those that are "off to the side" (condition 2).
  partition(Vs0, [A,B]),

  % Notice that the order in which partition members occur is arbitrary.
  % For each binary partition we therefore have two causal paths to try out.
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
  %
  % A cause must be the case (Condition 1).
  % This means that it must consist entirely of values from the real world.
  subset(Xs, Zs),

  % No subcause of a cause should be considered a cause
  % (condition 3: minimality).
  % Notice that smaller causes are considered first.
  \+ ((
    cause0(M, Us, Phi, Xs0),
    subset(Xs0, Xs)
  )),

  % A cause must be non-empty.
  Xs \== [],

  % NONDET (x2).
  % Construct a contingency under which the counterfactual
  % can be satisfied (Condition 2).
  assign_variables(Xs, AXs_contingent),
  assign_variables(Ws, AWs_contingent),

  % 2A:
  ord_union(AXs_contingent, AWs_contingent, Contingency1),
  satisfy_formula(M, Contingency1, not(Phi)),
  debug_models(M, Us, Contingency1, not(Phi)), % DEB

  % 2B:
  ord_subtract(Zs, Xs, ZsMinusXs),
  forall(
    (
      sublist(Ws_subset, Ws),
      sublist(ZsMinusXs_subset, ZsMinusXs)
    ),
    (
      subpairs(AWs_contingent, Ws_subset, Contingency2a),
      subpairs(AVs, ZsMinusXs_subset, Contingency2b),
      ord_union(Contingency2a, Contingency2b, Contingency2),
      debug_models(M, Us, Contingency2, Phi), % DEB
      satisfy_formula(M, Contingency2, Phi)
    )
  ).





% HELPERS %

%! assign_variables(
%!   +Variables:list(iri),
%!   -Assignment:list(pair(iri,integer))
%! ) is nondet.

assign_variables([], []).
assign_variables([Var|T1], [Var-Val|T2]):-
  % NONDET.
  potential_value(Var, Val),
  assign_variables(T1, T2).



%! context(+Model:iri, +Context:list(pair(iri,integer))) is semidet.
%! context(+Model:iri, -Context:list(pair(iri,integer))) is nondet.
% Succeeds for context of Model.
%
% A context is a possible assignment of values to
% all and only outer variables.

context(M, Us):-
  nonvar(Us), !,
  context0(M, Us), !.
context(M, Us):-
  context0(M, Us).

context0(M, Us):-
  aggregate_all(
    set(Var),
    outer_variable(M, Var),
    Vars
  ),
  assign_variables(Vars, Us).



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
