:- module(
  ac_models,
  [
    calculate_models/4 % +Model:iri
                       % ?CausalFormula1:atom
                       % ?CausalFormula2:atom
                       % ?Cause:ordset(pair(iri,integer))
  ]
).

/** <module> Actual causation

@author Wouter Beek
@see [http://arxiv.org/abs/1106.2652](Actual causation and the art of modeling)
@tbs Causal path restricts possible partitions.
@version 2014/12-2015/02
*/

:- use_module(library(apply)).
:- use_module(library(aggregate)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(generics/list_ext)).
:- use_module(plc(generics/pair_ext)).

:- use_module(plSet(set_theory)).

:- use_module(plRdf(api/rdf_read)).

:- use_module(ac(ac_build)).
:- use_module(ac(ac_build_sim)).
:- use_module(ac(ac_calc)).
:- use_module(ac(ac_debug)).
:- use_module(ac(ac_graph)).
:- use_module(ac(ac_read)).
:- use_module(ac(ac_trans)).

:- dynamic(models0/5).





%! calculate_models(
%!   +Model:iri,
%!   ?CausalFormula1:atom,
%!   ?CausalFormula2:atom,
%!   ?Cause:ordset(iri)
%! ) is det.

calculate_models(M, Phi_atom, Phi, Xs):-
  var(Phi_atom), !,
  once(rdf_simple_literal(M, aco:default_causal_formula, Phi_atom)),
  calculate_models(M, Phi_atom, Phi, Xs).
calculate_models(M, Phi_atom, Phi, Xs):-
  % @tbd Store causal formulas explicitly in RDF.
  read_term_from_atom(Phi_atom, Phi_term, []),
  % Replace the causal variable names with Prolog variables.
  instantiate_term(M, var, Phi_term, Phi),

  setup_call_cleanup(
    % Reset cause memoization on a per-context basis.
    retractall(models0(M, _, Phi, _, _, _)),
    % NONDET.
    % Generate contexts for the given causal model.
    forall(
      context(M, Us),
      (
        rdf_transaction(
          forall(
            % Set the context in the current database snapshot.
            calculate_models(M, Us, Phi, Xs, Zs),
            % Store this result to ensure minimality of future results.
            assertz(models0(M, Us, Phi, Xs, Zs))
          ),
          _,
          [snapshot(true)]
        )
      )
    ),
    forall(
      retract(models0(M, Us, Phi, Xs, Zs)),
      assert_models(M, Us, Phi_term, Xs, Zs)
    )
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
  satisfy_formula(M, [], Phi),
  debug_models(M, Us, [], Phi), % DEB

  % Calculate the value of the endogenous variables.
  calculate_all_values(M, AVs),
  % Collect the endogenous variables.
  pairs_keys(AVs, Vs),

  % NONDET
  % Find a potential cause.
  % The cause must belong to a *causal explanation*,
  % i.e., a collection of causal paths.
  causal_path(M, Phi, Xs, Zs),

  % No subcause of a cause should be considered a cause
  % (condition 3: minimality).
  % Notice that smaller causes are considered first.
  \+ ((
    models0(M, Us, Phi, Xs0, _),
    subset(Xs0, Xs)
  )),

  % A cause must be non-empty.
  Xs \== [],

  % The causal explanation splits the endogenous variables into:
  %   1. Those that constituting the causal explanation and
  %   2. those that are "off to the side" (condition 2).
  ord_subtract(Vs, Zs, Ws),

  % A cause must be the case (Condition 1).
  % This means that it must consist entirely of values from the real world.
  % THIS IS ALWAYS THE CASE!

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
