:- module(
  ac_build_sim,
  [
    assert_default_causal_formula/2, % +Model:iri
                                     % +CausalFormula:compound
    assert_models/6 % +Model:iri
                    % +Context:ordset(pair(iri,integer))
                    % +CausalFormula:compound
                    % +Cause:ordset(iri)
                    % +CausalPath:ordset(iri)
                    % -Models:iri
  ]
).

/** <module> Actual Causation: Build simulation results

Simulation results stored in RDF.

@author Wouter Beek
@version 2015/01
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(term/rdf_list)).

:- use_module(ac(ac_read_sim)).

:- rdf_meta(assert_assignment0(+,r,+,-)).
:- rdf_meta(assert_vars0(+,r,+,-)).





assert_cause(Models, Xs):-
  assert_vars0([cause], aco:'Cause', Xs, Cause),
  rdf_assert(Models, aco:cause, Cause, ac).



assert_causal_formula(Models, Phi):-
  assert_causal_formula0(Models, Phi, false).



assert_causal_path(Models, Zs):-
  assert_vars0([causal_path], aco:'CausalPath', Zs, CausalPath),
  rdf_assert(Models, aco:causal_path, CausalPath, ac).



assert_context(Models, Us):-
  assert_assignment0([context], aco:'Context', Us, Context),
  rdf_assert(Models, aco:context, Context, ac).



assert_default_causal_formula(M, Phi):-
  assert_causal_formula0(M, Phi, true).



%! assert_models(
%!   +Model:iri,
%!   +Context:ordset(pair(iri,integer)),
%!   +Phi:compound,
%!   +Cause:ordset(iri),
%!   +CausalPath:ordset(iri),
%!   -Models:iri
%! ) is det.

assert_models(M, Us, Phi, Xs, Zs, Models):-
  models(M, Us, Phi, Xs, Zs, Models), !.
assert_models(M, Us, Phi, Xs, Zs, Models):-
  rdf_create_next_resource(ac, [models], aco:'Models', ac, Models),
  rdf_assert(M, aco:models, Models, ac),
  assert_context(Models, Us),
  assert_causal_formula(Models, Phi),
  assert_cause(Models, Xs),
  assert_causal_path(Models, Zs).





% HELPERS %

%! assert_vars0(
%!   +SubPaths:list(atom),
%!   +Class:iri,
%!   +Vs:ordset(iri),
%!   -Variables:iri
%! ) is det.

assert_vars0(SubPaths, Class, Vs, Vars):-
  rdf_create_next_resource(ac, SubPaths, Class, ac, Vars),
  rdf_assert_list(Vs, Vars, ac, []).



%! assert_assignment0(
%!   +SubPaths:list(atom),
%!   +Class:iri,
%!   +As:ordset(pair(iri,integer)),
%!   -Assignment:iri
%! ) is det.

assert_assignment0(SubPaths, Class, As, Assignment):-
  rdf_create_next_resource(ac, SubPaths, Class, ac, Assignment),
  maplist(assert_assignment_entry, As, Elements),
  rdf_assert_list(Elements, Assignment, ac, []).



%! assert_assignment_entry(
%!   +AssignmentEntryPl:pair(iri,integer),
%!   -AssignmentEntry:iri
%! ) is det.

assert_assignment_entry(Var-Val, Entry):-
  primitive_event(Var-Val, Entry), !.
assert_assignment_entry(Var-Val, Entry):-
  rdf_create_next_resource(
    ac,
    [primitive_event],
    aco:'AssignmentEntry',
    ac,
    Entry
  ),
  rdf_assert(Entry, aco:entry_variable, Var, ac),
  rdf_assert_typed_literal(Entry, aco:entry_value, Val, xsd:integer, ac).



assert_causal_formula0(S, Phi, Default):-
  with_output_to(atom(Phi0), write_canonical(Phi)),
  (   Default == true
  ->  rdf_global_id(aco:default_causal_formula, P)
  ;   rdf_global_id(aco:causal_formula, P)
  ),
  rdf_assert_simple_literal(S, P, Phi0, ac).