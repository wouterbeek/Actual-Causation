:- module(
  ac_read_sim,
  [
    models/6, % ?Model:iri
              % ?Context:ordset(pair(iri,integer))
              % ?CausalFormula:compound
              % ?Cause:ordset(iri)
              % ?CausalPath:ordset(iri)
              % ?Models:iri
    primitive_event/2 % ?AssignmentEntryPl:pair(iri,integer)
                      % ?AssignmentEntry:iri
  ]
).

/** <module> Actual Causation: Build simulation results

Simulation results stored in RDF.

@author Wouter Beek
@version 2015/01
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(term/rdf_list)).

:- use_module(ac(ac_read)).
:- use_module(ac(ac_trans)).





%! context(?ContextPl:ordset(pair(iri,integer)), ?Context:iri) is nondet.

context(Us, Context):-
  rdfs_individual_of(Context, aco:'Context'),
  rdf_list(Context, PrimitiveEvents),
  maplist(primitive_event, Us, PrimitiveEvents).



models(M, Us, Phi, Xs, Zs, Models):-
  % rdf:type
  % aco:Models
  rdfs_individual_of(Models, aco:'Models'),

  % aco:model
  rdf(M, aco:models, Models, ac),

  % aco:causal_formula
  rdf_simple_literal(Models, aco:causal_formula, Phi_atom),
  read_term_from_atom(Phi_atom, Phi_term, []),
  instantiate_term(M, var, Phi_term, Phi),

  % aco:context
  rdf(Models, aco:context, Context, ac),
  context(Us, Context),

  % aco:cause
  rdf(Models, aco:cause, Cause, ac),
  rdf_list(Cause, Xs),

  % aco:causal_path
  rdf(Models, aco:causal_path, CausalPath, ac),
  rdf_list(CausalPath, Zs).



%! primitive_event(
%!   ?AssignmentEntryPl:pair(iri,integer),
%!   ?AssignmentEntry:iri
%! ) is nondet.

primitive_event(Var-Val, Entry):-
  rdfs_individual_of(Entry, aco:'AssignmentEntry'),
  rdf(Entry, aco:entry_variable, Var, ac),
  rdf_typed_literal(Entry, aco:entry_value, Val, xsd:integer, ac).
