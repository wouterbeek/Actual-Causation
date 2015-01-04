:- module(
  ac_read_sim,
  [
    assignment_entry/2, % ?AssignmentEntry:iri
                        % ?AssignmentEntryPl:pair(iri,integer)
    models/5
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





%! assignment(
%!   ?Assignment:iri,
%!   ?AssignmentPl:ordset(pair(iri,integer))
%! ) is nondet.

assignment(Assignment, As):-
  rdfs_individual_of(Assignment, aco:'Assignment'),
  rdf_list(Assignment, Entries),
  maplist(assignment_entry, Entries, As).



%! assignment_entry(
%!   ?AssignmentEntry:iri,
%!   ?AssignmentEntryPl:pair(iri,integer)
%! ) is nondet.

assignment_entry(Entry, Var-Val):-
  rdfs_individual_of(Entry, aco:'AssignmentEntry'),
  rdf(Entry, aco:entry_variable, Var, ac),
  rdf_typed_literal(Entry, aco:entry_value, Val, xsd:integer, ac).




models(M, Us, Phi, Xs, Zs):-
  % rdf:type
  % aco:Models
  rdfs_individual_of(Models, aco:'Models'),
  
  % aco:model
  rdf(M, aco:model, Models, ac),
  
  % aco:causal_formula
  rdf_simple_literal(Models, aco:causal_formula, Phi_atom),
  read_term_from_atom(Phi_atom, Phi_term, []),
  instantiate_term(M, var, Phi_term, Phi),
  
  % aco:context
  rdf(Models, aco:context, Context, ac),
  assignment(Context, Us),
  
  % aco:cause
  rdf(Models, aco:cause, Cause, ac),
  rdf_list(Cause, Xs),
  
  % aco:causal_path
  rdf(Models, aco:causal_path, CausalPath, ac),
  rdf_list(CausalPath, Zs).
