:- module(
  ac_build_model,
  [
    assert_model/4, % +Name:atom
                    % +Description:atom
                    % +Signature:list(pair(atom,list(integer))),
                    % +StructuralEquations:list(compound)
    assert_model/5, % +Name:atom
                    % +Description:atom
                    % +Signature:list(pair(atom,list(integer))),
                    % +StructuralEquations:list(compound)
                    % -Model:iri
    assign_value/1, % +Assignment:pair(iri,integer)
    run_with_assigned_values/2 % +Assignment:list(pair(iri,integer))
                               % :Goal
  ]
).

/** <module> AC: Build model

@author Wouter Beek
@tbd Causal formulas should include conjunction, negation, primitive event.
@version 2014/12-2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(lambda_meta)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(reification/rdf_reification_write)).

:- use_module(ac(ac_read)).

:- meta_predicate(run_with_assigned_values(+,0)).





%! assert_model(
%!   +Name:atom,
%!   +Description:atom,
%!   +Signature:list(pair(atom,list(integer))),
%!   +StructuralEquations:list(compound)
%! ) is det.

assert_model(Name, Description, Signature, StructuralEquations):-
  assert_model(Name, Description, Signature, StructuralEquations, _).

%! assert_model(
%!   +Name:atom,
%!   +Description:atom,
%!   +Signature:list(pair(atom,list(integer))),
%!   +StructuralEquations:list(compound),
%!   -Model:iri
%! ) is det.

assert_model(Name, Description, Signature, StructuralEquations, M):-
  % aco:Model
  rdf_create_next_resource(ac, [model], aco:'Model', ac, M),

  % rdfs:label
  rdfs_assert_label(M, Name, ac),

  % dcterms:description
  rdf_assert_simple_literal(M, dcterms:description, Description, ac),

  % aco:EndogenousVariable
  % aco:endogenous_variable
  % aco:possible_value
  maplist(assert_signature(M), Signature),

  % aco:structural_equation
  % aco:causes
  maplist(assert_structural_equation(M), StructuralEquations).



%! assert_signature(
%!   +Model:iri,
%!   +SignatureEntry:pair(atom,list(integer))
%! ) is det.

assert_signature(M, in(Name,Range0)):-
  % aco:EndogenousVariable
  % rdf:type
  rdf_create_next_resource(
    ac,
    [variable,endogenous],
    aco:'EndogenousVariable',
    ac,
    Var
  ),

  % aco:endogenous_variable
  rdf_assert(M, aco:endogenous_variable, Var, ac),

  % rdfs:label
  rdfs_assert_label(Var, Name, ac),

  % aco:possible_value
  Range0 = Low..High,
  rdf_bnode(Range),
  rdf_assert(Var, aco:range, Range, ac),
  rdf_assert_typed_literal(Range, aco:low, Low, xsd:integer, ac),
  rdf_assert_typed_literal(Range, aco:high, High, xsd:integer, ac).



%! assert_structural_equation(
%!   +Model:iri,
%!   +StructuralEquation:compound
%! ) is det.

assert_structural_equation(M, Eq):-
  Eq = #=(Left,Right),
  term_to_atom_subterms(Right, Rights),
  maplist(variable(M), [Left|Rights], [LeftVar|RightVars]),

  % aco:causes
  maplist(\RightVar^rdf_assert(RightVar, aco:causes, LeftVar, ac), RightVars),

  % aco:structural_equation
  with_output_to(atom(Eq0), write_canonical(Eq)),
  rdf_assert_simple_literal(LeftVar, aco:structural_equation, Eq0, ac).



%! assign_value(+Assignment:pair(iri,integer)) is det.

assign_value(Var-Val):-
  rdf_retractall(Var, aco:value, _),
  rdf_assert_typed_literal(Var, aco:value, Val, xsd:integer, ac).



%! run_with_assigned_values(+Assignment:list(pair(iri,integer)), :Goal) .

run_with_assigned_values(As, Goal):-
  findall(
    Var-OldVal,
    (
      member(Var-_, As),
      rdf_typed_literal(Var, aco:value, OldVal, xsd:integer)
    ),
    OldAs
  ),
  setup_call_cleanup(
    maplist(assign_value, As),
    Goal,
    maplist(assign_value, OldAs)
  ).





% HELPERS %

%! term_to_atom_subterms(+Term, -Atoms:ordset) is det.

term_to_atom_subterms(Term, Atoms):-
  aggregate_all(
    set(Atom),
    term_to_atom_subterm(Term, Atom),
    Atoms
  ).

%! term_to_atom_subterm(+Term, -Atom) is nondet.

term_to_atom_subterm(X, X):-
  atom(X), !.
term_to_atom_subterm(Term, Subterm):-
  Term =.. [_|Args],
  member(Arg, Args),
  term_to_atom_subterm(Arg, Subterm).
