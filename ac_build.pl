:- module(
  ac_build_model,
  [
    assert_model/5, % +Name:atom
                    % +Description:atom
                    % +Signature:list(pair(atom,list(integer))),
                    % +StructuralEquations:list(compound)
                    % -Model:iri
    assign_value/1 % +Assignment:pair(iri,integer)
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
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(lambda_meta)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(reification/rdf_reification_write)).

:- use_module(ac(ac_read)).





%! assert_model(
%!   +Name:atom,
%!   +Description:atom,
%!   +Signature:list(pair(atom,list(integer))),
%!   +StructuralEquations:list(compound),
%!   -Model:iri
%! ) is det.

assert_model(Name, Description, Signature, StructuralEquations, M):-
  % ac:Model
  rdf_create_next_resource(ac, [model], aco:'Model', ac, M),

  % rdfs:label
  rdfs_assert_label(M, Name, ac),

  % dcterms:description
  rdf_assert_plain_literal(M, dcterms:description, Description, ac),
  
  % ac:EndogenousVariable
  % ac:endogenous_variable
  % ac:possible_value
  maplist(assert_signature(M), Signature),
  
  % ac:structural_equation
  % ac:causes
  maplist(assert_structural_equation(M), StructuralEquations).



%! assert_signature(
%!   +Model:iri,
%!   +SignatureEntry:pair(atom,list(integer))
%! ) is det.

assert_signature(M, Name-Vals):-
  % ac:EndogenousVariable
  % rdf:type
  rdf_create_next_resource(
    ac,
    [variable,endogenous],
    aco:'EndogenousVariable',
    ac,
    Var
  ),
  
  % ac:endogenous_variable
  rdf_assert(M, ac:endogenous_variable, Var, ac),
  
  % rdfs:label
  rdfs_assert_label(Var, Name, ac),
  
  % ac:possible_value
  maplist(
    \Val^rdf_assert_typed_literal(
      Var,
      ac:possible_value,
      Val,
      xsd:integer,
      ac
    ),
    Vals
  ).



%! assert_structural_equation(
%!   +Model:iri,
%!   +StructuralEquation:compound
%! ) is det.

assert_structural_equation(M, Eq):-
  Eq = #=(Left,Right),
  term_to_atom_subterms(Right, Rights),
  maplist(variable(M), [Left|Rights], [LeftVar|RightVars]),
  
  % ac:causes
  maplist(\RightVar^rdf_assert(RightVar, ac:causes, LeftVar, ac), RightVars),

  % ac:structural_equation
  with_output_to(atom(Eq0), write_canonical(Eq)),
  rdf_assert_simple_literal(LeftVar, ac:structural_equation, Eq0, ac).



%! assign_value(+Assignment:pair(iri,integer)) is det.

assign_value(Var-Val):-
  rdf_retractall(Var, ac:value, _),
  rdf_assert_typed_literal(Var, ac:value, Val, xsd:integer, ac).





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
