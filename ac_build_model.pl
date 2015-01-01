:- module(
  ac_build_model,
  [
    assert_model/4 % +Name:atom
                   % +Description:atom
                   % +StructuralEquations:list(compound)
                   % +CausalFormula:pair(atom,integer)
  ]
).

/** <module> AC: Build model

@author Wouter Beek
@version 2014/12-2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(reification/rdf_reification_write)).

:- rdf_register_prefix(ac, 'http://ac.org/resource/').
:- rdf_register_prefix(aco, 'http://ac.org/ontology/').





%! assert_causal_formula(+CausalFormula:pair(atom,integer)) is det.

assert_causal_formula(Name-Val):-
  once(rdfs_label_value(Var, Name, _, ac)),
  rdf_assert_typed_literal(Var, ac:causal_formula, Val, xsd:integer, ac).



%! assert_causal_link(+Left:iri, +Right:iri) is det.

assert_causal_link(Left, Right):-
  rdf_assert(Left, ac:causes, Right, ac).



%! assert_endogenous_variable(+Model:iri, +Name:atom, -Var:iri) is det.

assert_endogenous_variable(Model, Name, Var):-
  rdf_create_next_resource(
    ac,
    [variable,endogenous],
    aco:'EndogenousVariable',
    ac,
    Var
  ),
  rdfs_assert_label(Var, Name, ac),
  rdf_assert(Model, ac:endogenous_variable, Var, ac).



%! assert_model(
%!   +Name:atom,
%!   +Description:atom,
%!   +StructuralEquations:list(compound),
%!   +CausalFormula:pair(atom,integer)
%! ) is det.

assert_model(Name, Description, StructuralEquations, CausalFormula):-
  % ac:Model
  rdf_create_next_resource(ac, [model], aco:'Model', ac, Model),

  % rdfs:label
  rdfs_assert_label(Model, Name, ac),

  % dcterms:description
  rdf_assert_plain_literal(Model, dcterms:description, Description, ac),

  % ac:endogenous_variable
  % ac:EndogenousVariable
  % ac:structural_equation
  maplist(assert_structural_equation(Model), StructuralEquations),

  % ac:causal_formula
  assert_causal_formula(CausalFormula).



%! assert_structural_equation(
%!   +Model:iri,
%!   +StructuralEquation:compound
%! ) is det.

assert_structural_equation(Model, Eq):-
  Eq = #=(Left,Right),
  term_to_atom_subterms(Right, Rights),

  % ac:endogenous_variable
  % ac:EndogenousVariable
  maplist(
    assert_endogenous_variable(Model),
    [Left|Rights],
    [LeftVar|RightVars]
  ),

  % ac:causal_link
  maplist(assert_causal_link(LeftVar), RightVars),

  % ac:structural_equation
  with_output_to(atom(Eq0), write_canonical(Eq)),
  rdf_assert_simple_literal(LeftVar, ac:structural_equation, Eq0, ac).





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
