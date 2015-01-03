:- module(
  ac_debug,
  [
    debug_models/4 % +Model:iri
                   % +Context:list(pair(iri,integer))
                   % +Assignment:ordset(pair(iri,integer))
                   % +Formula:compound
  ]
).

/** <module> Actual Causation: Debug tools

@author Wouter Beek
@version 2014/12-2015/01
*/

:- use_module(library(debug)).
:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plDcg(dcg_atom)).
:- use_module(plDcg(dcg_bracket)).
:- use_module(plDcg(dcg_code)).
:- use_module(plDcg(dcg_generics)).
:- use_module(plDcg(dcg_unicode)).

:- use_module(plRdf(api/rdfs_read)).





%! assignment(+Assignment:list(pair(iri,integer)))// is det.

assignment([]) --> "".
assignment(As) -->
  bracketed(square, assignment0(As)).

assignment0([]) --> "".
assignment0([A1]) --> !,
  assignment_entry(A1).
assignment0([A1,A2|As]) -->
  assignment_entry(A1),
  ",",
  assignment0([A2|As]).



%! assignment_entry(+AssignmentEntry:pair(iri,integer))// is det.

assignment_entry(Var-Val) -->
  {rdfs_label_value(Var, VarLabel)},
  atom(VarLabel),
  code_radix(hex('2190')),
  {rdfs_label_value(Val, ValLabel)},
  atom(ValLabel).



%! debug_models(
%!   +Model:iri,
%!   +Context:list(pair(iri,integer)),
%!   +Assignment:ordset(pair(iri,integer)),
%!   +Formula:compound
%! ) is det.

debug_models(M, Us, As, Phi):-
  debugging(ac), !,
  dcg_with_output_to(atom(Atom), models(M, Us, As, Phi)),
  debug(ac, '~a', [Atom]).
debug_models(_, _, _, _).



%! formula(+Formula:compound)// is det.

formula(not(Phi)) --> !,
  code_radix(hex('00AC')),
  formula(Phi).
formula(and(Phi1,Phi2)) --> !,
  bracketed((
    formula(Phi1),
    code_radix(hex('2227')),
    formula(Phi2)
  )).
formula(or(Phi1,Phi2)) --> !,
  bracketed((
    formula(Phi1),
    code_radix(hex('2228')),
    formula(Phi2)
  )).
formula(Var-Val) -->
  {rdfs_label_value(Var, VarLabel)},
  atom(VarLabel),
  "=",
  {rdfs_label_value(Val, ValLabel)},
  atom(ValLabel).



%! models(
%!   +Model:iri,
%!   +Context:list(pair(iri,integer)),
%!   +Assignment:ordset(pair(iri,integer)),
%!   +Formula:compound
%! )// is det.

models(M, Us, As, Phi) -->
  bracketed(langular, model_context0(M, Us)),
  models,
  assignment(As),
  formula(Phi).

model_context0(M, Us) -->
  {once(rdfs_label_value(M, MLabel))},
  atom(MLabel),
  ",",
  assignment(Us).
