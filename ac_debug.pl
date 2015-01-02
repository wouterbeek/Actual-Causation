:- module(
  ac_debug,
  [
    debug_models/3 % +Cause:list(pair)
                   % +AlternativeAssignments:ordset(pair)
                   % +CausalFormula:compound
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





%! debug_models(
%!   +Cause:list(pair(iri,integer)),
%!   +AlternativeAssignments:ordset(pair(iri,integer)),
%!   +CausalFormula:compound
%! ) is det.

debug_models(AXs, As, Phi):-
  debugging(ac), !,
  dcg_with_output_to(atom(Atom), models(AXs, As, Phi)),
  debug(ac, '~a', [Atom]).
debug_models(_, _, _, _).

assignment(Key-Value) -->
  {rdfs_label_value(Key, KeyLabel)},
  atom(KeyLabel),
  code_radix(hex('2190')),
  {rdfs_label_value(Value, ValueLabel)},
  atom(ValueLabel).

assignments([]) --> "".
assignments(L) -->
  bracketed(square, assignments0(L)).

assignments0([]) --> "".
assignments0([H]) --> !,
  assignment(H).
assignments0([H1,H2|T]) -->
  assignment(H1),
  ",",
  assignments0([H2|T]).

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
formula(Key-Value) -->
  {rdfs_label_value(Key, KeyLabel)},
  atom(KeyLabel),
  "=",
  {rdfs_label_value(Value, ValueLabel)},
  atom(ValueLabel).

models(AXs, As, Phi) -->
  assignments(AXs),
  " ",
  bracketed(langular, atom('M,u')),
  models,
  assignments(As),
  formula(Phi).
