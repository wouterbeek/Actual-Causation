:- module(
  ac_debug,
  [
    debug_models/2 % +AlternativeAssignments:ordset(pair)
                   % +Phi:compound
  ]
).

/** <module> Actual Causation: Debug tools

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(debug)).
:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plDcg(dcg_atom)).
:- use_module(plDcg(dcg_bracket)).
:- use_module(plDcg(dcg_code)).
:- use_module(plDcg(dcg_generics)).
:- use_module(plDcg(dcg_unicode)).





%! debug_models(+AlternativeAssignments:ordset(pair), +Phi:compound) is det.

debug_models(As, Phi):-
  debugging(ac), !,
  dcg_with_output_to(atom(Atom), models(As, Phi)),
  debug(ac, '~a', [Atom]).
debug_models(_, _, _, _).

assignment(Key-Value) -->
  atom(Key),
  code_radix(hex('2190')),
  atom(Value).

assignments([]) --> "".
assignments(L) -->
  bracketed(square, assignments0(L)).

assignments0([]) --> "".
assignments0([H|T]) -->
  assignment(H),
  assignments0(T).

formula(not(Phi)) --> !,
  code_radix(hex('00AC')),
  formula(Phi).
formula(and(Phi1,Phi2)) --> !,
  formula(Phi1),
  code_radix(hex('2227')),
  formula(Phi2).
formula(or(Phi1,Phi2)) --> !,
  formula(Phi1),
  code_radix(hex('2228')),
  formula(Phi2).
formula(Key-Value) -->
  atom(Key),
  "=",
  atom(Value).

models(As, Phi) -->
  bracketed(langular, atom('M,u')),
  models,
  assignments(As),
  formula(Phi).
