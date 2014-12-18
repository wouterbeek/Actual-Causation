:- module(forest_fire, []).

/** <module> Actual Causation Model: Forest fire

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(clpfd)).



% Causal formula.
causal_formula(f-1).


% Causal links.
causal_link(l-f).
causal_link(m-f).


% Context.
context([1,1]).


% Description.
description(aap).


%! determine_values(+PartialAssignment, -FullAssignment) is det.
% Returns a assignment for all endogenous variables that is
%   1. complete,
%   2. consistent with the structural equations,
%   3. consistent with the context, and,
%   4. consistent with the given partial assignment.

determine_values(As, [l-L,m-M,f-F]):-
  context([U1,U2]),
  % Structural equations.
  % Notice that the given PartialAssignment can overrule the values of
  % exogenous variables (i.e., the context).
  % From the article: "(Note that here setting L and ML to 0 overrides
  % the effects of U; this is critical.)"
  (   memberchk(l-L, As)
  ->  true
  ;   L #= U1
  ),
  (   memberchk(m-M, As)
  ->  true
  ;   M #= U2
  ),
  (   memberchk(f-F, As)
  ->  true
  ;   F #= max(L,M)
  ).


% Endogenous varialbes: names and ranges.
endogenous_variable(f, 'Forest fire', 0, 1).
endogenous_variable(l, 'Lightning strikes', 0, 1).
endogenous_variable(m, 'Match dropped', 0, 1).
