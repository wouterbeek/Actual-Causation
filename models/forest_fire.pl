:- module(
  forest_fire,
  [
    causal_formula/1,
    causal_link/1,
    context/1,
    determine_values/2,
    range/3
  ]
).

/** <module> Actual Causation Model: Forest fire

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(clpfd)).





% Causal formula.
causal_formula([f-1]).



% Causal links.
causal_link(l-f).
causal_link(m-f).



% Context.
context([1,1]).



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
  F #= max(L,M).



% Ranges.
range(f, 0, 1).
range(l, 0, 1).
range(m, 0, 1).
