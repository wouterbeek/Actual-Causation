:- module(billy_suzy_bottle, []).

/** <module> Actual Causation Model: Suzy & Billy

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(clpfd)).



% Causal formula.
causal_formula(bs-1).


% Causal links.
causal_link(bt-bh).
causal_link(bh-bs).
causal_link(sh-bh).
causal_link(st-sh).
causal_link(sh-bs).


% Context.
context([1,1]).


description('Suzy and Billy both pick up rocks and throw them at a bottle. \c
Suzy’s rock gets there first, shattering the bottle. \c
Since both throws are perfectly accurate, Billy’s would have shattered \c
the bottle had Suzy not thrown.').


%! determine_values(+PartialAssignment, -FullAssignment) is det.
% Returns a assignment for all endogenous variables that is
%   1. complete,
%   2. consistent with the structural equations,
%   3. consistent with the context, and,
%   4. consistent with the given partial assignment.

determine_values(As, [bh-BH,bs-BS,bt-BT,sh-SH,st-ST]):-
  context([U1,U2]),
  % Structural equations.
  % Notice that the given PartialAssignment can overrule the values of
  % exogenous variables (i.e., the context).
  % From the article: "(Note that here setting L and ML to 0 overrides
  % the effects of U; this is critical.)"
  (   memberchk(bt-BT, As)
  ->  true
  ;   BT #= U1
  ),
  (   memberchk(st-ST, As)
  ->  true
  ;   ST #= U2
  ),
  (   memberchk(bh-BH, As)
  ->  true
  ;   BH #= min(BT,1-SH)
  ),
  (   memberchk(sh-SH, As)
  ->  true
  ;   SH #= ST
  ),
  (   memberchk(bs-BS, As)
  ->  true
  ;   BS #= max(BH,SH)
  ).


% Endogenous varialbes: names and ranges.
endogenous_variable(bh, 'Patient has emphysema', 0, 1). %Billy hits bottle
endogenous_variable(bs, 'Petient has chronic bronchitis', 0, 1). %Bottle shatters
endogenous_variable(bt, 'Patient smokes', 0, 1). %Billy throws
endogenous_variable(sh, 'Negligence damages\npatient\'s lungs', 0, 1). %Suzy hits bottle
endogenous_variable(st, 'Doctor is negligent', 0, 1). %Suzy throws
