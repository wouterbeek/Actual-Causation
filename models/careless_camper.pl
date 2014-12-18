:- module(careless_camper, []).

/** <module> Actual Causation Model: Trumping

@author Wouter Beek
@see Introduced by Schaffer 2000.
@version 2014/12
*/

:- use_module(library(clpfd)).



% Causal formula.
causal_formula(f2-1).


% Causal links.
causal_link(f1-c).
causal_link(c-f2).


% Context.
context([0]).


description('Suppose that the Careless Camper (CC for short) has plans to \c
go camping on the first weekend in June. He will go camping unless there is \c
a fire in the forest in May. If he goes camping, he will leave a campfire \c
unattended, and there will be a forest fire. Let the variable C take the \c
value 1 if CC goes camping, and 0 otherwise.').


%! determine_values(+PartialAssignment, -FullAssignment) is det.
% Returns a assignment for all endogenous variables that is
%   1. complete,
%   2. consistent with the structural equations,
%   3. consistent with the context, and,
%   4. consistent with the given partial assignment.

determine_values(As, [c-C,f1-F1,f2-F2]):-
  context([U1]),
  % Structural equations.
  % Notice that the given PartialAssignment can overrule the values of
  % exogenous variables (i.e., the context).
  % From the article: "(Note that here setting L and ML to 0 overrides
  % the effects of U; this is critical.)"
  (   memberchk(f1-F1, As)
  ->  true
  ;   F1 #= U1
  ),
  (   memberchk(c-C, As)
  ->  true
  ;   C #= 1 - F1
  ),
  (   memberchk(f2-F2, As)
  ->  true
  ;   F2 #= C * (1 - F1)
  ).


% Endogenous varialbes: names and ranges.
endogenous_variable(c, 'Patient starts smoking again', 0, 1). %Camper camps in June
endogenous_variable(f1, 'First operation fails', 0, 1). %Forest burns in May
endogenous_variable(f2, 'Second operation fails', 0, 1). %Forest burns in June
