:- module(trumping, []).

/** <module> Actual Causation Model: Trumping

@author Wouter Beek
@see Introduced by Schaffer 2000.
@version 2014/12
*/

:- use_module(library(clpfd)).



% Causal formula.
causal_formula(p-1).


% Causal links.
causal_link(d-p).
causal_link(y-p).


% Context.
context([1,0]).


description('Suppose that a group of soldiers is very well trained, \c
so that they will obey any order given by a superior officer; \c
in the case of conflicting orders, they obey the highest-ranking officer. \c
Both a sergeant and a major issue the order to march, and the soldiers \c
march. \c
In the case of conflicting orders, the soldiers obey the major.').


%! determine_values(+PartialAssignment, -FullAssignment) is det.
% Returns a assignment for all endogenous variables that is
%   1. complete,
%   2. consistent with the structural equations,
%   3. consistent with the context, and,
%   4. consistent with the given partial assignment.

determine_values(As, [d-D,p-P,y-Y]):-
  context([U1,U2]),
  % Structural equations.
  % Notice that the given PartialAssignment can overrule the values of
  % exogenous variables (i.e., the context).
  % From the article: "(Note that here setting L and ML to 0 overrides
  % the effects of U; this is critical.)"
  (   memberchk(d-D, As)
  ->  true
  ;   D #= U1
  ),
  (   memberchk(y-Y, As)
  ->  true
  ;   Y #= U2
  ),
  (   memberchk(p-P, As)
  ->  true
  ;   (   D =:= 0
      ->  P #= 0
      ;   D =:= 2,
          Y =:= 0
      ->  P #= 0
      ;   P #= 1
      )
  ).


% Endogenous varialbes: names and ranges.
endogenous_variable(d, 'Doctor\'s advise', 0, 2).
endogenous_variable(p, 'Patient undergoes treatment', 0, 1).
endogenous_variable(y, 'Quack\'s advise', 0, 2).
