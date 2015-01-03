:- module(
  ac_graph,
  [
    has_causal_path/4 % +Model:iri
                      % +Cause:ordset(iri)
                      % +Caused:ordset(iri)
                      % -Causal:ordset(iri)
  ]
).

/** <module> Actual Causation: Graph theory

Graph-theoretic predicates used for calculating actual causation.

@author Wouter Beek
@version 2014/12-2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).

:- use_module(plGraph(graph_walk)).





%! has_causal_path(
%!   +Model:atom,
%!   +CauseVars:ordset,
%!   +CausedVars:ordset,
%!   -PathVars:ordset
%! ) is det.
% A causal path must adhere to the following conditions:
%   1. It must end in all and only caused variables.
%   2. It must start in all and only cause variables.
%   3. No superfluous or non-traversed variables are included.

has_causal_path(Model, Xs, PhiVars, Zs):-
  aggregate_all(
    set(Path),
    (
      member(X, Xs),
      member(Y, PhiVars),
      path(
        ca_vertex,
        ca_edge,
        ca_neighbor,
        Model,
        Y,
        X,
        Path
      )
    ),
    Paths
  ),
  maplist(path_to_vertices, Paths, Vss),
  ord_union(Vss, Zs).

path_to_vertices([_,_|T], Vs):-
  path_to_vertices0(T, Vs0),
  list_to_ord_set(Vs0, Vs).

path_to_vertices0([], []).
path_to_vertices0([H], [H]):- !.
path_to_vertices0([V,_|T1], [V|T2]):-
  path_to_vertices0(T1, T2).

ca_vertex(Model, V):-
  Model:causal_link(V-_).
ca_vertex(Model, V):-
  Model:causal_link(_-V).

ca_edge(Model, V-W):-
  Model:causal_link(W-V).

ca_neighbor(Model, V, W):-
  ca_edge(Model, V-W).
