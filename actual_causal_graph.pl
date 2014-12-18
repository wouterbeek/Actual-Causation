:- module(
  actual_causal_graph,
  [
    causal_graph/2 % +Model:atom
                   % -ExportGraph:compound
  ]
).

/** <module> Actual Causal Graph

Export graphs of actual causality.

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).

:- use_module(plGraph(l_graph/l_graph)).

:- use_module(plGraphDraw(build_export_graph)).





causal_graph(Mod, ExportGraph):-
  aggregate_all(
    set(edge(X,_,Y)),
    Mod:causal_link(X-Y),
    EndogenousLinks
  ),
  findall(
    Outer,
    (
      member(edge(Outer,_,_), EndogenousLinks),
      \+ member(edge(_,_,Outer), EndogenousLinks)
    ),
    Outers
  ),
  maplist(exogenous_link, Outers, ExogenousLinks),
  ord_union(EndogenousLinks, ExogenousLinks, Links),
  l_edges_vertices(Links, Vs),
  build_export_graph(Vs, Links, ExportGraph, []).

exogenous_link(X, edge(u,_,X)).
