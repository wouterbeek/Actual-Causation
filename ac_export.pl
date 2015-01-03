:- module(
  ac_export,
  [
    causal_graph/2 % +Model:iri
                   % -ExportGraph:compound
  ]
).

/** <module> Actual Causal Graph

Export graphs of actual causality.

@author Wouter Beek
@version 2014/12-2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plGraph(l_graph/l_graph)).

:- use_module(plGraphDraw(build_export_graph)).

:- use_module(plRdf(api/rdfs_read)).

:- rdf_meta(ac_vertex_label(r,-)).
:- rdf_meta(exogenous_edge(+,t)).





causal_graph(M, ExportG):-
  % 1: Endogenous edges.
  aggregate_all(
    set(edge(X,_,Y)),
    (
      rdf_has(X, aco:causes, Y),
      rdf_has(M, aco:endogenous_variable, X)
    ),
    EndogenousEs
  ),
  
  % 2: Exogenous edges.
  % Take the *set* here, because an outer node may link to multiple
  % (non-outer) nodes.
  aggregate_all(
    set(Outer),
    (
      member(edge(Outer,_,_), EndogenousEs),
      \+ member(edge(_,_,Outer), EndogenousEs)
    ),
    Outers
  ),
  maplist(exogenous_edge, Outers, ExogenousEs),
  
  % Edges -> vertices.
  ord_union(EndogenousEs, ExogenousEs, Es),
  l_edges_vertices(Es, Vs),
  
  build_export_graph(
    Vs,
    Es,
    ExportG,
    [
      graph_directed(true),
      vertex_label(ac_vertex_label)
    ]
  ).

exogenous_edge(X, edge(aco:exogenous,_,X)).

ac_vertex_label(aco:exogenous, 'Exogenous'):- !.
ac_vertex_label(V, VLabel):-
  rdfs_label_value(V, VLabel), !.
ac_vertex_label(_, nolabel).