% The load file for the Actual-Causation project.

:- dynamic(user:project/3).
:- multifile(user:project/3).
user:project('Actual-Causation', 'Modeling tool for Actual Causation.', ac).

:- use_module(load_project).
:- load_project([
     plc-'Prolog-Library-Collection',
     plDcg,
     plGraph,
     plGraphDraw,
     plGraphViz,
     plHtml,
     plHttp,
     plLangTag,
     plLattice,
     plLatticeDraw,
     plRdf,
     plServer,
     plSet,
     plSvg,
     plTabular,
     plTms,
     plTree,
     plTreeDraw,
     plUri,
     plXml,
     plXsd
   ]).

:- use_module(library(semweb/rdf_db)).
:- rdf_register_prefix(ac, 'http://ac.org/resource/').
:- rdf_register_prefix(aco, 'http://ac.org/ontology/').
