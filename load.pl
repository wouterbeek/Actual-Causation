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
     plServer,
     plSet,
     plSvg,
     plUri,
     plXml
   ]).
