:- module(ac_cli_web, []).

/** <module> Web-based UI to the CLI of Actual-Causation

@author Wouter Beek
@version 2014/12-2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(plHttp(request_ext)).

:- use_module(plXml(xml_dom)).

:- use_module(plHtml(html_pl_term)).
:- use_module(plHtml(html_table)).
:- use_module(plHtml(html_tuple)).

:- use_module(plServer(templates/menu_page)). % HTML template

:- use_module(plGraphDraw(svg_gv)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_read)).

:- use_module(ac(ac_read)).
:- use_module(ac(actual_causal_graph)).
:- use_module(ac(actual_causation)).
:- use_module(ac(models/ac_test_models)).

:- http_handler(ac(cli/model), cli_model, [prefix]).
:- http_handler(ac(cli/simulate), cli_simulate, [prefix]).





cli_model(Request):-
  request_to_model(Request, M),
  once(rdfs_label_value(M, MLabel)),
  reply_html_page(
    menu_page,
    [title(['Actual-Causation CLI - Model - ',MLabel])],
    [\description(M),\causal_graph(M)]
  ).

cli_simulate(Request):-
  request_to_model(Request, M),
  reply_html_page(
    menu_page,
    [title('Actual-Causation CLI :: Causes')],
    [\description(M),\causal_graph(M),\causes(M)]
  ).





% GRAMMAR %

assignment_entry(Var-Val) -->
  html([\var(Var),=,\val(Val)]).

causal_graph(M) -->
  {
    causal_graph(M, ExportG),
    export_graph_to_svg_dom(ExportG, Svg, [method(dot)])
  },
  html([
    h1('Causal graph'),
    \xml_dom_as_atom(Svg)
  ]).

causes(M) -->
  {
    variable(M, match, VarM),
    variable(M, lightning, VarL),
    Us = [VarL-1,VarM-1],
    variable(M, fire, VarF),
    aggregate_all(
      set([assignment(Us),vars(Xs)]),
      models(M, Us, VarF-1, Xs),
      DataRows
    )
  },
  html([
    h1('Contexts & Causes'),
    \html_table(
      html('Contexts & Causes'),
      html_ac,
      [['Contexts','Cause']|DataRows],
      [header_row(true),indexed(true)]
    )
  ]).

description(M) -->
  {rdf_plain_literal(M, dcterms:description, Description, _)}, !,
  html([h1('Description'),p(Description)]).
description(_) --> html([]).

html_ac(assignment(L)) --> !,
  html_tuple(assignment_entry, L).
html_ac(vars(L)) --> !,
  html_tuple(var, L).
html_ac(Term) -->
  html_pl_term(Term).

val(Val) -->
  html(span(class=val,\html_pl_term(integer(Val)))).

var(Var) -->
  {once(rdfs_label_value(Var, VarLabel))},
  html(span(class=var,VarLabel)).





% HELPERS %

request_to_model(Request, M):-
  request_query_nvpair(Request, model, M), !.
request_to_model(_, M):-
  test_model(forest_fire, M).
