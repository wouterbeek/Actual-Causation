:- module(ac_cli_web, []).

/** <module> Web-based UI to the CLI of Actual-Causation

@author Wouter Beek
@version 2014/12-2015/01
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdfs)).

:- use_module(plUri(uri_query)).

:- use_module(plHttp(request_ext)).

:- use_module(plXml(xml_dom)).

:- use_module(plHtml(html_collection)).
:- use_module(plHtml(html_list)).
:- use_module(plHtml(html_pl_term)).
:- use_module(plHtml(html_table)).

:- use_module(plServer(templates/menu_page)). % HTML template

:- use_module(plGraphDraw(svg_gv)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_read)).

:- use_module(plTabular(rdf_html_table)).
:- use_module(plTabular(rdf_term_html)).

:- use_module(ac(ac_export)).
:- use_module(ac(ac_models)).
:- use_module(ac(ac_read)).

:- http_handler(ac(cli/model), cli_model, [prefix]).
:- http_handler(ac(cli/simulate), cli_simulate, [prefix]).





cli_model(Request):-
  request_query_nvpair(Request, model, M), !,
  once(rdfs_label_value(M, MLabel)),
  reply_html_page(
    menu_page,
    [title(['Actual-Causation CLI :: Model :: ',MLabel])],
    [\description(M),\causal_graph(M),\signature(M)]
  ).
cli_model(_):-
  reply_html_page(
    menu_page,
    title('Actual-Causation CLI :: Model :: Overview'),
    \list_models(cli_model)
  ).

cli_simulate(Request):-
  request_query_nvpair(Request, model, M), !,
  once(rdfs_label_value(M, MLabel)),
  reply_html_page(
    menu_page,
    [title('Actual-Causation CLI :: Simulate :: ',MLabel)],
    [\description(M),\causal_graph(M),\signature(M),\causes(M)]
  ).
cli_simulate(_):-
  reply_html_page(
    menu_page,
    title('Actual-Causation CLI :: Simulate :: Overview'),
    \list_models(cli_simulate)
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
    h2('Causal graph'),
    \xml_dom_as_atom(Svg)
  ]).

causes(M) -->
  {
gtrace,
    aggregate_all(
      set([assignment(Us),vars(Xs)]),
      models(M, Us, _, Xs),
      DataRows
    )
  },
  html(
    \html_table(
      html(['Contexts & Causes of model ',\rdf_term_html(plTabular, M)]),
      html_ac,
      [['Contexts','Cause']|DataRows],
      [header_row(true),indexed(true)]
    )
  ).

description(M) -->
  {rdf_simple_literal(M, dcterms:description, Description, _)}, !,
  html([h2('Description'),p(Description)]).
description(_) --> html([]).

html_ac(assignment(L)) --> !,
  html_set(assignment_entry, L).
html_ac(vars(L)) --> !,
  html_set(var, L).
html_ac(Term) -->
  html_pl_term(Term).

list_models(HandlerId) -->
  {
    aggregate_all(
      set(uri(Location,MLabel)),
      (
        rdfs_individual_of(M, aco:'Model'),
        http_location_by_id(HandlerId, Base),
        uri_query_add_nvpair(Base, model, M, Location),
        once(rdfs_label_value(M, MLabel))
      ),
      Links
    )
  },
  html_list(Links).

signature(M) -->
  {
    aggregate_all(
      set([Var,Low,High]),
      (
        rdf_has(M, aco:endogenous_variable, Var),
        rdf_has(Var, aco:range, Range),
        rdf_typed_literal(Range, aco:low, Low, xsd:integer),
        rdf_typed_literal(Range, aco:high, High, xsd:integer)
      ),
      DataRows
    ),
    HeaderRow = ['Variable','Low','High']
  },
  rdf_html_table(
    html(['Signature of model ',\rdf_term_html(plTabular, M)]),
    [HeaderRow|DataRows],
    [graph(ac),header_row(true),indexed(true),location(plTabular)]
  ).

val(Val) -->
  html(span(class=val,\html_pl_term(integer(Val)))).

var(Var) -->
  {once(rdfs_label_value(Var, VarLabel))},
  html(span(class=var,VarLabel)).
