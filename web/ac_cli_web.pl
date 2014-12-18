:- module(ac_cli_web, []).

/** <module> Web-based UI to the CLI of Actual-Causation

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(plHttp(request_ext)).

:- use_module(plXml(xml_dom)).

:- use_module(plHtml(html_list)).

:- use_module(plServer(templates/menu_page)). % HTML template

:- use_module(plGraphDraw(svg_gv)).

:- use_module(ac(actual_causal_graph)).
:- use_module(ac(actual_causation)).

:- http_handler(ac(cli/model), cli_model, [prefix]).
:- http_handler(ac(cli/simulate), cli_simulate, [prefix]).





cli_model(Request):-
  request_to_model(Request, Model),
  reply_html_page(
    menu_page,
    [title('Actual-Causation CLI - Model')],
    [\description(Model),\context(Model),\causal_graph(Model)]
  ).

cli_simulate(Request):-
  request_to_model(Request, Model),
  reply_html_page(
    menu_page,
    [title('Actual-Causation CLI :: Causes')],
    [\description(Model),\context(Model),\causes(Model),\causal_graph(Model)]
  ).



% GRAMMAR %

assignment(Model, Var-Val) -->
  {Model:endogenous_variable(Var, Name, _, _)},
  html([Name,' = ',Val]).

causal_graph(Model) -->
  {
    causal_graph(Model, Graph),
    export_graph_to_svg_dom(Graph, SvgDom, [method(dot)])
  },
  html([
    h1(['Causal graph for model ',i(Model)]),
    \xml_dom_as_atom(SvgDom)
  ]).

causes(Model) -->
  {
    aggregate_all(
      set(Cause),
      cause(Model, Cause),
      Causes
    )
  },
  html([
    h1(['Causes of model ',i(Model)]),
    \html_list(Causes, cause(Model), [ordered(true)])
  ]).

cause(Model, As) -->
  html_list(As, assignment(Model), [ordered(false)]).

context(Model) -->
  {
    Model:context(Values),
    format(atom(Values0), '~w', [Values])
  },
  html([
    h1('Context'),
    p(Values0)
  ]).

description(Model) -->
  {Model:description(Desc)}, !,
  html([h1('Description'),p(Desc)]).
description(_) --> html([]).



% HELPERS %

request_to_model(Request, Model):-
  request_query_nvpair(Request, model, Model), !.
request_to_model(_, forest_fire).
