:- module(ac_cli_web, []).

/** <module> Web-based UI to the CLI of Actual-Causation

Displays a model (description, causal graph, signature)
and its simulation results (causes and contexts).

---

@author Wouter Beek
@version 2014/12-2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdfs)).

:- use_module(plUri(uri_query)).

:- use_module(plHttp(request_ext)).

:- use_module(plXml(xml_dom)).

:- use_module(plHtml(html_pl_term)).
:- use_module(plHtml(elements/html_collection)).
:- use_module(plHtml(elements/html_list)).
:- use_module(plHtml(elements/html_table)).
:- use_module(plHtml(template/menu_page)). % HTML template

:- use_module(plGraphDraw(svg_gv)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_read)).

:- use_module(plRdfHtml(rdf_html_table)).
:- use_module(plRdfHtml(rdf_term_html)).

:- use_module(ac(ac_export)).
:- use_module(ac(ac_models)).
:- use_module(ac(ac_read)).
:- use_module(ac(ac_read_sim)).
:- use_module(ac(ac_trans)).

:- http_handler(ac(cli/model), cli_model, [prefix]).
:- http_handler(ac(cli/simulate), cli_simulate, [prefix]).





% Model: A specific model.
cli_model(Request):-
  request_query_nvpair(Request, model, M), !,
  once(rdfs_label_value(M, MLabel)),
  reply_html_page(
    menu_page,
    [title(['Actual-Causation CLI :: Model :: ',MLabel])],
    [\description(M),\causal_graph(M),\signature(M)]
  ).
% Model: Enumerate all models by default.
cli_model(_):-
  reply_html_page(
    menu_page,
    title('Actual-Causation CLI :: Model :: Overview'),
    ['Models:',\list_models(cli_model)]
  ).

% Simulate: A specific model.
cli_simulate(Request):-
  request_query_nvpair(Request, model, M), !,
  once(rdfs_label_value(M, MLabel)),
  reply_html_page(
    menu_page,
    [title('Actual-Causation CLI :: Simulate :: ',MLabel)],
    [
      \description(M),
      \causal_graph(M),
      \signature(M),
      \causes(M)
    ]
  ).
% Simulate: A specific model and a specific causal path.
cli_simulate(Request):-
  request_query_nvpair(Request, models, Models), !,
  rdf_has(M, aco:models, Models),
  once(rdfs_label_value(M, MLabel)),
  reply_html_page(
    menu_page,
    [title('Actual-Causation CLI :: Simulate :: ',MLabel)],
    [
      \description(M),
      \causal_path(Models),
      \signature(M),
      \causes(M)
    ]
  ).
% Simulate: Enumerate all models by default:
cli_simulate(_):-
  reply_html_page(
    menu_page,
    title('Actual-Causation CLI :: Simulate :: Overview'),
    \list_models(cli_simulate)
  ).





% GRAMMAR %

primitive_event(Var-Val) -->
  html([\var(Var),=,\val(Val)]).

causal_formula(and(Phi,Psi)) --> !,
  causal_formula(Phi),
  html(&(8743)),
  causal_formula(Psi).
causal_formula(not(Phi)) --> !,
  html(&(172)),
  causal_formula(Phi).
causal_formula(PrimitiveEvent) -->
  primitive_event(PrimitiveEvent).

causal_graph(M) -->
  {
    causal_graph(M, ExportG),
    export_graph_to_svg_dom(ExportG, Svg, [method(dot)])
  },
  html([
    h2('Causal graph'),
    \xml_dom_as_atom(Svg)
  ]).

causal_path(Models) -->
  {
    causal_path(Models, ExportG),
    export_graph_to_svg_dom(ExportG, Svg, [method(dot)])
  },
  html([
    h2('Causal path through causal graph'),
    \xml_dom_as_atom(Svg)
  ]).

causes(M) -->
  {
    aggregate_all(
      set([assignment(Us),vars(Xs),vars(Zs),uri(Location,'$@$')]),
      (
        models0(M, Us, Phi_atom, _, Xs, Zs, Models),
        http_location_by_id(cli_simulate, Base),
        uri_query_add_nvpair(Base, models, Models, Location)
      ),
      DataRows
    )
  },
  html(
    \html_table(
      html([
        'Contexts & Causes of ',
        \rdf_term_html(plTabular, M),
        &(8871),
        Phi_atom
        % @tbd Use `Phi` i.o. `Phi_atom`.
        %\causal_formula(Phi)
      ]),
      html_ac,
      [['Contexts','Cause','Causal path','Focus']|DataRows],
      [header_row(true),indexed(true)]
    )
  ).

models0(M, Us, Phi_atom, Phi, Xs, Zs, Models):-
  (   models(M, _, _, _, _, _)
  ->  true
  ;   calculate_models(M, Phi_atom, Phi, Xs)
  ),
  % NONDET.
  models(M, Us, Phi, Xs, Zs, Models).

description(M) -->
  {rdf_simple_literal(M, dcterms:description, Description, _)}, !,
  html([h2('Description'),p(Description)]).
description(_) --> html([]).

html_ac(assignment(L)) --> !,
  html_set(primitive_event, L).
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
      set([Var,Name,Low,High]),
      (
        rdf_has(M, aco:endogenous_variable, Var),
        rdfs_label_value(Var, Name),
        rdf_has(Var, aco:range, Range),
        rdf_typed_literal(Range, aco:low, Low, xsd:integer),
        rdf_typed_literal(Range, aco:high, High, xsd:integer)
      ),
      DataRows
    ),
    HeaderRow = ['Variable','Name','Low','High']
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
