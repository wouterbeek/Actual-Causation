% Standalone startup file for Actual-Causation.


% Debug tools can be loaded by using the `--debug` flag in the console.
:- if(current_prolog_flag(argv, ['--debug'|_])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.


% Start a server that runs the Web interface.
:- use_module(load_project).
:- load_subproject(dh, plServer).

:- use_module(plServer(app_server)).
:- use_module(plServer(web_modules)). % Web module registration.

:- start_app_server_clas.


:- multifile(user:file_search_path/2).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).

:- multifile(http:location/3).
:- dynamic(http:location/3).

user:file_search_path(css, ac(web/css)).
user:file_search_path(js, ac(web/js)).

http:location(ac, /, []).

% AC: Main
:- use_module(ac(web/ac_main_web)).
user:web_module('AC', ac_main_web).

% AC: CLI
:- use_module(ac(web/ac_cli_web)).
user:web_module('AC CLI :: Model', cli_model).
user:web_module('AC CLI :: Simulation', cli_simulate).

% AC: GUI
:- use_module(ac(web/ac_gui_web)).
user:web_module('AC GUI', ac_gui_web).

user:current_html_style(menu_page).

% plTabular
:- use_module(plTabular(rdf_tabular)).
user:web_module(plTabular, rdf_tabular).

