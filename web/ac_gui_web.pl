:- module(webqr_gui_web, []).

/** <module> Web-based GUI for WebQR

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(plHtml(templates/menu_page)). % HTML template.

:- http_handler(ac(gui), ac_gui_web, []).





ac_gui_web(_):-
  reply_html_page(menu_page, title('Actual-Causation'), p('TODO')).
