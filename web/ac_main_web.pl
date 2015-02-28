:- module(ac_main_web, []).

/** <module> Web-based main page of Actual-Causation

@author Wouter Beek
@version 2014/12
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(plHtml(templates/menu_page)).

:- http_handler(/, ac_main_web, [prefix]).





ac_main_web(_):-
  reply_html_page(
    menu_page,
    [title('Actual-Causation')],
    ['Welcome to Actual Causation!']
  ).
