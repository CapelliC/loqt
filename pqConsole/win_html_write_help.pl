:- module(win_html_write_help, [win_html_write_help/1]).
:- use_module(library(http/html_write)).

win_html_write_help(D) :-
	phrase(html(D), Tokens),
    with_output_to(atom(X), print_html(Tokens)),
    win_html_write(X).
