:- module(about_box, [about_box/1]).
:- use_module(library(http/html_write)).

about_box(T) :-
    message_to_string(about, P),
    append(P),
    phrase(html([p([img([src=':/swipl.png'],[]),
		    hr([]),
		    \[P],
		    p(table(\pqs))])]), Tokens),
    with_output_to(atom(T), print_html(Tokens)).

pqs --> html(Rows), {maplist(pqRow, [pqConsole, pqSource, pqGraphviz], Rows)}.

:- meta_predicate pqRow(1, -).
pqRow(P, tr([td(P), tr(D)])) :- call(P, D).

pqConsole('SWI-Prolog interface to Qt').
pqSource('SWI-Prolog source goodies').
pqGraphviz('RDF in QGraphicView via Graphviz').
