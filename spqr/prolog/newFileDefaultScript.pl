/* Prolog source %1
 * created at %2
 */
:- module(%3, [%3/0]).

:- use_module(spqr(gv_uty)).

% entry point
%3 :- graph_window(%3(G), G, [window_title(hello)]).

%% %3(+G) is det.
%  build a dummy graph
%3(G) :-
        make_node(G, hello, H),
        make_node(G, world, W),
        new_edge(G, H, W).
