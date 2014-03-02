/** <module> termtree
 *
 *  make a graph window representation of a term
 *  --------
 *
 *  source file /home/carlo/cpp/loqt/pqGraphviz/prolog/termtree.pl
 *  created at Thu Feb 20 10:56:50 2014
 *
 *  @author carlo
 *  @version 0.9.9
 *  @copyright carlo
 *  @license LGPL v2.1
 */

:- module(termtree,
	[termtree/0
	,termtree/1
	]).

:- use_module(library(debug)).

termtree :-
	termtree('[a{a:1,b:[1,2,3],c:[]}]').

%% termtree(+A:atom) is det.
%
%  create a graph representation of term as read from A
%
%  @arg A atom with textual rappresentation of term to be displayed
%
termtree(A) :-
	catch((	read_term_from_atom(A, T, [variable_names(L)]),
		graph_window(graph_term(T-L), [])
	), E, ( print_message(error, E), fail )).

graph_term(T-Vars, G) :-
	debug(graph_term, '~w ~w', [T, Vars]),
	graph_term(G, Vars, T, _).

graph_term(G, Vars, T, N) :-
	(	compound(T)
	->	compound_name_arguments(T, Funct, Args),
		make_node_id(G, [label:Funct, shape:folder], N),
		maplist(graph_term(G, Vars), Args, ArgPs),
		maplist(new_edge(G, N), ArgPs)
	;	var(T)
	->	member(VName=V, Vars),
		V == T,
		make_node_id(G, [label:VName, shape:ellipse], N)
	;	term_to_atom(T, At),
		make_node_id(G, [label:At, shape:box], N)
	).

% link_arg(G, N, A) :- (G, N, A).
