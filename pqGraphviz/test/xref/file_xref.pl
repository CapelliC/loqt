/** <module> file_xref
 *	testbed for preliminary library(prolog_xref) usage
 *
 *  @file /home/carlo/prolog/file_xref.pl, created at Fri Nov 8 10:35:36 2013
 *  @user carlo
 *
 *  library xref does a great job analyzing call graph and inclusions,
 *  here I'm just trying to get that represented by means
 *  of graphviz + html, showcasing integration pqConsole/meta Qt.
 */

:- module(file_xref,
	[file_xref/0
	,file_inclusions_graph/1
	,file_inclusions_simple/1
	]).

:- use_module(library(prolog_xref)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(debug)).

:- use_module(spqr(gv_uty)).
:- use_module(paths_prefix).

:- rdf_register_prefix(fx, 'http://www.swi-prolog.org/file_xref#').
:- meta_predicate show_inclusions(1).

%%	pqSourceFileXref is det
%
%	use this file as basic test case
%
file_xref :-
	show_inclusions(file_inclusions_graph).
%file_xref :-
%	show_inclusions(file_inclusions_simple).
    
show_inclusions(Pred) :-
	module_property(file_xref, file(F)),
	call(Pred, F).
    

%%	file_inclusions_graph(+File) is det
%
%	display some graphs as built from library(prolog_xref)
%	make a cluster for each common path
%
file_inclusions_graph(File) :-
	init,
	time(store_inclusions(File)),

	setof(Path, B^A^(rdf(B, fx:source, A), paths_prefix:path_names(A, Path)), Paths),
        paths_prefix:paths_prefix(Paths, Trie),
	paths_prefix:paths_simplify(Trie, Simple),
paths_prefix:paths_display(Simple,[]),
	graph_window(clustered_inclusions(G, Simple, File, _), G, [window_title(File)]).

%%	file_inclusions_simple(+File) is det
%
%	display some graphs as built from library(prolog_xref)
%	no clusters
%
file_inclusions_simple(File) :-
	init,
	debug(inclusions, 'started ~s', [File]),
	time(store_inclusions(File)),
	debug(inclusions, 'building graph ~s', [File]),
	graph_window(inclusions(G, File, _), G, [window_title(File)]).

%%	store_inclusions(+Path) is det.
%
%	recurse on xref_uses_file, storing visited inclusions in RDF DB
%
store_inclusions(Path) :-
		rdf(_, fx:source, Path)
	->	debug(inclusions, 'circular dependency ~s', [Path])
	;	xref_source(Path, [register_called(all)]),
		rdf_bnode(B),
		rdf_assert(B, fx:source, Path),
		forall(xref_uses_file(Path, Spec, Used),
		(	rdf_assert(B, fx:uses, Used),
			term_to_atom(Spec, ASpec),
			rdf_assert(B, fx:spec, ASpec),
			store_inclusions(Used)
		))
	.

%%	clustered_inclusions(+G, +Trie, +File, -N) is det
%
%	lookup node (ID is the bnode(ID)) in cluster appropriate for path
%
clustered_inclusions(G, Trie, Path, N) :-
	rdf(B, fx:source, Path),
	(	find_node(G, B, N)
	->	debug(inclusions, 'Path ~w found ~w', [Path, N])
	;	paths_prefix:path_names(Path, Parts),
		last(Parts, Name),
        debug(inclusions, 'locating ~w', [Path]),
		locate_cluster(G, Trie, Parts, [], C),
        debug(inclusions, 'cluster is ~w', [C]),
		make_node(C, B, [label:Name], N),
		forall(rdf(B, fx:uses, U),
		(	rdf(B, fx:spec, S),
			clustered_inclusions(G, Trie, U, M),
			new_edge(G, N, M, label:S, _)
		))
	).

%%	locate_cluster(+Subgraph, +Trie, +File, +Seen, -Cluster) is det
%
locate_cluster(G, _Trie, [_File], Seen, C) :-
	lookup_cluster(G, Seen, C).
locate_cluster(G, Dir:Trie, [Dir|Path], Seen, C) :-
	locate_cluster(G, Trie, Path, [Dir|Seen], C).
locate_cluster(G, Trie, [Dir|Path], Seen, C) :-
	lookup_cluster(G, Seen, T),
	member(Dir:Rest, Trie),
	locate_cluster(T, Rest, Path, [Dir], C).

%%	lookup_cluster(+G, +Seen, -C) is det.
%
lookup_cluster(G, Seen, C) :-
	reverse(Seen, RSeen),
	paths_prefix:path_names(CPath, RSeen),
	(	find_cluster(G, CPath, C)
	->	true
	;	make_cluster(G, CPath, label:CPath, C)
	).

%%	inclusions(+G, +F, -N) is det.
%
%	recurse on xref_uses_file, making node N and storing in DB
%	preliminary test without clustering
%
inclusions(G, F, N) :-
	find_node(G, F, N) -> true ;
	make_node(G, F, N),
	rdf(B, fx:source, F),
	forall(rdf(B, fx:uses, U),
	(	rdf(B, fx:spec, S),
		inclusions(G, U, M),
		new_edge(G, N, M, E),
		set_attrs(E, label:S)
	)).

%%	init is det.
%
%	reset_db and init metadata about this representation
%
init :-
	rdf_reset_db,
	rdf_assert(fx:xref_source, rdf:type, rdfs:'Class').
