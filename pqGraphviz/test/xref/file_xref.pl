/** <module> file_xref
 *	testbed for preliminary library(prolog_xref) usage
 *
 *  library xref does a great job analyzing call graph and inclusions,
 *  here I'm just trying to get that represented by means
 *  of graphviz + html, showcasing integration pqConsole/meta Qt.
 *
 *  @version 1.0.0
 *  @author carlo
 *  @license LGPL 2.1
 *  @copyright carlo 2013
 */

:- module(file_xref,
	[file_xref/0
	,file_inclusions_graph/1
	,file_inclusions_simple/1
	]).

:- use_module(library(prolog_xref)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(debug)).

:- if(\+current_module(gv_uty)).
:- use_module('../../prolog/gv_uty').
:- endif.

:- use_module(fs_rdf).

:- rdf_register_prefix(file_xref, 'http://www.swi-prolog.org/file_xref#').
:- meta_predicate show_inclusions(1).

%%	file_xref is det.
%
%	use this file as basic test case
%
file_xref :-
%	show_inclusions(file_inclusions_graph).
	show_inclusions(file_inclusions_simple).
    
show_inclusions(Pred) :-
	module_property(file_xref, file(F)),
	call(Pred, F).

%%	file_inclusions_graph(+File) is det.
%
%	display some graphs as built from library(prolog_xref)
%	make a cluster for each common path
%
file_inclusions_graph(File) :-
	init(File). /*,
	setof(Path, B^A^(rdf(B, file_xref:source, A), path_names(A, Path)), Paths),
	paths_prefix(Paths, Trie),
	graph_window(clustered_inclusions(G, Trie, File, _), G, [window_title(File)]).*/

%%	file_inclusions_simple(+File) is det
%
%	display some graphs as built from library(prolog_xref)
%	no clusters
%
file_inclusions_simple(File) :-
	init(File),
	graph_window(simple_inclusions(G, File, _), G, [window_title(File)]).

%%	clustered_inclusions(+G, +Trie, +File, -N) is det.
%
%	lookup node (ID is the bnode(ID)) in cluster appropriate for path
%
clustered_inclusions(G, Trie, Path, N) :-
	rdf(B, file_xref:source, Path),
	(	find_node(G, B, N)
	->	debug(inclusions, 'Path ~w found ~w', [Path, N])
	;	path_names(Path, Parts),
		last(Parts, Name),
		locate_cluster(G, Trie, Parts, [], C),
		make_node(C, B, [label:Name], N),
		forall(rdf(B, file_xref:uses, U),
		(	rdf(B, file_xref:spec, S),
			clustered_inclusions(G, Trie, U, M),
			new_edge(G, N, M, label:S, _)
		))
	).

%%	locate_cluster(+Subgraph, +Trie, +File, +Seen, -Cluster) is det.
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
	path_names(CPath, RSeen),
	(	find_cluster(G, CPath, C)
	->	true
	;	make_cluster(G, CPath, label:CPath, C)
	).

%%	simple_inclusions(+G, +F, -N) is det.
%
%	recurse on xref_uses_file, making node N and storing in DB
%	preliminary test without clustering
%
simple_inclusions(G, F, N) :-
	find_node(G, F, N) -> true ;
	make_node(G, F, N),
	rdf(B, file_xref:source, F),
	forall(rdf(B, file_xref:uses, U),
	(	rdf(B, file_xref:spec, S),
		simple_inclusions(G, U, M),
		new_edge(G, N, M, E),
		set_attrs(E, label:S)
	)).

%%	init(+File) is det.
%
%	reset_db and init metadata about this representation, then 
%
init(File) :-
	rdf_reset_db,
	time(store_inclusions(File)).

%%	store_inclusions(+Path) is det.
%
%	recurse on xref_uses_file, storing visited inclusions in RDF DB
%
store_inclusions(Path) :-
		store_absolute_filepath(Path, RefPath),
		rdf(_, file_xref:source, RefPath)
	->	debug(inclusions, 'circular dependency ~s', [Path])
	;	xref_source(Path, [register_called(all)]),
		rdf_bnode(B),
		rdf_assert(B, file_xref:source, RefPath),
		forall(xref_uses_file(Path, Spec, Used),
		(	rdf_assert(B, file_xref:uses, Used),
			term_to_atom(Spec, ASpec),
			rdf_assert(B, file_xref:spec, ASpec),
			store_inclusions(Used)
		))
	.

%%	path_names(?Path, ?Names) is det.
%
%	concatenates path fragments with slash
%
path_names(Path, Names) :- atomic_list_concat(Names, /, Path).
