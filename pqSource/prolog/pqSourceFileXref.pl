/** <module> pqSourceFileXref
 *
 *  testbed for preliminary library(prolog_xref) usage.
 *
 *  Library xref does a great job analyzing call graph and inclusions,
 *  Here I'm just trying to get that represented by means
 *  of graphviz + html, showcasing integration pqConsole/meta Qt.
 *
 *  @author carlo
 *  @created Fri Nov 8 10:35:36 2013
 *  @version 0.9.9
 *  @copyright 2014 Carlo Capelli
 *  @license LGPL v2.1
 */

:- module(pqSourceFileXref,
	[pqSourceFileXref/0
	,file_inclusions_graph/1
	,file_inclusions_graph/2
	]).

:- use_module(library(prolog_xref)).
:- use_module(library(debug)).

%%	pqSourceFileXref is det
%
%	use this file as basic test case
%
pqSourceFileXref :-
	file_inclusions_graph('/home/carlo/prolog/pqSourceFileXref.pl').

%%	file_inclusions_graph(+File,) is det
%
%	display some graphs as built from library(prolog_xref)
%	make a cluster for each common path
%
file_inclusions_graph(File) :-
	graph_window(clustered_inclusions(File), [layout(sfdp),window_title(File)]).

%%	file_inclusions_graph(+File) is det
%
%	display some graphs as built from library(prolog_xref)
%	make a cluster for each common path
%
file_inclusions_graph(File, GraphWindow) :-
	clustered_inclusions(File, GraphWindow).

%%	clustered_inclusions(File, G) is det.
%
%	recurse on xref_uses_file, storing visited inclusions in GV nodes
%
clustered_inclusions(File, G) :-
	set_attrs(G, [/*splines:ortho*/]),
	clustered_inclusions(G, File, _).

%% clustered_inclusions(G, Path, Anc) is det.
%
%  visits recursively the inclusion graph
%
%  @arg G the Graphviz context graph
%  @arg Path the current file as reported by xref_uses_file/3
%  @arg Anc including file
%
clustered_inclusions(G, Path, Anc) :-
	log('clustered_inclusions'(Path)),
	(	find_node(G, Path, Node)
	->	log('circular dependency'(Path)),
		(true; new_edge(G, Node, Anc, E), set_attrs(E, style:dotted))
	;	xref_source(Path, [register_called(all)]),
		directory_file_path(Dir, File, Path),
		log(directory_file_path(Dir, File, Path)),
		path_dir([''|Dirs], Dir),
		reverse(Dirs, RDirs),
		make_context_clusters(RDirs, G, Cluster),
		make_node(Cluster, Path, [label:File], Node),
		(var(Anc) -> true ; new_edge(G, Anc, Node, E), set_attrs(E, style:filled)),
		forall(	xref_uses_file(Path, Spec, Used),
		(	log(xref_uses_file(Path, Spec, Used)),
			clustered_inclusions(G, Used, Node)
		))
	).

make_cluster(Path, G, C) :-
	format(atom(IdG), 'cluster_"~w"', [Path]),	%atomic_list_concat(Path,/,Subpath),
	(	find_subgraph(G, IdG, C)
	->	true
	;	new_subgraph(G, IdG, C),
		Path=[Dir|_],
		set_attrs(C, [label:Dir])
	),
	log(make_cluster(Path, G, C)).

make_context_clusters([Root], G, C) :-
	log(root(Root)),
	make_cluster([Root], G, C).
make_context_clusters([Dir|Path], G, C) :-
	log(make_context_clusters([Dir|Path], G, C)),
	make_context_clusters(Path, G, A),
	make_cluster([Dir|Path], A, C).

path_dir(P, D) :-
	log(path_dir(P, D)),
	atomic_list_concat(P, /, D).

log(T) :- debug(inclusions, '~w', T).
