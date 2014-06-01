/** <module> calledgraph
 *
 *  Describe your module.
 *  --------
 *
 *  source file /home/carlo/cpp/loqt/pqSource/prolog/calledgraph.pl
 *  created at Tue Feb 18 17:38:58 2014
 *
 *  @author carlo
 *  @version 0.9.9
 *  @copyright carlo
 *  @license LGPL v2.1
 */

:- module(calledgraph,
	[calledgraph/0
	,calledgraph/1
	,calledgraph/2
	]).
:- use_module(library(prolog_xref)).

%% calledgraph is det.
%
%  test calledgraph/2 with simple source
%
calledgraph :-
        %calledgraph('/home/carlo/prolog/various/gulp/gulp4swi.pl').
        calledgraph('/home/carlo/cpp/loqt/pqSource/prolog/calledgraph.pl').

%% calledgraph(Source) is det.
%
%  display graph of Caller/Callee of Source in free window
%
calledgraph(Source) :-
        xref_source(Source),
        graph_window(calledgraph(Source), [node_defaults([shape=box])]).

%% calledgraph(Source, GraphWindow) is det.
%
%  build a caller/callee basic graph
%
%  @arg Source the Source file full path - as required by xref_source/1
%  @arg GraphWindow the Graph context
%
calledgraph(Source, GraphWindow) :-
	forall(xref_called(Source, Called, By), add_called(GraphWindow, Called, By)).

%% add_called(GraphWindow, Called, By) is det.
%
%  add Caller and Callee labels, make an edge between those 2 nodes
%
%  @arg GraphWindow Graph context
%  @arg Called Callee predicate - as returned by xref_called/3
%  @arg By Caller predicate - as returned by xref_called/3
%
add_called(GraphWindow, Called, By) :-
	label_pred(GraphWindow, Called, NCalled),
	label_pred(GraphWindow, By, NBy),
	new_edge(GraphWindow, NBy, NCalled).

%% label_pred(Graph, Pred, NodePred) is det.
%
%  make a Functor/Arity label to lookup in Graph
%
%  @arg Graph context Graph
%  @arg Pred predicate - as returned by xref_called/3
%  @arg NodePred - Node pointer referencing Functor/Arity Label
%
label_pred(Graph, Pred, NodePred) :-
	functor(Pred, Functor, Arity),
	term_to_atom(Functor/Arity, Label),
	(	find_node(Graph, Label, NodePred)
	->	true
	;	make_node(Graph, Label, NodePred)
	).
