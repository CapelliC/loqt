/** <module> gv_uty

    pqGraphviz   : SWI-Prolog Qt Rendering

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:-  module(gv_uty,
        [graph_window/3
        ,make_node/3
        ,make_node/4
        ,find_node/3
        ,new_edge/3
        ,new_edge/4
        ,new_edge/5
        ,make_edges/2
        ,nodes_chain/2
        ,new_subgraph/3
        ,make_cluster/4
        ,find_cluster/3
        ,set_attrs/2
        ]).

:-  use_module(library(option)).
:-  use_module(library(debug)).

:-  predicate_options(graph_window/3, 3,
        [graph_name(+atom)
        ,graph_kind(+oneof(['Agdirected', 'Agstrictdirected', 'Agundirected', 'Agstrictundirected']))
        ,graph_layout(+oneof([dot, neato, fdp, sfdp, twopi, circo]))
        ,window_title(+atom)
        ,node_defaults(+list)
        ,edge_defaults(+list)
        % system low level control
        ,pq_class_view(+atom)
        ,pq_method_context_graph_layout(+atom)
        ,pq_instance_class(+atom)
        ,pq_instance_module(+atom)
        ]).

:-  meta_predicate graph_window(0, -, +).

%%	graph_window(+Pred:callable, -G, +Opts)
%
%	make a Graphviz from library pqGraphviz
%
graph_window(Pred, G, Opts) :-
    debug(gv_uty, '~w', [graph_window(Pred, G, Opts)]),
	pqGraphviz:gvContext(C),
	term_to_atom(Pred, PredAtom),
	option(graph_name(Name), Opts, PredAtom),	% default to qualified predicate
	option(graph_kind(Kind), Opts, 'Agdirected'),
	option(graph_layout(Layout), Opts, dot),
	pqGraphviz:agopen(Name, Kind, 0, G),
	set_attrs(G, rankdir:'TD'),
        option(node_defaults(Node_defaults), Opts, []),
        option(edge_defaults(Edge_defaults), Opts, []),
        default_attrs(G, 'AGNODE', Node_defaults),
        default_attrs(G, 'AGEDGE', Edge_defaults),
        once(Pred),
	option(pq_instance_class(IC), Opts, create),
	option(pq_class_view(VC), Opts, lqXDotView), %'GraphvizView'),
	XC =.. [IC,VC,V],
	option(pq_instance_module(XM), Opts, pqConsole),
	XM:XC,
	option(pq_method_context_graph_layout(MCGL), Opts, show_context_graph_layout),
	pqConsole:invoke(V, MCGL, [C, G, Layout], _),
	pqConsole:property(V, size, 'QSize'(800,600)),
	option(window_title(Title), Opts, 'a graph'),
	pqConsole:property(V, windowTitle, Title).

%%  make_node(+Graph, +Name, -NodePtr)
%
%   build a named node
%
make_node(Graph, Name, NodePtr) :-
        pqGraphviz:agnode(Graph, Name, 1, NodePtr).

%%  make_node(+Graph, +Name, +Attrs, -NodePtr)
%
%   build a named node with given attributes
%
make_node(G, P, As, N) :-
        make_node(G, P, N), set_attrs(N, As).

%%  find_node(+G, +P, -N)
%
%   search
find_node(G, P, N) :-
        pqGraphviz:agnode(G, P, 0, N).

%%  new_edge(+Graph, +NodeSource, +NodeTarget)
%
%   build an unnamed edge between NodeSource, NodeTarget
%
new_edge(Graph, NodeSource, NodeTarget) :-
        pqGraphviz:agedge(Graph, NodeSource, NodeTarget, _, 1, _).

%%  new_edge(+Graph, +NodeSource, +NodeTarget, -EdgePtr)
%
%   build an unnamed EdgePtr between NodeSource, NodeTarget
%
new_edge(Graph, NodeSource, NodeTarget, EdgePtr) :-
        pqGraphviz:agedge(Graph, NodeSource, NodeTarget, _, 1, EdgePtr).

%%  new_edge(+Graph, +NodeSource, +NodeTarget, +Name, -EdgePtr)
%
%   build a named EdgePtr between NodeSource, NodeTarget
%
new_edge(G, NodeSource, NodeTarget, Name, EdgePtr) :-
        pqGraphviz:agedge(G, NodeSource, NodeTarget, Name, 1, EdgePtr).

%%  make_edges(+Graph, +Arrows) is det.
%%  make_edges(+Graph, +Arrow) is det.
%%  make_edges(+Graph, +List:list) is det.
%
%   given a directed chain of nodes, build correspoing edges
%
make_edges(G, X->(Y->Z)) :-
        new_edge(G, X, Y),
        make_edges(G, Y->Z).
make_edges(G, X->Y) :-
        new_edge(G,X,Y).

make_edges(G, X) :-
        is_list(X) -> maplist(make_edges(G), X).

%%  nodes_chain(+Nodes:list, ?Chain) is det
%
%   transform a nodes' list to an expression of directed edges
%
nodes_chain([A,B|L], A->R) :- nodes_chain([B|L], R).
nodes_chain([A,B], A->B).

%%  new_subgraph(+Graph, +Name, -Subg) is det
%
%   create a new subgraph
%
new_subgraph(G, Name, Subg) :-
        pqGraphviz:agsubg(G, Name, 1, Subg).

%%  make_cluster(+Graph, +Name, +Attrs, -C) is det
%
%   apply naming convention to create a visible subgraph
%
make_cluster(G, Name, Attrs, C) :-
        atom_concat(cluster, Name, Id),
        pqGraphviz:agsubg(G, Id, 1, C),
        set_attrs(C, Attrs).

%% find_cluster(+Graph, +Name, -C)
%
find_cluster(G, Name, C) :-
        atom_concat(cluster, Name, Id),
        pqGraphviz:agsubg(G, Id, 0, C).

%%  set_attrs(+Objects:list, +Attrs:list) is det
%%  set_attrs(+Object:pointer, +Attrs:list) is det
%
%   assign a set of attributes to a set of objects
%
set_attrs(Os, As) :-
        is_list(Os) -> forall(member(O, Os), set_attrs(O, As)).
set_attrs(O, As) :-
        is_list(As) ->
        maplist(set_attrs(O), As)
        ;       (As=(K=V) ->
                pqGraphviz:agset(O, K, V)
                ;       As=K:V ->
                        pqGraphviz:agsafeset(O, K, V, V)).

%%  use_node_attrs(+G, +Defaults)
%
%   defaulting nodes attributes required to call agset
%
use_node_attrs(G, Defaults) :-
    default_attrs(G, 'AGNODE', Defaults).

%%  use_edge_attrs(+G, +Defaults)
%
%   defaulting edges attributes required to call agset
%
use_edge_attrs(G, Defaults) :-
        default_attrs(G, 'AGEDGE', Defaults).

default_attrs(G, Kind, Defaults) :-
    debug(gv_uty, '~w', [default_attrs(G, Kind, Defaults)]),
        forall(member(K=V, Defaults), pqGraphviz:agattr(G, Kind, K, V, _)).
