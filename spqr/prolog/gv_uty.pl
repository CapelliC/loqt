/** <module> gv_uty

    spqr         : SWI-Prolog Qt Rendering

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013, Carlo Capelli

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

:-  module(gv_uty, [
        graph_window/3,
        make_node/3,
        make_node/4,
        find_node/3,
        new_edge/3,
        new_edge/4,
        new_edge/5,
        make_edges/2,
        nodes_chain/2,
        new_subgraph/3,
        make_cluster/4,
        find_cluster/3,
        set_attr/2,
        use_node_attrs/2,
        use_edge_attrs/2
    ]).

:-  use_module(library(option)).
:-  use_module(library(debug)).

:-  predicate_options(graph_window/3, 3, [
        graph_name(+atom),
        graph_kind(+oneof(['Agdirected', 'Agstrictdirected', 'Agundirected', 'Agstrictundirected'])),
        graph_layout(+oneof([dot, neato, fdp, sfdp, twopi, circo])),
        window_title(+atom),
        pq_class_view(+atom),
        pq_method_context_graph_layout(+atom),
        pq_instance_class(+atom),
        pq_instance_module(+atom)
    ]).

:-  meta_predicate graph_window(0, -, +).

%%	graph_window(+Pred:callable, -G, +Opts)
%
%	make a Graphviz from library spqr
%
graph_window(Pred, G, Opts) :- % debug(graph_window),
        spqr:gvContext(C),
	term_to_atom(Pred, PredAtom),
        option(graph_name(Name), Opts, PredAtom),	% default to qualified predicate
	option(graph_kind(Kind), Opts, 'Agdirected'),
	option(graph_layout(Layout), Opts, dot),
        spqr:agopen(Name, Kind, 0, G),
	set_attr(G, rankdir:'TD'),
	once(Pred),
	option(pq_instance_class(IC), Opts, create),
        option(pq_class_view(VC), Opts, lqXDotView), %'GraphvizView'),
        XC =.. [IC,VC,V],
        option(pq_instance_module(XM), Opts, pqConsole),
        XM:XC,
        option(pq_method_context_graph_layout(MCGL), Opts, show_context_graph_layout),
        pqConsole:invoke(V, MCGL, [C, G, Layout], _),
	pqConsole:property(V, size, 'QSize'(800,600)),
	option(window_title(Title), Opts, Name),
        pqConsole:property(V, windowTitle, Title).

%% new_node(+Graph, +Name, -NodePtr)
%
make_node(G, P, N) :-
        spqr:agnode(G, P, 1, N).

%% new_node(+Graph, +Name, +Attrs, -NodePtr)
%
make_node(G, P, As, N) :-
        make_node(G, P, N), set_attr(N, As).

%% find_node(+G, +P, -N)
%
find_node(G, P, N) :-
        spqr:agnode(G, P, 0, N).

%% new_edge(+Graph, +NodeSource, +NodeTarget)
%
new_edge(Graph, NodeSource, NodeTarget) :-
        spqr:agedge(Graph, NodeSource, NodeTarget, _, 1, _).

%% new_edge(+Graph, +NodeSource, +NodeTarget, -EdgePtr)
%
new_edge(Graph, NodeSource, NodeTarget, EdgePtr) :-
        spqr:agedge(Graph, NodeSource, NodeTarget, _, 1, EdgePtr).

%% new_edge(+Graph, +NodeSource, +NodeTarget, +Name, -EdgePtr)
%
new_edge(G, NodeSource, NodeTarget, Name, EdgePtr) :-
        spqr:agedge(G, NodeSource, NodeTarget, Name, 1, EdgePtr).

%% make_edges(+Graph, +Arrows)
%
make_edges(G, X->(Y->Z)) :-
        new_edge(G, X, Y),
        make_edges(G, Y->Z).

%% make_edges(+Graph, +Arrow)
%
make_edges(G, X->Y) :-
        new_edge(G,X,Y).

%% make_edges(+Graph, +List:list)
%
make_edges(G, X) :-
        is_list(X) -> maplist(make_edges(G), X).

%% nodes_chain(+Nodes:list, ?Chain) is det
%
nodes_chain([A,B|L], A->R) :- nodes_chain([B|L], R).
nodes_chain([A,B], A->B).

%% new_subgraph(+Graph, +Name, -Subg)
%
new_subgraph(G, Name, Subg) :-
        spqr:agsubg(G, Name, 1, Subg).

%% make_cluster(+Graph, +Name, +Attrs, -C)
%
make_cluster(G, Name, Attrs, C) :-
        atom_concat(cluster, Name, Id),
        spqr:agsubg(G, Id, 1, C),
        set_attr(C, Attrs).

%% find_cluster(+Graph, +Name, -C)
%
find_cluster(G, Name, C) :-
        atom_concat(cluster, Name, Id),
        spqr:agsubg(G, Id, 0, C).

%% set_attr(+Objects:list, +Attrs:list)
%% set_attr(+Object:pointer, +Attrs:list)
%
set_attr(Os, As) :-
        is_list(Os) -> forall(member(O, Os), set_attr(O, As)).

set_attr(O, As) :-
        is_list(As) ->
        maplist(set_attr(O), As)
        ;       (As=(K=V) ->
                spqr:agset(O, K, V)
                ;       As=K:V ->
                        spqr:agsafeset(O, K, V, V)).

% defaulting attributes
% required to call agset
%
use_node_attrs(G, Defaults) :- 
        default_attrs(G, 'AGNODE', Defaults).

use_edge_attrs(G, Defaults) :- 
        default_attrs(G, 'AGEDGE', Defaults).

default_attrs(G, Kind, Defaults) :-
        forall(member(K=V, Defaults), spqr:agattr(G, Kind, K, V, _)).
