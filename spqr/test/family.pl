%%  <module> family
%
%   build a genealogical 'tree' on binary relation
%   parent_child(P, C), using gv_uty direct graphviz pointers model
%   attribute nodes' color,shape by female,male,died
%

:- module(family, [family/0]).

:- use_module(library(clpfd)).
:- use_module(spqr(gv_uty)).

% required relations
:- multifile parent_child/2.
:- multifile female/1.
:- multifile male/1.
:- multifile died/1.

family :-
	graph_window(ranked(G), G, [graph_name(family)]).

ranked(G) :-
	use_node_attrs(G, [style=radial, fillcolor=white, shape=ellipse, label='']),
	use_edge_attrs(G, [arrowhead=vee]),

	% thanks to CLP(FD), making a correctly layered DAG its a breeze...
	setof(P, A^(parent_child(P, A) ; parent_child(A, P)), Ps),
	length(Ps, Pn),
	length(Rs, Pn),
	Rs ins 1..100,
	pairs_keys_values(RPs, Rs, Ps),
	maplist(make_rank(RPs), RPs),
	label(Rs),
	keysort(RPs, SRPs),
	group_pairs_by_key(SRPs, Ranks),

	% ranks done, now we can insert subgraphs and related persons
	forall(member(Rank-Pr, Ranks), (
		new_subgraph(G, Rank, Subg),
		set_attrs(Subg, rankdir:'LR'),
                maplist(make_person(Subg), Pr)
	       )),
	make_links(Ps, G).

%%	make_person(+G, +P)
%
%	create a node for each person, mapping to visual attributes
%
make_person(G, P) :-
	make_node(G, P, N),
	( female(P) -> C = red ; male(P) -> C = cyan ; C = green ),
	( died(P) -> set_attrs(N, shape=octagon) ; true ),
	set_attrs(N, [fillcolor=white:C, label=P]).

%%	make_links(+Ps, +G)
%
%	build a join node for all couples sharing all children
%
make_links(Ps, G) :-
	% select a couple sharing a child
	select(P, Ps, Rs), parent_child(P, A),
	select(Q, Rs, Zs), parent_child(Q, A),
	% see if they share all children
	setof(Gen, parent_child(P, Gen), Children),
	setof(Gen, parent_child(Q, Gen), Children),
	% make a join node
	atomic_list_concat([P,-,Q], NJoin),
	make_node(G, NJoin, Join),
	set_attrs(Join, [shape=point, width:0.1, height:0.1]),
	% get pointers
	maplist(find_node(G), [P,Q|Children], [NP,NQ|NChildren]),
	
	new_edge(G, NP, Join, '', EP),
	new_edge(G, NQ, Join, '', EQ),
	set_attrs([EP, EQ], arrowhead=none),
	forall(member(NChild, NChildren), new_edge(G, Join, NChild, '', _)),

	!, make_links(Zs, G).
make_links(Singles, G) :-
	forall( ( member(A, Singles), parent_child(A, B) ),
	(	find_node(G, A, NA),
		find_node(G, B, NB),
		new_edge(G, NA, NB, '', _)
	)).

make_rank(RPs, Rp-P) :-
	findall(G, parent_child(P, G), Gs),
	maplist(generated(Rp, RPs), Gs).

generated(Rp, RPs, G) :-
	member(Rg-G, RPs),
	Rg #= Rp+1,
	(   parent_child(Q, G)
	->  memberchk(Rq-Q, RPs),
	    Rq #= Rp
	;   true
	).
