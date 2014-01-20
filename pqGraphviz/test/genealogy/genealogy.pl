%%  <module> genealogy
%
%   build a genealogical 'tree' on relation parent_child/2
%   parent_child(P, C), using gv_uty direct graphviz pointers model
%   assign  nodes'attributes color,shape depending by female/1,male/1,died/1
%

:- module(genealogy, [genealogy/1]).
:- use_module(library(clpfd)).

% required relations
:- multifile parent_child/2.
:- multifile female/1.
:- multifile male/1.
:- multifile died/1.

genealogy(T) :-
	graph_window(generations(G), G,
		[graph_name(family)
		,window_title(T)
		,node_defaults([style=radial, fillcolor=white, shape=ellipse, label=''])
		,edge_defaults([arrowhead=vee])
		]).

%%	generations(+G)
%
%	build a generational tree (a DAG, really), that is, persons in stacked layers
%
generations(G) :-
	% thanks to CLP(FD), making a correctly layered DAG its a breeze...
	setof(P, A^(parent_child(P, A) ; parent_child(A, P)), Ps),
	length(Ps, Pn),
	length(Rs, Pn),
	Rs ins 1..100,	% arbitrary, but doesn't accept +inf
	pairs_keys_values(RPs, Rs, Ps),
	maplist(make_rank(RPs), RPs),
	label(Rs),
	keysort(RPs, SRPs),
	group_pairs_by_key(SRPs, Ranks),
	% ranks done, now we can place persons at generations
	forall(member(Rank-Pr, Ranks), (
		new_subgraph(G, Rank, Subg),	% generation subgraph
		set_attrs(Subg, [rank:same, rankdir:'LR']),
		shared_children(Pr, ParentsPairs, Singles),
		maplist(make_family(Subg), ParentsPairs),
		maplist(make_person(Subg), Singles),
		maplist(link_parents(G), Pr)
	)).

%%	make_family(+GenerationGraph, +Parents)
%
%	bind strictly with a subgraph
%
make_family(GenerationGraph, (X,Y)) :-
	new_subgraph(GenerationGraph, [X,Y], Family),
	make_person(Family, X),
	make_person(Family, Y).

%%	make_person(+G, +P)
%
%	create a node for each person, mapping to visual attributes
%
make_person(G, P) :-
	make_node(G, P, N),
	( female(P) -> C = red ; male(P) -> C = cyan ; C = green ),
	( died(P) -> set_attrs(N, shape=octagon) ; true ),
	set_attrs(N, [fillcolor=white:C, label=P]).

%%	link_parents(+G, +P, +N)
%
%	link a person to its parents, if any available
%
link_parents(G, P) :-
	(	parent_child(X, P)
	->	(	( parent_child(Y, P), X \= Y )
		->	two_parents(G, P, X, Y)
		;	one_parent(G, P, X)
		)
	;	true
	).

two_parents(G, P, X, Y) :-
	maplist(find_node(G), [P,X,Y], [Pp,Xp,Yp]),
	sort([X, Y], S),
	(	find_node(G, S, Join)
	->	true
	;	make_node(G, S, [shape=point, width:0.1, height:0.1], Join),
		new_edge(G, Xp, Join),
		new_edge(G, Yp, Join)
	),
	new_edge(G, Join, Pp).

one_parent(G, P, X) :-
	maplist(find_node(G), [P,X], [Pp,Xp]),
	find_node(G, X, Xp),
	new_edge(G, Xp, Pp).

%%	make_rank(+RankPersons, -RankPersons)
%
%	compute generation based ranking via CLP(FD) 
%
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

%%	shared_children(+Ps, -Pairs, -NoShared)
%
%	group persons in Ps, pairing two persons sharing all children
%
shared_children(Ps, [(P,Q)|Shared], NoShared) :-
	old_fashion_family(Ps, P,Q, _, Zs),
	!, shared_children(Zs, Shared, NoShared).
shared_children(Ps, [], Ps).

%%	old_fashion_family(+Persons, +P,+Q, -Children, -RestPersons)
%
%	P,Q from Persons share all and only children - a proper, old fashion family
%
old_fashion_family(Ps, P,Q, Children, Zs) :-
	% select a couple sharing a child
	select(P, Ps, Rs), parent_child(P, A),
	select(Q, Rs, Zs), parent_child(Q, A),
	% see if they share all children
	setof(Gen, parent_child(P, Gen), Children),
	setof(Gen, parent_child(Q, Gen), Children).
