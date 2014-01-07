/** <module> grouping paths resulting from prolog_xref
 *
 *  @file /home/carlo/prolog/paths_prefix.pl
 *  @created Tue Nov 12 11:27:01 2013
 *  @version 1.0.0
 *  @author carlo
 *  @license LGPL 2.1
 *  @copyright carlo 2013
 */

:- module(paths_prefix,
	[path_names/2
    ,paths_prefixes/2
	,paths_simplify/2
	,paths_display/2
	]).

:- use_module(library(apply), [maplist/3]).


%%	path_names(?Path, ?Names) is det.
%
path_names(Path, Names) :- atomic_list_concat(Names, /, Path).

%%	paths_prefix(+Paths:list, -Trie:list) is det.
%
%	group element prefixes, produce a (kind of) trie
%
paths_prefixes([E|Es], Trie) :-
	E = [K|R],
	first_remove(K, Es, Ys, Ns),
	(	Ys = [_|_],
		paths_prefixes([R|Ys],P1),
		Trie = [K-P1|T1]
	;	Trie = [E|T1]
	),
	!, paths_prefixes(Ns, T1).
paths_prefixes(L, L).

first_remove(_, [], [], []).
first_remove(K, [[K|Ys]|Es], [Ys|REs], Ns) :-
	!, first_remove(K, Es, REs, Ns).
first_remove(K, [E|Es], Ys, [E|Ns]) :-
	first_remove(K, Es, Ys, Ns).

%%	paths_simplify(+Path:list, -Simpler) is det.
%
%	remove unneeded nesting from structure built by paths_prefix/2
%
paths_simplify(A-[B-T], A:C:S) :- paths_simplify(B, C), paths_simplify(T, S), !.
paths_simplify(K-T, K:S) :- paths_simplify(T, S), !.
paths_simplify([T], S) :- paths_simplify(T, S), !.
paths_simplify(T, S) :- maplist(paths_simplify, T, S), !.
paths_simplify(E, E).

%%	paths_display(+Paths,+Indent) is det
%
%	display indented after simplification
%
paths_display(Paths, Indent) :-
	paths_display(Paths, [], Indent).

paths_display(P:Paths, Ps, Indent) :-
	!, paths_display(Paths, [P|Ps], Indent).
paths_display(Paths, Ps, Indent) :-
	reverse(Ps,RPs),
	format('~s~w{~n', [Indent,RPs]),
	paths_list_display(Paths, [0' |Indent]),
	format('~s}~n', [Indent]).

paths_list_display([D:P|Paths], Indent) :-
	!, paths_display(D:P, [], Indent),
	paths_list_display(Paths, Indent).
paths_list_display([P|Paths], Indent) :-
	format('~s~w~n', [Indent,P]),
	paths_list_display(Paths, Indent).
paths_list_display([], _Indent).

:- begin_tests(paths_prefix).

test(1) :-
	paths_prefixes([[1,2,3],[1,2,4]],P),
	paths_simplify(P,S),
	P = [1-[2-[[3], [4]]]],
	S = 1:2:[3, 4] .

test(2) :-
	paths_prefixes([[1,2,3],[1,2,4],[a,b,c],[a,b,d]],P),
	paths_simplify(P,S),
	P = [1-[2-[[3], [4]]], a-[b-[[c], [d]]]],
	S = [1:2:[3, 4], a:b:[c, d]] .

:- end_tests(paths_prefix).
