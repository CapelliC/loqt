/** <module> lqGraphix
 *
 *  Describe your module.
 *  --------
 *
 *  source file /home/carlo/prolog/lqGraphix.pl
 *  created at gio mar 3 23:41:45 2016
 *
 *  @author carlo
 *  @version 0.9.9
 *  @copyright carlo
 *  @license LGPL v2.1
 */

:- module(lqGraphix, [lqGraphix/0, list_metatypes/0]).

:- initialization(lqGraphix).

%%  lqGraphix is det.
%
%   setup the initial scene
%
lqGraphix :-
	% pqConsole:types(T),maplist(writeln,T),
	current_predicate(scene/1)
	-> scene(S),
        rect(S, [qRectF(810,20,30,40),qPen('DashDotDotLine'),qBrush(darkRed)], _),
	pqConsole:create(lqPushButton, B),
	pqConsole:property(B, button, X),
	pqConsole:property(pqObj(_, X), text, pippo),
	pqConsole:invoke(S, addProxyWidget, [X], _)
/*
        rect(S, [qRectF(810,20,30,40),qPen('DashDotDotLine'),qBrush(darkRed)], R),
        ellipse(S, [qRectF(30,40,50,60)], E),
        text(S, ['hello world'], T),
        line(S, [qLineF(qPointF(200,200),qPointF(400,400))] ,L),
	polygon(S, [qPolygonF([qPointF(30,40),qPointF(200,300),qPointF(500,300),qPointF(200,30)])], P),
        format('hello by lqGraphix [scene ~w] ~w ~w ~w ~w ~w ~n', [S, R,E,T,L,P])
*/
    ;   true.

list_metatypes :-
	pqConsole:types(Ts),
	maplist(writeln, Ts).

rect(S, D, R) :-
	lit(D,D1),
        pqConsole:invoke(S, addRect, D1, R).
ellipse(S, D, E) :-
	lit(D,D1),
        pqConsole:invoke(S, addEllipse, D1, E).
text(S, W, T) :-
	lit(W,W1),
        pqConsole:invoke(S, addSimpleText, W1, T).
line(S, D, L) :-
	lit(D,D1),
        pqConsole:invoke(S, addLine, D1, L).
polygon(S, D, P) :-
	lit(D,D1),
        pqConsole:invoke(S, addPolygon, D1, P).

lit(P,Q) :-
	P=..[F|As],
	atom_codes(F, [0'q|Cs]),
	!, atom_codes(G, [0'Q|Cs]),
	maplist(lit,As,Bs),
	Q=..[G|Bs].
lit(L,M) :-
	is_list(L), !, maplist(lit, L, M).
lit(P,P).
