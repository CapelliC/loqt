/** <module> lqShapes_test
 *
 *  some test code for lqShapes
 *  --------
 *
 *  source file /home/carlo/develop/loqt/lqShapes_test/lqShapes_test.pl
 *
 *  @author carlo
 *  @version 0.9.9
 *  @copyright carlo
 *  @license LGPL v2.1
 */

:- module(lqShapes_test, [lqShapes_test/0, list_metatypes/0]).

:- initialization(lqShapes_test).

%%  lqShapes_test is det.
%
%   setup the initial scene
%
lqShapes_test :-
	% pqConsole:types(T),maplist(writeln,T),
	current_predicate(scene/1)
	-> scene(S),
        rect(S, [qRectF(10,20,30,40),qPen('DashDotDotLine'),qBrush(darkRed)], _),
/*
	pqConsole:create(lqPushButton, B),
	pqConsole:property(B, button, X),
	pqConsole:property(pqObj(_, X), text, pippo),
	pqConsole:invoke(S, addProxyWidget, [X], _)
*/
        pushButton(S, [hello], Make_Red),
        connect(Make_Red, clicked, on_make_red)
/*
        rect(S, [qRectF(810,20,30,40),qPen('DashDotDotLine'),qBrush(darkRed)], R),
        ellipse(S, [qRectF(30,40,50,60)], E),
        text(S, ['hello world'], T),
        line(S, [qLineF(qPointF(200,200),qPointF(400,400))] ,L),
	polygon(S, [qPolygonF([qPointF(30,40),qPointF(200,300),qPointF(500,300),qPointF(200,30)])], P),
        format('hello by lqShapes_test [scene ~w] ~w ~w ~w ~w ~w ~n', [S, R,E,T,L,P])
*/
    ;   true.

list_metatypes :-
	pqConsole:types(Ts),
	maplist(writeln, Ts).

pushButton(S, [P], B) :-
    pqConsole:create(lqPushButton, B),
    pqConsole:property(B, button, X),
    pqConsole:property(pqObj(_, X), text, P),
    pqConsole:invoke(S, addProxyWidget, [X], _).

connect(PQObj, Signal, Slot) :-
    pqConsole:connect(PQObj, Signal, Slot).

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

/*
*/
% filled(R, C).
 filled(1, 1).
 filled(1, 2).
 filled(1, 3).
 filled(1, 4).
 filled(1, 5).
 filled(1, 6).
 filled(1, 7).
 filled(1, 8).
 filled(1, 9).
 filled(1, 10).
 filled(1, 11).
 filled(1, 12).

 filled(2, 1).
 filled(2, 2).
 filled(2, 3).
 filled(2, 6).
 filled(2, 7).
 filled(2, 8).
 filled(2, 9).
 filled(2, 12).

 filled(3, 1).
 filled(3, 2).
 filled(3, 3).
 filled(3, 8).
 filled(3, 9).
 filled(3, 12).

 filled(4, 1).
 filled(4, 2).
 filled(4, 5).
 filled(4, 10).

 filled(5, 1).
 filled(5, 6).
 filled(5, 7).
 filled(5, 10).

 filled(6, 1).
 filled(6, 4).
 filled(6, 9).

 filled(7, 1).
 filled(7, 2).
 filled(7, 5).
 filled(7, 9).
 filled(7, 12).

 filled(8, 1).
 filled(8, 5).
 filled(8, 10).

 filled(9, 1).
 filled(9, 4).
 filled(9, 7).
 filled(9, 8).

 filled(10, 1).
 filled(10, 4).
 filled(10, 9).
 filled(10, 12).

 filled(11, 1).
 filled(11, 2).
 filled(11, 5).
 filled(11, 6).
 filled(11, 11).
 filled(11, 12).

 filled(12, 1).
 filled(12, 2).
 filled(12, 5).
 filled(12, 6).
 filled(12, 7).
 filled(12, 8).
 filled(12, 11).
 filled(12, 12).

 % horizontal(R, C, L, S)
 horizontal(2,4,2,4).
 horizontal(2,10,2,4).
 horizontal(3,4,4,12).
 horizontal(3,10,2,6).
 horizontal(4,3,2,6).
 horizontal(4,6,4,10).
 horizontal(4,11,2,3).
 horizontal(5,2,4,12).
 horizontal(5,8,2,3).
 horizontal(5,11,2,6).
 horizontal(6,2,2,6).
 horizontal(6,5,4,10).
 horizontal(6,10,3,7).
 horizontal(7,3,2,7).
 horizontal(7,6,3,8).
 horizontal(7,10,2,8).
 horizontal(8,2,3,11).
 horizontal(8,6,4,15).
 horizontal(8,11,2,8).
 horizontal(9,2,2,4).
 horizontal(9,5,2,6).
 horizontal(9,9,4,14).
 horizontal(10,2,2,12).
 horizontal(10,5,4,15).
 horizontal(10,10,2,11).
 horizontal(11,3,2,8).
 horizontal(11,7,4,11).
 horizontal(12,3,2,11).
 horizontal(12,9,2,7).

 % vertical(R, C, L, S)
 vertical(5,2,2,3).
 vertical(8,2,3,8).
 vertical(4,3,9,45).
 vertical(2,4,4,13).
 vertical(7,4,2,6).
 vertical(11,4,2,3).
 vertical(2,5,2,8).
 vertical(5,5,2,3).
 vertical(9,5,2,6).
 vertical(3,6,2,3).
 vertical(6,6,5,16).
 vertical(3,7,2,5).
 vertical(6,7,3,10).
 vertical(10,7,2,5).
 vertical(4,8,5,20).
 vertical(10,8,2,4).
 vertical(4,9,2,5).
 vertical(8,9,2,3).
 vertical(11,9,2,5).
 vertical(2,10,2,3).
 vertical(6,10,2,3).
 vertical(9,10,4,12).
 vertical(2,11,9,45).
 vertical(4,12,3,7).
 vertical(8,12,2,4).
