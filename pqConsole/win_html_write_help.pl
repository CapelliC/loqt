/*  File         : win_html_write_help.pl
    Purpose      : output HTML DCG to win console

    pqConsole    : interfacing SWI-Prolog and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

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

:- module(win_html_write_help, [win_html_write_help/1]).
:- use_module(library(http/html_write)).

:- meta_predicate win_html_write(0).
:- meta_predicate win_html_write_help(0).

%% win_html_write_help(+D) is det.
%
%  expand HTML DCG output to win console
%
%  @arg D DCG definition
%
win_html_write_help(D) :-
	phrase(html(D), Tokens),
	with_output_to(atom(X), print_html(Tokens)),
	win_html_write(X).
