/*  File         : syncol.pl
    Purpose      : interface SWI-prolog syntax coloring library

    pqSource     : interfacing SWI-Prolog source files and Qt

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

:- module(syncol, [syncol/2, recolor/4, syncolours/0, syncol_allfile/2]).

:- use_module(library(prolog_xref)).
:- use_module(library(prolog_colour)).
:- use_module(library(prolog_source)).

%% call the main interface to get a source colourized
%
%  see http://www.swi-prolog.org/pldoc/doc_for?object=prolog_colour:prolog_colourise_stream/3
%  and http://www.swi-prolog.org/pldoc/doc_for?object=prolog_xref:xref_source/2
%
%  note C is Qt callback argument: see PREDICATE(callback, 4) in pqSource.cpp
%
syncol(F, C) :-
  load_source(F),
  xref_source(F),
  open(F, read, S),
  prolog_colourise_stream(S, F, callback(C)),
  close(S).

%% recolor(+Text, +File, +CallbackArg, -PositionsOrError)
%
recolor(Text, File, CallbackArg, ErrorPos) :-
  atom_to_memory_file(Text, H),
  open_memory_file(H, read, S, [free_on_close(true)]),
  prolog_colourise_term(S, File, callback(CallbackArg), [subterm_positions(PositionsOrError)]),
  ( PositionsOrError = error_position(_StartClause, _EndClause, ErrorPos) ; true ),
  close(S).

/*
read_clause_positions(Atom, Module, Offset, Term, Positions, Error) :-
  atom_to_memory_file(Atom, H),
  open_memory_file(H, read, S, [free_on_close(true)]),
  read_source_term_at_location(S, Term, [module(Module), offset(Offset), subterm_positions(Positions), error(Error)]),
  close(S).
*/

%% list known fragments
%
syncolours :-
    forall(syntax_colour(C,A), pqSource:class_attributes(C,A)).

%% process entire file in Prolog, get back results list
%
:- thread_local frag/4.

syncol_allfile(F, L) :-
    retractall(frag(_,_,_,_)),
    load_source(F),
    xref_source(F),
    open(F, read, S),
    prolog_colourise_stream(S, F, callback_allfile),
    close(S),
    setof(frag(A, B, C, D), frag(A, B, C, D), L).

callback_allfile(U, V, Z) :-
    syntax_colour(U, Attributes) -> assertz(frag(V, Z, U, Attributes)) ; assertz(frag(V, Z, U, [])).

%%  apply useful behaviour changes
%
load_source(F) :-
    user:load_files(F, [silent(true),redefine_module(true)]).

/*
callback_allfile(U,V,Z) :-
        (   syntax_colour(U, Attributes), Attributes \= []
        ->  assertz(frag(V, Z, U, Attributes))
        ;   true
        ).
*/
