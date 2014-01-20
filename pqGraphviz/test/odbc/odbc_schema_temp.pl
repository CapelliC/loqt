/** <module> odbc_schema
 *
 *  build a graph of DB schema
 */

:- module(odbc_schema,
	[show_schema/3
	,connect_db/4
	]).

% :- use_module(gv_uty).
:- use_module(library(odbc)).
:- use_module(library(http/html_write)).

show_schema(Db, Uid, Pwd) :-
	connect_db(Db, Uid, Pwd, Connection),
	graph_window(schema_db(RootGraph, Connection), RootGraph, [node_defaults([shape=none])]),
	odbc_disconnect(Connection).

schema_db(RootGraph, Connection) :-
	set_attrs(RootGraph, rankdir:'TD'),
	forall(odbc_current_table(Connection, Table), make_table(RootGraph, Connection, Table)),
	forall(	(	odbc_current_table(Connection, PkTable),
			odbc_table_foreign_key(Connection, PkTable, PkCol, FkTable, FkCol)
		),
		(	find_node(RootGraph, PkTable, PkTp),
			find_node(RootGraph, FkTable, FkTp),
			new_edge(RootGraph, PkTp, FkTp, E),
			set_attrs(E, [headport:FkCol, tailport:PkCol])
		)
	).

make_table(RootGraph, Connection, Table) :-
	setof(Column, odbc_table_primary_key(Connection, Table, Column), PrimaryKeys),
	findall(tr([ td([port=Column|Attrs], Column), td(TypeC) ]), (
		odbc_table_column(Connection, Table, Column, type(Type)), term_to_atom(Type, TypeS),
		% clean up some problem in metadata interface
		( sub_atom(TypeS,_,_,_,smallin) -> TypeC = smallint ; sub_atom(TypeS,_,_,_,datetim) -> TypeC = datetime ; TypeC = TypeS ),
		( memberchk(Column, PrimaryKeys) -> Attrs = [bgcolor=green] ; Attrs = [] )
	), Columns),
	phrase(html(table([tr(td([bgcolor=yellow, colspan=2], Table))|Columns])), Tokens),
	with_output_to(atom(HTML), print_html(Tokens)),
	make_node(RootGraph, Table, [label:html(HTML)], _).

connect_db(Db, Uid, Pwd, Connection) :-
	format(atom(S), 'driver=mysql;db=~s;uid=~s;pwd=~s', [Db, Uid, Pwd]),
	odbc_driver_connect(S, Connection, []).
