/** <module> timelab
 *
 *  file /home/carlo/cpp/loqt/pqGraphviz/test/odbc/timelab.pl, created at Sun Jan 19 10:28:04 2014
 *  user carlo
 */

:- module(timelab,
	[timelab/0
	,show_table/1
	]).
:- use_module(odbc_schema).

timelab :-
	show_schema(dev_timelab,root,carlo).

show_table(Table) :-
	connect_db(dev_timelab,root,carlo,Connection),
	forall(odbc_table_column(Connection, Table, Column, Facet), writeln(Column:Facet)).
