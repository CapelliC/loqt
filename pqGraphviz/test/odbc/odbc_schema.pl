/** <module> odbc_schema
 *
 *  build a schema out of ODBC connection, links by foreign keys or conventional identifier
 */

:- module(odbc_schema,
        [show_schema/1
	,connection_schema/2
	,connect_db/4
	,connect_db/5
	,disconnect_db/1
        ]).

:- use_module(library(odbc)).
:- use_module(graph_schema).

%%	show_schema(+Connection) is semidet.
%
%	capture schema from connection, then create a graph window
%
show_schema(Connection) :-
	connection_schema(Connection, Schema),
	graph_schema(Schema).

%% connection_schema(+Connection, -Schema:dict) is det.
%
%	Get (and cache) a schema object, containing DB name and a list of tables.
%	Each table{name,columns,primaryKeys,foreignKeys}
%
%	* a name
%	* a not empty list of columns
%	* a list (maybe empty) of columns' names partecipating in the primary key
%	* a list (maybe empty) of optional foreign keys
%	* a list (maybe empty) of namedKeys of kind `table`_id, where `table` has primary_key `table_id`
%
%  @arg Connection	a valid ODBC connection
%  @arg Schema		a schema{name, tables}
%
connection_schema(Connection, Schema) :-
	odbc_get_connection(Connection, database_name(Db)),
	(	schema(Db, Schema)
	->	true
	;	findall(table{name:Table, columns:Fields, primaryKey:PrimaryKey, foreignKeys:ForeignKeys}, (
			odbc_current_table(Connection, Table),
			findall(column{name:Column, type:Type},
				table_column(Connection, Table, Column, Type), Fields),
			findall(Column,
				odbc_table_primary_key(Connection, Table, Column), PrimaryKey),
			findall(foreignKey{pkCol:PkCol, fkTable:FkTable, fkCol:FkCol},
				odbc_table_foreign_key(Connection, Table, PkCol, FkTable, FkCol), ForeignKeys)
		), Tables),
		namedKeys(Tables, Named),
		Schema = schema{name:Db, tables:Named},
		assertz(schema(Db, Schema))
	).

:- dynamic schema/2.

%% table_column(Connection, Table, Column, Type) is semidet.
%
%  on backtracking, enumerate columns of Table. Accounts for some small problem in metadata.
%
%  @see odbc_table_column/3
%
%  @arg Connection	a valid connection
%  @arg Table		table name
%  @arg Column		column name
%  @arg Type		describe (Prolog Type)[sqltype]
%
table_column(Connection, Table, Column, TypeC) :-
	odbc_table_column(Connection, Table, Column, type(Type)), term_to_atom(Type, TypeS),
	% clean up some problem in metadata interface
	(	sub_atom(TypeS,_,_,_,smallin)
	->	TypeC = smallint
	;	sub_atom(TypeS,_,_,_,datetim)
	->	TypeC = datetime
	;	TypeC = TypeS
	).

namedKeys(Tables, Named) :-
	maplist(namedKeys(Tables), Tables, Named).
namedKeys(Tables, Table, Named) :-
	findall(namedKey{key:Key, targetTable:TId}, (
		member(C, Table.columns),
		C >:< column{name:Key, type:integer},
		sub_atom(Key, _, 3, 0, '_id'),
		sub_atom(Key, _, _, 3, TId),
		TId \= Table.name,
		member(T, Tables),
		T.name == TId,
		memberchk(Key, T.primaryKey)
	), NamedKeys),
	Named = Table.put([namedKeys(NamedKeys)]).

%% connect_db(Db, Uid, Pwd, Connection) is det.
%
%  build a mysql connect string and connect
%
%  @arg Db		Database Name
%  @arg Uid		user ID
%  @arg Pwd		password
%  @arg Connection	handler to opened ODBC connection
%
connect_db(Db, Uid, Pwd, Connection) :-
	connect_db(mysql, Db, Uid, Pwd, Connection).

%% connect_db(Driver, Db, Uid, Pwd, Connection) is det.
%
%  build a driver connect string and connect
%
%  @arg Driver		Driver Key: for instance mysql (default)
%  @arg Db		Database Name
%  @arg Uid		user ID
%  @arg Pwd		password
%  @arg Connection	handler to opened ODBC connection
%
connect_db(Driver, Db, Uid, Pwd, Connection) :-
	format(atom(S), 'driver=~s;db=~s;uid=~s;pwd=~s', [Driver, Db, Uid, Pwd]),
	odbc_driver_connect(S, Connection, []).

%% disconnect_db(C) is det.
%
%  close the connection
%
%  @arg C a valid connection
%
disconnect_db(C) :-
	odbc_disconnect(C).
