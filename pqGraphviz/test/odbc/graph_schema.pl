/** <module> graph_schema
 *
 *  Make a graph of schema as recovered by [odbc_schema](odbc_schema.pl)
 *  --------
 *
 *  source file /home/carlo/cpp/loqt/pqGraphviz/test/odbc/graph_schema.pl
 *  created at Tue Feb 18 00:33:37 2014
 *
 *  @author carlo
 *  @version 0.9.9
 *  @copyright carlo
 *  @license LGPL v2.1
 */

:- module(graph_schema,
	[graph_schema/1
	,graph_schema/2
	]).

:- use_module(library(http/html_write)).
:- use_module(library(option)).

:-  predicate_options(graph_schema/2, 2,
	[pass_to(show_schema_db/3, 2)
	]).
:-  predicate_options(show_schema_db/3, 3,
	[pass_to(show_table/3, 3)
	]).
:-  predicate_options(show_table/3, 3,
        [tableHeaderStyle(+atom)
	,primaryKeyStyle(+atom)
	,namedKeyStyle(+atom)
        ]).

%%  graph_schema(+Schema:dict) is det.
%
%   put Schema in a graph window, with Schema name as title
%
%   @arg Schema must contains (database) name and tables
%
graph_schema(Schema) :- graph_schema(Schema, []).

%%  graph_schema(+Schema:atom, Options) is det.
%
%   put Schema in a graph window, with Schema name as title
%
graph_schema(Schema, Options) :-
	graph_window(show_schema_db(Schema, Options),
		[window_title(Schema.name)
		,node_defaults([shape=none])
		]).

%% show_schema_db(Schema, Graph) is det.
%
%  build all Table nodes found in Schema, then build links on foreign keys
%
%  @arg Schema contains tables and foreignKeys
%  @arg Graph is the context graph
%
show_schema_db(Schema, Options, Graph) :-
	forall(member(T, Schema.tables), show_table(Graph, T, Options)),
	forall(member(T, Schema.tables), show_foreignkey(Graph, T)),
	forall(member(T, Schema.tables), show_namedkey(Graph, T)).

%% show_table(G, Table) is det.
%
%  make an HTML node with all typed fields and attributes to show PrimaryKey
%
%  @arg G current Graph context
%  @arg Table Table descriptor
%  @tbd Apply true style options. Now just the background color is handed to cells.
%
show_table(G, Table, Options) :-
	option(tableHeaderStyle(THS), Options, yellow),
	option(primaryKeyStyle(PKS), Options, green),
	findall(tr([ td([port=Column|Attrs], Column), td(Type) ]), (
		dict_member(_{name:Column, type:Type}, Table.columns),
		( memberchk(Column, Table.primaryKey) -> Attrs = [bgcolor=PKS] ; Attrs = [] )
	), ColRows),
	phrase(html(table([tr(td([bgcolor=THS, colspan=2], Table.name))|ColRows])), Tokens),
	with_output_to(atom(HTML), print_html(Tokens)),
	make_node(G, Table.name, [label:html(HTML)], _),
	debug(graph_schema, '~w:~w', [Table.name, HTML]).

%% show_foreignkey(G:atom, T:dict, FKs:list(dict)) is det.
%
%  build a link between fields from PrimaryKey Table to Foreign Table fields
%
%  @arg G current Graph context
%  @arg T Table descriptor
%  @arg FKs list of foreign key descriptors
%
show_foreignkey(G, T) :-
	forall(dict_member(_{pkCol:PkCol, fkTable:FkTable, fkCol:FkCol}, T.foreignKeys), (
		find_node(G, T.name, PkTp),
		find_node(G, FkTable, FkTp),
		new_edge(G, PkTp, FkTp, E),
		set_attrs(E, [headport:FkCol, tailport:PkCol])
	)).

show_namedkey(G, T) :-
	forall(dict_member(_{key:Col, targetTable:Tab}, T.namedKeys), (
		find_node(G, T.name, PkTp),
		find_node(G, Tab, FkTp),
		new_edge(G, PkTp, FkTp, E),
		set_attrs(E, [style:dotted, headport:Col, tailport:Col])
	)).
	
%% dict_member(D, L) is det.
%
%  apply permissive member semantic to a list of dicts, matching elements
%
%  @arg D a dict to match
%  @arg L a list of dicts
%
dict_member(D, L) :- member(E, L), D >:< E.
