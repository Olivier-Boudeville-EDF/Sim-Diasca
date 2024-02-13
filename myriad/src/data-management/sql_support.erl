% Copyright (C) 2016-2024 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Wednesday, June 8, 2016.


% @doc Gathering of various facilities regarding the management of <b>SQL
% databases</b>.
%
% The current implementation is mostly geared towards the use of:
%
% - `SQLite3' (an optional dependency), through the <a
% href="https://github.com/alexeyr/erlang-sqlite3">erlang-sqlite3</a> binding
%
% - `PostgreSQL', through either its command-line client (psql; the least
% recommended approach) or the epgsql [https://github.com/epgsql/epgsql] binding
%
% See `sql_support_test.erl' for the corresponding test.
%
-module(sql_support).



% Design notes:
%
% To manage SQL databases, we rely on:
%
% - either SQLite3 and the erlang-sqlite3 binding,
% expected to be already built and installed, in ~/Software/erlang-sqlite3
%
% - or PostgreSQL and the epgsql binding, expected to be already built and
% installed, in ~/Software/epgsql



% Vocabulary section (the terms that we retain as canonical here are
% emphasised):
%
% - a computer (*database host*) may host multiple clusters (on different ports)
%
% - a cluster is a *database server* (i.e. a daemon, a software service), and
% may have multiple catalogs
%
% - a catalog is a *database instance*, and may have multiple schemas
%
% - a *schema* is a table namespace defined in the context of a database
% instance, and may have multiple tables
%
% - a *table* gathers *records* (i.e. rows), structured according to *fields*
% (i.e. columns)


% Binary extraction:
%
% When extracting the value of a field containing raw (binary) data, it comes as
% a binary string containing hexadecimal characters that, at least with epgsql,
% at least in some cases, are prefixed with "\\x".
%
% For example, if the actual content in the database is (in hexadecimal form)
% A=<<"e3648a0024">>, then R=<<"\\xe3648a0024">> will be read.
%
% To get back the expected binary, when obtaining R from a record field returned
% by a query, one may use: '<<"\\x", A/binary>> = R' to get A, and then:
% ActualData = text_utils:hexabinstring_to_binary(A) to finally obtain the
% requested data. See get_data/1.



% For the various database records involved:
-include("sql_support.hrl").

-if(myriad_sql_backend =:= sqlite3).

% For the various types defined:
-include_lib("sqlite3.hrl").

-endif.


-if(myriad_sql_backend =:= postgresql).

% For the various types defined:
-include_lib("epgsql.hrl").

-endif.



-type database_host_name() :: net_utils:string_host_name().
% Designates the hostname on which a target database server instance is running.
% For example "baz.foobar.org."


-type database_port() :: net_utils:tcp_port().
% The (TCP) port at which the target database server instance is running.
% For example 5432.


% No database_server() of use.


-type database_name() :: ustring().
% The name of the target database instance. For example "acme_stock_db".
% A database instance is also designated as a catalog.


-type bin_database_name() :: bin_string().
% The name of the target database instance. For example `<<"acme_stock_db">>'.
% A database instance is also designated as a catalog.


-type any_database_name() :: database_name() | bin_database_name().



-type schema_name() :: ustring().
% A schema describes the organisation of the tables of a database. It defines a
% namespace of tables, and security boundaries.
% For example "customer_schema".


-type bin_schema_name() :: bin_string().
% A schema describes the organisation of the tables of a database. It defines a
% namespace of tables, and security boundaries.
% For example `<<"customer_schema">>'.

-type any_schema_name() :: schema_name() | bin_schema_name().



-type table_name() :: ustring().
% A table of a database contains records comprising the same number of fields.


-type bin_table_name() :: bin_string().
% A table of a database contains records comprising the same number of fields.

-type any_table_name() :: table_name() | bin_table_name().



% Meaningless: -type record_name() :: ustring().
% A record (also called a 'row' or a 'tuple') represents a single, implicitly
% structured data item in a table.


-type field_name() :: ustring().
% A field (also designated as 'column' or 'attribute') represents a set of data
% values of a particular type, one value for each record of the table.

-type bin_field_name() :: ustring().
% A field (also designated as 'column' or 'attribute') represents a set of data
% values of a particular type, one value for each record of the table.

-type any_field_name() :: field_name() | bin_field_name().
% A field (also designated as 'column' or 'attribute') represents a set of data
% values of a particular type, one value for each record of the table.




-type database_connection_settings() :: #database_connection_settings{}.
% The settings needed to designate a database server.



-type database_user_name() :: ustring().
% The name of a user of a database server. For example "john_smith".

-type database_user_password() :: ustring().
% The password of a user of a database server.

-type database_user_settings() :: #database_user_settings{}.
% The settings corresponding to a given user of a database server.


-type database_access_settings() ::
		{ database_connection_settings(), database_user_settings() }.
% All settings necessary to access a database server.


-type backend_name() :: 'sqlite3' | 'postgresql'.
% The supported SQL backends (as atom names).


% A PID for epgsql:
% Not accepted: -opaque connection() :: any().
-type connection() :: any().
% Designates an (opaque, and backend-dependent) connection handle.


-type query_string() :: ustring().
% A SQL query, as a string.
% For example "select * from customers".


-type query_format() :: text_utils:format_string().
% The format string to compose a query.

-type query_values() :: text_utils:format_values().
% The format values to compose a query.



-type operation_count() :: count().
% A number of database operations, typically performed in the context of a
% query.


-type field_description() :: #field_description{}.
% The description of a field (a column).




-type record() :: type_utils:tuple( field_value() ).
% An actual record (a row).


-type read_binary() :: binary().
% Data as directly read, as a (prefixed) binary string containing hexadecimal
% values.


-type field_value() :: maybe( read_binary() ).
% The value of a field in a returned record, that may be 'null', that is
% translated here as 'undefined' (hence the maybe).


-type user_name() :: ustring().
% Any notion of user identifier.

-type user_id() :: count().
% Any notion of user identifier.



-type timestamp() :: ustring().
% A database timestamp.
% For example "2021-11-08 13:33:52.895374".


-type query_result() :: select_result() | update_result() | insert_result()
					  | delete_result().
% The result of the execution of a query.

-type select_result() :: { [ field_description() ], [ record() ] }.
% The result of a select operation.

-type update_result() :: operation_count().
% The result of an update operation, that is the number of operations performed.

-type insert_result() :: operation_count().
% The result of an insert operation, that is the number of operations performed.

-type delete_result() :: operation_count().
% The result of a delete operation, that is the number of operations performed.



-export_type([ database_host_name/0, database_port/0,
			   database_name/0, bin_database_name/0, any_database_name/0,
			   schema_name/0, bin_schema_name/0, any_schema_name/0,
			   table_name/0, bin_table_name/0, any_table_name/0,

			   field_name/0, bin_field_name/0, any_field_name/0,

			   database_connection_settings/0,

			   database_user_name/0, database_user_password/0,
			   database_user_settings/0,

			   database_access_settings/0,

			   backend_name/0, connection/0,

			   query_string/0, operation_count/0, field_description/0,

			   record/0, read_binary/0, field_value/0,
			   user_name/0, user_id/0, timestamp/0,

			   query_result/0, query_format/0, query_values/0,
			   select_result/0, update_result/0, insert_result/0,
			   delete_result/0 ]).



-export([ list_possible_backend_names/0, has_backend/1, get_backend_name/0,
		  start/0,
		  connect/2, connect/3,
		  execute_query/2, execute_query/3,
		  list_database_names/1,
		  list_schema_names/2,
		  list_table_names/3,
		  get_data/1,
		  %extract_field/6,
		  close/1,
		  stop/0,
		  connection_settings_to_string/1, user_settings_to_string/1 ]).




% Shorthands:

-type count() :: basic_utils:count().
-type base_status() :: basic_utils:base_status().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type time_out() :: time_utils:time_out().



% General support section, regarding the SQL service itself.



% @doc Returns the various known SQL backend names (in general; not telling
% whether they are available locally or not).
%
-spec list_possible_backend_names() -> [ backend_name() ].
list_possible_backend_names() ->
	[ sqlite3, postgresql ].



% @doc Tells whether, from the point of view of Myriad, the specified SQL
% backend is available (that is currently and locally, at runtime).
%
-spec has_backend( backend_name() ) -> boolean().
has_backend( BackendName ) ->

	% The availability of a given backend is determined on whether its main
	% module can be found in the code path:
	%
	BackendModule = case BackendName of

		sqlite3 ->
			sqlite3;

		postgresql ->
			epgsql

	end,

	case code_utils:is_beam_in_path( BackendModule ) of

		not_found ->
			false;

		[ _SinglePath ] ->
			true;

		MultiplePaths ->
			trace_utils:warning_fmt( "Multiple '~ts' modules found for the SQL "
				"backend ~ts, in: ~ts", [ BackendModule, BackendName,
					text_utils:strings_to_string( MultiplePaths ) ] ),
			true

	end.



% @doc Returns the type of the currently used (build-time) SQL backend (if any).
-spec get_backend_name() -> maybe( backend_name() ).
get_backend_name() ->
	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3, sqlite3 },

		{ postgresql, postgresql },

		{ none, undefined } ],

		% Default token:
		none ).



% @doc Starts (checks and inits) the SQL service support.
-spec start() -> void().
start() ->
	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			% We have to secure the erlang-sqlite3 binding, nevertheless nothing
			% special is needed (e.g. finding ebin/sqlite3.beam results in
			% finding automatically priv/sqlite3_drv.so).

			%trace_utils:debug( "Starting the SQL support, using SQLite3." )
			ok
		},

		{ postgresql,
			%trace_utils:debug( "Starting the SQL support, using PostgreSQL." )
			ok
		},

		{ none, throw( no_myriad_sql_backend_enabled ) } ],

		% Default token:
		none ).



% @doc Connects to the specified database, with a default time-out.
-spec connect( database_connection_settings(), database_user_settings() ) ->
							fallible( connection() ).
connect( ConnSettings, UserSettings ) ->
	connect( ConnSettings, UserSettings, _TimeOutMs=5000 ).


% @doc Connects to the specified database, with specified time-out.
-spec connect( database_connection_settings(), database_user_settings(),
			   time_out() ) -> fallible( connection() ).
connect( ConnSettings=#database_connection_settings{ host_name=HostnameStr,
													 port=MaybePort,
													 name=DbNameStr },
		 UserSettings=#database_user_settings{ user_name=UserName,
											   user_password=UserPassword },
		 TimeOut ) ->

	ActualPort = case MaybePort of

		undefined ->
			?default_database_port;

		Port ->
			Port

	end,

	ConnRes = cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			begin
				basic_utils:ignore_unused(
					[ ConnSettings, HostnameStr, MaybePort, DbNameStr,
					  UserSettings, UserName, UserPassword, TimeOut,
					  ActualPort ] ),
				{ ok, myriad_sqlite3_connection }
			 end },

		{ postgresql,
			epgsql:connect( _Opts=#{ host => HostnameStr,
									 port => ActualPort,
									 username => UserName,
									 password => UserPassword,
									 database => DbNameStr,
									 timeout => TimeOut,
									 % SQL NULL not translated as 'null':
									 nulls => [ undefined ] } ) },

		{ none,
			begin
				basic_utils:ignore_unused(
					[ ConnSettings, HostnameStr, MaybePort, DbNameStr,
					  UserSettings, UserName, UserPassword, TimeOut,
					  ActualPort ] ),
				throw( no_myriad_sql_backend_enabled )
			 end } ],

		% Default:
		none ),

	cond_utils:if_defined( myriad_debug_sql_support,
		trace_utils:debug_fmt( "Connection attempt to ~ts, as ~ts reported: ~w",
			[ connection_settings_to_string( ConnSettings ),
			  user_settings_to_string( UserSettings ), ConnRes ] ),
		basic_utils:ignore_unused( [ ConnSettings, UserSettings ] ) ),

	ConnRes.



% @doc Executes the specified SQL query based on the specified connection.
-spec execute_query( connection(), query_string() ) ->
											fallible( query_result() ).
execute_query( Conn, Query ) ->

	cond_utils:if_defined( myriad_debug_sql_support,
		trace_utils:debug_fmt( "Executing on connection ~w query '~ts'.",
							   [ Conn, Query ] ) ),

	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			begin
				basic_utils:ignore_unused( [ Conn, Query ] ),
				throw( to_do )
			end },

		{ postgresql,
			% Simple query:
			case epgsql:squery( Conn, Query ) of

				{ ok, ColumnDesc, Rows } ->
					cond_utils:if_defined( myriad_debug_sql_support,
						trace_utils:debug_fmt( "Query success, returning ~B "
							"field descriptions and ~B records.",
							[ length( ColumnDesc ), length( Rows ) ] ) ),

					{ ok, { ColumnDesc, Rows } };


				P={ ok, OpCount } ->
					cond_utils:if_defined( myriad_debug_sql_support,
						trace_utils:debug_fmt( "Query success, ~B operations "
							"performed.", [ length( OpCount ) ] ),
						basic_utils:ignore_unused( OpCount ) ),
					P;

				P={error, QError } ->
					cond_utils:if_defined( myriad_debug_sql_support,
						trace_utils:error_fmt( "Query failed: ~p.",
											   [ QError ] ),
						basic_utils:ignore_unused( QError ) ),
					P

			end },

		{ none,
			begin
				basic_utils:ignore_unused( [ Conn, Query ] ),
				throw( no_myriad_sql_backend_enabled )
			end } ],

		% Default:
		none ).



% @doc Executes the specified SQL query to formant, based on the specified
% connection.
%
-spec execute_query( connection(), query_format(), query_values() ) ->
											fallible( query_result() ).
execute_query( Conn, QueryFormat, QueryValues ) ->
	execute_query( Conn, text_utils:format( QueryFormat, QueryValues ) ).



% @doc Returns a list of all the database instances hosted by the database
% server designated by the specified connection.
%
-spec list_database_names( connection() ) ->
											fallible( [ bin_database_name() ] ).
list_database_names( Conn ) ->

	% Queries depend on the database backend:
	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			begin
				basic_utils:ignore_unused( Conn ),
				throw( to_do )
			end },

		{ postgresql,
			 begin
				case execute_query( Conn, "select datname from pg_database "
									"where datistemplate = false" ) of

					{ ok, { _FieldDescs, Records } } ->
						DbNames = [ N || { N } <- Records ],
						{ ok, DbNames };

					Error ->
						Error

				end
			 end },

		{ none,
			begin
				basic_utils:ignore_unused( Conn ),
				throw( no_myriad_sql_backend_enabled )
			end } ],

		% Default:
		none ).



% @doc Returns a list of the names of all the existing schemas in the database
% instance of the database server designated by the specified connection.
%
% For example `[<<"pg_catalog">>, <<"information_schema">>, <<"public">>]'.
%
-spec list_schema_names( connection(), any_database_name() ) ->
											fallible( [ bin_schema_name() ] ).
list_schema_names( Conn, DbInstanceName ) ->

	BinDbInstanceName = text_utils:ensure_binary( DbInstanceName ),

	% Queries depend on the database backend:
	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			begin
				basic_utils:ignore_unused( [ Conn, BinDbInstanceName ] ),
				throw( to_do )
			end },

		% Supposedly better than 'select nspname from pg_catalog.pg_namespace':
		{ postgresql,
			 begin
				Query = text_utils:format( "select schema_name from "
					"information_schema.schemata where catalog_name = '~ts'",
					[ BinDbInstanceName ] ),

				case execute_query( Conn, Query ) of

					{ ok, { _FieldDescs, Records } } ->
						SchNames = [ N || { N } <- Records ],
						{ ok, SchNames };

					Error ->
						Error

				end
			 end },

		{ none,
			begin
				basic_utils:ignore_unused( [ Conn, BinDbInstanceName ] ),
				throw( no_myriad_sql_backend_enabled )
			end } ],

		% Default:
		none ).



% @doc Returns a list of the names of all the existing tables in the specified
% schema of the specified database instance of the database server designated by
% the specified connection.
%
-spec list_table_names( connection(), bin_database_name(),
						bin_schema_name() ) -> fallible( [ bin_table_name() ] ).
list_table_names( Conn, DbInstanceName, SchemaName ) ->

	BinDbInstanceName = text_utils:ensure_binary( DbInstanceName ),
	BinSchemaName = text_utils:ensure_binary( SchemaName ),

	% Queries depend on the database backend:

	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			begin
				basic_utils:ignore_unused(
					[ Conn, BinDbInstanceName, BinSchemaName ] ),
				throw( to_do )
			end },

		{ postgresql,
			 begin
				% To exclude views, put "AND table_type = 'BASE TABLE'" to the
				% where clause:
				%
				Query = text_utils:format( "select table_name from "
					"information_schema.tables where table_catalog = '~ts' "
					" and table_schema = '~ts'",
					[ BinDbInstanceName, BinSchemaName ] ),

				case execute_query( Conn, Query ) of

					{ ok, { _FieldDescs, Records } } ->
						TbNames = [ N || { N } <- Records ],
						{ ok, TbNames };

					Error ->
						Error

				end
			 end },

		{ none,
			begin
				basic_utils:ignore_unused( [ Conn, BinDbInstanceName,
											 BinSchemaName ] ),
				throw( no_myriad_sql_backend_enabled )
			end } ],

		% Default:
		none ).



% @doc Returns the actual binary that is stored in a database data field, from
% the one directly read (with is prefixed and in hexadecimal character).
%
% Refer to the 'Binary extraction' section above for further details.
%
-spec get_data( read_binary() ) -> binary().
get_data( ReadBinString ) ->

	% ReadBinString is a binary string containing hexadecimal characters.
	%
	% For some reason, at least with epgsql, a "\x" prefix is present, let's
	% remove it:
	%
	<<"\\x", ReadHexaBinStr/binary>> = ReadBinString,

	text_utils:hexabinstring_to_binary( ReadHexaBinStr ).



% @doc Closes the specified database connection.
-spec close( connection() ) -> base_status().
close( Conn ) ->

	cond_utils:if_defined( myriad_debug_sql_support,
		trace_utils:debug_fmt( "Closing connection ~p.", [ Conn ] ) ),

	cond_utils:switch_set_to( myriad_sql_backend, [

		{ sqlite3,
			begin
				basic_utils:ignore_unused( Conn ),
				throw( to_do )
			end },

		{ postgresql, epgsql:close( Conn ) },

		{ none,
			begin
				basic_utils:ignore_unused( Conn ),
				throw( no_myriad_sql_backend_enabled )
			end } ],

		% Default:
		none ).



% @doc Stops the SQL support.
-spec stop() -> void().
stop() ->
	cond_utils:if_defined( myriad_debug_sql_support,
						   trace_utils:debug( "Stopping the SQL support." ),
						   ok ).



% @doc Returns a textual description of the specified database connection
% settings.
%
-spec connection_settings_to_string( database_connection_settings() ) ->
											ustring().
connection_settings_to_string( #database_connection_settings{
									host_name=DbHostname,
									port=MaybeDbPort,
									name=DbName } ) ->

	HostStr = case MaybeDbPort of

		undefined ->
			DbHostname;

		Port ->
			text_utils:format( "~ts (port: ~B)", [ DbHostname, Port ] )

	end,

	text_utils:format( "database instance '~ts' running on host ~ts",
					   [ DbName, HostStr ] ).



% @doc Returns a textual description of the specified database user settings.
-spec user_settings_to_string( database_user_settings() ) -> ustring().
user_settings_to_string( #database_user_settings{
							user_name=UserName,
							user_password=UserPassword } ) ->
	% Password could be hidden:
	text_utils:format( "user '~ts' with password '~ts'",
					   [ UserName, UserPassword ] ).
