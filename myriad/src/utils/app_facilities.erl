% Copyright (C) 2011-2022 Olivier Boudeville
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
% Creation date: 2011.


% @doc This module defines a few basic facilities for <b>applications</b> (in
% the Myriad sense, not OTP one).
%
-module(app_facilities).


-export([ start/1, stop/0,
		  get_app_info/2, get_app_info/3, get_app_info_map/1,
		  display/1, display/2, fail/1, fail/2, finished/0 ] ).


-include("app_facilities.hrl").

-type app_info() :: #app_info{}.

-type app_info_map() :: filename:basedir_opts().
% Needed by some standard functions in the filename module.

-type any_app_info() :: app_info() | app_info_map().


-export_type([ app_info/0, app_info_map/0, any_app_info/0 ]).


% Shorthands:

-type any_version() :: basic_utils:any_version().

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().
-type string_like() :: text_utils:string_like().

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils:format_values().



% @doc Starts an application; expected to be the first application statement.
%
% Here we disable explicitly the trapping of EXIT events, as a function run
% through `erl -eval' (like our cases) or through `erl -run' will be executed in
% a process which will silently trap EXIT events, which would mean that the
% crash of any process created from the case, even thanks to spawn_link, would
% most probably remain unnoticed (just leading to an EXIT message happily
% sitting in the mailbox of the case process).
%
-spec start( module() | [ module() ] ) -> void().
start( Module ) when is_atom( Module ) ->
	erlang:process_flag( trap_exit, false ),
	basic_utils:display( "~n~n--> Starting application ~ts.~n", [ Module ] );

start( Modules ) when is_list( Modules ) ->
	erlang:process_flag( trap_exit, false ),
	basic_utils:display( "~n~n--> Starting application ~p.~n", [ Modules ] ).



% @doc Stops an application; expected to be the last application statement in
% the normal case.
%
-spec stop() -> no_return().
stop() ->
	basic_utils:display( "\n--> Successful termination of application.\n" ),
	finished().



% @doc Returns an application information corresponding to specified application
% name and version.
%
-spec get_app_info( string_like(), any_version() ) -> app_info().
get_app_info( AppName, AppVersion ) ->
	get_app_info( AppName, AppVersion, _MaybeAuthorDesc=undefined ).


% @doc Returns an application information corresponding to specified application
% name, version and possibly author description.
%
-spec get_app_info( string_like(), any_version(),
					maybe( any_string() ) ) -> app_info().
get_app_info( AppName, AppVersion, MaybeAuthorDesc )
								when is_tuple( AppVersion ) ->

	MaybeBinAuthorDesc = case MaybeAuthorDesc of

		undefined ->
			undefined;

		AuthorDesc ->
			text_utils:ensure_binary( AuthorDesc )

	end,

	{ OSFamily, OSName } = system_utils:get_operating_system_type(),

	BinAppName = case is_atom( AppName ) of

		true ->
			text_utils:atom_to_binary( AppName );

		false ->
			text_utils:ensure_binary( AppName )

	end,

	#app_info{ name=BinAppName,
			   version=AppVersion,
			   author=MaybeBinAuthorDesc,
			   os_family=OSFamily,
			   os_name=OSName }.



% @doc Returns a map typically relevant for filename:basedir/3.
-spec get_app_info_map( app_info() ) -> app_info_map().
get_app_info_map( #app_info{ name=BinAppName,
							 version=AppVersion,
							 author=MaybeBinAuthorDesc,
							 os_family=OSFamily,
							 os_name=OSName } ) ->

	OSType = case { OSFamily, OSName } of

		{ _, linux } ->
			linux;

		{ unix, _ } ->
			darwin;

		{ win32, _ } ->
			windows;

		_Other ->
			trace_utils:error_fmt( "Unable to categorise this operating "
				"system, whose family is '~ts' and name is '~ts'.",
				[ OSFamily, OSName ] ),
			throw( { unexpected_os, OSFamily, OSName } )

	end,

	BaseMap = #{ name => BinAppName,
				 version => text_utils:version_to_string( AppVersion ),
				 os => OSType },

	case MaybeBinAuthorDesc of

		undefined ->
			BaseMap;

		BinAuthorDesc ->
			BaseMap#{ author => BinAuthorDesc }

	end.



% @doc Displays an application message.
-spec display( ustring() ) -> void().
display( Message ) ->
	% Carriage return already added in basic_utils:display/1:
	%
	% (empty format string added to force elements in message such as '~n' to be
	% transformed)
	%
	basic_utils:display( lists:flatten( Message ), _ValueList=[] ).


% @doc Displays an application message, once formatted.
%
% @param FormatString an io:format-style format string, ValueList is the
% corresponding list of field values.
%
-spec display( format_string(), format_values() ) -> void().
display( FormatString, ValueList ) ->
	basic_utils:display( FormatString, ValueList ).


% Comment out to be able to use the interpreter after the app:
-define(exit_after_app,).

% @doc Called whenever the execution is finished.
-spec finished() -> no_return().


-ifdef(exit_after_app).

finished() ->

	basic_utils:display( "(execution finished, interpreter halted)" ),

	% Probably not that useful:
	system_utils:await_output_completion(),

	% Implies flushing as well:
	basic_utils:stop_on_success(),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	app_success.

-else. % exit_after_app


finished() ->

	basic_utils:display( "(execution finished, "
						 "interpreter still running)~n" ),

	%system_utils:await_output_completion(),

	app_success.

-endif. % exit_after_app




% @doc To be called whenever an application is to fail (crash on error)
% immediately.
%
% Ex: `app_facilities:fail( "server on strike" )'
%
-spec fail( ustring() ) -> no_return().
fail( Reason ) ->

	% For some reason erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	basic_utils:display( "~n!!!! Application failed, reason: ~ts.~n~n",
						 [ Reason ] ),

	% Never returns:
	erlang:error( "Application failed" ),

	% Hence probably not that useful:
	system_utils:await_output_completion(),

	basic_utils:stop_on_failure(),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	app_failed.



% @doc To be called whenever an application is to fail (crash on error)
% immediately.
%
% @param FormatString an io:format-style format string.
%
% @param ValueList the corresponding list of field values.
%
% Ex: `app_facilities:fail("server ~ts on strike", ["foobar.org"])'.
%
-spec fail( format_string(), format_values() ) -> no_return().
fail( FormatString, ValueList ) ->

	% For some reason, erlang:error is unable to interpret strings as strings,
	% they are always output as unreadable lists.

	ErrorMessage = io_lib:format( "~n!!!! Application failed, reason: ~ts.~n~n",
								[ io_lib:format( FormatString, ValueList ) ] ),

	basic_utils:display( "~n!!!! Application failed, reason: ~ts.~n~n",
						 [ ErrorMessage ] ),

	% Never returns:
	erlang:error( "Application failed" ),

	% Hence probably not that useful:
	system_utils:await_output_completion(),

	basic_utils:stop_on_failure(),

	% Useless, but otherwise Dialyzer will complain that this function has no
	% local return:
	app_failed.
