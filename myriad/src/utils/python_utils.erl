% Copyright (C) 2007-2021 Olivier Boudeville
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
% Adapted from code kindly contributed by EDF R&D.
%
% Authors: Robin Huart (robin-externe.huart@edf.fr)
%		   Samuel Thiriot (samuel.thiriot@edf.fr)


% Gathering of some convenient facilities for the binding to the Python
% language.
%
% See python_utils_test.erl for the corresponding tests.
%
% See also: java_utils.erl for a similar binding.
%
-module(python_utils).



% Helper exports:
-export([ get_beam_directories_for_binding/0,
		  send_oneway/3, wait_for_request_result/2,
		  pep8_class_to_pep8_module/1 ]).



% PID corresponding to a Python interpreter:
-type interpreter_pid() :: pid().


% The title of a request sent to Python.
-type title() :: atom().


% The parameters of a request sent to Python.
-type body() :: [ any() ].


% The result from a request that was sent to Python.
-type result() :: any().


% The name of a Python class, according to the PEP8:
-type pep8_classname() :: atom().


% The name of a Python module, according to the PEP8:
-type pep8_class_module() :: atom().


-export_type([ interpreter_pid/0, title/0, body/0, result/0,
			   pep8_classname/0, pep8_class_module/0 ]).


% Shorthands:
-type ustring() :: text_utils:ustring().



% Implementation notes:
%
% The actual Erlang-Python binding is obtained thanks to the ErlPort library
% (http://erlport.org/; source in https://github.com/hdima/erlport), to be
% installed in ~/Software/ErlPort/ErlPort-current-install.
%
% Please refer to the 'Prerequisite section' of system_utils.erl for more
% guidance about installation paths.



% Finds the BEAM locations of all the dependencies required for binding to
% Python.
%
-spec get_beam_directories_for_binding() -> [ file_utils:directory_name() ].
get_beam_directories_for_binding() ->

	% A single directory is apparently necessary and sufficient so that the
	% computing nodes have access both to ErlPort.
	%
	% Previously we thought that system_utils:get_dependency_base_directory(
	% "ErlPort" ) was needed as well, so that initialization of the interpreters
	% would not fail with {not_found,"erlport/priv"}, short of being able to
	% find erlport.beam, yet it was due to the base directory bearing a
	% different name than 'erlport' in our installation settings.

	% Needed so that python:start/1 and all are found:
	[ system_utils:get_dependency_code_directory( "ErlPort" ) ].



% Requests specified interpreter to execute specified oneway.
-spec send_oneway( interpreter_pid(), title(), body() ) -> void().
send_oneway( InterpreterPid, MessageTitle, MessageBody )
  when is_atom( MessageTitle ) ->

	python:cast( InterpreterPid, { self(), MessageTitle, MessageBody } ).



% Receives a message from the Python world, usually in answer to a send_oneway/3
% call having used the same MessageTitle argument, and tries to match it with
% the different accepted types of messages.
%
-spec wait_for_request_result( interpreter_pid(), title() ) -> result().
wait_for_request_result( InterpreterPid, MessageTitle )
  when is_atom( MessageTitle ) ->

	% Waits for the response:
	Message = receive

		_Msg={ Headers, Body } when is_tuple( Headers )
				andalso erlang:element( 1, Headers ) == python_message ->

			erlang:append_element( erlang:delete_element( 1, Headers ), Body )

	end,

	case Message of

		% Return of a successful request:
		{ request_completed, _ReceivedData } ->
			Message;

		% Trace emitted from Python:
		TraceMessage = { trace_emitted, TraceType, _TraceFormattedMessage }
		  when is_atom( TraceType ) ->
			TraceMessage;


		% Exception raised from Python:
		ExceptionMessage = { exception_raised, ExceptionType,
							 _ExceptionFormattedMessage }
		  when is_atom( ExceptionType ) ->
			ExceptionMessage;


		% Catch-all clause for message receiving:
		OtherMessage ->
			trace_utils:error_fmt( "A message received from the Python "
				"interpreter driven by ~w, in answer to '~p', does not "
				"respect the expected format: ~p~n",
				[ InterpreterPid, MessageTitle, OtherMessage ] ),
			throw( { invalid_python_message_received, OtherMessage } )

	end.



% Deduces the name of the Python module from the name of the Python class
% (possibly prefixed with package names) that it implements, according notably
% to the naming conventions adopted in PEP 8.
%
% Ex: 'Partner__TransportModel__MyFoobarExample' resulting in
% 'partner.transport_model.my_foobar_example'.
%
-spec pep8_class_to_pep8_module( pep8_classname() | ustring() ) ->
										pep8_class_module().
pep8_class_to_pep8_module( Classname ) when is_atom( Classname ) ->
	pep8_class_to_pep8_module( text_utils:atom_to_string( Classname ) );

pep8_class_to_pep8_module( ClassnameString ) ->

	% Splits the classname on "__", so that for example
	% "Partner__TransportModel__MyFoobarExample" becomes
	% [ "Partner", "TransportModel", "MyFoobarExample" ]:
	%
	TokensCamel = string:split( ClassnameString, _Pattern="__",
								_Where=all ),

	% All of them will be switched from CamelCase to snake_case, so:
	% [ "Partner", "TransportModel", "MyFoobarExample" ] becomes:
	% [ "partner", "transport_model", "my_foobar_example" ].
	%
	TokensSnake = [ string:join( [ string:to_lower( CamelWord )
				|| CamelWord <- text_utils:split_camel_case( CamelCaseToken )
								 ], _Separator="_" )
					|| CamelCaseToken <- TokensCamel ],

	% The concatenation of those should lead to a valid package name, so that
	% [ "partner", "transport_model", "my_foobar_example" ] becomes
	% ultimately the 'partner.transport_model.my_foobar_example' atom:
	%
	ModuleRef = string:join( TokensSnake, "." ),

	text_utils:string_to_atom( ModuleRef ).
