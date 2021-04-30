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
%		   Olivier Boudeville (olivier.boudeville@edf.fr)


% Gathering of some convenient facilities for the binding to the Java language.
%
% See java_utils_test.erl for the corresponding tests.
%
% See also: python_utils.erl for a similar binding.
%
-module(java_utils).



% Helper exports:
-export([ get_beam_directories_for_binding/0,
		  send_oneway/3, send_request/3, execute_request/3,
		  wait_for_request_result/2, classname_to_bytecode_filename/1,
		  fully_qualified_classname_to_string/1 ]).



% Design notes:
%
% Even if this module belongs to the Ceylan-Myriad layer, it tries to follow the
% conventions enforced in the upper Ceylan-WOOPER layer regarding method
% management.


% PID associated to a Java mailbox (a Java-based pseudo-process):
-type java_mbox_pid() :: pid().



% Designates a method to trigger on the Java side:
-type method_name() :: atom().

% Designates a oneway to trigger on the Java side:
-type oneway_name() :: method_name().

% Designates a request to trigger on the Java side:
-type request_name() :: method_name().



% The parameters of a method triggered on the Java side:
-type method_parameters() :: [ any() ].

% The parameters of a oneway triggered on the Java side:
-type oneway_parameters() :: [ any() ].

% The parameters of a request triggered on the Java side:
-type request_parameters() :: [ any() ].


% The result from a request that was sent to Java:
-type request_result() :: any().


% The name of a Java package (ex: 'org.foobar.research.someteam'):
-type java_package_name() :: atom().

% The name of a Java package, as a string (ex: "org.foobar.research.someteam"):
-type java_string_package_name() :: atom().


% The name of a Java class (ex: 'Foobar'):
-type java_classname() :: atom().

% The name of a Java class, as a string (ex: "Foobar"):
-type java_string_classname() :: ustring().


% Designates as precisely as possible a Java class:
-type java_fully_qualified_classname() :: java_classname() |
								  { java_package_name(), java_classname() }.


% The name of a Java source file (ex: "Foobar.java"):
-type java_source_filename() :: file_utils:file_name().


% The name of a Java compiled file (ex: "Foobar.class"):
-type java_bytecode_filename() :: file_utils:file_name().


-export_type([ java_mbox_pid/0,
			   method_name/0, oneway_name/0, request_name/0,
			   method_parameters/0, oneway_parameters/0, request_parameters/0,
			   request_result/0,
			   java_package_name/0, java_string_package_name/0,
			   java_classname/0, java_string_classname/0,
			   java_fully_qualified_classname/0,
			   java_source_filename/0, java_bytecode_filename/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().



% Implementation notes:

% The actual Erlang-Java binding is obtained thanks to the native Jinterface
% package, typically expected to be found in the
% lib/erlang/lib/jinterface-current-install directory (generally a symbolic link
% specifically created from the base directory of the Erlang installation,
% usually ~/Software/Erlang/Erlang-current-install).
%
% See http://erlang.org/doc/apps/jinterface/jinterface_users_guide.html
% for more information.
%
% On the Java side, the equivalent instance of an Erlang node is an OtpNode
% (approximately a JVM) and messages can be sent and/or received through
% instances of mailboxes (OtpMbox) that are associated to a PID (referred to as
% java_mbox_pid/0).
%
% From the point of view of an Erlang process, such a PID can be treated as if
% it was also an Erlang process.



% Finds the BEAM locations of all the dependencies required for binding to
% Java.
%
-spec get_beam_directories_for_binding() -> [ file_utils:directory_name() ].
get_beam_directories_for_binding() ->
	[].



% Sends the specified oneway to the specified Java pseudo-process.
%
-spec send_oneway( java_mbox_pid(), oneway_name(), oneway_parameters() ) ->
							void().
send_oneway( MailboxPid, OnewayName, OnewayParameters )
  when is_atom( OnewayName ) andalso is_list( OnewayParameters ) ->

	Message = { OnewayName, OnewayParameters },

	%trace_utils:debug_fmt( "Sending to ~w: ~p.", [ MailboxPid, Message ] ),

	% No PID sent, no answer to expect:
	MailboxPid ! Message.



% Sends the specified request to the specified Java pseudo-process.
%
-spec send_request( java_mbox_pid(), request_name(), request_parameters() ) ->
							void().
send_request( MailboxPid, RequestName, RequestParameters )
  when is_atom( RequestName ) andalso is_list( RequestParameters ) ->
	% PID sent, as a reply is wanted:
	MailboxPid ! { RequestName, RequestParameters, self() }.



% Sends for execution the specified request to the specified Java
% pseudo-process, and collects (synchronously) the corresponding result.
%
-spec execute_request( java_mbox_pid(), request_name(),
					   request_parameters() ) -> request_result().
execute_request( MailboxPid, RequestName, RequestParameters ) ->

	send_request( MailboxPid, RequestName, RequestParameters ),

	receive

		{ java_request_result, Result } ->
			Result

	end.



% Receives a message from the Java world, usually in answer to a send_oneway/3
% call having used the same MethodName argument, and tries to match it
% with the different accepted types of messages.
%
-spec wait_for_request_result( java_mbox_pid(), method_name() ) -> any().
wait_for_request_result( MailboxPid, MethodName )
  when is_atom( MethodName ) ->

	% Waits for the response:
	Message = receive

		_Msg={ Headers, MethodParameters } when is_tuple( Headers ) andalso
							erlang:element( 1, Headers ) == java_message ->

			erlang:append_element( erlang:delete_element( 1, Headers ),
								   MethodParameters )

	end,

	case Message of

		% Return of a successful request:
		{ request_completed, _ReceivedData } ->

			Message;

		% Trace emitted from Java:
		TraceMessage = { trace_emitted, TraceType, _TraceFormattedMessage }
		  when is_atom( TraceType ) ->

			TraceMessage;


		% Exception raised from Java:
		ExceptionMessage = { exception_raised, ExceptionType,
							 _ExceptionFormattedMessage }
		  when is_atom( ExceptionType ) ->

			ExceptionMessage;


		% Catch-all clause for message receiving:
		OtherMessage ->
			trace_utils:error_fmt( "A message received from a Java (Jinterface)"
				" OtpMbox driven by ~w, in answer to '~p', "
				"does not respect the expected format: ~p~n",
				[ MailboxPid, MethodName, OtherMessage ] ),
			throw( { invalid_java_message_received, OtherMessage } )

	end.



% Deduces the (root) name of a Java bytecode file from the name of the class it
% implements, according to the naming conventions used by the language.
%
% With Java, both names are identical except the extension, hence we just check
% if the name looks CamelCased, i.e. if at least its first letter is in upper
% case.
%
-spec classname_to_bytecode_filename( java_classname() | ustring() ) ->
											java_bytecode_filename().
classname_to_bytecode_filename( Classname ) when is_atom( Classname ) ->
	classname_to_bytecode_filename( text_utils:atom_to_string( Classname ) );

classname_to_bytecode_filename( ClassnameString )
  when is_list( ClassnameString ) ->

	case text_utils:is_uppercase( ClassnameString ) of

		true ->
			ClassnameString ++ ".class";

		false ->
			throw( { java_classname_not_camelcased, ClassnameString } )

	end.


% Returns a textual description of specified fully qualified classname.
%
-spec fully_qualified_classname_to_string( java_fully_qualified_classname() ) ->
													ustring().
fully_qualified_classname_to_string( { PackageName, Classname } ) ->
	text_utils:format( "class '~ts' of package '~ts'",
					   [ Classname, PackageName ] );

fully_qualified_classname_to_string( Classname ) ->
	text_utils:format( "class '~ts'", [ Classname ] ).
