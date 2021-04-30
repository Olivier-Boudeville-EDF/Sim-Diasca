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
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: July 1, 2007.


% Gathering of various convenient facilities.
%
% See basic_utils_test.erl for the corresponding test.
%
-module(basic_utils).


% Notification-related functions.
-export([ speak/1, notify_user/1, notify_user/2 ]).



% Message-related functions.
-export([ flush_pending_messages/0, flush_pending_messages/1,
		  notify_pending_messages/0, check_no_pending_message/0,
		  wait_for/2, wait_for/4, wait_for_acks/4, wait_for_acks/5,
		  wait_for_summable_acks/5,
		  wait_for_many_acks/4, wait_for_many_acks/5,
		  send_to_pid_set/2 ]).


% Miscellaneous functions.
-export([ size/1, get_process_info/1, get_process_info/2,
		  display_process_info/1,
		  checkpoint/1,
		  display/1, display/2, display_timed/2, display_timed/3,
		  display_error/1, display_error/2,
		  throw_diagnosed/1, throw_diagnosed/2,
		  debug/1, debug/2,
		  parse_version/1, check_version/1, compare_versions/2,
		  get_process_specific_value/0, get_process_specific_value/1,
		  get_process_specific_value/2,
		  get_process_size/1,
		  is_alive/1, is_alive/2, is_alive/3,
		  is_debug_mode_enabled/0, get_execution_target/0,
		  describe_term/1,
		  create_uniform_tuple/2,
		  stop/0, stop/1, stop_on_success/0, stop_on_failure/0,
		  stop_on_failure/1,
		  identity/1,
		  check_undefined/1, check_all_undefined/1,
		  check_defined/1, check_not_undefined/1, check_all_defined/1,
		  ignore_unused/1,
		  freeze/0, crash/0, enter_infinite_loop/0,
		  trigger_oom/0 ]).


% Hints about retrieving the name of the function being currently evaluated by a
% process (as a ?FUNCTION macro could do):
%
% - either:
%
% current_function() ->
%    catch throw( x ), [_, {_, F, _, _} | _] = erlang:get_stacktrace(),
%    F.
%
% - or, maybe better:
%
% erlang:element( 2, erlang:element( 2, erlang:process_info( self(),
%   current_function ) ) ) ).



% To tell that a returned value is not of interest to the caller:
% (could have been: "-type void() :: 'VoiD' or 'myriad_void'" for example)
%
% Nevertheless, should, for any reason, a value of the void/0 type have to be
% specified, the 'void' atom shall be preferred, knowing that any value can be
% returned and complies with this type.
%
% Opaque types currently not always well managed by the Erlang standard
% toolchain:
%
%-opaque void() :: any() | 'void'.
%
-type void() :: any() | 'void'.


% Allows to count elements (positive integer, possibly zero):
-type count() :: non_neg_integer().


% Allows to count elements (strictly positive integer):
-type non_null_count() :: pos_integer().


% Allows to count levels (ex: indentation ones, nesting ones):
-type level() :: non_neg_integer().


% Describes a mask of bits:
-type bit_mask() :: integer().


% Describes an (Erlang, inter-process) messsage:
-type message() :: any().


% Describes a PID or a port:
-type pid_or_port() :: pid() | port().


% For tables:
-type atom_key() :: atom().



% Error-related types.


% Term designating a reason (which may be any term):
%
% Note: useful to have self-describing types.
%
-type reason() :: any().


-type exit_reason() :: reason().

-type error_reason() :: reason().


% Designates an error type (a specific, simple error reason), when we know it is
% an atom (often the first element of an error tuple), like 'invalid_name'.
%
-type error_type() :: atom().


% An error pseudo-tuple, i.e. an error tuple (ex: {invalid_name,1.0}) or a
% single error term (instead of a tuple with a single element), preferably an
% atom (like 'invalid_password').
%
% See also: throw_diagnosed/{1,2}.
%
-type error_tuploid() :: error_tuploid( error_reason() ).

% To specify at least some information about the error type:
-type error_tuploid( T ) :: type_utils:tuploid( T ).


% A textual description associated to an error (typically for richer traces):
-type error_message() :: ustring().

% An error with its diagnosis:
-type diagnosed_error_reason() :: { error_tuploid(), error_message() }.

% An error with its diagnosis:
-type diagnosed_error_reason( T ) :: { error_tuploid( T ), error_message() }.


% A (tagged) error term:
-type error_term() :: { 'error', error_reason() }.


% A (tagged) error term with a diagnosis:
-type diagnosed_error_term() :: { 'error', diagnosed_error_reason() }.

% A (tagged) error term with a diagnosis:
-type diagnosed_error_term( T ) :: { 'error', diagnosed_error_reason( T ) }.


% Tells whether an operation succeeded; if not, an error reason is specified (as
% a term).
%
-type base_status() :: 'ok' | error_term().


% Quite often, variables (ex: record fields) are set to 'undefined'
% (i.e. "Nothing") before being set later:
%
-type maybe( T ) :: T | 'undefined'.


% To account for wildcard entries:
-type wildcardable( T ) :: T | 'any'.


% Return type for operations that may fail (with a sufficient likelihood that no
% exception is to be raised then, thus the choice is left to the caller):
%
-type fallible( T ) :: { 'ok', T } | error_term().


% Return type for operations that may fail (with a sufficient likelihood that no
% exception is to be raised then, thus the choice is left to the caller), when
% wanting to specify the error type as well:
%
-type fallible( TSuccess, TFailure ) ::
		{ 'ok', TSuccess } | { 'error', TFailure }.


% Thus either {ok,T} or {error,{ErrorTuploid,ErrorMsg}}:
-type diagnosed_fallible( T ) :: fallible( T, diagnosed_error_term() ).


% Thus either {ok,T} or {error,{ErrorTuploid,ErrorMsg}}:
-type diagnosed_fallible( TSuccess, TFailure ) ::
		fallible( TSuccess, diagnosed_error_term( TFailure ) ).



% To denote that a piece of data comes from the program boundaries (interfaces
% with the outside word, possibly in link with the user) and thus may or may not
% be of the expected type (as long as it has not been checked):
%
% (opaque, unspecified type - yet not declared as 'opaque' to avoid a
% compilation warning telling it is "underspecified and therefore meaningless").
%
-type external_data() :: term().


% Designates data whose type and value have not been checked yet.
-type unchecked_data() :: term().


% Designates user-specified data (users shall not be trusted either):
-type user_data() :: external_data().


% Designates an accumulator (of any type), to document typically fold-like
% operations:
%
% (useful for documentation purposes)
%
-type accumulator() :: any().

-type version_number() :: non_neg_integer().

% By default we consider that a version is a triplet of integer numbers:
-type version() :: { version_number(), version_number(), version_number() }.


-type two_digit_version() :: { version_number(), version_number() }.

-type any_version() :: version() | two_digit_version().


% For all non-null index (i.e. the ones that start at 1).
-type positive_index() :: pos_integer().


% To distinguish with the built-in type, which can be a parameterised module:
-type module_name() :: atom().

-type function_name() :: atom().

-type argument() :: any().

-type arguments() :: [ argument() ].



% Shorthand for Module, Function, Arity:
%
% (commented-out, as mfa() is a builtin type; it cannot be redefined)
%
%-type mfa() :: { module_name(), function_name(), arity() }.


% A command (module-function-arguments):
-type command_spec() :: { module_name(), function_name(), arguments() }.


% The name of a layer (ex: "Myriad"):
-type layer_name() :: ustring().


% The name of a record:
-type record_name() :: atom().

% The name of a field of a record:
-type field_name() :: atom().


% To store (UNIX-like) user names:
-type user_name() :: nonempty_string().
-type atom_user_name() :: atom().


% Possible outcome of a partial-order comparison of two elements:
-type comparison_result() :: 'lower' | 'equal' | 'higher'.



% Compile-time execution target (not to be mixed up with execution_context/0):
-type execution_target() :: 'development' | 'production'.

% Runtime-time execution context (not to be mixed up with execution_target/0,
% although gathering the same values - but conveying a different meaning):
%
-type execution_context() :: 'development' | 'production'.



% The exception classes that can be raised:
-type exception_class() :: 'throw' | 'exit' | 'error'.

% The status code returned by a shell command:
-type status_code() :: 0..255. % i.e. byte()


% Useful as a temporary type placeholder, during development (easy to grep and
% eliminate afterwards):
%
-type fixme() :: any().


-export_type([ void/0, count/0, non_null_count/0, level/0,
			   bit_mask/0, message/0, pid_or_port/0, atom_key/0,
			   reason/0, exit_reason/0,
			   error_reason/0, error_type/0, error_tuploid/0, error_message/0,
			   diagnosed_error_reason/0, error_term/0, diagnosed_error_term/0,
			   base_status/0, maybe/1, wildcardable/1,
			   fallible/1, fallible/2,
			   diagnosed_fallible/1, diagnosed_fallible/2,
			   external_data/0, unchecked_data/0, user_data/0,
			   accumulator/0,
			   version_number/0, version/0, two_digit_version/0, any_version/0,
			   positive_index/0,
			   module_name/0, function_name/0, argument/0, arguments/0,
			   command_spec/0, layer_name/0, record_name/0, field_name/0,
			   user_name/0, atom_user_name/0,
			   comparison_result/0, execution_target/0, execution_context/0,
			   exception_class/0, status_code/0,
			   fixme/0 ]).


% To define get_execution_target/0:
-include("basic_utils.hrl").


% Shorthands:

-type time_out() :: time_utils:time_out().

-type milliseconds() :: unit_utils:milliseconds().

-type set( T ) :: set_utils:set( T ).

-type format_string() :: text_utils:format_string().
-type format_values() :: text_utils: format_values().
-type ustring() :: text_utils:ustring().

-type atom_node_name() :: net_utils:atom_node_name().


% Even if not exported:
-type process_info_result_item() :: erlang:process_info_result_item().



% Creates a tuple of specified size, all elements having the same, specified,
% value.
%
-spec create_uniform_tuple( Size :: count(), Value :: any() ) -> tuple().
create_uniform_tuple( Size, Value ) ->

	List = lists:duplicate( Size, Value ),

	list_to_tuple( List ).



% Stops smoothly the underlying VM, with a normal, success status code (0).
%
% Also also to potentially override Erlang standard teardown procedure.
%
-spec stop() -> no_return().
stop() ->
	stop( _Success=0 ).



% Stops smoothly, synchronously the underlying VM, with specified error code.
%
% Also allows to potentially override Erlang standard teardown procedure.
%
-spec stop( status_code() ) -> no_return().
stop( StatusCode ) ->

	%trace_utils:debug( "Immediate stop." ),

	% Far less brutal than erlang:halt/{0,1}, yet awfully slow, and
	% actually non-blocking:
	%
	%init:stop( StatusCode ),

	% So, finally preferred (as blocking and fast):
	halt( StatusCode ),

	%trace_utils:debug( "Stopped." ),

	% To avoid that the calling process continues with the next instructions:
	% (would happen with init:stop/1, but not halt/{0,1})
	%
	freeze().



% Stops smoothly, synchronously the underlying VM, with a normal, success status
% code (0).
%
-spec stop_on_success() -> no_return().
stop_on_success() ->
	stop( _Success=0 ).



% Stops smoothly the underlying VM, with a default error status code (1).
-spec stop_on_failure() -> no_return().
stop_on_failure() ->
	stop_on_failure( _OurDefaultErrorCode=5 ).


% Stops smoothly the underlying VM, with a default error status code (1).
-spec stop_on_failure( status_code() ) -> no_return().
stop_on_failure( StatusCode ) ->
	stop( StatusCode ).



% Identity function: returns its argument as it is.
%
% Useful to avoid having the compiler being too smart by notifying annoying,
% spurious messages (ex: no clause will ever match) in some tests.
%
-spec identity( term() ) -> term().
identity( Term ) ->
	Term.



% Checks that specified term is 'undefined'.
-spec check_undefined( term() ) -> void().
check_undefined( undefined ) ->
	ok;

check_undefined( Term ) ->
	throw( { not_undefined, Term } ).



% Checks that all elements of the specified list are equal to 'undefined';
% returns that list.
%
-spec check_all_undefined( term() ) -> void().
check_all_undefined( List ) ->
	[ check_undefined( Term ) || Term <- List ].


% Checks that specified term is not 'undefined'; returns that term.
-spec check_not_undefined( term() ) -> term().
check_not_undefined( undefined ) ->
	throw( is_undefined );

check_not_undefined( Term ) ->
	Term.



% Checks that specified term is "defined" (not 'undefined'); returns that term.
-spec check_defined( term() ) -> term().
check_defined( Term ) ->
	check_not_undefined( Term ).


% Checks that all elements of the specified list are "defined" (not
% 'undefined'); returns that list.
%
-spec check_all_defined( [ term() ] ) -> [ term() ].
check_all_defined( List ) ->
	[ check_defined( Term ) || Term <- List ].



% Ignores specified argument.
%
% Useful to define, for debugging purposes, terms that will be (temporarily)
% unused without blocking the compilation.
%
% Ex: basic_utils:ignore_unused(A) or basic_utils:ignore_unused([A, B, C]).
%
-spec ignore_unused( any() ) -> void().
ignore_unused( _Term ) ->
	% Preferred silent:
	ok.
	%trace_utils:warning_fmt( "unused term (~p) ignored "
	%			 "(thanks to basic_utils:ignore_unused/1).", [ _Term ] ).



% Freezes the current process immediately.
%
% Useful to block the process while for example an ongoing termination
% occurs.
%
% See also: enter_infinite_loop/0.
%
-spec freeze() -> no_return().
freeze() ->

	%trace_utils:debug( "Freezing..." ),

	receive

		not_expected_to_be_received ->
			freeze()

	end.



% Crashes the current process immediately.
%
% Useful for testing reliability, for example.
%
-spec crash() -> any().
crash() ->

	trace_bridge:warning_fmt( "*** Crashing on purpose process ~w ***",
							  [ self() ] ),

	% Must outsmart the compiler; there should be simpler solutions:
	A = system_utils:get_core_count(),
	B = system_utils:get_core_count(),

	% Dividing thus by zero:
	1 / ( A - B ).



% Makes the current process enter in an infinite, mostly idle loop.
%
% Useful for testing reliability, for example.
%
% See also: freeze/0.
%
enter_infinite_loop() ->

	io:format( "~p in infinite loop...", [ self() ] ),

	% Loops every minute:
	timer:sleep( 60000 ),

	enter_infinite_loop().



% Triggers a OOM crash, i.e. Out of Memory.
%
% Useful for testing reliability, for example.
%
trigger_oom() ->

	io:format( "~p triggering OOM (out of memory) crash...", [ self() ] ),

	% Expected: Crash dump was written to: erl_crash.dump
	%  binary_alloc: Cannot allocate 1000000000031 bytes of memory (of type
	% "binary").

	<<1:8000000000000>>.






% Notification-related functions.


% Speaks the specified message, using espeak.
-spec speak( ustring() ) -> void().
speak( Message ) ->
	system_utils:run_background_command(
		"espeak -s 140 \"" ++ Message ++ "\"" ).



% Notifies the user of the specified message, with log output and synthetic
% voice.
%
-spec notify_user( ustring() ) -> void().
notify_user( Message ) ->
	io:format( Message ),
	speak( Message ).



% Notifies the user of the specified message, with log output and synthetic
% voice.
%
% Example: 'basic_utils:notify_user("Hello ~w", [ Name ]).'
%
-spec notify_user( format_string(), format_values() ) -> void().
notify_user( Message, FormatList ) ->

	ActualMessage = io_lib:format( Message, FormatList ),

	io:format( ActualMessage ),
	speak( ActualMessage ).




% Message-related section.


% Flushes all the messages still in the mailbox of this process.
-spec flush_pending_messages() -> void().
flush_pending_messages() ->

	receive

		_ ->
			flush_pending_messages()

	after 0 ->
		ok

	end.



% Flushes all the messages still in the mailbox of this process that (exactly)
% match the specified one.
%
-spec flush_pending_messages( any() ) -> void().
flush_pending_messages( Message ) ->

	receive

		Message ->
			flush_pending_messages( Message )

	after 0 ->
		ok

	end.



% Reads all pending messages in the mailbox of this process and notifies about
% them on the console.
%
% Does not block.
%
% Useful for tests.
%
-spec notify_pending_messages() -> void().
notify_pending_messages() ->

	receive

		Message ->
			trace_utils:warning_fmt( "Following message was pending: ~p.",
									 [ Message ] ),
			notify_pending_messages()

	after 0 ->
		ok

	end.



% Ensures that no message is pending in the mailbox of this process
%
% Does not block.
%
% Useful for tests.
%
-spec check_no_pending_message() -> void().
check_no_pending_message() ->

	receive

		Message ->
			trace_utils:error_fmt( "Following message was pending in the "
				"mailbox of ~w:~n  ~p", [ self(), Message ] ),
			throw( { pending_message_in_mailbox, Message, self() } )

	after 0 ->
		ok

	end.



% Waits (indefinitively) for the specified count of the specified message to be
% received.
%
-spec wait_for( term(), count() ) -> void().
wait_for( _Message, _Count=0 ) ->
	ok;

wait_for( Message, Count ) ->

	%trace_utils:debug_fmt( "Waiting for ~B messages '~p'.",
	%                      [ Count, Message ] ),
	receive

		Message ->
			wait_for( Message, Count-1 )

	end.



% Waits (indefinitively) for the specified count of the specified message to be
% received, displaying repeatedly on the console a notification should the
% duration between two receivings exceed the specified time-out.
%
% Typical usage: basic_utils:wait_for( {foobar_result, done}, _Count=5,
% _Duration=2000, "Still waiting for ~B task(s) to complete").
%
-spec wait_for( term(), count(), milliseconds(), format_string() ) -> void().
wait_for( _Message, _Count=0, _TimeOutDuration, _TimeOutFormatString ) ->
	ok;

wait_for( Message, Count, TimeOutDuration, TimeOutFormatString ) ->

	%trace_utils:debug_fmt( "Waiting for ~B messages '~p'.",
	%                       [ Count, Message ] ),

	receive

		Message ->
			%io:format( "Received message '~p'.~n", [ Message ] ),
			wait_for( Message, Count-1 )

	after TimeOutDuration ->

		io:format( TimeOutFormatString ++ " after ~ts",
				   [ Count, time_utils:duration_to_string( TimeOutDuration ) ] )

	end.





% Wait patterns, safer and better defined once for all.




% Waits until receiving from all expected senders the specified acknowledgement
% message, expected to be in the form of {AckReceiveAtom, WaitedSenderPid}.
%
% Throws a {ThrowAtom, StillWaitedSenders} exception on time-out (if any, as the
% time-out can be disabled if set to 'infinity').
%
% See wait_for_many_acks/{4,5} if having a large number of senders that are
% waited for.
%
-spec wait_for_acks( [ pid() ], time_out(), atom(), atom() ) -> void().
wait_for_acks( WaitedSenders, MaxDurationInSeconds, AckReceiveAtom,
			   ThrowAtom ) ->
	wait_for_acks( WaitedSenders, MaxDurationInSeconds, _DefaultPeriodMs=1000,
				   AckReceiveAtom, ThrowAtom ).



% Waits until receiving from all expected senders the specified acknowledgement
% message, expected to be in the form of {AckReceiveAtom, WaitedSenderPid},
% ensuring a check is performed at least at specified period.
%
% Throws a {ThrowAtom, StillWaitedSenders} exception on time-out.
%
% See wait_for_many_acks/{4,5} if having a large number of senders waited for.
%
-spec wait_for_acks( [ pid() ], time_out(), milliseconds(), atom(), atom() ) ->
			void().
wait_for_acks( WaitedSenders, MaxDurationInSeconds, Period,
			   AckReceiveAtom, ThrowAtom ) ->

	%trace_bridge:debug_fmt( "Waiting for ~p (period: ~ts, max duration: ~ts, "
	%	"ack atom: '~ts', throw atom: '~ts').",
	%	[ WaitedSenders, time_utils:duration_to_string( Period ),
	%	  time_utils:duration_to_string( MaxDurationInSeconds ),
	%	  AckReceiveAtom, ThrowAtom ] ),

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_acks_helper( WaitedSenders, InitialTimestamp,
		MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom ).



% (helper)
wait_for_acks_helper( _WaitedSenders=[], _InitialTimestamp,
			_MaxDurationInSeconds, _Period, _AckReceiveAtom, _ThrowAtom ) ->
	ok;

wait_for_acks_helper( WaitedSenders, InitialTimestamp, MaxDurationInSeconds,
					  Period, AckReceiveAtom, ThrowAtom ) ->

	receive

		{ AckReceiveAtom, WaitedPid } ->

			NewWaited = list_utils:delete_existing( WaitedPid, WaitedSenders ),

			%trace_bridge:debug_fmt( "(received ~p, still waiting for "
			%						"instances ~p)", [ WaitedPid, NewWaited ] ),

			wait_for_acks_helper( NewWaited, InitialTimestamp,
				  MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom )

	after Period ->

			NewDuration = time_utils:get_duration_since( InitialTimestamp ),

			case ( MaxDurationInSeconds =/= infinity ) andalso
					  ( NewDuration > MaxDurationInSeconds ) of

				true ->
					throw( { ThrowAtom, WaitedSenders } );

				false ->
					% Still waiting then:
					%trace_bridge:debug_fmt( "(still waiting for instances ~p)",
					%						[ WaitedSenders ] ),

					wait_for_acks_helper( WaitedSenders, InitialTimestamp,
						MaxDurationInSeconds, Period, AckReceiveAtom,
						ThrowAtom )

			end

	end.



% Waits until receiving from all expected senders the specified acknowledgement
% message, expected to be in the form of:
% {AckReceiveAtom, ToAdd, WaitedSenderPid}.
%
% Returns the sum of the specified initial value with all the ToAdd received
% values.
%
% Throws a {ThrowAtom, StillWaitedSenders} exception on time-out (if any, as the
% time-out can be disabled if set to 'infinity').
%
-spec wait_for_summable_acks( [ pid() ], number(), time_out(), atom(),
							  atom() ) -> number().
wait_for_summable_acks( WaitedSenders, InitialValue, MaxDurationInSeconds,
						AckReceiveAtom, ThrowAtom ) ->

	wait_for_summable_acks( WaitedSenders, InitialValue, MaxDurationInSeconds,
							_DefaultPeriod=1000, AckReceiveAtom, ThrowAtom ).



% Waits until receiving from all expected senders the specified acknowledgement
% message, expected to be in the form of:
% {AckReceiveAtom, ToAdd, WaitedSenderPid}.
%
% ensuring a check is performed at least at specified period and summing all
% ToAdd values with the specified initial one
%
% Throws a {ThrowAtom, StillWaitedSenders} exception on time-out.
%
-spec wait_for_summable_acks( [ pid() ], number(), time_out(),
							  milliseconds(), atom(), atom() ) -> number().
wait_for_summable_acks( WaitedSenders, CurrentValue, MaxDurationInSeconds,
						Period, AckReceiveAtom, ThrowAtom ) ->

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_summable_acks_helper( WaitedSenders, CurrentValue,
		InitialTimestamp, MaxDurationInSeconds, Period, AckReceiveAtom,
		ThrowAtom ).



% (helper)
wait_for_summable_acks_helper( _WaitedSenders=[], CurrentValue,
		_InitialTimestamp, _MaxDurationInSeconds,  _Period, _AckReceiveAtom,
		_ThrowAtom ) ->
	CurrentValue;

wait_for_summable_acks_helper( WaitedSenders, CurrentValue, InitialTimestamp,
			MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom ) ->

	receive

		{ AckReceiveAtom, ToAdd, WaitedPid } ->

			NewWaited = list_utils:delete_existing( WaitedPid, WaitedSenders ),

			%io:format( "(received ~p, still waiting for instances ~p)~n",
			%		   [ WaitedPid, NewWaited ] ),

			wait_for_summable_acks_helper( NewWaited, CurrentValue + ToAdd,
				  InitialTimestamp, MaxDurationInSeconds, Period,
				  AckReceiveAtom, ThrowAtom )

	after Period ->

			NewDuration = time_utils:get_duration_since( InitialTimestamp ),

			case ( MaxDurationInSeconds =/= infinity ) andalso
					  ( NewDuration > MaxDurationInSeconds ) of

				true ->
					throw( { ThrowAtom, WaitedSenders } );

				false ->
					% Still waiting then:

					%io:format( "(still waiting for instances ~p)~n",
					%   [ WaitedSenders ] ),

					wait_for_summable_acks_helper( WaitedSenders, CurrentValue,
						InitialTimestamp, MaxDurationInSeconds, Period,
						AckReceiveAtom, ThrowAtom )

			end

	end.





% Waits until receiving from all expected (numerous) senders the specified
% acknowledgement message.
%
% Throws specified exception on time-out.
%
% Note: each sender shall be unique (as they will be gathered in a set, that
% does not keep duplicates)
%
-spec wait_for_many_acks( set( pid() ), milliseconds(), atom(), atom() ) ->
											void().
wait_for_many_acks( WaitedSenders, MaxDurationInSeconds, AckReceiveAtom,
					ThrowAtom ) ->
	wait_for_many_acks( WaitedSenders, MaxDurationInSeconds,
						_DefaultPeriod=1000, AckReceiveAtom, ThrowAtom ).



% Waits until receiving from all expected (numerous) senders the specified
% acknowledgement message.
%
% Throws specified exception on time-out, checking at the specified period.
%
-spec wait_for_many_acks( set( pid() ), milliseconds(), milliseconds(), atom(),
						  atom() ) -> void().
wait_for_many_acks( WaitedSenders, MaxDurationInSeconds, Period,
					AckReceiveAtom, ThrowAtom ) ->

	InitialTimestamp = time_utils:get_timestamp(),

	wait_for_many_acks_helper( WaitedSenders, InitialTimestamp,
		MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom ).



% For this version we prefer a look-up optimised list to a plain one.
%
% (helper)
%
wait_for_many_acks_helper( WaitedSenders, InitialTimestamp,
		MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom ) ->

	case set_utils:is_empty( WaitedSenders ) of

		true ->
			ok;

		false ->

			receive

				{ AckReceiveAtom, WaitedPid } ->

					NewWaited = set_utils:delete_existing( WaitedPid,
														   WaitedSenders ),

					wait_for_many_acks_helper( NewWaited, InitialTimestamp,
					   MaxDurationInSeconds, Period, AckReceiveAtom, ThrowAtom )

			after Period ->

					NewDuration = time_utils:get_duration_since(
									InitialTimestamp ),

					case NewDuration > MaxDurationInSeconds of

						true ->
							throw( { ThrowAtom, WaitedSenders } );

						false ->
							% Still waiting then:
							wait_for_many_acks_helper( WaitedSenders,
								InitialTimestamp, MaxDurationInSeconds, Period,
								AckReceiveAtom, ThrowAtom )

					end

			end

	end.



% Sends the specified message to all elements (supposed to be PID) of the
% specified set, and returns the number of sent messages.
%
% (helper)
%
-spec send_to_pid_set( term(), set( pid() ) ) -> count().
send_to_pid_set( Message, PidSet ) ->

	% Conceptually (not a basic list, though):
	% [ Pid ! Message || Pid <- PidSet ]

	% With iterators, it is done slightly slower yet with less RAM rather than
	% first using set_utils:to_list/1 then iterating on the resulting plain
	% list:
	%
	Iterator = set_utils:iterator( PidSet ),

	% Returns the count:
	send_to_pid_set( Message, set_utils:next( Iterator ), _Count=0 ).



% (helper)
send_to_pid_set( _Message, none, Count ) ->
	Count;

send_to_pid_set( Message, { Pid, NewIterator }, Count ) ->
	Pid ! Message,
	send_to_pid_set( Message, set_utils:next( NewIterator ), Count+1 ).




% Miscellaneous functions.


% Returns the number of bytes used by specified term.
-spec size( term() ) -> system_utils:byte_size().
size( Term ) ->
	system_utils:get_size( Term ).




% Returns all general information regarding specified process (which is local or
% not), provided it is still alive (otherwise returns undefined).
%
-spec get_process_info( pid() ) -> maybe( [ process_info_result_item() ] ).
get_process_info( Pid ) ->

	LocalNode = node(),

	% erlang:process_info/1 throws badarg if the process is not local:
	case node( Pid ) of

		LocalNode ->
			erlang:process_info( Pid );

		OtherNode ->

			% The current module may not be on this node:
			case rpc:call( OtherNode, _M=erlang, _F=process_info, _A=[ Pid ] )
			   of

				{ badrpc, Reason } ->
					trace_utils:error_fmt( "No information found for "
						"process ~w running on remote node ~p; reason: ~p.~n",
						[ Pid, OtherNode, Reason ] ),
					throw( { process_info_failed, Pid, Reason } );

				% Either 'undefined' or proplist:
				Res ->
					Res

			end

	end.



% Returns the specified information regarding specified process (which is local
% or not), provided it is still alive (otherwise returns undefined).
%
-spec get_process_info( pid(), process_info_result_item() ) ->
								maybe( process_info_result_item() );
					  ( pid(), [ process_info_result_item() ] ) ->
								maybe( [ process_info_result_item() ] ).
get_process_info( Pid, ItemTerm ) ->

	LocalNode = node(),

	% erlang:process_info/1 throws badarg if the process is not local:
	case node( Pid ) of

		LocalNode ->
			erlang:process_info( Pid, ItemTerm );

		OtherNode ->

			% The current module may not be on this node:
			case rpc:call( OtherNode, _M=erlang, _F=process_info,
						   _A=[ Pid, ItemTerm ] ) of

				{ badrpc, Reason } ->
					trace_utils:error_fmt( "No information found for "
						"process ~w running on remote node ~p; reason: ~p.~n",
						[ Pid, OtherNode, Reason ] ),
					throw( { process_info_failed, Pid, Reason } );

				% Either 'undefined' or proplist:
				Res ->
					Res

			end

	end.



% Displays information about the process(es) identified by specified PID(s).
-spec display_process_info( pid() | [ pid() ] ) -> void().
display_process_info( PidList ) when is_list( PidList ) ->
	[ display_process_info( Pid ) || Pid <- PidList ];

display_process_info( Pid ) when is_pid( Pid ) ->

	LocalNode = node(),

	% erlang:process_info/1 throws badarg if the process is not local:
	case node( Pid ) of


		LocalNode ->
			case erlang:process_info( Pid ) of

				undefined ->
					io:format( "PID ~w refers to a (local) dead process~n",
							   [ Pid ] );

				PropList ->
					Strings = [ io_lib:format( "~ts: ~p", [ K, V ] )
								|| { K, V } <- PropList ],
					io:format( "PID ~w refers to a local live process, "
						"whose information is: ~ts",
						[ Pid, text_utils:strings_to_string( Strings ) ] )

			end;


		OtherNode ->

			% The current module may not be on this node:
			case rpc:call( OtherNode, _M=erlang, _F=process_info, _A=[ Pid ] )
			   of

				{ badrpc, Reason } ->
					io:format( "No information found for process ~w "
						"running on remote node ~p; reason: ~p.~n",
						[ Pid, OtherNode, Reason ] );

				undefined ->
					io:format( "PID ~w refers to a dead process on "
							   "remote node ~ts.~n", [ Pid, OtherNode ] );

				PropList ->

					Strings = [ io_lib:format( "~ts: ~p", [ K, V ] )
								|| { K, V } <- PropList ],

					io:format( "PID ~w refers to a live process on "
						"remote node ~ts, whose information are: ~ts",
						[ Pid, OtherNode,
						  text_utils:strings_to_string( Strings ) ] )

			end

	end.



% Displays a numbered checkpoint.
%
% Useful for debugging purposes.
%
-spec checkpoint( integer() ) -> void().
checkpoint( Number ) ->
	display( "----- CHECKPOINT #~B -----", [ Number ] ).



% Displays specified string on the standard output of the console, ensuring as
% much as possible this message is output synchronously, so that it can be
% output on the console even if the virtual machine is to crash just after.
%
-spec display( ustring() ) -> void().
display( Message ) ->

	% Finally io:format has been preferred to erlang:display, as the latter one
	% displays quotes around the strings.
	%
	% ~ts, not ~ts, as we want to properly output Unicode characters:
	%
	io:format( "~ts~n", [ Message ] ),

	% Possibly to allow for a yield (100 being far too high):
	timer:sleep( 10 ),

	system_utils:await_output_completion().


	% May not go through group leader (like io:format), thus less likely to
	% crash without displaying the message:
	%
	%erlang:display( lists:flatten( [ Message, ".~n" ] ) ).
	%erlang:display( Message ).



% Displays specified format string filled according to specified values on the
% standard output of the console, ensuring as much as possible this message is
% output synchronously, so that it can be output on the console even if the
% virtual machine is to crash just after.
%
-spec display( format_string(), format_values() ) -> void().
display( Format, Values ) ->

	%io:format( "Displaying format '~p' and values '~p'.~n",
	%		   [ Format, Values ] ),

	Message = text_utils:format( Format, Values ),

	display( Message ).




% Displays specified string on the standard output of the console, ensuring as
% much as possible this message is output synchronously, so that it can be
% output on the console even if the virtual machine is to crash just after.
%
-spec display_timed( ustring(), time_out() ) -> void().
display_timed( Message, TimeOut ) ->

	% Finally io:format has been preferred to erlang:display, as the latter one
	% displays quotes around the strings.

	io:format( "~ts~n", [ Message ] ),
	system_utils:await_output_completion( TimeOut ).

	% May not go through group leader (like io:format), thus less likely to
	% crash without displaying the message:
	%
	%erlang:display( lists:flatten( [ Message, ".~n" ] ) ).
	%erlang:display( Message ).



% Displays specified format string filled according to specified values on the
% standard output of the console, ensuring as much as possible this message is
% output synchronously, so that it can be output on the console even if the
% virtual machine is to crash just after.
%
-spec display_timed( format_string(), format_values(), time_out() ) -> void().
display_timed( Format, Values, TimeOut ) ->

	%trace_utils:debug_fmt( "Displaying format '~p' and values '~p'.",
	%						[ Format, Values ] ),

	Message = text_utils:format( Format, Values ),

	display_timed( Message, TimeOut ).





% Displays specified string on the standard error output of the console,
% ensuring as much as possible this message is output synchronously, so that it
% can be output on the console even if the virtual machine is to crash just
% after.
%
-spec display_error( ustring() ) -> void().
display_error( Message ) ->

	% At least once, following call resulted in no output at all (standard_error
	% not functional):
	%
	% Reintroduced for testing after 21.0:
	%
	io:format( standard_error, "~ts~n", [ Message ] ),

	% So:
	%io:format( "~ts~n", [ Message ] ),

	system_utils:await_output_completion().



% Triggers specified diagnosed error: reports first its embedded diagnosis, then
% throws this error as an exception.
%
% Typical use:
%
% case Expr of
%
%  { ok, X } ->
%    [...];
%
%  { ok, Y } ->
%    [...];
%
%  { error, DiagnosedReason } ->
%     basic_utils:throw_diagnosed( DiagnosedReason )
%
% end
%
-spec throw_diagnosed( diagnosed_error_reason() ) -> no_return().
throw_diagnosed( _DiagnosedReason={ ErrorTuploid, ErrorMsg } ) ->
	trace_bridge:error( ErrorMsg ),
	throw( ErrorTuploid ).



% Triggers specified diagnosed error, augmented by specified term: reports first
% its embedded diagnosis, then throws this error, as an augmented tuploid, as an
% exception.
%
% Typical use:
%
% case Expr of
%
%  { ok, X } ->
%    [...];
%
%  { ok, Y } ->
%    [...];
%
%  { error, DiagnosedReason } ->
%     basic_utils:throw_diagnosed( DiagnosedReason, Z )
%
% end
%
-spec throw_diagnosed( diagnosed_error_reason(), term() ) -> no_return().
throw_diagnosed( _DiagnosedReason={ ErrorTuploid, ErrorMsg },
				 ExtraErrorTerm ) ->
	trace_bridge:error( ErrorMsg ),
	throw( type_utils:augment_tuploid( ErrorTuploid, ExtraErrorTerm ) ).



% Displays specified format string filled according to specified values on the
% standard error output of the console, ensuring as much as possible this
% message is output synchronously, so that it can be output on the console even
% if the virtual machine is to crash just after.
%
-spec display_error( format_string(), format_values() ) -> void().
display_error( Format, Values ) ->
	Message = text_utils:format( Format ++ "~n", Values ),
	display_error( Message ).




% Displays, for debugging purposes, specified string, ensuring as much as
% possible this message is output synchronously, so that it can be output on the
% console even if the virtual machine is to crash just after.
%
-spec debug( ustring() ) -> void().
debug( Message ) ->
	trace_utils:debug( Message ).
	%system_utils:await_output_completion().
	%erlang:display( "## Debug: " ++ Message ).



% Displays, for debugging purposes, specified format string filled according to
% specified values, ensuring as much as possible this message is output
% synchronously, so that it can be output on the console even if the virtual
% machine is to crash just after.
%
-spec debug( format_string(), format_values() ) -> void().
debug( Format, Values ) ->
	debug( text_utils:format( Format, Values ) ).




% Parses specified textual version.
%
% Ex: "4.2.1" should become {4,2,1}, and "2.3" should become {2,3}.
%
-spec parse_version( ustring() ) -> any_version().
parse_version( VersionString ) ->

	% First transform "4.22.1" into ["4","22","1"]:
	Elems = string:tokens( VersionString, "." ),

	% Then simply switch to {4,22,1}:
	list_to_tuple( [ text_utils:string_to_integer( E ) || E <- Elems ] ).



% Checks that specified term is a triplet-based version.
-spec check_version( term() ) -> void().
check_version( { A, B, C } ) when is_integer( A ) andalso is_integer( B )
								  andalso is_integer( C )->
	ok;

check_version( T ) ->
	throw( { invalid_triplet_version, T } ).



% Compares the two pairs or triplets, which describe two version numbers (ex:
% {0,1,0} or {4,2}) and returns either first_bigger, second_bigger, or equal.
%
% The two compared versions must have the same number of digits.
%
% Note: the default term order is already what we needed.
%
-spec compare_versions( any_version(), any_version() ) ->
								'equal' | 'first_bigger' | 'second_bigger'.
compare_versions( {A1,A2,A3}, {B1,B2,B3} ) ->

	case {A1,A2,A3} > {B1,B2,B3} of

		true ->
			first_bigger;

		false ->

			case {A1,A2,A3} =:= {B1,B2,B3} of

				true ->
					equal;

				false ->
					second_bigger

			end

	end;

compare_versions( {A1,A2}, {B1,B2} ) ->

	case {A1,A2} > {B1,B2} of

		true ->
			first_bigger;

		false ->

			case {A1,A2} =:= {B1,B2} of

				true ->
					equal;

				false ->
					second_bigger

			end

	end.



% Returns a value (a strictly positive integer) expected to be as much as
% possible specific to the current process.
%
% Mostly based on its PID.
%
% Useful for example when a large number of similar processes try to access to
% the same resource (ex: a set of file descriptors) at the same time: they can
% rely on some random waiting based on that process-specific value in order to
% smooth the accesses over time.
%
% We could imagine taking into account as well the current time, the process
% reductions, etc. or generating a reference.
%
-spec get_process_specific_value() -> pos_integer().
get_process_specific_value() ->
	get_process_specific_value( self() ).



% Returns a value (a strictly positive integer) expected to be as much as
% possible specific to the specified PID.
%
% Useful for example when a large number of similar processes try to access to
% the same resource (ex: a set of file descriptors) at the same time: they can
% rely on some random waiting based on that process-specific value in order to
% smooth the accesses over time.
%
% We could imagine taking into account as well the current time, the process
% reductions, etc. or generating a reference.
%
-spec get_process_specific_value( pid() ) -> pos_integer().
get_process_specific_value( Pid ) ->

	% PID are akin to <X.Y.Z>.

	PidAsText = lists:flatten( io_lib:format( "~w", [ Pid ] ) ),

	%io:format( "PID: ~w.~n", [ self() ] ) ,
	% Ex: ["<0","33","0>"]:
	[ [ $< | First ], Second, Third ] = string:tokens( PidAsText, "." ),

	% We add 1 to x and z as they might be null:
	{ F, [] } = string:to_integer( First ),
	{ S, [] } = string:to_integer( Second ),

	[ $> | ExtractedThird ] = lists:reverse( Third ),

	{ T, [] } = string:to_integer( ExtractedThird ),

	X = F+1,
	Y = S,
	Z = T+1,

	% Hash part probably a bit overkill:
	Res = X*Y*Z + erlang:phash2( erlang:make_ref() ),

	%trace_utils:debug_fmt( "Process-specific value: ~B.", [ Res ] ),
	Res.



% Returns a process-specific value in [Min,Max[.
-spec get_process_specific_value( integer(), integer() ) -> integer().
get_process_specific_value( Min, Max ) ->

	Value = get_process_specific_value(),

	{ H, M, S } = erlang:time(),

	( ( ( H + M + S + 1 ) * Value ) rem ( Max - Min ) ) + Min.



% Returns the total size in (RAM) memory used by specified (local, alive)
% process, in bytes: this includes call stack, heap, and internal structures.
%
% See https://erlang.org/doc/man/erlang.html#process_info-1 for more
% information.
%
-spec get_process_size( pid() ) -> system_utils:byte_size().
get_process_size( Pid ) ->

	% 4 bytes is returned on a 32-bit architecture, and 8 is returned on a pure
	% 64-bit architecture:
	%
	%WordSize = erlang:system_info( { wordsize, internal } ),

	%ProcessPropList = erlang:process_info( Pid ),

	%trace_utils:debug_fmt( "Process info for ~w:~n~p",
	%					   [ Pid, ProcessPropList ] ),

	% Includes call stack, heap, and internal structures:
	% (apparentlyalready in bytes, not words:
	%
	%WordSize * list_table:get_value( memory, ProcessPropList ).
	%list_table:get_value( memory, ProcessPropList ).
	{ memory, Size } = get_process_info( Pid, memory ),
	Size.



% Tells whether the specified process, designated by its PID, by a textual
% representation of it (like "<9092.61.0>") or by a registered name (local
% otherwise global) like 'foobar_service' is still existing at the moment of
% this call.
%
% Note:
% - the process may run on the local node or not
% - generally not to be used, when relying on a good design
%
-spec is_alive( pid() | ustring() | naming_utils:registration_name() ) ->
			boolean().
is_alive( TargetPid ) when is_pid( TargetPid ) ->
	is_alive( TargetPid, node( TargetPid ) );

is_alive( TargetPidString ) when is_list( TargetPidString ) ->
	TargetPid = list_to_pid( TargetPidString ),
	is_alive( TargetPid, node( TargetPid ) );

is_alive( TargetPidName ) when is_atom( TargetPidName ) ->
	TargetPid = naming_utils:get_registered_pid_for( TargetPidName,
						_RegistrationType=local_otherwise_global ),
	is_alive( TargetPid, node( TargetPid ) ).



% Tells whether the specified process (designated by its PID) supposed to run on
% specified node (specified as an atom) was still existing at the moment of this
% call.
%
% Note: generally not to be used when relying on a good design; and is_alive/1
% should be preferred.
%
-spec is_alive( pid(), atom_node_name() ) -> boolean().
is_alive( TargetPid, Node )  ->
	is_alive( TargetPid, Node, _Verbose=true ).


% Tells whether the specified process (designated by its PID) supposed to run on
% specified node (specified as an atom) was still existing at the moment of this
% call.
%
% May emit trace warnings if told to be verbose.
%
% Note: generally not to be used when relying on a good design; and is_alive/1
% should be preferred.
%
-spec is_alive( pid(), atom_node_name(), boolean() ) -> boolean().
is_alive( TargetPid, Node, Verbose ) when is_pid( TargetPid ) ->
	% erlang:is_process_alive/1 is more intended for debugging purposes...

	case node() of

		Node ->
			% Would fail with 'badarg' if the process ran on another node:
			erlang:is_process_alive( TargetPid );

		_OtherNode ->
			%trace_utils:debug_fmt( "Testing liveliness of process ~p "
			%  "on node ~p.", [ TargetPid, Node ] ),
			case rpc:call( Node, _Mod=erlang, _Fun=is_process_alive,
					  _Args=[ TargetPid ] ) of

				Res when is_boolean( Res ) ->
					Res;

				{ badrpc, nodedown } ->
					case Verbose of

						true ->
							trace_utils:warning_fmt( "Reporting that process "
								"of PID ~w is not alive as its node ('~ts') "
								"is reported as down.", [ TargetPid, Node ] );

						false ->
							ok

					end,
					false;

				Other ->
					throw( { unexpected_liveliness_report, Other } )

			end

	end.



% Returns whether the (Myriad-enforced) debug mode is activated for the
% compilation of this module.

% Dispatched in actual clauses, otherwise Dialyzer will detect an
% underspecification:
%
%-spec is_debug_mode_enabled() -> boolean().

-ifdef(myriad_debug_mode).

-spec is_debug_mode_enabled() -> true.
is_debug_mode_enabled() ->
	true.

-else. % myriad_debug_mode

-spec is_debug_mode_enabled() -> false.
is_debug_mode_enabled() ->
	false.

-endif. % myriad_debug_mode


% Describes specified term in a controlled manner.
-spec describe_term( term() ) -> ustring().
describe_term( T ) ->
	text_utils:ellipse_fmt( "~p", [ T ] ).
