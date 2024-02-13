% Copyright (C) 2007-2024 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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
% Creation date: 2007.


% Modular WOOPER header gathering the main loop of instances
% (wooper_main_loop/1).


% Implies wooper_defines.hrl:
%-include("wooper_execute.hrl").


% For wooper_destruct/1:
%-include("wooper_destruction_functions.hrl").



% In theory, no closure nor export needed for wooper_main_loop/1, as the various
% spawns are done based on the wooper_construct_and_run* functions, which are
% not exported, and are executed thanks to a closure.
%
% Yet the export of the main loop is needed so that it can be hot-updated and
% branched to (thanks to a module-qualified code).
%
% Note that, in the definition of this closure, self() should not be used,
% otherwise this will correspond to the PID of the spawned process, and not to
% the one of the creating one (hence CreatorPid = self() which is defined
% outside of these closures).

% As code defined in this main loop will be duplicated in all class modules, it
% is preferable to offset most code in separate modules, so that the
% corresponding bytecode sits at most once in cache (knowing that some features
% like serialisation or hot-update may not be used in all cases, so their
% modules may not have to be loaded).



% @doc Waits for the incoming method calls, and serves them.
-spec wooper_main_loop( wooper:state() ) -> 'deleted'. % no_return().
wooper_main_loop( State ) ->

	% For all instances (Upgradable or not):
	%trace_utils:debug_fmt( "~w in main loop in ~p with state:~n ~p",
	%   [ self(), class_Upgradable:get_maybe_version( State ), State ] ),

	%?wooper_log( "wooper_main_loop start.~n" ),

	% Uncomment to display the current state prior to each method call:
	% wooper:display_state( State )

	receive

		% Requests (thus with response):

		% The target instance PID could be sent back as well, in order to
		% discriminate received answers on the caller side (if it was
		% interleaving requests).

		{ MethodAtom, ArgumentList, CallerPid }
				when is_pid( CallerPid ) andalso is_list( ArgumentList ) ->

			?wooper_log_format( "Main loop (case A) for ~w: "
				"request '~ts' with argument list ~w for ~w.~n",
				[ self(), MethodAtom, ArgumentList, CallerPid ] ),

			NewState = wooper_handle_remote_request_execution( MethodAtom,
				State, ArgumentList, CallerPid ),

			%?wooper_log( "Main loop (case A) ended.~n" ),

			wooper_main_loop( NewState );



		% Auto-wrapping single arguments implies putting lists between
		% double-brackets for this request:
		%
		{ MethodAtom, Argument, CallerPid } when is_pid( CallerPid ) ->

			?wooper_log_format( "Main loop (case B) for ~w: request '~ts' "
				"with argument ~w for ~w.~n",
				[ self(), MethodAtom, Argument, CallerPid ] ),

			NewState = wooper_handle_remote_request_execution( MethodAtom,
				State, [ Argument ], CallerPid ),

			%?wooper_log( "Main loop (case B) ended.~n" ),

			wooper_main_loop( NewState );



		% Oneway calls (no caller PID sent, no answer sent back).
		%
		% We check though that indeed no value is returned, by pattern-matching
		% against wooper_method_returns_void, which can be removed if not in
		% debug mode.
		%
		% (if no pattern-matching was done, then either this method would not
		% return anything, or the sender would not be interested in the result)
		%
		{ MethodAtom, ArgumentList } when is_list( ArgumentList ) ->

			?wooper_log_format( "Main loop (case C) for ~w: "
				"oneway '~ts' with argument list ~w.~n",
				[ self(), MethodAtom, ArgumentList ] ),

			NewState = wooper_handle_remote_oneway_execution( MethodAtom, State,
															  ArgumentList ),

			%?wooper_log( "Main loop (case C) ended.~n" ),

			wooper_main_loop( NewState );



		{ synchronous_delete, CallerPid } ->

			?wooper_log( "Main loop: oneway synchronous delete.~n" ),

			% Triggers the recursive call of destructors in the inheritance
			% graph (bottom-up):
			%
			_IntentionallyUnmatched=wooper_destruct( State ),

			CallerPid ! { deleted, self() },
			deleted;
			% (do nothing, loop ends here).



		% ping is always available and cannot be overridden:
		{ ping, CallerPid } ->

			?wooper_log_format( "Main loop (case D) for ~w: oneway ping.~n",
								[ self() ] ),

			CallerPid ! { pong, self() },

			%?wooper_log( "Main loop (case D) ended.~n" ),
			wooper_main_loop( State );



		% Oneway with parameters:
		{ MethodAtom, Argument } ->

			?wooper_log_format( "Main loop (case E) for ~w: "
				"oneway '~ts' with argument ~w.~n",
				[ self(), MethodAtom, Argument ] ),

			NewState = wooper_handle_remote_oneway_execution( MethodAtom, State,
															  [ Argument ] ),

			%?wooper_log( "Main loop (case E) ended.~n" ),

			wooper_main_loop( NewState );



		delete ->

			?wooper_log( "Main loop: oneway delete.~n" ),

			% Triggers the recursive call of destructors in the inheritance
			% graph (bottom-up):
			%
			_IntentionallyUnmatched=wooper_destruct( State ),

			deleted;
			% (do nothing, loop ends here).



		MethodAtom when is_atom( MethodAtom ) ->

			?wooper_log_format(
				"Main loop (case F) for ~w: oneway from atom ~ts.~n",
				[ self(), MethodAtom ] ),

			% Any result should be ignored, only the updated state is kept:
			NewState = wooper_handle_remote_oneway_execution( MethodAtom, State,
				_ArgumentList=[] ),

			%?wooper_log( "Main loop (case F) ended.~n" ),
			wooper_main_loop( NewState );


		% Section for the processing of Erlang standard that do not map directly
		% to WOOPER conventions.


		% Not necessarily a PID per se (e.g. can be #Port<0.2194>):
		{ 'EXIT', PidOrPort, ExitType } -> %when is_pid( Pid ) ->

			?wooper_log_format( "Main loop (case G) for ~w: exit with ~w.~n",
								[ self(), { PidOrPort, ExitType } ] ),

			case ?wooper_table_type:lookup_entry(
					{ _Name=onWOOPERExitReceived, _Arity=3 },
					%persistent_term:get(
					%    State#state_holder.virtual_table_key ) )
					State#state_holder.virtual_table ) of

				{ value, _Key } ->

					% Reusing safe execution facilities rather than directly
					% 'apply( LocatedModule, onWOOPERExitReceived, ...)':

					% Will thus call the 'onWOOPERExitReceived( State, Pid,
					% ExitType )' oneway, where ExitType is typically a stack
					% trace:

					{ NewState, _ } = wooper_execute_method(
						onWOOPERExitReceived, [ PidOrPort, ExitType ], State ),

					%?wooper_log( "Main loop (case G) ended.~n" ),
					wooper_main_loop( NewState );

				% Key not found:
				_ ->
					% EXIT handler not overridden, using default one:
					%?wooper_log( "Main loop (case G) ended.~n" ),
					NewState = wooper:default_exit_handler( PidOrPort, ExitType,
															State ),
					wooper_main_loop( NewState )

			end;


		{ 'DOWN', MonitorRef, MonitoredType, MonitoredElement, ExitReason } ->

			?wooper_log_format( "Main loop (case H) for ~w: down message "
				"with ~w.~n", [ self(),
				{ MonitorRef, MonitoredType, MonitoredElement, ExitReason } ] ),

			case ?wooper_table_type:lookup_entry(
					{ _Name=onWOOPERDownNotified, _Arity=5 },
					%persistent_term:get(
					%   State#state_holder.virtual_table_key ) )
					State#state_holder.virtual_table ) of

				{ value, _Key } ->

					% Reusing safe execution facilities rather than directly
					% 'apply( LocatedModule, onWOOPERDownNotified, ...)':

					% Will thus call 'onWOOPERDownNotified( State, MonitorRef,
					% MonitoredType, MonitoredElement, ExitReason )':

					{ NewState, _ } = wooper_execute_method(
						onWOOPERDownNotified,
						[ MonitorRef, MonitoredType, MonitoredElement,
						  ExitReason ], State ),

					%?wooper_log( "Main loop (case H) ended.~n" ),
					wooper_main_loop( NewState );


				% Key not found:
				_ ->

					% DOWN handler not overridden, using default one:
					%?wooper_log( "Main loop (case H) ended.~n" ),
					NewState = wooper:default_down_handler( MonitorRef,
						MonitoredType, MonitoredElement, ExitReason, State ),
					wooper_main_loop( NewState )

			end;


		{ nodeup, Node, MonitorNodeInfo } ->

			?wooper_log_format( "Main loop (case I) for ~w: node up message "
				"for '~ts' with ~p.~n", [ self(), Node, MonitorNodeInfo ] ),

			case ?wooper_table_type:lookup_entry(
					{ _Name=onWOOPERNodeConnection, _Arity=3 },
					%persistent_term:get(
					%   State#state_holder.virtual_table_key ) )
					State#state_holder.virtual_table ) of

				{ value, _Key } ->

					% Reusing safe execution facilities rather than directly
					% 'apply( LocatedModule, onWOOPERNodeConnection, ...)':

					% Will thus call 'onWOOPERNodeConnection( State, NodeName,
					% MonitorNodeInfo )':

					{ NewState, _ } = wooper_execute_method(
						onWOOPERNodeConnection, [ Node, MonitorNodeInfo ],
						State ),

					%?wooper_log( "Main loop (case I) ended.~n" ),
					wooper_main_loop( NewState );


				% Key not found:
				_ ->

					% nodeup handler not overridden, using default one:
					%?wooper_log( "Main loop (case I) ended.~n" ),
					NewState = wooper:default_node_up_handler( Node,
						MonitorNodeInfo, State ),
					wooper_main_loop( NewState )

			end;


		{ nodedown, Node, MonitorNodeInfo } ->

			?wooper_log_format( "Main loop (case J) for ~w: node down message "
				"for '~ts' with ~p.~n", [ self(), Node, MonitorNodeInfo ] ),

			case ?wooper_table_type:lookup_entry(
					{ _Name=onWOOPERNodeDisconnection, _Arity=3 },
					%persistent_term:get(
					%   State#state_holder.virtual_table_key ) )
					State#state_holder.virtual_table ) of

				{ value, _Key } ->

					% Reusing safe execution facilities rather than directly
					% 'apply( LocatedModule, onWOOPERNodeDisconnection, ...)':

					% Will thus call 'onWOOPERNodeDisconnection( State,
					% NodeName, MonitorNodeInfo )':

					{ NewState, _ } = wooper_execute_method(
						onWOOPERNodeDisconnection, [ Node, MonitorNodeInfo ],
						State ),

					%?wooper_log( "Main loop (case J) ended.~n" ),
					wooper_main_loop( NewState );


				% Key not found:
				_ ->

					% nodedown handler not overridden, using default one:
					% ?wooper_log( "Main loop (case J) ended.~n" ),
					NewState = wooper:default_node_down_handler( Node,
													MonitorNodeInfo, State ),
					wooper_main_loop( NewState )

			end;



		% We intentionally deviate here from the standard form for requests, as
		% a version change must be treated specifically (differently from other
		% calls): it should not loop module-locally, but based on an explicit
		% module call so that the latest loaded version of the corresponding
		% module is used from then on.
		%
		% A standard call could have been supported instead of this one based on
		% an intentional quadruplet, but this would have induced the overhead of
		% a systematic (top-level) extra pattern-match to all incoming calls,
		% which is not felt desirable.
		%
		% Should the version change of a class require specific instance-side
		% update operations (this is the general case), the safest way
		% (implemented here) to proceed is first to "suspend" all existing
		% instances of that class (so that they stop listening to and processing
		% normal calls), then to update (globally) the corresponding, single
		% class module, then to tell each instance to update itself accordingly
		% and individually, and finally let it live its life afterwards.
		%
		{ freezeUntilVersionChange, TargetVersion, MaybeExtraData,
		  CallerPid } ->

			% Concentrated in a separate, dedicated module; never returning, as
			% switching to the main loop of the target module:
			%
			class_Upgradable:manage_version_change( TargetVersion,
				MaybeExtraData, CallerPid, State );


		Other ->

			% Catch-all:
			?wooper_log_format( "Main loop (case Z) for ~w: unmatched ~p.~n",
								[ self(), Other ] ),

			wooper:log_error( "As the following message did not respect "
				"the WOOPER conventions, the WOOPER instance ~w had to "
				"ignore it (abnormal):~n~p.~n", [ self(), Other ] ),

			%?wooper_log( "Main loop (case Z) ended.~n" ),
			throw( { wooper_erroneous_call, Other } )


	end.

	% Commented out to preserve (presumably) tail-recursion:
	% trace_utils:info( "wooper_main_loop exited." ).
