% Copyright (C) 2022-2024 Olivier Boudeville
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
% Creation date: Friday, August 12, 2022.


% @doc Interface class implementing the Upgradable trait, so that instances
% supporting that trait are able to <b>be hot-updated</b>, that is to have their
% class definition changed at runtime (either upgraded or downgraded), with no
% need to restart the system as a whole.
%
% So the objective is that the instances implementing this interface do not have
% to terminate, and may update themselves on the fly (once their new class has
% been loaded), code-wise and also state-wise.
%
% For that, a concreate Upgradable child class should (besides inheriting from
% this interface):
%
%  - implement a relevant get_version/1 static method, whose signature is:
%          -spec get_version() -> static_return(any_version()).
%
%  - possibly override its {up,down}gradeVersion/4 member methods
%
% See also class_Upgradable_test.erl and the support of the
% 'freezeUntilVersionChange' special message in the WOOPER main loop (refer to
% wooper_main_loop_functions.hrl).
%
% As by default no WOOPER-level instance tracking is performed (this is often an
% application-specific topic), the PIDs of the instances to update have to be
% provided by the caller.
%
% Should instances be left over (i.e. not be updated), depending on the
% preferences specified when triggering the update, either this update will be
% reported as failed, or these instances will be killed - not at the first
% missed upgrade (as they will just linger then in old code), but at the next
% one.
%
% To avoid unwanted calls to be processed during an update, the relevant
% processes, notably instances, shall be frozen. When a class is updated, this
% includes not only its direct instances but also the one of all classes
% deriving from it.
%
% A difficulty is that by default nothing prevents static methods / functions
% exported by this class to be called just before said update and to interfere /
% have their possibly mostly unrelated process be killed. Determining the
% culprits and freezing them until none of them gets in the way of a soft purge
% might be a good solution.
%
% A question is how the PIDs of the instances of an updated class are to be
% determined (see
% https://erlangforums.com/t/determining-processes-lingering-in-old-code/1755
% for a related discussion).
%
% Either each class keeps track of its instances (not recommended, as incurs
% systematic overhead and may not be scalable), or a massive scan is performed
% (then preferably in a concurrent way, as done by the ERTS code purger; see
% do_soft_purge/2 in erts_code_purger.erl for that).
%
-module(class_Upgradable).


-define( class_description,
		 "Interface implementing the Upgradable trait, for all instances able "
		 "to be upgraded/downgraded on the fly, at runtime, with no specific "
		 "restarting." ).


% Emitting traces in useful, but cannot be class_Traceable as we are at the
% level of WOOPER:
%
-define( superclasses, [] ).


-define( class_attributes, [

	% Previously, for a better controllability, this information was at
	% instance-level, but a static information is fully sufficient:
	%
	%{ wooper_upgradable_version, any_version(),
	%  "the current version of this class" }

						   ] ).


-type upgradable_pid() :: pid().
% The PID of an instance implementing the Upgradable interface.


-type extra_data() :: any().
% Any extra data (akin to release-specific information) of use when performing a
% version change.


% Note: for messages, we cannot describe values like [V1, V2] as being of type
% [T1(), T2()], so we use [T1() | T2()] instead.


-type freeze_info() :: { classname(), instance_pid() }.
% Information about an instance after it applied a freeze request.


-type freeze_notification() :: { 'onInstanceFrozen', freeze_info() }.
% Message sent back to the sender of a 'freezeUntilVersionChange' special
% message.
%
% This message may be interpreted as a oneway call.


-type update_success_report() :: { classname(), instance_pid(), any_version() }.
% A report sent by an instance that succeeded in updating itself to the
% specified version.


-type update_failure_report() ::
		{ error_reason(), classname(), instance_pid(), any_version() }.
% A report sent by an instance that failed in updating itself and ended up in
% the specified version (most probably its pre-update one).


-type update_outcome() :: { 'onUpdateSuccess', update_success_report() }
						| { 'onUpdateFailure', update_failure_report() }.
% Outcome of an instance update, which may be an upgrade or a downgrade.
%
% This is a message (possibly interpreted as a oneway call) sent back to the
% caller by an instance having being requested to update.


-export_type([ upgradable_pid/0, extra_data/0,
			   freeze_notification/0,
			   update_success_report/0, update_failure_report/0,
			   update_outcome/0 ]).


% Exported helper functions that can be applied to Upgradable instances (only):
-export([ manage_version_change/4, get_version/1, to_string/1 ]).


% Exported helper functions that can be applied to any WOOPER state:
-export([ is_upgradable/1, get_maybe_version/1, to_maybe_string/1 ]).




% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Must be included before the class_TraceEmitter header:
-define( trace_emitter_categorization, "Upgradable" ).


% Would allow to use macros for trace sending, yet we are in WOOPER here:
%-include_lib("traces/include/class_Traceable.hrl").


% The default, initial version for all classes (sole location where it is
% defined):
%
-define( default_initial_class_version, { 0, 0, 1 } ).


% Each concrete Upgradable class shall specify its own version like this:
-define( this_class_version, { 0, 0, 1 } ).




% Implementation notes:
%
% By default we rely on three-digit versions.
%
% Regarding code upgrade, this operation is by nature centralised (as it is VM /
% node level); this corresponds to performing an operation like:
%  compile:file( ?MODULE ),
%  code:purge( ?MODULE ),
%  code:load_file( ?MODULE )
%
% ERTS can handle two versions of a module, and calling a function with
% mod:func(...) will always call the latest version of this module (if the
% function is exported).
%
% The new version of the module shall still be explicitly loaded into the
% system.
%
% When the new module version has been loaded, all instances of the
% corresponding class shall switch, here thanks to the upgrade/1 request.
%
% See also the upgrade_class/1 static method.
%
% As stated in the documentation (refer to
% https://www.erlang.org/doc/reference_manual/code_loading.html#code-replacement
% for more information), code replacement is done on a module level.
%
% The code of a module can exist in two variants in a system: current and
% old. When a module is loaded into the system for the first time, the code
% becomes 'current'. If then a new instance of the module is loaded, the code of
% the previous instance becomes 'old' and the new instance becomes 'current'.
%
% Both old and current code is valid, and can be evaluated concurrently. Fully
% qualified function calls always refer to current code. Old code can still be
% evaluated because of processes lingering in the old code.
%
% If a third instance of the module is loaded, the code server removes (purges)
% the old code and any processes lingering in it is terminated. Then the third
% instance becomes 'current' and the previously current code becomes 'old'.
%
% To change from old code to current code, a process must make a fully qualified
% function call.
%
% For code replacement of funs to work, use the syntax fun
% Module:FunctionName/Arity.

% See also
% https://learnyousomeerlang.com/designing-a-concurrent-application#hot-code-loving,
% and https://learnyousomeerlang.com/relups regarding the need to be able to
% freeze processes between the update of their module(s) and the processing of
% an 'upgrade' message. Such kind of "time suspension" is typically useful if
% the definition of records changed.

% Note that now whether an instance is Upgradable and what its current version
% is are determined rather differently: the former by determining whether its
% class inherits (directly or not) from Upgradable, the latter by calling a
% (presumably defined) static method to obtain that version.

% At least currently, there is no way to force that all Upgradable classes
% define such as static method.


% Shorthands:

-type any_version() :: basic_utils:any_version().
-type base_status() :: basic_utils:base_status().
-type error_reason() :: basic_utils:error_reason().
-type base_outcome() :: basic_utils:base_outcome().
-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type define() :: code_utils:define().


% @doc Constructs an upgradable instance.
%
% The corresponding version is determined statically.
%
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->
	% Traceable trait now optional:
	%TraceState = class_Traceable:construct( State ),
	%setAttribute( TraceState, wooper_upgradable_version,
	%              basic_utils:check_any_version( InitialVersion ) ).
	State.


% No destructor.



% Methods section.


% @doc Returns the current version of this Upgradable.
-spec getVersion( wooper:state() ) -> const_request_return( any_version() ).
getVersion( State ) ->

	% Previously: CurrentVer = ?getAttr(wooper_upgradable_version),
	CurrentVer = get_version( State ),

	wooper:const_return_result( CurrentVer ).



% @doc Upgrades this instance (thus to a more recent version) both in terms of
% code and state, taking into account any specified extra data.
%
% So performs an actual state upgrade between the two specified versions; in the
% general case this may involve adding/removing attributes, changing their value
% and/or type.
%
% This implementation, meant to be overridden, does mostly nothing.
%
% It is strongly recommended that, as done by default, an instance requested to
% perform such update is in a frozen state (either explicitly created, or only
% obtained by construction, for example if not being scheduled/triggered anymore
% by some manager), lest it receives method calls between the overall class
% update and the processing of the actual version change.
%
% So this request should not be called directly, but whereas being already
% frozen in the context of a prior freezeUntilVersionChange special call.
% See also manage_version_change/4.
%
-spec upgradeVersion( wooper:state(), any_version(), any_version(),
			maybe( extra_data() ) ) -> request_return( base_outcome() ).
upgradeVersion( State, OriginalVersion, TargetVersion, MaybeExtraData ) ->

	cond_utils:if_defined( wooper_debug_hot_update,
		trace_bridge:debug_fmt( "Instance ~w upgrading from version ~ts to "
			"~ts, using extra data '~p'.",
			[ self(), text_utils:version_to_string( OriginalVersion ),
			  text_utils:version_to_string( TargetVersion ),
			  MaybeExtraData ] ),
		basic_utils:ignore_unused( MaybeExtraData ) ),

	cond_utils:if_defined( wooper_check_hot_update,
		case get_version( State ) of

			% Expected to be already at target:
			TargetVersion ->
				ok;

			OtherVersion ->
				throw( { invalid_version_on_upgrade, { read, OtherVersion },
						 { original, OriginalVersion },
						 { target, TargetVersion },
						 State#state_holder.actual_class, self() } )

		end ),

	% In this default implementation, the state remains const:
	UpgradedState = State,

	wooper:return_state_result( UpgradedState, ok ).



% @doc Downgrades this instance (thus to a less recent version) both in terms of
% code and state, taking into account any specified extra data.
%
% So performs an actual state downgrade between the two specified versions; in
% the general case this may involve adding/removing attributes, changing their
% value and/or type.
%
% This implementation, meant to be overridden, does mostly nothing.
%
% It is strongly recommended that, as done by default, an instance requested to
% perform such update is in a frozen state (either explicitly created, or only
% obtained by construction, for example if not being scheduled/triggered anymore
% by some manager), lest it receives method calls between the overall class
% update and the processing of the actual version change.
%
% So this request should not be called directly, but whereas being already
% frozen in the context of a prior freezeUntilVersionChange special call.
% See also manage_version_change/4.
%
-spec downgradeVersion( wooper:state(), any_version(), any_version(),
			maybe( extra_data() ) ) -> request_return( base_outcome() ).
downgradeVersion( State, OriginalVersion, TargetVersion, MaybeExtraData ) ->

	cond_utils:if_defined( wooper_debug_hot_update,
		trace_bridge:debug_fmt( "Downgrading from version ~ts to ~ts, "
			"using extra data '~p'.",
			[ text_utils:version_to_string( OriginalVersion ),
			  text_utils:version_to_string( TargetVersion ),
			  MaybeExtraData ] ),
		basic_utils:ignore_unused( MaybeExtraData ) ),


	cond_utils:if_defined( wooper_check_hot_update,
		case get_version( State ) of

			% Expected to be already at target:
			TargetVersion ->
				ok;

			OtherVersion ->
				throw( { invalid_version_on_upgrade, { read, OtherVersion },
						 { original, OriginalVersion },
						 { target, TargetVersion },
						 State#state_holder.actual_class, self() } )

		end ),


	% In this default implementation, the state remains const:
	DowngradedState = State,

	wooper:return_state_result( DowngradedState, ok ).




% Static section.


% @doc Returns the version of that class (that corresponds to this module).
%
% Each version of a class should define its own version of this static method.
%
-spec get_version() -> static_return( any_version() ).
get_version() ->
	% Each concrete Upgradable class is typically to return its own define:
	wooper:return_static( ?this_class_version ).



% @doc Freezes (synchronously) the specified instances, so that they are ready
% for an update of their class to the specified version, with no extra data
% specified.
%
% Returns a list (in no particular order) of freeze information, i.e. the
% classname of each frozen instance, associated to its PID.
%
% This implementation bypasses the WOOPER main loop (directly collecting
% messages, instead of interpreting them as oneway calls).
%
-spec freeze_instances( [ instance_pid() ], any_version() ) ->
								static_return( [ freeze_info() ] ).
freeze_instances( InstancePids, TargetVersion ) ->

	FreezeInfos = freeze_instances( InstancePids, TargetVersion,
									_MaybeExtraData=undefined ),

	wooper:return_static( FreezeInfos ).



% @doc Freezes (synchronously) the specified instances, so that they are ready
% for an update of their class to the specified version, with the specified
% extra data.
%
% Returns a list (in no particular order) of the classname of each frozen
% instance, associated to its PID.
%
% This implementation bypasses the WOOPER main loop (directly collecting
% messages, instead of interpreting them as oneway calls).
%
-spec freeze_instances( [ instance_pid() ], any_version(),
			maybe( extra_data() ) ) -> static_return( [ freeze_info() ] ).
freeze_instances( InstancePids, TargetVersion, MaybeExtraData ) ->

	InstCount = length( InstancePids ),

	cond_utils:if_defined( wooper_debug_hot_update,
		trace_bridge:debug_fmt( "Freezing ~B instances: ~ts.",
			[ InstCount, text_utils:pids_to_short_string( InstancePids ) ] ) ),

	% The special message understood by the main loop of all Upgradables:
	FreezeMsg = { freezeUntilVersionChange, TargetVersion, MaybeExtraData,
				  _CallerPid=self() },

	[ IPid ! FreezeMsg || IPid <- InstancePids ],

	FreezeInfos = collect_freeze_acks( InstCount ),

	cond_utils:if_defined( wooper_debug_hot_update,
		trace_bridge:debug_fmt( "~B instances frozen:~n ~p",
			[ length( FreezeInfos ), FreezeInfos ] ) ),

	% Now that in a stable situation, the class update can take place:

	wooper:return_static( FreezeInfos ).



% @doc Updates (globally) the specified class, by recompiling it, purging its
% current implementation, and reloading it.
%
% So operates at the class level, with no direct interaction with instances, not
% selecting any particular version (the one of the updated class will just
% apply). Typically all instances of that class have been already frozen (see
% the freeze_instances/{2,3} static methods and the freezeUntilVersionChange
% special call), waiting for the new code to be available and adapt to it.
%
% ForceRecompilation tells whether the class module shall be forcibly
% recompiled; useful for example if wanting to apply specific compilation
% options.
%
% KillAnyLingeringProcess tells whether any process (probably an instance)
% lingering on the old code shall be killed, or if the update shall just be
% considered as having failed.
%
% The first soft-purge will succeed even if an instance (e.g. agent C in
% class_Upgradable_test) was not updated, yet the next module reloading will
% fail, as the class will *not* be updated. If ignoring that failure, the old
% code will attempt to operate on newer instance states, which of course should
% not be done.
%
-spec update_class( classname(), boolean(), [ define() ], boolean() ) ->
									static_return( base_status() ).
update_class( Classname, ForceRecompilation, Defines,
			  KillAnyLingeringProcess ) ->

	% Refer to implementation notes.

	% First, we recompile that class:
	Res = case code_utils:recompile( Classname, ForceRecompilation, Defines ) of

		ok ->
			trace_utils:debug_fmt( "Class '~ts' successfully recompiled.",
								   [ Classname ] ),

			purge_and_reload_class( Classname, KillAnyLingeringProcess );

		RecompErr={ error, Reason } ->
			trace_utils:error_fmt( "Class '~ts' could not be recompiled; "
								   "reason: ~ts.", [ Classname, Reason ] ),

			RecompErr

	end,

	wooper:return_static( Res ).



% @doc Requests the specified instance to update themselves (asynchronously)
% against their new current (expected to have been updated) class.
%
% These instances are expected to have been frozen beforehand (see
% request_instances_to_update/1).
%
-spec request_instances_to_update( [ instance_pid() ] ) -> static_return(
		  { [ update_success_report() ], [ update_failure_report() ] } ).
request_instances_to_update( Instances ) ->

	% The frozen instances already know the PID of this caller:
	[ I ! updateFromUpgradableClass || I <- Instances ],

	% Better than just counting, to be able to determine any lacking outcome:
	WaitedSet = set_utils:from_list( Instances ),

	AccPair = wait_update_outcomes( WaitedSet, _SuccessAcc=[], _FailureAcc=[] ),

	wooper:return_static( AccPair ).



% Section for helper functions (not methods).


% @doc Takes in charge the instance-side part of the version update protocol:
% freezes until the class has been updated, and then applies the corresponding
% instance-specific state changes.
%
% Sends back to the caller first a freeze_notification() message, then, once
% requested to update and having attempted to do so, an update_outcome()
% message.
%
-spec manage_version_change( any_version(), extra_data(), pid(),
							 wooper:state() ) -> 'deleted'. % no_return().
manage_version_change( TargetVersion, MaybeExtraData, CallerPid, State ) ->

	ActualClassMod = State#state_holder.actual_class,

	% No interleaving possible; yet still safe to fetch by design, as the class
	% has not been reloaded yet:
	%
	OriginalVersion = case get_maybe_version( State ) of

		undefined ->
			% Faulty upgradable or wrong instance:
			throw( { no_get_version_defined, ActualClassMod, self() } );

		V ->
			V

	end,

	% Synchronisation needed, typically so that the caller can execute
	% class_Upgradable:update_class/2 on its own only once all instances are
	% frozen (i.e. not too early, whereas some of them may still be busy
	% executing their current methods):
	%
	% (sending of a freeze_notification() possibly-oneway message, possibly
	% interpreted as a oneway call, and preferably sending a pair instead of a
	% list)
	%
	CallerPid ! { onInstanceFrozen, _FreezeInfo={ ActualClassMod, self() } },

	% Trying to anticipate work as much as possible:
	MaybeTargetRequest = case basic_utils:compare_versions( OriginalVersion,
															TargetVersion ) of

		second_bigger ->
			cond_utils:if_defined( wooper_debug_hot_update,
				trace_bridge:debug_fmt( "Instance ~w will upgrade from "
					"version ~ts to ~ts.", [ self(),
						text_utils:version_to_string( OriginalVersion ),
						text_utils:version_to_string( TargetVersion ) ] ) ),
			upgradeVersion;


		equal ->
			% Believed to be abnormal:
			trace_bridge:warning_fmt( "No update will be done, as the original "
				"and target versions are the same (~ts).",
				[ text_utils:version_to_string( OriginalVersion ) ] ),

			undefined;


		first_bigger ->
			cond_utils:if_defined( wooper_debug_hot_update,
				trace_bridge:debug_fmt(
					"Instance ~w will downgrade from version ~ts to ~ts.",
					[ self(), text_utils:version_to_string( OriginalVersion ),
					  text_utils:version_to_string( TargetVersion ) ] ) ),
			downgradeVersion

	end,

	Args = [ OriginalVersion, TargetVersion, MaybeExtraData ],

	% Should the update succeed:
	SuccessOutcome =
		{ onUpdateSuccess, [ ActualClassMod, self(), TargetVersion ] },

	% Fully prepared, just waiting then, frozen until the next selective receive
	% is triggered:
	%
	% (any method calls pending in the mailbox to be processed just afterwards)
	%
	receive

		% Sent only once the new class module has been loaded:
		updateFromUpgradableClass ->

			cond_utils:if_defined( wooper_debug_hot_update,
				trace_bridge:debug_fmt( "Instance ~w requested now to update.",
										[ self() ] ) ),

			{ UpdateOutcome, FinalState } = case MaybeTargetRequest of

				% Nothing done, still considering it a success:
				undefined ->
					{ SuccessOutcome, State };

				TargetMethod ->
					% Thanks to the virtual table (implying a module-qualified
					% call), this will be resolved with the latest version of
					% the modules involved:
					%
					{ UpdatedState, UpdateRes } =
						executeRequest( State, TargetMethod, Args ),

					%trace_utils:debug_fmt( "Updated state:~n ~p",
					%                       [ UpdatedState ] ),

					Outcome = case UpdateRes of

						ok ->
							SuccessOutcome;

						{ error, ErrorReason } ->
							% So we consider that we remained in the original
							% version, retaining nevertheless the returned
							% state (which is probably the original one):
							%
							{ onUpdateFailure, [ ErrorReason, ActualClassMod,
												 self(), OriginalVersion ] }

					end,

					{ Outcome, UpdatedState }

			end,

			CallerPid ! UpdateOutcome,

			% The special-cased module-qualified call, to branch to the
			% new implementation for this loop as well:
			%
			% (the update could even change the class name!)
			%
			FinalClassMod = FinalState#state_holder.actual_class,

			cond_utils:if_defined( wooper_debug_hot_update,

				%trace_bridge:debug_fmt( "Update done (outcome: ~p), "
				%   "state:~n ~p.", [ UpdateOutcome, FinalState ] ) ),

				trace_bridge:debug_fmt( "Update done (outcome: ~p).",
										[ UpdateOutcome ] ) ),

			FinalClassMod:wooper_main_loop( FinalState )

	end.



% @doc Collects the specified number of freeze information messages.
-spec collect_freeze_acks( count() ) -> [ freeze_info() ].
collect_freeze_acks( InstCount ) ->
	collect_freeze_acks( InstCount, _Acc=[] ).



% (helper)
collect_freeze_acks( _InstCount=0, Acc ) ->
	% No order matters:
	Acc;

collect_freeze_acks( InstCount, Acc ) ->
	receive

		% Could be stored in a table(classname(),[instance_pid()]) if useful:
		{ onInstanceFrozen, FreezeInfoPair } ->
			collect_freeze_acks( InstCount-1, [ FreezeInfoPair | Acc ] )

	end.



% @doc Returns the current version of this upgradable version.
%
% (exported helper, defined for convenience)
%
-spec get_version( wooper:state() ) -> any_version().
get_version( State ) ->
	basic_utils:check_not_undefined( get_maybe_version( State ) ).



% @doc Returns a textual description of this instance.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
	text_utils:format( "upgradable instance ~ts",
					   [ to_maybe_string( State ) ] ).





% The following helper functions can be used in the context of any class,
% whether or not it implements this Upgradable interface.


% @doc Tells whether the corresponding instance implements the Upgradable
% interface.
%
% (exported helper)
%
-spec is_upgradable( wooper:state() ) -> boolean().
is_upgradable( State ) ->
	% Previously instance-level:
	%hasAttribute( State, wooper_upgradable_version ).

	% Now statically determined:
	lists:member( ?MODULE, wooper:get_all_superclasses( State ) ).



% @doc Returns any version available for the corresponding instance.
%
% This function is designed to apply to any WOOPER instance, whether it is a
% Upgradable one or not.
%
% (exported helper)
%
-spec get_maybe_version( wooper:state() ) -> maybe( any_version() ).
get_maybe_version( State ) ->

	ActualClassMod = State#state_holder.actual_class,

	case meta_utils:is_function_exported( ActualClassMod, get_version,
										  _Arity=0 ) of

		true ->
			ActualClassMod:get_version();

		false ->
			undefined

	end.



% @doc Returns a textual element of description of the corresponding instance,
% should it implement the Upgradable interface.
%
% (exported helper)
%
-spec to_maybe_string( wooper:state() ) -> maybe( ustring() ).
to_maybe_string( State ) ->
	case get_maybe_version( State ) of

		undefined ->
			undefined;

		AnyVer ->
			text_utils:format( "whose version is ~ts",
							   [ text_utils:version_to_string( AnyVer ) ] )

	end.



% Section for purely internal helpers.


% Purges and reloads the specified class, expected to be already recompiled.
%
% (helper)
%
-spec purge_and_reload_class( classname(), boolean() ) -> base_status().
purge_and_reload_class( Classname, KillAnyLingeringProcess ) ->

	% Removes code marked as old - but only if no process lingers in it:
	case code:soft_purge( Classname ) of

		% No process to clear:
		true ->
			trace_utils:debug_fmt( "(soft purge of '~ts' succeeded)",
								   [ Classname ] ),
			reload_class( Classname );

		% Process(es) in the way:
		false ->
			case KillAnyLingeringProcess of

				true ->
					trace_utils:warning_fmt( "Classname '~ts' cannot be "
						"soft-purged, as there is at least one process "
						"lingering in the old code; such processes to be "
						"killed now through a hard purge. "
						"If this test VM gets killed in the process, ensure "
						"that the module corresponding to this class was "
						"not compiled with LCO disabled "
						"(refer to the MYRIAD_LCO_OPT for that).",
						[ Classname ] ),

					case code:purge( Classname ) of

						% Understood as being successful:
						true ->
							trace_utils:debug( "(purge succeeded)" ),
							reload_class( Classname );

						false ->
							{ error, { purged_failed, Classname } }

					end;

				false ->
					{ error, { lingering_processes, Classname } }

			end

	end.



% Reloads the specified class, expected to be already recompiled and purged.
%
% (helper)
%
-spec reload_class( classname() ) -> base_status().
reload_class( Classname ) ->

	case code:load_file( Classname ) of

		{ module, Classname } ->
			trace_utils:debug_fmt( "Class '~ts' successfully reloaded.",
								   [ Classname ] ),
			ok;

		LoadErr={ error, Reason } ->
			trace_utils:error_fmt( "Class '~ts' could not be reloaded; "
								   "reason: ~ts.", [ Classname, Reason ] ),
			LoadErr

	end.



% Waits the update outcomes to be received, for all instances in the specified
% set.
%
wait_update_outcomes( WaitedSet, SuccessAcc, FailureAcc ) ->

	% Cannot be tested in a guard:
	case set_utils:is_empty( WaitedSet ) of

		true ->
			{ SuccessAcc, FailureAcc };

		false ->
			receive

				{ onUpdateSuccess, SuccessReport=[ _Classname, InstancePid,
												   _ResultingVersion ] } ->

					NewWaitedSet =
						set_utils:delete_existing( InstancePid, WaitedSet ),

					wait_update_outcomes( NewWaitedSet,
						[ SuccessReport | SuccessAcc ], FailureAcc );


				{ onUpdateFailure, FailureReport=[ _ErrorReason, _Classname,
						InstancePid, _ResultingVersion ] } ->

					NewWaitedSet =
						set_utils:delete_existing( InstancePid, WaitedSet ),

					wait_update_outcomes( NewWaitedSet, SuccessAcc,
										  [ FailureReport | FailureAcc ] )

			end

	end.
