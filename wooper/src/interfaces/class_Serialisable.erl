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
% Creation date: Sunday, July 24, 2022.


% @doc Interface class implementing the Serialisable trait, so that the
% instances having that trait can have their <b>state serialised and
% deserialised</b>.
%
% Such instances can have their state transformed in a term designed to be
% exchanged and/or stored (typically as a series of bytes) through any medium
% (e.g. written in a WSF file, or a network stream), and loaded afterwards from
% such a content.
%
% Such an interface is an abstract mother class from which all serialisable
% instances must derive.
%
% They may override the serialisation/deserialisation mechanisms (e.g. to target
% extra serialisation formats thanks to the serialiseTerm/2 request and the
% deserialise_term/1 static method) or, more frequently, they may introduce
% class-specific operations before and/or after serialisation and/or
% deserialisation, through the overriding of the base, do-nothing
% on{Pre,Post}Serialisation/2 and onPostDeserialisation/2 requests (a.k.a. OOP
% "hooks").
%
% Operations can be further specialised by providing a custom entry transformer,
% possibly with any user-data of choice (which additionally can be further
% transformed by aforementioned pre/post hooks). Returning user data is also a
% way of defining synchronous operations.
%
% Generally an entry transformer is project-specific and transverse to all
% classes, whereas hooks are class-specific.
%
% This interface provides also exported functions designed so that they can be
% applied to any WOOPER instance, whether or not it has this trait or not.
%
-module(class_Serialisable).


-define( class_description,
		 "Interface to be implemented by all instances able to store their "
		 "state in, and load it from, a content such as a series of bytes." ).


% No superclasses.


% Declaration of the interface-specific attributes:
%
% (as it is a WOOPER builtin, they are all prefixed with 'wooper' and the
% interface name)
%
% No specific attribute involved: -define( class_attributes, [] ).


-type serialisable_pid() :: pid().
% The PID of an instance implementing this Serialisable interface.


-type entry_transformer() ::
		fun( ( attribute_entry(), user_data() ) ->
				{ attribute_entry(), user_data() } ).
% Any function that is able to transform entries of the state of an instance.
%
% An entry transformer may be used for a smarter serialisation (e.g. PID-aware),
% in which case this function could be a functor holding a bijective table in
% charge of converting PIDs in entries into stable identifiers (e.g. see
% class_Identifiable).
%
% Note: see meta_utils:transform_term/4, which may be useful in that context,
% and also the TextTransformer example in serialisable_test.erl.



%-type state_transformer_fun() ::
%       fun( ( wooper:state(), user_data() ) ->
%               { wooper:state(), user_data() } ).
% An anonymous function in charge of transforming the specified state in another
% one, possibly using any specified user data for that, and returning an updated
% version thereof.
%
% Typically useful when having obtained a just deserialised state (possibly also
% already updated by an entry transformer), to perform any last change needed
% before this state is used from then on by a corresponding loaded instance.
%
% Finally disabled, as better implemented by the onPostDeserialisation/2
% request.


-type extra_data() :: term().
% Any non-state, extra data that shall be persisted as well.
%
% For example, associated files that may not be easily regenerated.


% For defines and wooper_serialisation_instance_record:
-include("class_Serialisable.hrl").


-type instance_record() :: #wooper_serialisation_instance_record{}.
% A serialisation-ready record storing all needed information regarding a given
% instance.
%
% This is the term that will be passed as-is, verbatim, to an actual serializer
% and that later will be read by a deserializer in order to create back a
% corresponding instance.


-type serialisation() :: bin_serialisation() | term().
% Designates the final serialisation form of an instance, which may or may not
% be a binary (e.g. our default bin_serialisation/0 versus, say, a JSON-based
% serialisation term).


-type bin_serialisation() :: binary().
% The serialisation form of an instance, as an ext_binary, that is a binary data
% object, structured according to the Erlang external term format.
%
% However erlang:ext_binary/0 is not exported:
%-type bin_serialisation() :: erlang:ext_binary().


-type restoration_marker() :: ?process_restoration_marker
							| ?file_restoration_marker
							| ?term_restoration_marker.
% The restoration markers are atoms that, when serialising state attribute
% entries, may replace transient values (refer to the 'About serialised
% elements' section) so that serialisation terms are context-free.
%
% Application-specific markers can also be defined, e.g.
% -define( resilience_marker, foobar_resilience_marker ).



-type load_info() :: { instance_pid(), classname(), user_data() }.
% The information returned about an instance having been synchronously loaded
% (that has been successfully deserialised in a dedicated spawned process);
% asynchronous loadings only have a PID to return first.


-type user_data() :: basic_utils:user_data().
% Any user-specified data that can be used when (de)serialising.
%
% Can be here anything (e.g. 'undefined') and/or be completly ignored.


-export_type([ serialisable_pid/0, entry_transformer/0,
			   %state_transformer_fun/0,
			   extra_data/0, instance_record/0,
			   serialisation/0, bin_serialisation/0,
			   restoration_marker/0, load_info/0,
			   user_data/0 ]).



% Exported helper functions, usable against any WOOPER instance:
-export([ is_serialisable/1 ]).



% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


% The conventional, reserved attribute name denoting the random state of the
% process dictionary to preserve:
%
% (define to avoid any risk of mispelling)
%
-define( random_attr_name, wooper_serialisable_random_state ).


% The options to be used when serialising/deserialising:
-define( serialisation_opts, [ { compressed, 9 }, { minor_version, 2 } ] ).


% Design notes

% This interface could be a root class for all WOOPER instances (notably
% because, as it does not introduce attributes, it would not increase their
% memory footprint), yet we prefer that WOOPER remains "pure", not tainted by a
% root class, even if such a class would remain mostly optional; selectable
% interfaces are more relevant. Moreover all class modules would be a bit
% heavier because of the methods additionally inherited, which is not desirable.


% About serialised elements:
%
% The goal of a serialisation is to store in a serialised form the minimal,
% complete, context-free state of, here, a WOOPER instance.
%
% This includes at least the value of all WOOPER attributes, knowing that these
% terms may reference transient values that cannot be reproducibly recreated
% (e.g. PID, references, open files or sockets) just by themselves; we
% introduced hooks/methods that can be overridden, and the support of any
% user-specified entry transformers, so that an application can manage them as
% wanted.
%
% As we do not have a typed (class-specific) state (record-like) data-structure
% or, currently, a reliable way of ensuring that all instances of a given class
% have the exact same list of attributes, we have to store not only the values,
% but also the keys; a significant progress in terms of compactness still lies
% there.
%
% A perfect serialisation should also encompass the following process-level
% information:
%
%  - the process dictionary (key/values, both of which can be also transient
%  values); this includes the random state of that instance process
%
%  - the processes to which this instance is linked
%
%  - whether this process traps EXIT messages
%
%  - possibly: group leader, priority, monitors, etc.
%
% Use erlang:process_info/1 to figure out process-level available information
% that may have to be restored and thus saved.
%
% The base WOOPER serialisation mechanisms do not account for these
% process-level currently (e.g. links may be dictated by the application logic
% and thus may not have to be stored), except one: the current random state of
% the serialised process, as known of Myriad's random_utils, which is
% transparently managed by WOOPER so that the deserialisation will lead to
% restoring the right random state.


% About serialisation operators:
%
% We concentrate on the most useful load operators, so we did not feel the
% specific need to define:
% - synchronous_timed_load
% - synchronous_timed_load_link
% - remote_load
% - remote_load_link
% - remote_synchronous_load
% - remote_synchronous_load_link
% - remote_synchronous_timed_load
%
% (but they can be easily added if wanted)

% This interface module superseded for the best the previous
% former wooper_serialisation module and the
% wooper_serialisable_{exports,functions}.hrl header files.

% At least with the current implementation, serialisation operations are const,
% yet the API and code already support overriding them with non-const ones
% (should they record a timestamp of the last serialisation, or a counter
% thereof, or anything else).


% About serialisation formats.
%
% Although any kind of serialisation format could be used, here, at least by
% default, we rely on the Erlang Term Format
% (https://www.erlang.org/doc/apps/erts/erl_ext_dist.html), at it is a compact,
% well-integrated, binary, at least reasonably efficient solution.
%
% Note that there is a difference between an input binary and its serialised
% form. Notably, a WSF file is not a single binary corresponding to a larger
% term (typically that would be a list) gathering a set of serialisations, it is
% the concatenation of binaries, each corresponding to a term (an instance
% record precisely).


% In this section, we define, for the current class, loading counterparts to the
% new operators, i.e. the various ways of creating an instance of that class
% from a deserialisation form (loading), instead of through a normal
% construction process of a brand new instance.
%
% For example, synchronous_new_link/* becomes synchronous_load_link/*.


% Entry transformers are expected not to depend on the order of their calls, as
% loadings can happen in parallel.


% Next improvements:
%
% - support the reading/writing of a series of instances from/to a stream,
% starting by the returning of a partly read serialisation
%
% - support also WOOPER passive



% Shorthands:

-type node_name() :: net_utils:node_name().



% @doc Constructs a Serialisable instance.
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->

	% Simplest constructor ever; the fact that it processes nothing and sets no
	% attribute is of course no reason to not call it from child classes.

	State.


% No need to define a specific destructor.



% Member method section.


% Default do-nothing oneways are defined here that are automatically triggered
% by the (de)serialisation procedure; they are meant to be overridden on a
% per-class basis (then most probably they would become non-const), should
% specific operations have to be performed at any (de)serialisation-related
% step.



% Serialisation section.


% @doc Serialises this instance (that is its state), using no specific entry
% transformer or user data, returning a corresponding serialisation term,
% together with the instance PID (useful to allow to discriminate between
% serialisations performed in parallel).
%
-spec serialise( wooper:state() ) ->
					request_return( { serialisation(), instance_pid() } ).
serialise( State ) ->

	{ SerialState, { SerialTerm, _SerialUserData, Self } } =
		executeRequest( State, serialise,
			[ _MaybeEntryTransformer=undefined, _UserData=undefined ] ),

	wooper:return_state_result( SerialState, { SerialTerm, Self } ).



% @doc Serialises this instance (that is its state), using any specified entry
% transformer and user data, returning corresponding serialisation term and
% updated user data, together with the instance PID (useful to allow to
% discriminate between serialisations performed in parallel).
%
-spec serialise( wooper:state(), maybe( entry_transformer() ), user_data() ) ->
		request_return( { serialisation(), user_data(), instance_pid() } ).
serialise( State, MaybeEntryTransformer, UserData ) ->

	% We have to discriminate between the states:
	%  - of the instance itself (*InstanceState), mostly const
	%  - used by the serialisation (the other states), possibly heavily
	%  transformed

	% May affect the original instance state, if ever needed:
	{ PreSerialInstanceState, SerialRet } =
		executeRequest( State, onPreSerialisation, [ UserData ] ),

	{ SerialReadyState, PreUserData, MaybeExtraData } =
		case SerialRet of

			{ PreSerialState, PreSerialUserData } ->
				{ PreSerialState, PreSerialUserData, undefined };

			T={ _PreSerialState, _PreSerialUserData, _MaybeExtraData } ->
				T

	end,

	% At least generally const, and supporting overriding:
	{ SerialInstanceState, { SerialTerm, SerialUserData } } =
		executeRequest( PreSerialInstanceState, performStateSerialisation,
			[ SerialReadyState, MaybeEntryTransformer, PreUserData,
			  MaybeExtraData ] ),

	{ PostSerialInstanceState, { PostSerialTerm, PostUserData } } =
		executeRequest( SerialInstanceState, onPostSerialisation,
						[ SerialTerm, SerialUserData ] ),

	wooper:return_state_result( PostSerialInstanceState,
						{ PostSerialTerm, PostUserData, self() } ).



% @doc Pre-serialisation hook triggered just before serialising the state of
% this instance, to perform any pre-processing on a copy of its current state,
% possibly based on any user-specified data.
%
% The state explicitly returned here is dedicated to serialisation (generally
% the actual instance state is not impacted by serialisation and thus this
% request is often const).
%
% Such a hook is to return a possibly transformed state (that will not be
% re-used by the serialised instance, which will keep its own state) directly
% suitable for serialisation, for example with no transient technical
% identifiers (like PID, open files, etc.) - unless a later entry transformer is
% able to take care of them.
%
% Any extra data returned will be persisted (verbatim, that is not specifically
% transformed) as well, and be available as is when deserialising.
%
% The default implementation of this request is a const, do-nothing one
% (returning an exact copy of the current state); it is meant to be overridden
% if needed.
%
-spec onPreSerialisation( wooper:state(), user_data() ) ->
	const_request_return( { wooper:state(), user_data() }
						| { wooper:state(), user_data(), extra_data() } ).
onPreSerialisation( State, UserData ) ->

	cond_utils:if_defined( wooper_debug_serialisation,
		trace_bridge:debug_fmt( "Default pre-serialisation of instance ~w, "
			"based on user data ~p.", [ self(), UserData ] ) ),

	% This default version is const:
	wooper:const_return_result( { State, UserData } ).



% @doc Performs the actual serialisation of the explicitly-specified
% serialisation-ready state, using the specified entry transformer (if any),
% user data and any extra data specified.
%
-spec performStateSerialisation( wooper:state(), wooper:state(),
		maybe( entry_transformer() ), user_data(), maybe( extra_data() ) ) ->
				request_return( { serialisation(), user_data() } ).
performStateSerialisation( State, ToSerialiseState, MaybeEntryTransformer,
						   UserData, MaybeExtraData ) ->

	cond_utils:if_defined( wooper_debug_serialisation,
		case MaybeEntryTransformer of

			undefined ->
				trace_bridge:debug_fmt( "Default serialisation of instance ~w, "
					"with no specific entry transformer.", [ self() ] );

			ET ->
				trace_bridge:debug_fmt( "Default serialisation of instance ~w, "
					"based on entry transformer ~p and user data ~p.",
					[ self(), ET, UserData ] )

		end ),

	% Note that the 'request_sender' of the state_holder field is not taken into
	% account for serialisation, as, by design, there is indeed such a caller
	% (since serialise/3 is a request), but it is of no interest here.

	% There are, for all Erlang processes, some extra information that are
	% contextual, implicit, like: whether they are linked (and with whom), their
	% process dictionary, whether they trap exits, etc.; refer to the 'About
	% serialised elements' section of the implementation notes.

	% Retrieving all attribute key/value pairs
	BaseEntries = ?wooper_table_type:enumerate(
		ToSerialiseState#state_holder.attribute_table ),


	%trace_bridge:debug_fmt( "Original entries:~n ~p", [ BaseEntries ] ),

	% So, directly from the instance process, let's add the WOOPER extra
	% information:
	%
	WithRandEntries = case random_utils:get_random_state() of

		undefined ->
			BaseEntries;

		CurrentRandomState ->
			RandomAttribute = { ?random_attr_name, CurrentRandomState },

		[ RandomAttribute | BaseEntries]

	end,


	% Sorting would be useless yet possibly a bit cleaner, with lists:sort/1.

	% Applying any entry transformer on each of them:
	{ TransformedEntries, ETUserData } = case MaybeEntryTransformer of

		undefined ->
			{ WithRandEntries, UserData };

		EntryTransformer ->
			lists:foldl( EntryTransformer,
						 _Acc0={ _ResultingEntries=[], UserData },
						 _List=WithRandEntries )

	end,

	%trace_bridge:debug_fmt( "Transformed entries: ~n~p",
	%                        [ TransformedEntries ] ),


	% Generally not useful, as a prior transformation already recursed in
	% attribute values in order to replace all transient elements:
	%
	cond_utils:if_defined( wooper_check_serialisation,
		begin
			%trace_utils:debug( "Checking that no transient term remains." ),
			[ meta_utils:transform_term( AttrValue,
				_TypeDescription=undefined,
				fun wooper_serialisation:check_no_transient/2,
				_NoTransientUserData={ attribute, AttrName } )
					|| { AttrName, AttrValue } <- TransformedEntries ]
		end ),

	InstanceRecord = #wooper_serialisation_instance_record{
		class_name=State#state_holder.actual_class,
		attributes=TransformedEntries,
		extra_data=MaybeExtraData },

	% Most probably a const request; user data generally also useless here:
	%
	% ResPair = { SerialisedContent, FinalUserData }
	{ NewInstanceState, ResPair } =
		executeRequest( State, serialiseTerm, [ InstanceRecord, ETUserData ] ),

	% NewInstanceState is generally the initial 'State'; we do not want to
	% continue with any state forged for the serialisation (e.g. with
	% transformed local processes), we want to continue as we were before the
	% serialisation!
	%
	wooper:return_state_result( NewInstanceState, ResPair ).



% @doc The actual method in charge of serialising a term already prepared for
% serialisation.
%
% Of course other forms of serialisation could be introduced at this level.
%
% Its reciprocal is the deserialise_term/1 static method.
%
% Precisely this default, specific implementation returns a bin_serialisation().
%
-spec serialiseTerm( wooper:state(), term(), user_data() ) ->
				const_request_return( { serialisation(), user_data() } ).
serialiseTerm( State, Term, UserData ) ->

	% The 'deterministic' option does not seem crucial here:
	Bin = term_to_binary( Term,
						  _Opts=[ { compressed, 9 }, { minor_version, 2 } ] ),

	wooper:const_return_result( { Bin, UserData } ).



% @doc Hook triggered just after the serialisation step, operating on specified
% serialisation term and user data.
%
% This is the last possible transformation before the final serialisation term
% is returned, and generally the least useful to override.
%
% The value returned by this hook will be converted "as is" by the serializer
% (e.g. in a binary), that will be sent to the serialisation sink (e.g. a WSF
% file).
%
% (we do not want to return a state, as we do not want a state modified by the
% serialisation be mistakenly used afterwards)
%
% This is a default do-nothing implementation, meant to be overridden if needed.
%
% Such a hook is to return a state directly suitable for serialisation, for
% example with no transient technical identifiers (like PID, open files, etc.).
%
-spec onPostSerialisation( wooper:state(), serialisation(), user_data() ) ->
			const_request_return( { serialisation(), user_data() } ).
onPostSerialisation( State, SerialisationTerm, UserData ) ->

	cond_utils:if_defined( wooper_debug_serialisation,
		trace_bridge:debug_fmt( "Default post-serialisation of instance ~w, "
			"based on user data ~p.", [ self(), UserData ] ) ),

	% This default version is const:
	wooper:const_return_result( { SerialisationTerm, UserData } ).





% Deserialisation section.
%
% By construction the target instance does not exist yet, so no member method
% can be considered at first.
%
% Instead, a set of static methods dedicated to instance loading is provided, so
% that the specified serialisation term is processed and a corresponding WOOPER
% instance is spawned based on the state information recorded in that term.



% @doc Spawns an instance of this class, based on the specified serialisation
% information, using no specific entry transformer or user data.
%
% Returns the PID of the instance created by this loading.
%
% This creation will be asynchronous: this function returns a legit PID as soon
% as the creation is triggered, without waiting for it to complete.
%
-spec load( serialisation() ) -> static_return( instance_pid() ).
load( Serialisation ) ->

	InstPid = load( Serialisation, _MaybeEntryTransformer=undefined,
					_UserData=undefined ),

	wooper:return_static( InstPid ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, on any entry transformer, state transformer and user data.
%
% Returns the PID of the instance created by this loading.
%
% This creation will be asynchronous: this function returns a legit PID as soon
% as the creation is triggered, without waiting for it to complete.
%
-spec load( serialisation(), maybe( entry_transformer() ), user_data() ) ->
											static_return( instance_pid() ).
load( Serialisation, MaybeEntryTransformer, UserData ) ->

	InstPid = ?myriad_spawn(
		fun() ->
			deserialise( Serialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=undefined )
		end ),

	wooper:return_static( InstPid ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, using no specific entry transformer or user data, and links it to
% the current process.
%
% Returns the PID of the instance created by this loading.
%
% This creation will be asynchronous: this function returns a legit PID as soon
% as the creation is triggered, without waiting for it to complete.
%
-spec load_link( serialisation() ) -> static_return( instance_pid() ).
load_link( Serialisation ) ->

	InstPid = load_link( Serialisation, _MaybeEntryTransformer=undefined,
						 _UserData=undefined ),

	wooper:return_static( InstPid ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, on entry transformer and user data, and links it to the current
% process.
%
% Returns the PID of the instance created by this loading.
%
% This creation will be asynchronous: this function returns a legit PID as soon
% as the creation is triggered, without waiting for it to complete.
%
-spec load_link( serialisation(), maybe( entry_transformer() ), user_data() ) ->
								static_return( instance_pid() ).
load_link( Serialisation, MaybeEntryTransformer, UserData ) ->

	InstPid = ?myriad_spawn_link(
		fun() ->
			deserialise( Serialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=undefined )
		end ),

	wooper:return_static( InstPid ).




% Synchronous loading subsection.


% @doc Spawns an instance of this class, based on the specified serialisation
% information, using no specific entry transformer or user data.
%
% Returns information regarding this instance loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec synchronous_load( serialisation() ) -> static_return( load_info() ).
synchronous_load( Serialisation ) ->

	LoadInfo = synchronous_load( Serialisation,
		_MaybeEntryTransformer=undefined, _UserData=undefined ),

	wooper:return_static( LoadInfo ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, on any entry transformer and user data.
%
% Returns information regarding this instance loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec synchronous_load( serialisation(), maybe( entry_transformer() ),
						user_data() ) -> static_return( load_info() ).
synchronous_load( Serialisation, MaybeEntryTransformer, UserData ) ->

	CreatorPid = self(),

	_SpawnedPid = ?myriad_spawn(
		fun() ->
			deserialise( Serialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=CreatorPid )
		end ),

	% Blocks until the spawned process answers:
	receive

		{ onDeserialisation, LoadInfo } ->
			wooper:return_static( LoadInfo )

	end.



% @doc Spawns an instance of this class, using no specific entry transformer or
% user data, and links it to the current process.
%
% Returns information regarding this instance loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec synchronous_load_link( serialisation() ) -> static_return( load_info() ).
synchronous_load_link( Serialisation ) ->

	LoadInfo = synchronous_load_link( Serialisation,
		_MaybeEntryTransformer=undefined, _UserData=undefined ),

	wooper:return_static( LoadInfo ).



% @doc Spawns an instance of this class, based on the specified serialisation
% information, on any entry transformer and user data, and links it to the
% current process.
%
% Returns information regarding this instance loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec synchronous_load_link( serialisation(), maybe( entry_transformer() ),
							 user_data() ) -> static_return( load_info() ).
synchronous_load_link( Serialisation, MaybeEntryTransformer, UserData ) ->

	CreatorPid = self(),

	_SpawnedPid = ?myriad_spawn_link(
		fun() ->
			deserialise( Serialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=CreatorPid )
		end ),

	% Blocks until the spawned process answers:
	receive

		{ onDeserialisation, LoadInfo } ->
			wooper:return_static( LoadInfo )

	end.



% @doc Spawns on the specified node an instance of this class, based on the
% specified serialisation information, using no specific entry transformer or
% user data, and links it to the current process.
%
% Returns information regarding this instance loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec remote_synchronous_timed_load_link( node_name(), serialisation() ) ->
										static_return( load_info() ).
remote_synchronous_timed_load_link( Node, Serialisation ) ->

	LoadInfo = remote_synchronous_timed_load_link( Node, Serialisation,
		_MaybeEntryTransformer=undefined, _UserData=undefined ),

	wooper:return_static( LoadInfo ).



% @doc Spawns on the specified node an instance of this class, based on the
% specified serialisation information, on entry transformer and user data, and
% links it to the current process.
%
% Returns information regarding this instance loading.
%
% This creation is synchronous: the call will return only when the created
% process reports that it is up and running.
%
-spec remote_synchronous_timed_load_link( node_name(),
	serialisation(), maybe( entry_transformer() ), user_data() ) ->
												static_return( load_info() ).
remote_synchronous_timed_load_link( Node, Serialisation,
									MaybeEntryTransformer, UserData ) ->

	CreatorPid = self(),

	SpawnedPid = ?myriad_spawn_link( Node,
		fun() ->
			deserialise( Serialisation, MaybeEntryTransformer,
						 UserData, _ListenerPid=CreatorPid )
		end ),

	% Blocks until the spawned process answers or a time-out occurs:
	receive

		{ onDeserialisation, LoadInfo } ->
			wooper:return_static( LoadInfo )

	after ?synchronous_time_out ->

		% Classname not known;
		trace_bridge:error_fmt( "(remote_synchronous_timed_load_link: "
			"throwing time-out on node ~ts for ~ts after ~B milliseconds)",
			[ Node, SpawnedPid, ?synchronous_time_out ] ),

		throw( { remote_synchronous_linked_time_out, Node, SpawnedPid } )

	end.


% Insert here any extra *load* function needed.


% Synchronisable loading subsection.


% @doc Spawns on the specified node an instance of this class, based on the
% specified serialisation information, using no specific entry transformer or
% user data, and links it to the current process.
%
% Returns the PID of the created instance; the loading information message will
% be received later by the calling process.
%
% This creation is asynchronous (the PID is directly returned), however a
% {onDeserialisation, LoadInfo} message will be received by the calling process
% once (if ever) the instance is up and running. This allows performing the
% actual instance creations in parallel, by waiting sets of concurrent
% loadings.
%
-spec remote_synchronisable_load_link( node_name(), serialisation() ) ->
											static_return( instance_pid() ).
remote_synchronisable_load_link( Node, Serialisation ) ->

	InstPid = remote_synchronisable_load_link( Node, Serialisation,
		_MaybeEntryTransformer=undefined, _UserData=undefined ),

	wooper:return_static( InstPid ).



% @doc Spawns on the specified node an instance of this class, based on the
% specified serialisation information, on any entry transformer and user data,
% and links it to the current process.
%
% Returns the PID of the created instance; the loading information message will
% be received later by the calling process.
%
% This creation is asynchronous (the PID is directly returned), however a
% {onDeserialisation, LoadInfo} message will be received by the calling process
% once (if ever) the instance is up and running. This allows performing the
% actual instance creations in parallel, by waiting sets of concurrent
% loadings.
%
-spec remote_synchronisable_load_link( node_name(), serialisation(),
			maybe( entry_transformer() ), user_data() ) ->
												static_return( instance_pid() ).
remote_synchronisable_load_link( Node, Serialisation, MaybeEntryTransformer,
								 UserData ) ->

	CreatorPid = self(),

	InstPid = ?myriad_spawn_link( Node,
		fun() ->
			deserialise( Serialisation, MaybeEntryTransformer, UserData,
						 _ListenerPid=CreatorPid )
		end ),

	wooper:return_static( InstPid ).




% @doc Deserialises the specified instance from the specified serialised form to
% obtain its corresponding state, using any specified entry transformer and any
% user data for that, before executing the class-specific, possibly overridden
% onPostDeserialisation/2 request, then having the currently executing, caller
% process embody this instance from then on, and for good.
%
% Does not return, as the WOOPER main loop will manage, from then on, this just
% deserialised instance.
%
% (spawn helper; any listener process may not even be a WOOPER instance)
%
% No pre-deserialisation function parameter (which could be a reciprocal of the
% onPostSerialisation/2 request) is supported here, as any transformation can be
% done on the serialisation term before calling this static method, and a
% pre-deserialisation transformation that would be done here could not be
% class-specific anyway (since the classname is not even known at this point);
% one should rely on the onPostDeserialisation/2 request instead for that. This
% (possibly overridden) method is called *after* any entry transformer, for
% symmetry reasons.
%
% If a listener PID is specified, a {'onDeserialisation', load_info()} message
% will be sent to the corresponding process, as soon as this information becomes
% available (no guarantee that afterwards this instance does not fail, even in
% the final parts of its deserialisation).
%
% Note: the hosting process is not created here, as, for an increased
% parallelism, we expect deserialisations to happen directly from the final
% instance processes; so we consider here that the process executing this helper
% is the final hosting one, regardless of its past.
%
-spec deserialise( serialisation(), maybe( entry_transformer() ), user_data(),
				   maybe( pid() ) ) -> static_no_return().
deserialise( Serialisation, MaybeEntryTransformer, UserData,
			 MaybeListenerPid ) ->

	DeserialisedTerm = deserialise_term( Serialisation ),

	embody( DeserialisedTerm, MaybeEntryTransformer, UserData,
			MaybeListenerPid ),

	wooper:no_return_static() .



% @doc Have the current process embody the instance described by the specified
% deserialised term, expected to be an instance record.
%
% Introduced to be used in multiple contexts, either directly for a single
% deserialisation, or for a series thereof.
%
-spec embody( term(), maybe( entry_transformer() ), user_data(),
					maybe( pid() ) ) -> static_no_return().
embody( #wooper_serialisation_instance_record{
			class_name=Classname,
			attributes=AttrEntries,
			extra_data=MaybeExtraData }, MaybeEntryTransformer, UserData,
		MaybeListenerPid ) ->

	% First we extract the WOOPER extra information (if any) to have it out of
	% the way:
	%
	{ MaybeRandomState, OtherEntries } =
		list_table:extract_entry_with_default( ?random_attr_name,
			_Default=undefined, AttrEntries ),

	{ TransformedEntries, EntryUserData } = case MaybeEntryTransformer of

		undefined ->
			{ AttrEntries, UserData };

		EntryTransformer ->
			lists:foldl( EntryTransformer,
						 _Acc0={ _ResultingEntries=[], UserData },
						 _List=OtherEntries )

	end,

	% Now we have the right attributes enumerated.

	% We need to bypass any constructor here.

	AttributeTable = ?wooper_table_type:add_entries( TransformedEntries,
										?wooper_table_type:new() ),

	% If ever useful:
	OptimisedAttributeTable = ?wooper_table_type:optimise( AttributeTable ),


	VirtualTableKey = wooper:retrieve_virtual_table_key( Classname ),

	VirtualTable = persistent_term:get( VirtualTableKey ),

	ForgedState = #state_holder{ virtual_table=VirtualTable,
								 attribute_table=OptimisedAttributeTable,
								 actual_class=Classname,
								 request_sender=undefined },

	% We could check here that no serialisation marker remains, with a specific
	% entry transformer and list_restoration_markers/0.

	 { FinalState, FinalUserData } =
		executeRequest( ForgedState, onPostDeserialisation,
						[ EntryUserData, MaybeExtraData ] ),


	% Sent as soon as available (rather than at the end):
	MaybeListenerPid =:= undefined orelse
		begin
			LoadInfo = { self(), Classname, FinalUserData },
			MaybeListenerPid ! { onDeserialisation, LoadInfo }
		end,

	% Must be restored as well, and preferably in a symmetrical order:
	MaybeRandomState =:= undefined orelse
		random_utils:set_random_state( MaybeRandomState ),

	% That's as simple as that!

	Classname:wooper_main_loop( FinalState ),

	% Never reached anyway:
	wooper:no_return_static();


embody( OtherDeserialisedTerm, _MaybeEntryTransformer, _UserData,
		_MaybeListenerPid ) ->
	throw( { unexpected_deserialised_term, OtherDeserialisedTerm } ).



% @doc The actual (static) method in charge of deserialising a serialisation
% term.
%
% This cannot be a member method as by construction no target instance exists
% yet.
%
% The serialisation is expected to contain a single full instance (and nothing
% more, no extra content, padding, etc.).
%
% Of course other forms of deserialisation could be introduced at this level,
% yet called as separate static methods/functions (no overriding of static
% methods).
%
% In the case of multi-instance serialisation terms, a variation could return
% the remaining serialisation content (if any).
%
-spec deserialise_term( serialisation() ) -> static_return( term() ).
deserialise_term( Serialisation ) ->

	% The 'safe' option does not seem relevant here:
	ReadTerm = cond_utils:if_defined( wooper_check_deserialisation,
		begin
			BinSize = system_utils:get_size( Serialisation ),
			% No ?serialisation_opts to specify here:
			case binary_to_term( Serialisation, [ used ] ) of

				% Matching:
				{ Term, BinSize } ->
					Term;

				{ Term, OtherSize } ->
					throw( { unmatching_deserialised_size, {full,BinSize},
							 {used,OtherSize}, Term } )

			end

		end,
		% No ?serialisation_opts to specify here:
		binary_to_term( Serialisation, [] ) ),

	wooper:return_static( ReadTerm ).



% No relevant onPreDeserialisation/2 request can exist, as no instance is
% available at this point.



% @doc Hook triggered at the end of the deserialisation step (after the raw
% deserialisation and the application of any entry transformer), operating on
% the resulting instance state, possibly based on any user-specified data.
%
% This potentially class-specific processing is the last possible transformation
% before the final deserialised state is used by the instance to live
% autonomously from then on.
%
% This is a default, const, do-nothing implementation (returning an exact copy
% of the current state), meant to be overridden if needed.
%
-spec onPostDeserialisation( wooper:state(), user_data() ) ->
			request_return( user_data() ).
onPostDeserialisation( State, UserData ) ->

	% Intentionally non-const:
	{ PostState, PostUserData } = executeRequest( State,
		onPostDeserialisation, [ UserData, _MaybeExtraData=undefined ] ),

	wooper:return_state_result( PostState, PostUserData ).


% @doc Hook triggered at the end of the deserialisation step (after the raw
% deserialisation and the application of any entry transformer), operating on
% the resulting instance state, possibly based on any user-specified data and
% persisted extra data.
%
% Refer to onPostDeserialisation/2 for further details.
%
% This is a default, const, do-nothing implementation (returning an exact copy
% of the current state), meant to be overridden if needed.
%
-spec onPostDeserialisation( wooper:state(), user_data(),
			maybe( extra_data() ) ) -> const_request_return( user_data() ).
onPostDeserialisation( State, UserData, _MaybeExtraData ) ->

	cond_utils:if_defined( wooper_debug_serialisation,
		trace_bridge:debug_fmt( "Default post-deserialisation of instance ~w, "
			"based on user data ~p and extra data ~p.",
			[ self(), UserData, MaybeExtraData ] ) ),

	wooper:const_return_result( UserData ).



% Static section.


% To obtain restoration markers without needing a header file:


% @doc Returns a list of the WOOPER built-in restoration markers.
-spec list_restoration_markers() -> static_return( [ restoration_marker() ] ).
list_restoration_markers() ->
	wooper:return_static( [ ?process_restoration_marker,
		?file_restoration_marker, ?term_restoration_marker ] ).


% @doc Returns the restoration marker for internal, local processes that must
% escape the serialisation/deserialisation processes.
%
-spec get_process_restoration_marker() -> static_return( restoration_marker() ).
get_process_restoration_marker() ->
	wooper:return_static( ?process_restoration_marker ).


% @doc Returns the restoration marker for internal, local open files (akin to
% file descriptors) that must escape the serialisation/deserialisation
% processes.
%
-spec get_file_restoration_marker() -> static_return( restoration_marker() ).
get_file_restoration_marker() ->
	wooper:return_static( ?file_restoration_marker ).


% @doc Returns the restoration marker for internal, local terms that must escape
% the serialisation/deserialisation processes (e.g. typically because they are
% large and may be recreated afterwards).
%
-spec get_term_restoration_marker() -> static_return( restoration_marker() ).
get_term_restoration_marker() ->
	wooper:return_static( ?term_restoration_marker ).




% Section for helper functions (not methods).



% @doc Tells whether the corresponding instance implements the Serialisable
% interface.
%
% (exported helper)
%
-spec is_serialisable( wooper:state() ) -> boolean().
is_serialisable( State ) ->
	% We cannot rely on a specific attribute being defined or not to determine
	% whether Serialisable, so:
	%
	lists:member( ?MODULE, wooper:get_all_superclasses( State ) ).
