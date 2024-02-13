% Copyright (C) 2012-2024 Olivier Boudeville
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
% Creation date: 2012.


% @doc Module containing facilities for the <b>serialisation and deserialisation
% of WOOPER instances</b>.
%
% Offsetting as much as possible code from the counterpart class header in this
% module allows for smaller, cache-friendly BEAMs, and shorter compilation
% times.
%
-module(wooper_serialisation).


-export([ serialise_instances/1,

		  load_instances/1, load_instances/3,
		  load_link_instances/1, load_link_instances/3,

		  synchronous_load_instances/1, synchronous_load_instances/3,
		  synchronous_load_link_instances/1, synchronous_load_link_instances/3

		]).



% Serialisation helpers, to better implement hooks:
-export([ handle_private_processes/2, mute_attributes/2,
		  check_attributes_equal/3, replace_attribute/3, replace_attributes/3,
		  merge_list_for/3, merge_lists_for/3,
		  instance_record_to_string/1 ]).


% Term transformers:
-export([ check_no_transient/2 ]).


% For restoration markers:
-include("class_Serialisable.hrl").

% For wooper_table_type:
-include("wooper_defines_exports.hrl").

% For getAttr/1, and for setAttributes/2 and all being unused:
-include("wooper_state_exports.hrl").

% For getAttribute/2 and al:
-include("wooper_state_functions.hrl").


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



% Shorthands:

-type ustring() :: text_utils:ustring().
-type user_data() :: basic_utils:user_data().
-type term_transformer() :: meta_utils:term_transformer().

-type attribute_name() :: wooper:attribute_name().
-type attribute_value() :: wooper:attribute_value().
-type attribute_entry() :: wooper:attribute_entry().
-type instance_pid() :: wooper:instance_pid().

-type serialisation() :: class_Serialisable:serialisation().
-type instance_record() :: class_Serialisable:instance_record().
-type entry_transformer() :: class_Serialisable:entry_transformer().
-type load_info() :: class_Serialisable:load_info().


% Implementation notes:
%
% The default deserialiser (based on binary_to_term) is used here.
%
% 'Serial' is used here as a shorthand of 'Serialisation', a term designating a
% binary containing (at least) an instance record.



% @doc Serialises the specified instances, using no specific entry transformer
% or user data; returns a unique serialisation term, aggregating the
% corresponding serialisations, in no specific order (as usually it does not
% matter).
%
-spec serialise_instances( [ instance_pid() ] ) -> serialisation().
serialise_instances( InstPids ) ->
	serialise_instances( InstPids, _MaybeEntryTransformer=undefined,
						 _UserData=undefined ).



% @doc Serialises the specified instances, using the specific entry transformer
% and user data (if any); returns a unique serialisation term, aggregating the
% corresponding serialisations, in no specific order (as usually it does not
% matter).
%
% Note that all these serialisations will use exactly the (same) specified user
% data, and that any updated version of it they would return will be ignored.
%
-spec serialise_instances( [ instance_pid() ], maybe( entry_transformer() ),
						   user_data() ) -> serialisation().
serialise_instances( InstPids, MaybeEntryTransformer, UserData ) ->

	% Getting a list of {Serialisation, UpdatedUserData, InstPid} triplets:
	SerialTriplets = wooper:obtain_results_for_requests( serialise,
		[ MaybeEntryTransformer, UserData ], InstPids ),

	%trace_utils:debug_fmt( "Serialisation of the ~B instances of PIDs ~w "
	%   "returned:~n ~p", [ length( InstPids ), InstPids, SerialTriplets ] ),

	Serials = [ S || { S, _UpdatedUserData, _InstPid } <- SerialTriplets ],

	% So the concatenation of (serialisation, binary) terms, not an overall
	% term:
	%
	text_utils:bin_concatenate( Serials ).



% Section for asynchronous loading.


% @doc Loads the instances stored in the specified serialisation, using no
% specific entry transformer or user data: spawns them and returns a list of
% their corresponding PIDs, in no specific order (as usually it does not
% matter).
%
% These creations will be asynchronous: this function returns legit PIDs as soon
% as the creations are triggered, without waiting for them to complete.
%
% Created instances will not be linked to the calling process.
%
-spec load_instances( serialisation() ) -> [ instance_pid() ].
load_instances( Serialisation ) ->
	load_instances( Serialisation, _MaybeEntryTransformer=undefined,
					_UserData=undefined ).


% @doc Loads the instances stored in the specified serialisation, using the
% specified entry transformer and user data: spawns them and returns a list of
% their corresponding PIDs, in no specific order (as usually it does not
% matter).
%
% These creations will be asynchronous: this function returns legit PIDs as soon
% as the creations are triggered, without waiting for them to complete.
%
% Note that all these deserialisations will use the specified user data, and
% that any updated version of it they would return will be ignored.
%
% Created instances will not be linked to the calling process.
%
-spec load_instances( serialisation(), maybe( entry_transformer() ),
					  user_data() ) -> [ instance_pid() ].
load_instances( Serialisation, MaybeEntryTransformer, UserData ) ->

	% The reading of the serialisation is sequential by nature (this overall
	% binary must be chopped chunk by chunk), yet the creations deriving from it
	% may be concurrent:
	%
	% (clearer than a fold)
	%
	load_instances_helper( Serialisation, MaybeEntryTransformer, UserData,
						   _DoLink=false, _AccPids=[] ).



% @doc Loads the instances stored in the specified serialisation, using no
% specific entry transformer or user data: spawns them and returns a list of
% their corresponding PIDs, in no specific order (as usually it does not
% matter).
%
% These creations will be asynchronous: this function returns legit PIDs as soon
% as the creations are triggered, without waiting for them to complete.
%
% Created instances will be linked to the calling process.
%
-spec load_link_instances( serialisation() ) -> [ instance_pid() ].
load_link_instances( Serialisation ) ->
	load_instances( Serialisation, _MaybeEntryTransformer=undefined,
					_UserData=undefined ).


% @doc Loads the instances stored in the specified serialisation, using the
% specified entry transformer and user data: spawns them and returns a list of
% their corresponding PIDs, in no specific order (as usually it does not
% matter).
%
% These creations will be asynchronous: this function returns legit PIDs as soon
% as the creations are triggered, without waiting for them to complete.
%
% Note that all these deserialisations will use the specified user data, and
% that any updated version of it they would return will be ignored.
%
% Created instances will be linked to the calling process.
%
-spec load_link_instances( serialisation(), maybe( entry_transformer() ),
					  user_data() ) -> [ instance_pid() ].
load_link_instances( Serialisation, MaybeEntryTransformer, UserData ) ->

	% The reading of the serialisation is sequential by nature (this overall
	% binary must be chopped chunk by chunk), yet the creations deriving from it
	% may be concurrent:
	%
	% (clearer than a fold)
	%
	load_instances_helper( Serialisation, MaybeEntryTransformer, UserData,
						   _DoLink=true, _AccPids=[] ).



% (helper)
load_instances_helper( _Serial= <<>>, _MaybeEntryTransformer, _UserData,
					   _DoLink, AccPids ) ->

	%trace_utils:debug_fmt( "Returning ~B instance PIDs: ~w",
	%                       [ length( AccPids ), AccPids ] ),

	AccPids;

load_instances_helper( Serial, MaybeEntryTransformer, UserData, DoLink,
					   AccPids ) ->

	% DeserialisedTerm is expected to be an instance record:
	{ DeserialisedTerm, UsedCount } = binary_to_term( Serial, [ used ] ),

	% Go concurrent ASAP:

	EmbodyFun = fun() ->
		class_Serialisable:embody( DeserialisedTerm, MaybeEntryTransformer,
								   UserData, _ListenerPid=undefined )
				end,

	InstPid = case DoLink of

		true ->
			?myriad_spawn_link( EmbodyFun );

		false ->
			?myriad_spawn( EmbodyFun )

	end,

	{ _ReadSerial, RemainingSerial } = split_binary( Serial, UsedCount ),

	load_instances_helper( RemainingSerial, MaybeEntryTransformer, UserData,
						   DoLink, [ InstPid | AccPids ] ).




% Section for synchronous loading.



% @doc Loads synchronously the instances stored in the specified serialisation,
% using no specific entry transformer or user data: spawns them and returns a
% list of their corresponding loading information, in no specific order (as
% usually it does not matter).
%
% These creations will be synchronous: this function returns only when all
% created processes report that they are up and running.
%
% Created instances will not be linked to the calling process.
%
-spec synchronous_load_instances( serialisation() ) -> [ load_info() ].
synchronous_load_instances( Serialisation ) ->
	synchronous_load_instances( Serialisation, _MaybeEntryTransformer=undefined,
								_UserData=undefined ).



% @doc Loads synchronously the instances stored in the specified serialisation,
% using the specified entry transformer and user data: spawns them and returns a
% list of their corresponding loading information, in no specific order (as
% usually it does not matter).
%
% These creations will be synchronous: this function returns only when all
% created processes report that they are up and running.
%
%
% Created instances will not be linked to the calling process.
%
-spec synchronous_load_instances( serialisation(), maybe( entry_transformer() ),
								  user_data() ) -> [ load_info() ].
synchronous_load_instances( Serialisation, MaybeEntryTransformer, UserData ) ->

	% The reading of the serialisation is sequential by nature (this overall
	% binary must be chopped chunk by chunk), yet the creations deriving from it
	% may be concurrent:
	%
	% (clearer than a fold)
	%
	sync_load_instances_helper( Serialisation, MaybeEntryTransformer, UserData,
								_DoLink=false, self(), _AccPids=[] ).



% @doc Loads synchronously the instances stored in the specified serialisation,
% using no specific entry transformer or user data: spawns them and returns a
% list of their corresponding loading information, in no specific order (as
% usually it does not matter).
%
% These creations will be synchronous: this function returns only when all
% created processes report that they are up and running.
%
% Created instances will be linked to the calling process.
%
-spec synchronous_load_link_instances( serialisation() ) -> [ load_info() ].
synchronous_load_link_instances( Serialisation ) ->
	synchronous_load_link_instances( Serialisation,
		_MaybeEntryTransformer=undefined, _UserData=undefined ).



% @doc Loads synchronously the instances stored in the specified serialisation,
% using the specified entry transformer and user data: spawns them and returns a
% list of their corresponding loading information, in no specific order (as
% usually it does not matter).
%
% These creations will be synchronous: this function returns only when all
% created processes report that they are up and running.
%
% Created instances will be linked to the calling process.
%
-spec synchronous_load_link_instances( serialisation(),
			maybe( entry_transformer() ), user_data() ) -> [ load_info() ].
synchronous_load_link_instances( Serialisation, MaybeEntryTransformer,
								 UserData ) ->

	% The reading of the serialisation is sequential by nature (this overall
	% binary must be chopped chunk by chunk), yet the creations deriving from it
	% may be concurrent:
	%
	% (clearer than a fold)
	%
	sync_load_instances_helper( Serialisation, MaybeEntryTransformer,
		UserData, _DoLink=true, self(), _AccPids=[] ).




% (helper)
sync_load_instances_helper( _Serial= <<>>, _MaybeEntryTransformer, _UserData,
							_DoLink, _Self, AccPids ) ->

	LoadInfos = [ receive
					{ onDeserialisation, LoadInfo } ->
						  LoadInfo
				  end || _Pid <- AccPids ],

	trace_utils:debug_fmt( "Returning ~B loading information: ~w",
						   [ length( LoadInfos ), LoadInfos ] ),

	LoadInfos;

sync_load_instances_helper( Serial, MaybeEntryTransformer, UserData, DoLink,
							Self, AccPids ) ->

	% DeserialisedTerm is expected to be an instance record:
	{ DeserialisedTerm, UsedCount } = binary_to_term( Serial, [ used ] ),

	% Go concurrent ASAP:

	EmbodyFun = fun() ->
		class_Serialisable:embody( DeserialisedTerm, MaybeEntryTransformer,
								   UserData, _ListenerPid=Self )
				end,

	InstPid = case DoLink of

		true ->
			?myriad_spawn_link( EmbodyFun );

		false ->
			?myriad_spawn( EmbodyFun )

	end,

	{ _ReadSerial, RemainingSerial } = split_binary( Serial, UsedCount ),

	sync_load_instances_helper( RemainingSerial, MaybeEntryTransformer,
								UserData, DoLink, Self, [ InstPid | AccPids ] ).




% Serialisation helpers.


% @doc Replaces any PID value associated to any of the specified attribute names
% with by 'undefined' atom.
%
% Handles private processes (through the name of the specified attributes),
% which are processes that are internal to an instance that is to be serialised,
% so that any next serialisation will see instead of their (former) PID a
% serialisation marker.
%
% Returns an updated state.
%
% Typically used by any onPreSerialisation/2 overridden request, to avoid that
% PIDs remain when serialising.
%
% (helper)
%
-spec handle_private_processes( [ attribute_name() ], wooper:state() ) ->
										wooper:state().
handle_private_processes( PrivateAttributeNames, State ) ->

	lists:foldl(
		fun( PrivateAttrName, AccState ) ->

			NewValue = case getAttribute( AccState, PrivateAttrName ) of

				undefined ->
					undefined;

				Pid when is_pid( Pid ) ->

					% We just hide these PIDs on the serialised form: after
					% serialisation, the live state will still reference them.
					%
					?process_restoration_marker

			end,

			setAttribute( AccState, PrivateAttrName, NewValue )

		end,
		_Acc0=State,
		_List=PrivateAttributeNames ).



% @doc Mutes the specified attributes, that is, replaces any attribute value not
% equal to 'undefined' by a term restoration marker, so that they can escape the
% serialisation process.
%
% Typically used by any onPreSerialisation/2 overridden request, to serialise
% only relevant, useful information.
%
% (helper)
%
-spec mute_attributes( [ attribute_name() ], wooper:state() ) -> wooper:state().
mute_attributes( AttributeNames, State ) ->

	lists:foldl(
		fun( AttrName, AccState ) ->

			case hasAttribute( AccState, AttrName ) of

				true ->
					case getAttribute( AccState, AttrName ) of

						undefined ->
							% Let it as is:
							AccState;

						_ ->
							setAttribute( AccState, AttrName,
										  ?term_restoration_marker )

					end;

				false ->
					throw( { unknown_attribute, AttrName, AccState } )

			end

		end,
		_Acc0=State,
		_List=AttributeNames ).



% @doc Checks that the specified attributes have the same value in the specified
% state and in the specified entries, otherwise throws an exception.
%
% (helper)
%
-spec check_attributes_equal( [ attribute_name() ], [ attribute_entry() ],
							  wooper:state() ) -> void().
check_attributes_equal( _AttributeNames=[], _AttributeEntries, _State ) ->
	ok;

check_attributes_equal( _AttributeNames=[ AttributeName | T ], AttributeEntries,
						State ) ->

	{ AttributeValue, RemainingEntries } =
		option_list:extract( _K=AttributeName, AttributeEntries ),

	case ?getAttr(AttributeName) of

		% Matching as expected:
		AttributeValue ->
			check_attributes_equal( T, RemainingEntries, State );

		OtherValue ->
			throw( { attribute_value_mismatch, AttributeName,
					 { OtherValue, AttributeValue } } )

	end.



% @doc Replaces the value held in the specified state by the one of the
% specified attribute found among the specified entries.
%
% Returns the remaining entries and a corresponding updated state.
%
% (helper)
%
-spec replace_attribute( attribute_name(), [ attribute_entry() ],
				wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
replace_attribute( AttributeName, AttributeEntries, State ) ->

	case hasAttribute( State, AttributeName ) of

		true ->
			{ ToSetValue, RemainingEntries } =
				option_list:extract( _K=AttributeName, AttributeEntries ),

			NewState = setAttribute( State, AttributeName, ToSetValue ),

			{ RemainingEntries, NewState };

		false ->
			throw( { unknown_attribute, AttributeName, State } )

	end.



% @doc Replaces the values held in the specified state by the ones of the
% specified attributes found among the specified entries.
%
% Returns the remaining entries and a corresponding updated state.
%
% (helper)
%
-spec replace_attributes( [ attribute_name() ], [ attribute_entry() ],
				wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
replace_attributes( AttributeNames, AttributeEntries, State ) ->

	lists:foldl(
		fun( AttrName, { AccEntries, AccState } ) ->
			replace_attribute( AttrName, AccEntries, AccState )
		end,
		_Acc0={ AttributeEntries, State },
		_List=AttributeNames ).



% @doc Extracts the value (supposedly, any type of list, or a set) of the
% specified attribute from the specified entries, and appends (combines) that
% value to the corresponding one found in the specified state, stored under the
% same attribute name.
%
% Returns the remaining entries, and an updated state.
%
-spec merge_list_for( attribute_name(), [ attribute_entry() ],
			wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
merge_list_for( AttributeName, AttributeEntries, State ) ->

	{ ToMergeValue, RemainingEntries } =
		option_list:extract( _K=AttributeName, AttributeEntries ),

	InitialValue = ?getAttr(AttributeName),

	MergedValue = case ToMergeValue of

		PlainList when is_list( PlainList ) ->
			InitialValue ++ PlainList;

		% We suppose it is a set (that cannot match a list):
		Set ->
			set_utils:union( InitialValue, Set )

	end,

	MergedState = setAttribute( State, AttributeName, MergedValue ),

	{ RemainingEntries, MergedState }.



% @doc Extracts the value (supposedly, any type of list, or a set) of each of
% the specified attributes from the specified entries, and appends (combines)
% that value to the corresponding one found in the specified state, stored under
% the same attribute name.
%
% Returns the remaining entries, and an updated state.
%
-spec merge_lists_for( [ attribute_name() ], [ attribute_entry() ],
			wooper:state() ) -> { [ attribute_entry() ], wooper:state() }.
merge_lists_for( AttributeNames, AttributeEntries, State ) ->

	lists:foldl(
		fun( AttrName, { AccEntries, AccState } ) ->
			merge_list_for( AttrName, AccEntries, AccState )
		end,
		_Acc0={ AttributeEntries, State },
		_List=AttributeNames ).



% @doc Returns a textual description of the specified instance record.
-spec instance_record_to_string( instance_record() ) -> ustring().
instance_record_to_string( #wooper_serialisation_instance_record{
		class_name=Classname,
		attributes=AttrEntries,
		extra_data=MaybeExtraData } ) ->

	AttrStrs = [ text_utils:format( "attribute '~ts' of value ~p",
					[ K, V ] ) || { K, V } <- AttrEntries ],

	ExtraStr = case MaybeExtraData of

		undefined ->
			"no extra data";

		ExtraData ->
			text_utils:format( "extra data ~p", [ ExtraData ] )

	end,

	text_utils:format( "serialisation record for an instance of ~ts, "
		"with ~ts and ~B entries: ~ts",
		[ Classname, ExtraStr, length( AttrEntries ),
		  text_utils:strings_to_string( lists:sort( AttrStrs ) ) ] ).



% Term transformers.


% @doc This is a term transformer (see meta_utils:term_transformer()) in charge
% of checking that entries do not contain transient terms such as PIDs,
% references, etc.
%
% Typically to be called from class_Serialisable:onPostSerialisation/3 to ensure
% that the entries to serialise are legit.
%
-spec check_no_transient( term(), user_data() ) -> term_transformer().
check_no_transient( T, UserData ) ->
	case type_utils:is_transient( T ) of

		true ->
			throw( { transient_term_found, T, type_utils:get_type_of( T ),
					 UserData } );

		false ->
			{ T, UserData }

	end.



% check_no_restoration_markers/2
