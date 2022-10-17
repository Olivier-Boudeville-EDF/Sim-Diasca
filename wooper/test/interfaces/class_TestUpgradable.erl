% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Tuesday, August 16, 2022.


% @doc Test class for the implementation of the Upgradable trait.
%
% It does not include a version in itself: it is defined through the
% command-line, as with this single test source it can be either 1.2.3 or 1.2.4,
% depending on whether enable_upgraded_test_class has been defined (then 1.2.4)
% or not (1.2.3).
%
-module(class_TestUpgradable).


-define( class_description,
		 "Test class for the implementation of the Upgradable trait." ).


% Emitting traces in useful, but cannot be class_Traceable as we are at the
% level of WOOPER:
%
-define( superclasses, [ class_Upgradable, class_Describable ] ).


% All Describable classes must define and export such an helper function:
-export([ to_string/1 ]).



-ifndef(enable_upgraded_test_class).


% Version 1.2.3:

-define( this_class_version, { 1, 2, 3 } ).

-define( class_attributes, [

	{ name, ustring(), "name of this test instance" },
	{ height, float(), "height of this test instance" }

						   ] ).


-else.


% Version 1.2.4: name kept, height removed, age added.

-define( this_class_version, { 1, 2, 4 } ).

-define( class_attributes, [

	{ name, ustring(), "name of this test instance" },
	{ age, integer(), "age of this test instance" }

						   ] ).

-endif.




% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").




% Shorthands:

-type ustring() :: text_utils:ustring().

-type any_version() :: basic_utils:any_version().
-type base_outcome() :: basic_utils:base_outcome().

-type extra_data() :: class_Upgradable:extra_data().



% @doc Constructs a test upgradable instance.
%
% The corresponding version is determined statically.

-ifndef(enable_upgraded_test_class).

% Version 1.2.3:

-spec construct( wooper:state(), ustring(), float() ) -> wooper:state().
construct( State, Name, Height ) ->
	trace_utils:debug( "(constructing a class_TestUpgradable version 1.2.3)" ),
	% Even though both are do-nothing:
	UpState = class_Upgradable:construct( State ),
	DescState = class_Describable:construct( UpState ),
	setAttributes( DescState, [
		{ name, Name },
		{ height, Height } ] ).

-else.

% Version 1.2.4:

-spec construct( wooper:state(), ustring(), integer() ) -> wooper:state().
construct( State, Name, Age ) ->
	trace_utils:debug( "(constructing a class_TestUpgradable version 1.2.4)" ),
	% Even though both are do-nothing:
	UpState = class_Upgradable:construct( State ),
	DescState = class_Describable:construct( UpState ),
	setAttributes( DescState, [
		{ name, Name },
		{ age, Age } ] ).

-endif.


% No destructor.



% Methods section.


% @doc Upgrades this instance (thus to a more recent version) both in terms of
% code and state, taking into account any specified extra data.
%
-spec upgradeVersion( wooper:state(), any_version(), any_version(),
			maybe( extra_data() ) ) -> request_return( base_outcome() ).
% Only these very specific settings are supported:
upgradeVersion( State, OriginalVersion={1,2,3}, TargetVersion={1,2,4},
				ExtraData=upgradable_test ) ->

	Height = ?getAttr(height),

	false = hasAttribute( State, age ),

	% Of course arbitrary:
	Age = round( Height * 50 ),

	NewName = ?getAttr(name) ++ ", then upgraded",


	trace_bridge:debug_fmt( "Upgrading from version ~ts to ~ts, "
		"using extra data '~p'; dropping height ~p, introducing age ~p, "
		"updating name to '~ts'.",
		[ text_utils:version_to_string( OriginalVersion ),
		  text_utils:version_to_string( TargetVersion ), ExtraData,
		  Height, Age, NewName ] ),

	case get_version() of

		% Expected to be already at target:
		TargetVersion ->
			ok;

		OtherVersion ->
			throw( { invalid_version_on_upgrade, { read, OtherVersion },
					 { original, OriginalVersion },
					 { target, TargetVersion },
					 State#state_holder.actual_class, self() } )

	end,

	UpdatedState = setAttributes( State, [
		{ name, NewName },
		{ age, Age } ] ),

	UpgradedState = removeAttribute( UpdatedState, height ),

	wooper:return_state_result( UpgradedState, ok ).



% @doc Downgrades this instance (thus to a less recent version) both in terms of
% code and state, taking into account any specified extra data.
%
-spec downgradeVersion( wooper:state(), any_version(), any_version(),
			maybe( extra_data() ) ) -> request_return( base_outcome() ).
downgradeVersion( State, OriginalVersion={1,2,4}, TargetVersion={1,2,3},
				  ExtraData=upgradable_test ) ->

	Age = ?getAttr(age),

	false = hasAttribute( State, height ),

	% Possibly with rounding errors:
	Height = float( Age / 50 ),

	NewName = ?getAttr(name) ++ ", then downgraded",


	trace_bridge:debug_fmt( "Downgrading from version ~ts to ~ts, "
		"using extra data '~p'; dropping age ~p, re-introducing height "
		"with ~p, updating name to '~ts'.",
		[ text_utils:version_to_string( OriginalVersion ),
		  text_utils:version_to_string( TargetVersion ), ExtraData,
		  Age, Height, NewName ] ),

	case get_version() of

		% Expected to be already at target:
		TargetVersion ->
			ok;

		OtherVersion ->
			throw( { invalid_version_on_upgrade, { read, OtherVersion },
					 { original, OriginalVersion },
					 { target, TargetVersion },
					 State#state_holder.actual_class, self() } )

	end,

	UpdatedState = setAttributes( State, [
		{ name, NewName },
		{ height, Height } ] ),

	DowngradedState = removeAttribute( UpdatedState, age ),

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




% @doc Returns a textual description of this instance.
-spec to_string( wooper:state() ) -> ustring().

-ifndef(enable_upgraded_test_class).

to_string( State ) ->
	text_utils:format( "test upgradable instance ~w of class ~ts version ~ts,"
		" of name '~ts' and height ~p m",
		[ self(), State#state_holder.actual_class,
		  text_utils:version_to_string( ?this_class_version ),
		  ?getAttr(name), ?getAttr(height) ] ).

-else.

to_string( State ) ->
	text_utils:format( "test upgradable instance ~w of class ~ts version ~ts,"
		" of name '~ts' and age ~p years",
		[ self(), State#state_holder.actual_class,
		  text_utils:version_to_string( ?this_class_version ),
		  ?getAttr(name), ?getAttr(age) ] ).

-endif.
