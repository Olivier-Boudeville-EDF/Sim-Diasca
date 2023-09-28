% Copyright (C) 2008-2023 Olivier Boudeville
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
% Creation date: 2008.


% @doc Interface class implementing the Describable trait, so that instances
% supporting that trait are able to output a <b>textual description</b> of them.
%
% Each Describable child class *must* define an exported helper with the
% following signature, as it will be relied upon:
%  -spec to_string(wooper:state()) -> text_utils:ustring().
%
% This interface provides also exported functions designed so that they can be
% applied to any WOOPER instance, whether or not it supports this trait.
%
% See class_StaticDescribable, for instances that can be described statically.
% -module(class_Describable).
%
-module(class_Describable).


-define( class_description,
		 "Interface implementing the Describable trait, for all instances able "
		 "to output their textual description." ).


% No superclasses.


% No interface-specific attribute to declare.


-type user_description() :: ustring().
% A user-provided description of interest.

-type description() :: bin_string().
% The internal description of interest.

-type any_description() :: any_string().
% A description of interest, as any string.


-type describable_pid() :: pid().
% The PID of an instance implementing the Describable interface.


-export_type([ user_description/0, description/0, any_description/0,
			   describable_pid/0 ]).


% All Describable classes must define and export such an helper function:
-export([ to_string/1 ]).


% Exported helper functions that can be applied to any WOOPER state:
-export([ is_describable/1, get_maybe_description/1, to_maybe_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Implementation notes:
%
% We force a to_string/1 helper to be defined and exported, instead of a mere
% getDescription/1 method, as this helper is generally of much use when
% implementing the class itself, typically for its traces.


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().



% @doc Constructs a describable instance.
-spec construct( wooper:state() ) -> wooper:state().
construct( State ) ->
	State.


% No destructor.



% Methods section.


% @doc Returns the description of this Describable.
-spec getDescription( wooper:state() ) -> const_request_return( description() ).
getDescription( State=#state_holder{ actual_class=Classname } ) ->

	% This is a default implementation that may be kept as is:
	BinDesc = text_utils:string_to_binary( Classname:to_string( State ) ),

	wooper:const_return_result( BinDesc ).




% Section for helper functions (not methods).


% @doc Returns a textual description of this instance.
%
% (exported helper, meant to be defined per child class of class_Describable)
%
-spec to_string( wooper:state() ) -> ustring().
to_string( _State ) ->
	"Describable instance".



% The following helper functions can be used in the context of any class,
% whether or not it implements this Describable interface.


% @doc Tells whether the corresponding instance implements the Describable
% interface.
%
% (exported helper)
%
-spec is_describable( wooper:state() ) -> boolean().
is_describable( State ) ->
	% We cannot rely on a specific attribute being defined or not to determine
	% whether Describable, so:
	%
	lists:member( ?MODULE, wooper:get_all_superclasses( State ) ).



% @doc Returns any description available for the corresponding instance.
%
% This function is designed to apply to any WOOPER instance, whether it is a
% Describable one or not.
%
% (exported helper)
%
-spec get_maybe_description( wooper:state() ) -> maybe( description() ).
get_maybe_description( State ) ->
	case is_describable( State ) of

		true ->
			executeConstRequest( State, getDescription );

		false ->
			undefined

	end.



% @doc Returns a textual element of description of the corresponding instance,
% should it implement the Describable interface.
%
% (exported helper)
%
-spec to_maybe_string( wooper:state() ) -> maybe( ustring() ).
to_maybe_string( State ) ->
	case get_maybe_description( State ) of

		undefined ->
			undefined;

		Desc ->
			text_utils:format( "whose description is: '~ts'", [ Desc ] )

	end.
