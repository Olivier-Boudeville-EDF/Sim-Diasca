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
% Creation date: Sunday, July 24, 2022.


% @doc Interface class implementing the Identifiable trait, so that its
% instances are able to return an <b>identifier</b> thereof, obviously unique.
%
% Such an interface is an abstract mother class from which all identifiable
% instances must derive.
%
% It provides also exported functions designed so that they can be
% applied to any WOOPER instance, whether or not it has this trait or not.
%
-module(class_Identifiable).


-define( class_description,
		 "Interface to be implemented by all instances able to return "
		 "an identifier thereof." ).


% No superclasses.

% Declaration of the interface-specific attributes:
%
% (as it is a WOOPER builtin, they are all prefixed with 'wooper' and the
% interface name)
%
-define( class_attributes, [

	{ wooper_identifiable_id, id(), "the identifier of this instance" }

						   ] ).


-type id() :: id_utils:id().
% Designates the identifiers used by identifiable instances.

-type identifiable_pid() :: pid().
% The PID of an instance implementing the identifiable interface.


-export_type([ id/0, identifiable_pid/0 ]).


% Exported helper functions, usable against any WOOPER instance:
-export([ is_identifiable/1, get_maybe_identifier/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").



% @doc Constructs an identifiable instance, whose identifier is the specified
% one.
%
-spec construct( wooper:state(), id() ) -> wooper:state().
construct( State, Identifier ) ->
	setAttribute( State, wooper_identifiable_id, Identifier ).



% Methods section.


% @doc Returns the identifier of this Identifiable.
-spec getIdentifier( wooper:state() ) -> const_request_return( id() ).
getIdentifier( State ) ->
	wooper:const_return_result( ?getAttr(wooper_identifiable_id) ).



% @doc Sets the identifier of this Identifiable.
-spec setIdentifier( wooper:state(), id() ) -> oneway_return().
setIdentifier( State, Identifier ) ->
	wooper:return_state(
		setAttribute( State, wooper_identifiable_id, Identifier ) ).


% @doc Compares the identifier of this Identifiable with the one of the
% specified Identifiable: returns whether these identifiers are equal.
%
% Note that comparing an identifiable with itself will deadlock it.
%
-spec compareWith( wooper:state(), identifiable_pid()  ) ->
						const_request_return( boolean() ).
compareWith( State, IdentifiablePid ) ->
	IdentifiablePid ! { getIdentifier, [], self() },

	MyId = ?getAttr(wooper_identifiable_id),

	receive

		% Matching:
		{ wooper_result, MyId } ->
			% Somewhat surprising/abnormal:
			wooper:const_return_result( true );

		{ wooper_result, _OtherId } ->
			wooper:const_return_result( false )

	end.



% Static section.


% @doc Tells whether the two specified identifiable instances have the same
% identifier.
%
% If yes, presumably they are expected to be the same, even if their PIDs are
% not directly compared.
%
-spec compare( identifiable_pid(), identifiable_pid() ) ->
							static_return( boolean() ).
compare( FirstIdPid, SecondIdPid ) ->
	Msg = { getIdentifier, [], self() },

	% Special case where interleaving is possible:
	FirstIdPid ! Msg,
	SecondIdPid ! Msg,

	FirstIdReceived = receive

		{ wooper_result, Id } ->
			Id

	end,

	receive

		% Matching:
		{ wooper_result, FirstIdReceived } ->
			wooper:return_static( true );

		{ wooper_result, _SecondIdReceived } ->
			wooper:return_static( false )

	end.




% Section for helper functions (not methods).


% @doc Tells whether the corresponding instance implements the Identifiable
% interface.
%
% (exported helper)
%
-spec is_identifiable( wooper:state() ) -> boolean().
is_identifiable( State ) ->
	hasAttribute( State, wooper_identifiable_id ).


% @doc Returns any identifier available for this instance.
%
% This function is designed to apply to any WOOPER instance, whether it is a
% Identifiable one or not.
%
% (exported helper)
%
-spec get_maybe_identifier( wooper:state() ) -> maybe( id() ).
get_maybe_identifier( State ) ->
	% Allowed, as the type of this attribute does not include the 'undefined'
	% atom:
	%
	?getMaybeAttr(wooper_identifiable_id).
