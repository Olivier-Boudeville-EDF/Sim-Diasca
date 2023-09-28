% Copyright (C) 2007-2023 Olivier Boudeville
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
% Creation date: August 30, 2007.


% @doc Gathering of various Finite State Machine related facilities.
%
% Should rely on gen_statem.
%
% See fsm_utils_test.erl for the corresponding test.
%
% @hidden Not ready.
%
-module(fsm_utils).


-export([ create_blank_fsm_state/0, setFsmAttribute/3, getFsmAttribute/2 ]).

% As these types are not exported by gen_statem:

-type callback_mode() :: 'state_functions' | 'handle_event_function'.

-type state_enter() :: 'state_enter'.

-type callback_mode_ret() ::
		callback_mode() | [ callback_mode() | state_enter() ].


-type state_callback_result( _T) :: term().

-export_type([ callback_mode/0, callback_mode_ret/0,
			   state_callback_result/1 ]).



% @doc Creates an attribute table appropriate to store a FSM state.
%
% setFsmAttribute, getFsmAttribute and getFsmAttr are to be used with these
% variables too.
%
-spec create_blank_fsm_state() -> table:table().
create_blank_fsm_state() ->
	table:new().




% @doc Sets the specified FSM state attribute.
-spec setFsmAttribute( table:table(), table:key(), table:value() ) ->
								table:table().
setFsmAttribute( FsmState, AttributeName, AttributeValue ) ->
	table:add_entry( AttributeName, AttributeValue, FsmState ).



% @doc Retrieves the specified FSM state attribute.
-spec getFsmAttribute( table:table(), table:key() ) ->
				{ 'value', table:value() } | 'attribute_not_found'.
getFsmAttribute( FsmState, AttributeName ) ->

	case table:lookup_entry( AttributeName, FsmState ) of

		key_not_found->
			attribute_not_found ;

		VPair ->
			VPair

	end.
