% Copyright (C) 2016-2021 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Robin Huart (robin-externe.huart@edf.fr)



% Module storing all the helper functions defined to facilitate the definition
% and use of the binding APIs, for all the supported implementation languages
% (nothing is expected to be language-specific in this module, in the sense that
% if a language is involved somewhere, others should also be, aside).
%
-module(dataflow_binding_utils).


% For agent_pid() in next include:
-include("engine_common_defines.hrl").

% For types and names related to dataflows:
-include("dataflow_defines.hrl").

% For types and names related to language bindings:
-include("bindings.hrl").


% Exports of helpers:
-export([ get_all_erlang_unit_types/0, get_erlang_unit_type/1,
		  get_unit_types/1 ]).


% For trace notifications:
-include_lib("traces/include/traces.hrl").




% Returns the default unit types (classes) that are the Erlang-based
% counterparts to all units implemented in a foreign programming language.
%
-spec get_all_erlang_unit_types() -> [ dataflow_unit_type() ].
get_all_erlang_unit_types() ->
	[ dataflow_python_binding_utils:get_erlang_unit_type(),
	  dataflow_java_binding_utils:get_erlang_unit_type() ].



% Returns the default Erlang unit type associated with specified programming
% language.
%
-spec get_erlang_unit_type( language_utils:language() ) -> dataflow_unit_type().
get_erlang_unit_type( python ) ->
	dataflow_python_binding_utils:get_erlang_unit_type();

get_erlang_unit_type( java ) ->
	dataflow_java_binding_utils:get_erlang_unit_type();

get_erlang_unit_type( Other ) ->
	throw( { unsupported_language_for_unit, Other } ).



% Returns the types (Erlang classnames) of units listed in the specified unit
% specifications.
%
% Note: checks also that their declared implementation language (if any is
% specified) is supported.
%
-spec get_unit_types( [ class_DataflowUnitManager:managed_unit_spec() ] ) ->
							[ dataflow_unit_type() ].
get_unit_types( UnitSpecs ) ->
	SupportedLanguages = language_utils:get_supported_languages(),
	get_unit_types( UnitSpecs, SupportedLanguages, _Acc=[] ).

get_unit_types( _UnitSpecs=[], _SupportedLanguages, Acc ) ->
	lists:reverse( Acc );

get_unit_types( _UnitSpecs =[ { UnitType, ImplementationLanguage } | T ],
				SupportedLanguages, Acc ) ->

	case lists:member( ImplementationLanguage, SupportedLanguages ) of

		true ->
			get_unit_types( T, SupportedLanguages, [ UnitType | Acc ] );

		false ->
			?notify_error_fmt( "The '~p' language (declared by the unit '~ts') "
				"is not supported by the binding API.",
				[ ImplementationLanguage, UnitType ] ),
			throw( { binding_language_not_supported, ImplementationLanguage,
					 UnitType } )

	end;

% Erlang is of course an (auto-accepted) default language:
get_unit_types( _UnitSpecs =[ UnitType | T ], SupportedLanguages, Acc ) ->
	get_unit_types( [ { UnitType, erlang } | T ], SupportedLanguages, Acc ).
