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



% Module gathering the helper functions introduced to facilitate the support of
% the binding APIs, for all the runtime programming languages supported by the
% engine.
%
-module(binding_utils).



% For all bindings-related types and defines:
-include("bindings.hrl").


% Exports of helpers:
-export([ get_binding_manager/1, get_binding_manager/2,
		  get_binding_manager_class/1, set_binding_managers_record/1,
		  check_implementation_language/2 ]).


% For sending traces:
-include_lib("traces/include/traces.hrl").


% Implementation notes:
%
% Nothing is expected to be language-specific in this module, or if a language
% is involved at some point, all others should be considered too, at the same
% level.



% Returns the binding manager for the specified language.
-spec get_binding_manager( language() ) -> binding_manager_pid().
get_binding_manager( Language ) ->

	% Gets the class corresponding to the binding manager associated with the
	% target language:
	%
	BindingManagerClass = get_binding_manager_class( Language ),

	% Returns the PID of this (uniquely) instantiated and registered manager:
	BindingManagerClass:get_registered_manager().



% Returns the binding manager associated to the specified foreign language,
% based on a binding manager record.
%
-spec get_binding_manager( language(), binding_managers() ) ->
									binding_manager_pid().
get_binding_manager( python, BindingManagers ) ->
	BindingManagers#binding_managers.python_binding_manager;

get_binding_manager( java, BindingManagers ) ->
	BindingManagers#binding_managers.java_binding_manager;

get_binding_manager( UnknownLanguage, _BindingManagers )
  when is_atom( UnknownLanguage ) ->
	throw( { unsupported_binding_language, UnknownLanguage } );

get_binding_manager( InvalidLanguageSpec, _BindingManagers ) ->
	throw( { invalid_language_binding_spec, InvalidLanguageSpec } ).



% Returns the name of the class of the binding manager that is associated to
% specified foreign language.
%
-spec get_binding_manager_class( language() ) -> wooper:classname().
get_binding_manager_class( python ) ->
	class_PythonBindingManager;

get_binding_manager_class( java ) ->
	class_JavaBindingManager;

get_binding_manager_class( UnknownLanguage ) when is_atom( UnknownLanguage ) ->
	throw( { unsupported_binding_language, UnknownLanguage } );

get_binding_manager_class( InvalidLanguageSpec ) ->
	throw( { invalid_language_binding_spec, InvalidLanguageSpec } ).



% Generates the binding manager record, federating all known foreign languages.
-spec set_binding_managers_record(
		[ { language(), binding_manager_pid() } ] ) -> binding_managers().
set_binding_managers_record( [] ) ->
	% All managers default to 'none':
	%trace_utils:debug( "No binding manager declared." ),
	#binding_managers{};

set_binding_managers_record( BindingManagerPairs )
  when is_list( BindingManagerPairs ) ->

	%trace_utils:debug_fmt( "Following binding manager pairs declared: ~p.",
	%					   [ BindingManagerPairs ] ),

	% Also ensures uniqueness:
	BindingsTable = table:new( BindingManagerPairs ),

	% Check that each language was mentioned no more than once:
	LangCount = table:size( BindingsTable ),

	case length( BindingManagerPairs ) of

		LangCount ->
			ok;

		_ ->
			throw( { binding_multiple_declaration, BindingManagerPairs } )

	end,

	PythonBindingManagerPid = case table:lookup_entry( python,
													   BindingsTable ) of

		key_not_found ->
			none;

		{ value, PyManPid } ->
			PyManPid

	end,

	JavaBindingManagerPid = case table:lookup_entry( java, BindingsTable ) of

		key_not_found ->
			none;

		{ value, JaManPid } ->
			JaManPid

	end,

	#binding_managers{ python_binding_manager=PythonBindingManagerPid,
					   java_binding_manager=JavaBindingManagerPid }.



% Checks the availability of the language bindings necessary for instantiation
% specifications which declare an implementation language (the expected specs
% take the form of a 2-tuple {Classname, Language}).
%
% This availability is tested through the presence of a valid PID in a
% binding_managers record.
%
% (helper)
%
-spec check_implementation_language(
		[ wooper:classname() | { wooper:classname(), language() } ],
		binding_managers() ) -> void().
check_implementation_language( _Specs=[], _BindingManagers ) ->
	ok;

check_implementation_language( [ _Spec={ _Class, erlang } | MoreSpecs ],
							   BindingManagers ) ->
	check_implementation_language( MoreSpecs, BindingManagers );

check_implementation_language( [ _Spec={ _Class, Language } | MoreSpecs ],
							   BindingManagers ) ->

	%trace_utils:debug_fmt( "Looking for language '~ts' through ~p.",
	%					   [ Language, BindingManagers ] ),

	case get_binding_manager( Language, BindingManagers ) of

		none ->
			throw( { binding_manager_not_activated_for_language, Language } );

		Pid when is_pid( Pid ) ->
			check_implementation_language( MoreSpecs, BindingManagers );

		NotPid ->
			throw( { binding_manager_is_not_pid, Language, NotPid } )

	end;

% Erlang implied here:
check_implementation_language( [ _Spec=Class | MoreSpecs ], BindingManagers ) ->
	check_implementation_language( [ { Class, erlang } | MoreSpecs ],
								   BindingManagers ).
