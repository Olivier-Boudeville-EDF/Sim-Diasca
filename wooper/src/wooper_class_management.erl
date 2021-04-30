% Copyright (C) 2014-2021 Olivier Boudeville
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
% Creation date: Wednesday, December 24, 2014.


% Centralises, on behalf of the WOOPER parse transform, the support for classes,
% inheritance, etc.
%
-module(wooper_class_management).


-export([ check_classname/1, manage_classname/2, manage_superclasses/1 ]).



% For the function_info record:
-include_lib("myriad/include/ast_info.hrl").

% For the class_info record:
-include("wooper_info.hrl").


% Shorthands:

-type class_info() :: wooper_info:class_info().




% Implementation notes:

% The implementation of getClassname/1 might be created here rather than being
% defined in the wooper_classes_functions.hrl header file.


% Regarding WOOPER superclasses.
%
% They used to be defined with:
% '-define(wooper_superclasses, [class_A, class_B]).'.
%
% Now they are either defined with the optional:
%
% '-superclasses([class_A, class_B]).
%
% (which has been finally preferred to '-wooper_superclasses([]).').
%
% or with the alternative (optional as well):
% '-define(superclasses, [class_A, class_B]).'
%
% This latter form is less interesting than the former, yet it allows to follow
% the same convention as '-define( class_attributes, [...])', which is more
% constrained (due to the parentheses involved in a type declaration, it cannot
% be a wild attribute like -attributes or -class_attributes).
%
% So either method is supported (of course up to only one of them should be,
% knowing that, if -superclasses is used, no -define will be searched for), and
% the -define method is the recommended one, for homogeneity reasons.


% Ensures that specified name is a legit class name, and returns it.
-spec check_classname( any() ) -> atom().
check_classname( Name ) when is_atom( Name ) ->

	case text_utils:atom_to_string( Name ) of

		"class_" ++ _ ->
			Name;

		InvalidName ->
			wooper_internals:raise_usage_error( "invalid classname ('~ts'): "
				"no 'class_' prefix used.", [ InvalidName ] )

	end;

check_classname( Other ) ->
	wooper_internals:raise_usage_error(
	  "specified classname ('~p') is not an atom.", [ Other ] ).



% Registers the corresponding classname into specified class information.
-spec manage_classname( module_entry(), class_info() ) -> class_info().
manage_classname( _ModuleEntry=undefined, _ClassInfo ) ->
	wooper_internals:raise_usage_error( "no module name was defined" );

manage_classname( _ModuleEntry={ _ModuleName=Classname, ModuleDef },
				  ClassInfo ) ->
	check_classname( Classname ),
	ClassDef = ModuleDef,
	ClassInfo#class_info{ class={ Classname, ClassDef } }.



% Registers the declared superclasses (if any) into specified class information.
-spec manage_superclasses( class_info() ) -> class_info().
manage_superclasses( ClassInfo=#class_info{ class={ Classname, _ClassLocForm },
											functions=FunctionTable } ) ->

	% Following searching through parse attributes is not done anymore, as now
	% we rely exclusively on a define instead (i.e. -define( superclasses, [ A,
	% B ]):

	%{ Superclasses, RegisteredClassInfo } =
	%		% Looking first for any '-superclasses(...).' parse attribute:
	%		case table:lookup_entry( superclasses, ParseAttrTable ) of

	% The define itself is visible only from the preprocessor (not from the
	% compiler), so its definition is obtained thanks to an ad hoc, always
	% defined function (get_superclasses/0, defined in
	% wooper_for_classes.hrl); we used to remove that function once the actual
	% superclasses were known, yet it may be useful (typically for
	% introspection), so it is now kept.

	#function_info{ clauses=[ { clause, _Line, _Patterns=[], _Guards=[],
								_Body=[ ReturnForm ] } ] } =
		case table:lookup_entry( { get_superclasses, 0 }, FunctionTable ) of

			{ value, GetSupFunInfo } ->
				GetSupFunInfo;

			key_not_found ->
				wooper_internals:raise_usage_error( "no get_superclasses/0 "
				  "defined: has wooper.hrl been included?",
				  Classname, _NoSpecificLine=0 )

		end,

	% ReturnForm expected to correspond to wooper:return_static( [A, B] ):
	{ call, _, { remote, _, {atom,_,wooper}, {atom,_,return_static} } ,
	  [ AtomListForm ] } = ReturnForm,

	SuperNames = try

			ast_generation:form_to_atoms( AtomListForm )

		catch _:_ ->

			%wooper_internals:raise_usage_error( "invalid superclasses define: "
			%	"a list of atoms was expected.", Classname, _NoLine=0 )

			wooper_internals:raise_usage_error( "invalid superclasses define: "
				"a list of atoms was expected, not ~p.", [ AtomListForm ],
				Classname, _NoLine=0 )

	end,

	%trace_utils:debug_fmt( "Detected superclasses: ~p", [ SuperNames ] ),

	ClassInfo#class_info{ superclasses={ SuperNames, _LocForm=undefined } }.
