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


% Centralizes, on behalf of the WOOPER parse transform, the support for instance
% destruction.
%
-module(wooper_instance_destruction).


-export([ manage_destructor/1 ]).


% For the function_info record:
-include_lib("myriad/include/ast_info.hrl").


% For the class_info record:
-include("wooper_info.hrl").


% Shorthands:

-type function_info() :: ast_info:function_info().
-type marker_table() :: ast_info:section_marker_table().
-type compose_pair() :: wooper_parse_transform:compose_pair().



% Extracts any destructor found in the specified function table, interprets that
% information to update the specified class information.
%
% Returns an updated pair thereof.
%
-spec manage_destructor( compose_pair() ) -> compose_pair().
manage_destructor( { FunctionTable, ClassInfo } ) ->

	FunIdInfos = table:enumerate( FunctionTable ),

	% First, check that no destruct/N with N =/= 1 exists:
	{ DestructFunInfo, FilteredFunIdInfos } =
		case scan_for_destructors( FunIdInfos ) of

			undefined ->
				MarkerTable = ClassInfo#class_info.markers,
				% None defined, adding thus a basic, do-nothing one:
				{ get_default_destructor_info( MarkerTable ), FunIdInfos };


			% Found and extracted as:
			{ DestrFunInfo, OtherFunIdInfos } ->

				% Here the developer has defined destruct/1, yet may or may not
				% have exported it:
				%
				NewDestrFunInfo = case DestrFunInfo#function_info.exported of

					[] ->
						% Never exported, so exporting it at the standard
						% location:
						MarkerTable = ClassInfo#class_info.markers,

						ExportLocation =
								 ast_info:get_default_export_function_location(
								   MarkerTable ),

						DestrFunInfo#function_info{
						  exported=[ ExportLocation ] };

					_ ->
						DestrFunInfo

				end,

				{ NewDestrFunInfo, OtherFunIdInfos }

		end,

	ShrunkFunctionTable = table:new( FilteredFunIdInfos ),

	% In all cases a destructor is defined then:
	NewClassInfo = ClassInfo#class_info{ destructor=DestructFunInfo },

	%trace_utils:debug_fmt( "Destructor info: ~ts",
	%	[ wooper_info:destructor_to_string( DestructFunInfo,
	%				_DoIncludeForms=true, _IndentationLevel=1 ) ] ),

	{ ShrunkFunctionTable, NewClassInfo }.





% Checks arities and extracts any destruct/1 found, returning it and the list of
% remaining pairs, if found, otherwise undefined.
%
% (helper)
%
scan_for_destructors( FunIdInfos ) ->
	scan_for_destructors( FunIdInfos, _Acc={ undefined, [] } ).



% (helper)
scan_for_destructors( _FunIdInfos=[], _Acc={ undefined, _AllFunIdInfos } ) ->
	% No need to return a list of pairs already known of the caller:
	undefined;


% Here Acc is { DestFunInfo, RemainingFunIdInfos }:
scan_for_destructors( _FunIdInfos=[], Acc ) ->
	Acc;


scan_for_destructors( _FunIdInfos=[ { { destruct, 1 },
									  #function_info{ clauses=[],
													  spec=S } } | _T ],
					  _Acc ) when S =/= undefined ->
	wooper_internals:raise_usage_error( "destructor (destruct/1) has a spec, "
										"yet it has never been defined." );


scan_for_destructors( _FunIdInfos=[ { { destruct, 1 }, DestFunInfo } | T ],
					  _Acc={ undefined, AccFunIdInfos } ) ->
	scan_for_destructors( T, { DestFunInfo, AccFunIdInfos } );


scan_for_destructors( _FunIdInfos=[ { { destruct, N }, _DestFunInfo } | _T ],
					  _Acc ) ->
	wooper_internals:raise_usage_error(
	  "wrong arity for destructor (destruct/1): expected 1, got ~B.", [ N ] );


scan_for_destructors( _FunIdInfos=[ Other | T ],
					  _Acc={ DestElem, OtherFunIdInfos } ) ->
	scan_for_destructors( T, { DestElem, [ Other | OtherFunIdInfos ] } ).



% Returns a function information corresponding to the default destructor, which
% is:
%
% -spec destruct( wooper:state() ) -> wooper:state().
% destruct( State ) ->
%	State.
%
-spec get_default_destructor_info( marker_table() ) -> function_info().
get_default_destructor_info( MarkerTable ) ->

	Line = 0,

	% First, let's define the destructor spec, which is:
	%   -spec destruct( wooper:state() ) -> wooper:state().

	% Corresponds to wooper:state():
	StateType = wooper_parse_utils:get_state_type(),

	SpecForm = { attribute, Line, spec, { {destruct,1},
	   [ { type, Line, 'fun',
		   [ { type, Line, product, _Params=[ StateType ] }, _Result=StateType ]
		 } ] } },

	% Then let's define the destructor function itself, based on:
	%   destruct( State ) ->
	%     State.

	StateVar = wooper_parse_utils:get_state_var(),

	% In AST, we shall have { function, Line, destruct, 1, [ DestructClause ] }:
	DestructClause = { clause, Line, _Pattern=[ StateVar ], [],
					   _Body=[ StateVar ] },

	% The spec and definition are to be placed together at this definition
	% marker:
	%
	DefLocation = ast_info:get_default_definition_function_location(
					MarkerTable ),

	% While the export is to be done in (and will be automatically declared in
	% that export form):
	%
	ExportLocation = ast_info:get_default_export_function_location(
						MarkerTable ),

	#function_info{ name=destruct,
					arity=1,
					location=DefLocation,
					line=Line,
					clauses=[ DestructClause ],
					spec={ DefLocation, SpecForm },
					callback=false,
					exported=[ ExportLocation ] }.
