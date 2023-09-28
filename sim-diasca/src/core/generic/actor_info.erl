% Copyright (C) 2018-2023 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2018.


% @doc Module centralising most <b>actor-level</b> information, in the context
% of the Sim-Diasca parse transform.
%
-module(actor_info).


% For the actor_class_info record:
-include("actor_info.hrl").


-type actor_class_info() :: #actor_class_info{}.

-type actor_oneway_id() :: wooper:oneway_id().



-type actor_oneway_export_table() :: table( ast_info:ast_location(), 
								{ ast_base:line(), [ actor_oneway_id() ] } ).
% Table storing the export declarations for actor oneways.
%
% Quite similar to wooper_info:oneway_export_table().


-type actor_oneway_table() :: table( actor_oneway_id(), actor_oneway_info() ).
% Table storing reference definitions of actor oneways.
%
% Quite similar to wooper_info:oneway_table().


-export_type([ actor_oneway_info/0, actor_class_info/0, actor_oneway_id/0,
			   actor_oneway_export_table/0, actor_oneway_table/0 ]).


-export([ init_actor_class_info/0,

		  actor_class_info_to_string/1, actor_class_info_to_string/2,
		  actor_class_info_to_string/3,

		  actor_oneways_to_string/3, actor_oneway_info_to_string/3 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type indentation_level() :: text_utils:indentation_level().



% @doc Returns a new, blank instance of the actor_class_info record, typically
% to be fed with an input AST afterwards.
%
-spec init_actor_class_info() -> actor_class_info().
init_actor_class_info() ->

	EmptyTable = table:new(),

	% All other fields (commented out) expected to have a default value defined,
	% or being initialised at record construction:
	%
	#actor_class_info{ class=undefined,
		superclasses=[],
		attributes=EmptyTable,
		inherited_attributes=EmptyTable,
		compilation_options=EmptyTable,
		%compilation_option_defs
		parse_attributes=EmptyTable,
		%remote_spec_defs
		%includes
		%include_defs
		type_exports=EmptyTable,
		types=EmptyTable,
		records=EmptyTable,
		function_imports=EmptyTable,
		%function_imports_defs
		function_exports=EmptyTable,
		functions=EmptyTable,
		constructors=EmptyTable,
		new_operators=EmptyTable,
		destructor=undefined,
		request_exports=EmptyTable,
		requests=EmptyTable,
		oneway_exports=EmptyTable,
		oneways=EmptyTable,
		actor_oneway_exports=EmptyTable,
		actor_oneways=EmptyTable,
		static_exports=EmptyTable,
		statics=EmptyTable,
		%optional_callbacks_defs
		%debug_mode
		%last_line
		markers=EmptyTable
		%errors
		%unhandled_forms
	}.



% @doc Returns a textual description of specified actor information, not
% including forms, and based on a default indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec actor_class_info_to_string( actor_class_info() ) -> ustring().
actor_class_info_to_string( ActorInfo ) ->
	actor_class_info_to_string( ActorInfo, _DoIncludeForms=false ).


% @doc Returns a textual description of specified actor information, including
% forms if requested, and with specified indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec actor_class_info_to_string( actor_class_info(), boolean() ) -> ustring().
actor_class_info_to_string( ActorInfo, DoIncludeForms ) ->
	actor_class_info_to_string( ActorInfo, DoIncludeForms,
								_IndentationLevel=0 ).


% @doc Returns a textual description of specified actor information, including
% forms if requested, and with specified indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec actor_class_info_to_string( actor_class_info(), boolean(),
								  indentation_level() ) -> ustring().
actor_class_info_to_string( #actor_class_info{
		class=ClassEntry,
		superclasses=SuperclassesEntry,
		attributes=AttributeTable,
		inherited_attributes=InheritedAttributes,
		compilation_options=CompileOpts,
		compilation_option_defs=CompileOptDefs,
		parse_attributes=ParseAttributeTable,
		remote_spec_defs=RemoteSpecDefs,
		includes=Includes,
		include_defs=IncludeDefs,
		type_exports=TypeExportTable,
		types=TypeTable,
		records=RecordTable,
		function_imports=FunctionImportTable,
		function_imports_defs=FunctionImportDefs,
		function_exports=_FunctionExportTable,
		functions=FunctionTable,
		constructors=ConstructorTable,
		new_operators=NewOperatorTable,
		destructor=DestructorInfo,
		request_exports=_RequestExports,
		requests=RequestTable,
		oneway_exports=_OnewayExports,
		oneways=OnewayTable,
		actor_oneway_exports=_ActorOnewayExports,
		actor_oneways=ActorOnewayTable,
		static_exports=_StaticExports,
		statics=StaticTable,
		optional_callbacks_defs=OptCallbacksDefs,
		debug_mode=IsDebugMode,
		last_file_location=LastLineLocDef,
		markers=MarkerTable,
		errors=Errors,
		unhandled_forms=UnhandledForms },
							DoIncludeForms,
							IndentationLevel ) ->

	% For this textual description, we mostly rely on the higher-level
	% information available.

	% As the next strings will be collected at a level of their own:
	NextIndentationLevel = IndentationLevel + 1,

	% Information gathered in the order of the fields (basically in a compatible
	% order with the ast_info:module_info_to_string/3 counterpart function):

	ClassnameString =
		wooper_info:class_entry_to_string( ClassEntry, DoIncludeForms ),

	DebugString = case IsDebugMode of

		true ->
			"in debug mode";

		false ->
			"not in debug mode"

	end,

	Infos = [ wooper_info:superclasses_to_string( SuperclassesEntry,
				DoIncludeForms, NextIndentationLevel ),

			  wooper_info:class_specific_attributes_to_string( AttributeTable,
				DoIncludeForms, NextIndentationLevel ),

			  wooper_info:inherited_attributes_to_string( InheritedAttributes,
				DoIncludeForms, NextIndentationLevel ),

			  ast_info:compilation_options_to_string( CompileOpts,
				CompileOptDefs, DoIncludeForms, NextIndentationLevel ),

			  ast_info:optional_callbacks_to_string( OptCallbacksDefs,
				DoIncludeForms, NextIndentationLevel ),

			  DebugString,

			  ast_info:parse_attribute_table_to_string( ParseAttributeTable,
				DoIncludeForms, NextIndentationLevel ),

			  ast_info:remote_spec_definitions_to_string( RemoteSpecDefs,
				DoIncludeForms, NextIndentationLevel ),

			  ast_info:includes_to_string( Includes, IncludeDefs,
				DoIncludeForms, NextIndentationLevel ),

			  % No form to manage:
			  ast_info:type_exports_to_string( TypeExportTable,
											   NextIndentationLevel ),

			  ast_info:types_to_string( TypeTable, DoIncludeForms,
										NextIndentationLevel ),

			  ast_info:records_to_string( RecordTable, NextIndentationLevel ),

			  ast_info:function_imports_to_string( FunctionImportTable,
				FunctionImportDefs, DoIncludeForms, NextIndentationLevel ),

			  ast_info:functions_to_string( FunctionTable, DoIncludeForms,
											NextIndentationLevel ),

			  wooper_info:constructors_to_string( ConstructorTable,
				DoIncludeForms, NextIndentationLevel ),

			  "regarding new operators, " ++ ast_info:functions_to_string(
				NewOperatorTable, DoIncludeForms, NextIndentationLevel ),

			  wooper_info:destructor_to_string( DestructorInfo, DoIncludeForms,
												NextIndentationLevel ),

			  wooper_info:requests_to_string( RequestTable, DoIncludeForms,
											  NextIndentationLevel ),

			  wooper_info:oneways_to_string( OnewayTable, DoIncludeForms,
											 NextIndentationLevel ),

			  actor_oneways_to_string( ActorOnewayTable, DoIncludeForms,
									   NextIndentationLevel ),

			  wooper_info:static_methods_to_string( StaticTable, DoIncludeForms,
													NextIndentationLevel ),

			  ast_info:last_file_loc_to_string( LastLineLocDef ),

			  ast_info:markers_to_string( MarkerTable, NextIndentationLevel ),

			  ast_info:errors_to_string( Errors, NextIndentationLevel ),

			  ast_info:unhandled_forms_to_string( UnhandledForms,
				DoIncludeForms, NextIndentationLevel ) ],

		text_utils:format( "Information about actor ~ts: ~ts",
			[ ClassnameString,
			  text_utils:strings_to_string( Infos, IndentationLevel ) ] ).



% @doc Returns a textual representation of the specified information about actor
% oneways.
%
-spec actor_oneways_to_string( actor_oneway_table(), boolean(),
							   indentation_level() ) -> ustring().
actor_oneways_to_string( ActorOnewayTable, DoIncludeForms, IndentationLevel ) ->

	case table:values( ActorOnewayTable ) of

		[] ->
			"no actor oneway defined";

		ActInfos ->
			ActString = text_utils:strings_to_string( [
				actor_oneway_info_to_string( ActInfo, DoIncludeForms,
					IndentationLevel ) || ActInfo <- ActInfos ],
													  IndentationLevel ),

			text_utils:format( "~B actor oneway(s) defined: ~ts",
							   [ length( ActInfos ), ActString ] )

	end.



% @doc Returns a textual representation of the specified actor oneway
% information.
%
-spec actor_oneway_info_to_string( actor_oneway_info(), boolean(),
								   indentation_level() ) -> ustring().
actor_oneway_info_to_string( #actor_oneway_info{ name=Name,
												 arity=Arity,
												 qualifiers=Qualifiers,
												 %ast_location=_ASTLocation,
												 file_location=FileLoc,
												 clauses=Clauses,
												 spec=LocatedSpec },
							 DoIncludeForms,
							 IndentationLevel ) ->

	% Very close to wooper_info:oneway_info_to_string/3:

	QualString = wooper_info:qualifiers_to_string( Qualifiers ),
	DefString = wooper_info:definition_to_string( Clauses, FileLoc ),
	SpecString = wooper_info:located_spec_to_string( LocatedSpec ),

	BaseString = text_utils:format(
		"actor oneway ~ts/~B with ~ts defined, ~ts and ~ts",
		[ Name, Arity, QualString, DefString, SpecString ] ),

	case DoIncludeForms of

		true ->
			text_utils:format( "~ts~n~ts", [ BaseString,
				ast_function:clauses_to_string( Clauses,
												IndentationLevel+1 ) ] );

		false ->
			BaseString

	end.
