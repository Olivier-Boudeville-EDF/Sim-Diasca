% Copyright (C) 2018-2021 EDF R&D

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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% Centralisation of actor-level information.
-module(actor_info).


% For the actor_class_info record:
-include("actor_info.hrl").


-type actor_class_info() :: #actor_class_info{}.

-type actor_oneway_id() :: wooper:oneway_id().



% Table storing the export declarations for actor oneways.
%
% Quite similar to wooper_info:oneway_export_table().
%
-type actor_oneway_export_table() :: table( ast_info:location(),
						   { ast_base:line(), [ actor_oneway_id() ] } ).



% Table storing reference definitions of actor oneways.
%
% Quite similar to wooper_info:oneway_table().
-type actor_oneway_table() :: table( actor_oneway_id(), actor_oneway_info() ).


-export_type([ actor_oneway_info/0, actor_class_info/0, actor_oneway_id/0,
			   actor_oneway_export_table/0, actor_oneway_table/0 ]).


-export([ init_actor_class_info/0,

		  actor_class_info_to_string/1, actor_class_info_to_string/2,
		  actor_class_info_to_string/3,

		  actor_oneways_to_string/3, actor_oneway_info_to_string/3 ]).




% Returns a new, blank instance of the actor_class_info record, typically to be
% fed with an input AST afterwards.
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



% Returns a textual description of specified actor information, not including
% forms, and based on a default indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec actor_class_info_to_string( actor_class_info() ) -> text_utils:ustring().
actor_class_info_to_string( ActorInfo ) ->
	actor_class_info_to_string( ActorInfo, _DoIncludeForms=false ).


% Returns a textual description of specified actor information, including forms
% if requested, and with specified indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec actor_class_info_to_string( actor_class_info(), boolean() ) ->
										text_utils:ustring().
actor_class_info_to_string( ActorInfo, DoIncludeForms ) ->
	actor_class_info_to_string( ActorInfo, DoIncludeForms,
								_IndentationLevel=0 ).


% Returns a textual description of specified actor information, including forms
% if requested, and with specified indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec actor_class_info_to_string( actor_class_info(), boolean(),
					 text_utils:indentation_level() ) -> text_utils:ustring().
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
							   last_line=LastLineLocDef,
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

			  ast_info:last_line_to_string( LastLineLocDef ),

			  ast_info:markers_to_string( MarkerTable, NextIndentationLevel ),

			  ast_info:errors_to_string( Errors, NextIndentationLevel ),

			  ast_info:unhandled_forms_to_string( UnhandledForms,
							 DoIncludeForms, NextIndentationLevel ) ],

		text_utils:format( "Information about actor ~s: ~s", [ ClassnameString,
				 text_utils:strings_to_string( Infos, IndentationLevel ) ] ).



% Returns a textual representation of the specified information about actor
% oneways.
%
-spec actor_oneways_to_string( actor_oneway_table(), boolean(),
					   text_utils:indentation_level() ) -> text_utils:ustring().
actor_oneways_to_string( ActorOnewayTable, DoIncludeForms, IndentationLevel ) ->

	case table:values( ActorOnewayTable ) of

		[] ->
			"no actor oneway defined";

		ActInfos ->
			ActString = text_utils:strings_to_string( [
				actor_oneway_info_to_string( ActInfo, DoIncludeForms,
						   IndentationLevel ) || ActInfo <- ActInfos ],
													  IndentationLevel ),

			text_utils:format( "~B actor oneway(s) defined: ~s",
							   [ length( ActInfos ), ActString ] )

	end.



% Returns a textual representation of the specified oneway information.
-spec actor_oneway_info_to_string( actor_oneway_info(), boolean(),
				   text_utils:indentation_level() ) -> text_utils:ustring().
actor_oneway_info_to_string( #actor_oneway_info{ name=Name,
												 arity=Arity,
												 qualifiers=Qualifiers,
												 location=_Location,
												 line=Line,
												 clauses=Clauses,
												 spec=LocatedSpec },
							 DoIncludeForms,
							 IndentationLevel ) ->

	QualString = case Qualifiers of

	  Q when Q =:= [] orelse Q =:= none ->
		  "no qualifier";

	  [ SingleQualifier ] ->
		  text_utils:format( "the ~w qualifier", [ SingleQualifier ] );

	  Qualifiers ->
		  text_utils:format( "the ~w qualifiers", [ Qualifiers ] )

	end,

	DefString = case Line of

		undefined ->
			text_utils:format( "with ~B clause(s) defined",
							   [ length( Clauses ) ] );

		_ ->
			text_utils:format( "defined from line #~B, with "
			   "~B clause(s) specified", [ Line, length( Clauses ) ] )

	end,

	SpecString = case LocatedSpec of

		undefined ->
			"no type specification";

		_ ->
			"a type specification"

	end,

	BaseString = text_utils:format(
				   "actor oneway ~s/~B with ~s defined, ~s and ~s",
				   [ Name, Arity, QualString, DefString, SpecString ] ),

	case DoIncludeForms of

		true ->
			text_utils:format( "~s~n~s",
							   [ BaseString, ast_function:clauses_to_string(
										  Clauses, IndentationLevel + 1 ) ] );

		false ->
			BaseString

	end.
