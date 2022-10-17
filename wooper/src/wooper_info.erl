% Copyright (C) 2018-2022 Olivier Boudeville
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
% Creation date: Friday, April 13, 2018.


% Defined now here, as the rebar-based build system would not allow us to define
% per-module rules (ex: this module shall itself be compiled by the Myriad parse
% transform).
%
-compile( {parse_transform, myriad_parse_transform } ).


% @doc Centralisation of <b>class-level information</b>.
-module(wooper_info).


% For the attribute_info record:
-include("wooper_info.hrl").


-type class_info() :: #class_info{}.


% The description of a class:
-type class_description() :: ustring().


-type superclasses() :: [ wooper:classname() ].
% Mother classes (usually direct ones).


% Shorthands:
-type clause_def() :: meta_utils:clause_def().
-type file_loc() :: ast_base:file_loc().

-type attribute_qualifier() :: wooper:attribute_qualifier().



-type attribute_qualifiers() ::
		attribute_qualifier() | [ attribute_qualifier() ].

-type attribute_description() :: ustring().


-type attribute_spec() ::

		wooper:attribute_name()

	  | { wooper:attribute_name(), attribute_description() }

	  | { wooper:attribute_name(), wooper:attribute_type(),
		  attribute_description() }

	  | { wooper:attribute_name(), wooper:attribute_type(),
		  attribute_qualifiers(), attribute_description() }.
% How attributes are to be specified by the user.


-type attribute_info() :: #attribute_info{}.


-type attribute_table() :: table( attribute_name(), attribute_info() ).
% Stores all class-level information (i.e. metadata) regarding attributes (class
% ones, not parse ones).


-type method_spec() :: ast_base:form().
% The form corresponding to a method specification.


-type located_method_spec() :: { ast_location(), method_spec() }.
% The type specification of a method.


-type request_info() :: #request_info{}.
-type oneway_info() :: #oneway_info{}.
-type static_info() :: #static_info{}.




% Method export tables.


-type oneway_export_table() ::
		table( ast_location(), { line(), [ wooper:oneway_id() ] } ).
% Table storing the export declarations for oneway methods.
%
% Quite similar to ast_info:function_export_table().



-type request_export_table() ::
		table( ast_location(), { line(), [ wooper:request_id() ] } ).
% Table storing the export declarations for request methods.
%
% Quite similar to ast_info:function_export_table().



-type static_export_table() ::
		table( ast_location(), { line(), [ wooper:static_id() ] } ).
% Table storing the export declarations for static methods.
%
% Quite similar to ast_info:function_export_table().



-type constructor_table() :: table( arity(), ast_info:function_info() ).
% A table storing information about the constructors involved, regarding a
% class.



% Method definition tables.


-type oneway_table() :: table( wooper:oneway_id(), oneway_info() ).
% Table storing reference definitions of oneway methods.
%
% Quite similar to ast_info:function_table().


-type request_table() :: table( wooper:request_id(), request_info() ).
% Table storing reference definitions of request methods.
%
% Quite similar to ast_info:function_table().


-type static_table() :: table( wooper:static_id(), static_info() ).
% Table storing reference definitions of static methods.
%
% Quite similar to ast_info:function_table().


-export_type([ class_info/0, class_description/0, class_entry/0,
			   attribute_qualifiers/0,
			   attribute_spec/0, attribute_info/0, attribute_table/0,
			   method_spec/0, located_method_spec/0,
			   oneway_export_table/0, request_export_table/0,
			   static_export_table/0,
			   constructor_table/0,
			   oneway_table/0, request_table/0, static_table/0,
			   oneway_info/0, request_info/0, static_info/0 ]).


-export([ init_class_info/0,

		  class_info_to_string/1, class_info_to_string/2,
		  class_info_to_string/3,

		  class_entry_to_string/2, superclasses_to_string/3,
		  class_specific_attributes_to_string/3,

		  inherited_attributes_to_string/3,
		  attribute_info_to_string/2,
		  qualifiers_to_string/1,
		  definition_to_string/2,
		  located_spec_to_string/1,

		  constructors_to_string/3, destructor_to_string/3,

		  requests_to_string/3, request_info_to_string/3,
		  oneways_to_string/3, oneway_info_to_string/3,
		  static_methods_to_string/3, static_method_info_to_string/3,

		  get_wooper_builtins/0, get_metadata_builtins/0, get_state_builtins/0,
		  get_execution_builtins/0, get_inner_builtins/0, get_helper_builtins/0,
		  get_serialisation_builtins/0 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().
-type indentation_level() :: text_utils:indentation_level().

-type ast_location() :: ast_info:ast_location().
-type line() :: ast_base:line().

-type function_id() :: meta_utils:function_id().

-type attribute_name() :: wooper:attribute_name().



% @doc Returns a new, blank instance of the class_info record, typically to be
% fed with an input AST afterwards.
%
-spec init_class_info() -> class_info().
init_class_info() ->

	EmptyTable = table:new(),

	% All other fields (commented out) expected to have a default value defined,
	% or being initialised at record construction:
	%
	#class_info{ class=undefined,
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
				 static_exports=EmptyTable,
				 statics=EmptyTable,
				 %optional_callbacks_defs
				 %debug_mode
				 %last_line
				 markers=EmptyTable
				 %errors
				 %unhandled_forms
			   }.



% @doc Returns a textual description of specified class information, not
% including forms, and based on a default indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec class_info_to_string( class_info() ) -> ustring().
class_info_to_string( ClassInfo ) ->
	class_info_to_string( ClassInfo, _DoIncludeForms=false ).



% @doc Returns a textual description of specified class information, including
% forms if requested, and with specified indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec class_info_to_string( class_info(), boolean() ) -> ustring().
class_info_to_string( ClassInfo, DoIncludeForms ) ->
	class_info_to_string( ClassInfo, DoIncludeForms, _IndentationLevel=0 ).


% @doc Returns a textual description of specified class information, including
% forms if requested, and with specified indentation level.
%
% Note: here the location information is dropped for all located definitions.
%
-spec class_info_to_string( class_info(), boolean(), indentation_level() ) ->
									ustring().
class_info_to_string( #class_info{
						 class=ClassEntry,
						 superclasses=Superclasses,
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

	ClassnameString = class_entry_to_string( ClassEntry, DoIncludeForms ),

	DebugString = case IsDebugMode of

		true ->
			"in debug mode";

		false ->
			"not in debug mode"

	end,

	Infos = [ superclasses_to_string( Superclasses, DoIncludeForms,
									  NextIndentationLevel ),

			  class_specific_attributes_to_string( AttributeTable,
										DoIncludeForms, NextIndentationLevel ),

			  inherited_attributes_to_string( InheritedAttributes,
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

			  constructors_to_string( ConstructorTable, DoIncludeForms,
									  NextIndentationLevel ),

			  "regarding new operators, " ++ ast_info:functions_to_string(
				   NewOperatorTable, DoIncludeForms, NextIndentationLevel ),

			  destructor_to_string( DestructorInfo, DoIncludeForms,
									NextIndentationLevel ),

			  requests_to_string( RequestTable, DoIncludeForms,
								  NextIndentationLevel ),

			  oneways_to_string( OnewayTable, DoIncludeForms,
								 NextIndentationLevel ),

			  static_methods_to_string( StaticTable, DoIncludeForms,
										NextIndentationLevel ),

			  ast_info:last_line_to_string( LastLineLocDef ),

			  ast_info:markers_to_string( MarkerTable, NextIndentationLevel ),

			  ast_info:errors_to_string( Errors, NextIndentationLevel ),

			  ast_info:unhandled_forms_to_string( UnhandledForms,
								DoIncludeForms, NextIndentationLevel ) ],

		text_utils:format( "Information about class ~ts: ~ts",
			[ ClassnameString,
			  text_utils:strings_to_string( Infos, IndentationLevel ) ] ).




% @doc Returns a textual representation of the name of the class corresponding
% to specified entry, possibly with forms.
%
-spec class_entry_to_string( class_entry(), boolean() ) -> ustring().
class_entry_to_string( _ClassEntry=undefined, _DoIncludeForms ) ->
	"(unnamed class)";

class_entry_to_string( _ClassEntry={ ThisClassname, _ClassLocDef },
					   _DoIncludeForms=false ) ->
	text_utils:atom_to_string( ThisClassname );

class_entry_to_string( _ClassEntry={ ThisClassname,
						_ClassLocDef={ _Loc, Form } }, _DoIncludeForms=true ) ->
	text_utils:format( "~ts (represented as form '~p')",
					   [ ThisClassname, Form ] ).



% @doc Returns a textual representation of the specified superclasses.
-spec superclasses_to_string( superclasses(), boolean(),
							  indentation_level() ) -> ustring().
superclasses_to_string( _Superclasses=[], _DoIncludeForms,
						_IndentationLevel ) ->
	"no known superclass";

superclasses_to_string( Superclasses, _DoIncludeForms,
						_NextIndentationLevel ) ->
	text_utils:format( "~B known superclasses: ~p",
					   [ length( Superclasses ), Superclasses ] ).



% @doc Returns a textual representation of the specified class-specific
% attributes.
%
-spec class_specific_attributes_to_string( attribute_table(), boolean(),
										   indentation_level() ) -> ustring().
class_specific_attributes_to_string( AttributeTable, DoIncludeForms,
									 IndentationLevel ) ->

	% Attribute names are also in their information record:
	case table:values( AttributeTable ) of

		[] ->
			"no known class-specific attribute";

		AttrInfos ->

			AttrStrings = [ attribute_info_to_string( AttrInfo, DoIncludeForms )
							|| AttrInfo <- AttrInfos ],

			text_utils:format( "~B known class-specific attribute(s): ~ts",
				[ length( AttrInfos ),
				  text_utils:strings_to_string( AttrStrings,
												IndentationLevel ) ] )

	end.



% @doc Returns a textual representation of the specified inherited (class)
% attributes.
%
-spec inherited_attributes_to_string( attribute_table(), boolean(),
									  indentation_level() ) -> ustring().
inherited_attributes_to_string( AttributeTable, DoIncludeForms,
								IndentationLevel ) ->

	% Attribute names are also in their information record:
	case table:values( AttributeTable ) of

		[] ->
			"no known inherited attribute";

		AttrInfos ->

			AttrStrings = [ attribute_info_to_string( AttrInfo, DoIncludeForms )
							|| AttrInfo <- AttrInfos ],

			text_utils:format( "~B known inherited attribute(s): ~ts",
				[ length( AttrInfos ),
				  text_utils:strings_to_string( AttrStrings,
												IndentationLevel ) ] )

	end.



% @doc Returns a textual representation of the specified attribute information.
-spec attribute_info_to_string( attribute_info(), boolean() ) -> ustring().
attribute_info_to_string( _AttributeInfo, _DoIncludeForms ) ->
	"attribute info".



% @doc Returns a textual representation of the specified constructor
% information.
%
-spec constructors_to_string( constructor_table(), boolean(),
							  indentation_level() ) -> ustring().
constructors_to_string( ConstructorTable, DoIncludeForms, IndentationLevel ) ->

	case table:enumerate( ConstructorTable ) of

		[] ->
			"no constructor defined";

		ArityFunInfoPairs ->

			% Sort by increasing arity:
			SortedPairs = lists:keysort( _Index=1, ArityFunInfoPairs ),

			ConstructString = text_utils:strings_to_string( [
				begin

					FunString = ast_info:function_info_to_string(
						ConstructFunInfo, DoIncludeForms, IndentationLevel ),

					text_utils:format( "of arity ~B, implemented as: ~ts",
									   [ Arity, FunString ] )

				end || { Arity, ConstructFunInfo } <- SortedPairs ] ),

			text_utils:format( "~B constructor(s) defined: ~ts",
							   [ length( SortedPairs ), ConstructString ] )

	end.



% @doc Returns a textual representation of the specified destructor information.
-spec destructor_to_string( maybe( ast_info:function_info() ), boolean(),
							indentation_level() ) -> ustring().
destructor_to_string( _DestructorInfo=undefined, _DoIncludeForms,
					  _IndentationLevel ) ->
	"no destructor defined";

destructor_to_string( DestructorFunInfo, DoIncludeForms, IndentationLevel ) ->
	text_utils:format( "following destructor defined: ~ts",
		[ ast_info:function_info_to_string( DestructorFunInfo,
										DoIncludeForms, IndentationLevel ) ] ).



% @doc Returns a textual representation of the specified information about
% requests.
%
-spec requests_to_string( request_table(), boolean(), indentation_level() ) ->
								ustring().
requests_to_string( RequestTable, DoIncludeForms, IndentationLevel ) ->

	case table:values( RequestTable ) of

		[] ->
			"no request defined";

		ReqInfos ->

			ReqString = text_utils:strings_to_string( [ request_info_to_string(
							ReqInfo, DoIncludeForms, IndentationLevel )
										|| ReqInfo <- ReqInfos ],
													  IndentationLevel ),

			text_utils:format( "~B request(s) defined: ~ts",
							   [ length( ReqInfos ), ReqString ] )

	end.



% @doc Returns a textual representation of the specified request information.
-spec request_info_to_string( request_info(), boolean(),
							  indentation_level() ) -> ustring().
request_info_to_string( #request_info{ name=Name,
									   arity=Arity,
									   qualifiers=Qualifiers,
									   %ast_location=_ASTLoc,
									   file_location=FileLoc,
									   clauses=Clauses,
									   spec=LocatedSpec },
						DoIncludeForms, IndentationLevel ) ->

	QualString = qualifiers_to_string( Qualifiers ),
	DefString = definition_to_string( Clauses, FileLoc ),
	SpecString = located_spec_to_string( LocatedSpec ),

	BaseString = text_utils:format(
		"request ~ts/~B with ~ts defined, ~ts and ~ts",
		[ Name, Arity, QualString, DefString, SpecString ] ),

	case DoIncludeForms of

		true ->
			text_utils:format( "~ts~n~ts", [ BaseString,
				ast_function:clauses_to_string( Clauses,
												IndentationLevel+1 ) ] );

		false ->
			BaseString

	end.



% @doc Returns a textual representation of the specified information about
% oneways.
%
-spec oneways_to_string( oneway_table(), boolean(), indentation_level() ) ->
								ustring().
oneways_to_string( OnewayTable, DoIncludeForms, IndentationLevel ) ->

	case table:values( OnewayTable ) of

		[] ->
			"no oneway defined";

		OnwInfos ->

			OnwString = text_utils:strings_to_string( [ oneway_info_to_string(
							OnwInfo, DoIncludeForms, IndentationLevel )
										|| OnwInfo <- OnwInfos ],
													  IndentationLevel ),

			text_utils:format( "~B oneway(s) defined: ~ts",
							   [ length( OnwInfos ), OnwString ] )

	end.



% @doc Returns a textual representation of the specified oneway information.
-spec oneway_info_to_string( oneway_info(), boolean(), indentation_level() ) ->
								   ustring().
oneway_info_to_string( #oneway_info{ name=Name,
									 arity=Arity,
									 qualifiers=Qualifiers,
									 %ast_location=_ASTLoc,
									 file_location=FileLoc,
									 clauses=Clauses,
									 spec=LocatedSpec },
						DoIncludeForms,
						IndentationLevel ) ->

	QualString = qualifiers_to_string( Qualifiers ),
	DefString = definition_to_string( Clauses, FileLoc ),
	SpecString = located_spec_to_string( LocatedSpec ),

	BaseString = text_utils:format(
		"oneway ~ts/~B with ~ts defined, ~ts and ~ts",
		[ Name, Arity, QualString, DefString, SpecString ] ),

	case DoIncludeForms of

		true ->
			text_utils:format( "~ts~n~ts", [ BaseString,
				ast_function:clauses_to_string( Clauses,
												IndentationLevel+1 ) ] );

		false ->
			BaseString

	end.



% @doc Returns a textual representation of the specified information about
% static methods.
%
-spec static_methods_to_string( static_table(), boolean(),
								indentation_level() ) -> ustring().
static_methods_to_string( StaticTable, DoIncludeForms, IndentationLevel ) ->

	case table:values( StaticTable ) of

		[] ->
			"no static method defined";

		StatInfos ->

			StatString = text_utils:strings_to_string( [
				static_method_info_to_string( StatInfo, DoIncludeForms,
								IndentationLevel ) || StatInfo <- StatInfos ],
													   IndentationLevel ),

			text_utils:format( "~B static method(s) defined: ~ts",
							   [ length( StatInfos ), StatString ] )

	end.



% @doc Returns a textual representation of the specified static method
% information.
%
-spec static_method_info_to_string( static_info(), boolean(),
									indentation_level() ) -> ustring().
static_method_info_to_string( #static_info{ name=Name,
											arity=Arity,
											qualifiers=Qualifiers,
											%ast_location=_ASTLoc,
											file_location=FileLoc,
											clauses=Clauses,
											spec=LocatedSpec },
							  DoIncludeForms,
							  IndentationLevel ) ->

	QualString = qualifiers_to_string( Qualifiers ),
	DefString = definition_to_string( Clauses, FileLoc ),
	SpecString = located_spec_to_string( LocatedSpec ),

	BaseString = text_utils:format(
		"static method ~ts/~B with ~ts defined, ~ts and ~ts",
		[ Name, Arity, QualString, DefString, SpecString ] ),

	case DoIncludeForms of

		true ->
			text_utils:format( "~ts~n~ts", [ BaseString,
				ast_function:clauses_to_string( Clauses,
												IndentationLevel+1 ) ] );

		false ->
			BaseString

	end.



% Section for built-in functions, notably to be able to filter them out.
%
% Note: over time, at least some of them are meant to be implemented by the
% WOOPER parse trandsform, and thus are to disappear from here.


% @doc Returns the function identifiers of all WOOPER builtins.
-spec get_wooper_builtins() -> [ function_id() ].
get_wooper_builtins() ->
	get_metadata_builtins() ++ get_state_builtins() ++ get_execution_builtins()
		++ get_inner_builtins() ++ get_helper_builtins()
		++ get_serialisation_builtins().



% @doc Returns the function identifiers of the WOOPER builtins regarding class
% metadata.
%
-spec get_metadata_builtins() -> [ function_id() ].
get_metadata_builtins() ->
	[ {getSuperclasses,1}

	  % This static method is generated yet not considered as a built-in, as we
	  % want it to be sorted among the static methods:
	  %{get_superclasses,0}

	  % Removed from here, as defined in an header yet still to be transformed:
	  % {getClassname,1},

	  % Removed from here, as useless as a static method:
	  % {get_classname,0}
	].



% @doc Returns the function identifiers of the WOOPER builtins regarding state.
-spec get_state_builtins() -> [ function_id() ].
get_state_builtins() ->
	[ {hasAttribute,2},
	  {removeAttribute,2},

	  {getAttribute,2},
	  {getAttributes,2},

	  {setAttribute,3},
	  {setAttributes,2},

	  {swapInAttribute,3},

	  {incrementAttribute,2},
	  {decrementAttribute,2},


	  {appendToAttribute,3},
	  {popFromAttribute,2},

	  {deleteFromAttribute,3},
	  {concatToAttribute,3},

	  {addToAttribute,3},
	  {subtractFromAttribute,3},

	  {toggleAttribute,2},

	  {addKeyValueToAttribute,4} ].



% @doc Returns the function identifiers of the WOOPER builtins regarding
% behaviour (execution).
%
-spec get_execution_builtins() -> [ function_id() ].
get_execution_builtins() ->
	[ {executeRequest,2},
	  {executeRequest,3},
	  {executeRequestAs,3},
	  {executeRequestAs,4},

	  {executeOneway,2},
	  {executeOneway,3},
	  {executeOnewayAs,3},
	  {executeOnewayAs,4} ].



% @doc Returns the function identifiers of the WOOPER inner builtins (defined
% for its own mode of operation).
%
-spec get_inner_builtins() -> [ function_id() ].
get_inner_builtins() ->
	[ {wooper_main_loop,1},
	  {wooper_lookup_method,3},

	  {wooper_execute_method,3},
	  {wooper_execute_method_as,4},
	  {wooper_effective_method_execution,4},

	  {wooper_handle_local_request_execution,3},
	  {wooper_handle_local_request_execution_as,4},
	  {wooper_handle_remote_request_execution,4},

	  {wooper_handle_local_oneway_execution,3},
	  {wooper_handle_local_oneway_execution_as,4},
	  {wooper_handle_remote_oneway_execution,3},

	  {chain_parent_destructors,1},
	  {trigger_destruct_error,4},

	  {wooper_destruct,1} ].



% @doc Returns the function identifiers of the WOOPER helper builtins.
-spec get_helper_builtins() -> [ function_id() ].
get_helper_builtins() ->
	[ {is_wooper_debug,0} ].



% @doc Returns the function identifiers of the WOOPER serialisation builtins.
-spec get_serialisation_builtins() -> [ function_id() ].
get_serialisation_builtins() ->
	[ {pre_serialise_hook,1},
	  {serialise,3},

	  {post_deserialise_hook,1},
	  {post_serialise_hook,3},

	  {pre_deserialise_hook,2} ].



% @doc Returns a textual description of the specified qualifiers.
-spec qualifiers_to_string( [ attribute_qualifier() ] ) -> ustring().
qualifiers_to_string( _Qualifiers=[] ) ->
	"no qualifier";

qualifiers_to_string( _Qualifiers=none ) ->
	"no qualifier";

qualifiers_to_string( [ SingleQualifier ] ) ->
	text_utils:format( "the ~w qualifier", [ SingleQualifier ] );

qualifiers_to_string( Qualifiers ) ->
	text_utils:format( "the ~w qualifiers", [ Qualifiers ] ).



% @doc Returns a textual description of the specified method definition.
-spec definition_to_string( [ clause_def() ], maybe( file_loc() ) ) ->
									ustring().
definition_to_string( Clauses, _FileLoc=undefined ) ->
	text_utils:format( "with ~B clause(s) defined", [ length( Clauses ) ] );

definition_to_string( Clauses, FileLoc ) ->
	text_utils:format( "defined from ~ts, with ~B clause(s) specified",
		[ ast_utils:file_loc_to_string( FileLoc ), length( Clauses ) ] ).


% @doc Returns a textual description of the specified located method spec.
-spec located_spec_to_string( maybe( located_method_spec() ) ) -> ustring().
located_spec_to_string( _LocatedSpec=undefined ) ->
	"no type specification";

located_spec_to_string( _LocatedSpec ) ->
	"a type specification".
