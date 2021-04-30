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


% Centralises, on behalf of the WOOPER parse transform, the support for the
% state management, including instance attributes.
%
-module(wooper_state_management).


-export([ manage_attributes/1, attributes_to_string/1 ]).


% For the class_info record:
-include("wooper_info.hrl").


-type attribute_table() :: wooper_info:attribute_table().


% Shorthands:

-type class_info() :: wooper_info:class_info().


% For the function_info record:
-include_lib("myriad/include/ast_info.hrl").


% Implementation notes:

% For attributes, we would have liked the user to be able to define them with:
% -attributes([ { name, name(), [ const, protected ], "Some name" }, ... ] ).
%
% However we then end up with {error,{24,erl_parse,"bad attribute"}} (because of
% name() being interpreted as an unexpected function call, targeting an
% undefined function), and this user attribute information is lost.
%
% The parentheses are necessary for types, as they can be polymorphic, so
% instead, we can:
%
% - either hide the type from the parser, like in:
% -attributes([ { name, 'name()', [ const, protected ], "Some name" }, ... ] ).
%
% - or use the define parse attribute, with is more permissive, for macros:
% -define( attributes, [ { name, name(), [ const, protected ], "Some name" },
%                          ... ] ).
%
% We finally preferred the latter to the former, even if it somehow would be
% inconsistent with a -superclasses([...]) attribute (as '-attributes(...).'
% would have thus been expected in turn).
%
% We finally chose to support the recommended '-define(superclasses,[...]).',
% for the sake of homogeneity/least surprise, and then to disable the support
% for the '-superclasses([...]).' parse attribute, for consistency.
%
% As a result, finally, both use defines instead of parse attributes.


% Processes the class-specific attributes.
-spec manage_attributes( class_info() ) -> class_info().
manage_attributes( ClassInfo=#class_info{ class={ Classname, _LocForm },
										  attributes=AttributeTable,
										  function_exports=FunExportTable,
										  functions=FunctionTable,
										  statics=StaticTable,
										  markers=MarkerTable } ) ->

	%trace_utils:info( "Managing class attributes." ),

	% Now the sole means of declaring the class attributes is by specifying a
	% class_attributes define (rather than a type-limiting -attributes parse
	% attribute).
	%
	% So we rely on the automatically-defined
	% wooper_get_class_specific_attributes/0 function to unveil (at compilation
	% time) the value of this attribute, should it be defined.
	%
	% We used to get rid of this automatically-defined function, yet getting
	% attribute information would surely be useful for various uses
	% (ex:introspection); however we cannot keep the AST that we got from the
	% macro as it is, as it would not compile (ex: if a class attribute involves
	% a integer() type, the compiler will expect a integer/0 function to be
	% defined in that module); so we have to transform that AST into one that
	% can compile, and name the function differently as it has a different
	% signature (moreover it shall be static method).
	%
	% So the ultimately targeted function get_class_specific_attributes/0 will
	% be obtained from% the temporary, automatically-defined:
	%
	AttrTempFunKey = { wooper_get_class_specific_attributes, 0 },

	% Thanks to the macro conditionals in wooper_for_classes.hrl, in all cases
	% (even if no class_attributes define was specified), the getter shall be
	% defined (possibly just as an empty list):
	%
	{ #function_info{

		 clauses=[ { clause, _Line, _Patterns=[], _Guards=[],
					 _Body=[ AttrListForm ] } ],
		 exported=ExportLocs },

	  ShrunkFunTable } = table:extract_entry( AttrTempFunKey, FunctionTable ),

	NewFunExportTable = ast_info:ensure_function_not_exported( AttrTempFunKey,
										ExportLocs, FunExportTable ),

	% First, register the corresponding attributes:
	NewAttributeTable =
	  register_attributes_from_form( AttrListForm, AttributeTable, Classname ),

	% Then fix the AST of the temporary function, for the final getter one:
	NewAttrListForm = transform_attribute_getter_form( AttrListForm ),

	NewLine = 0,

	NewClauses = [ { clause, NewLine, _NewPatterns=[], _NewGuards=[],
					 _NewBody=[ NewAttrListForm ] } ],


	% Note that we will have to add get_class_specific_attributes/0, but also to
	% remove wooper_get_class_specific_attributes/0; we cannot easily convert
	% the function_info of the latter to the one of the former (as for example
	% different exported names and specs will apply), so we do a bit like
	% meta_utils:remove_function/2 (cannot be reused as it is, as it operates on
	% a module_info, not a class_info).

	% First, adding get_class_specific_attributes/0:

	TargetFunName = get_class_specific_attributes,

	DefLoc = ast_info:get_default_definition_function_location( MarkerTable ),

	% Corresponds to '-spec get_class_specific_attributes() ->
	%    static_return( [ wooper_info:attribute_info() ] )':
	%
	%NewSpec = [ { type, NewLine, 'fun',
	%			  [ { type, NewLine, product, [] },
	%				{ user_type, NewLine, static_return,
	%				  [ { type, NewLine, list,
	%					  [ { remote_type, NewLine,
	%						  [ {atom,NewLine,wooper_info},
	%							{atom,NewLine,attribute_info},[] ]
	%						} ] } ] } ] } ],

	%NewSpec = [ { type, NewLine, 'fun',
	%			  [ { type, NewLine, product, [] },
	%				[ { type, NewLine, list,
	%					  [ { remote_type, NewLine,
	%						  [ {atom,NewLine,wooper_info},
	%							{atom,NewLine,attribute_info},[] ]
	%						} ] } ] ] } ],

	NewSpec = undefined,

	NewStaticInfo = #static_info{ name=TargetFunName,
								  arity=0,
								  qualifiers=[ public, final ],
								  location=DefLoc,
								  line=NewLine,
								  clauses=NewClauses,
								  spec=NewSpec },

	AttrTargetFunKey = { TargetFunName, 0 },

	NewStaticTable = table:add_new_entry( AttrTargetFunKey, NewStaticInfo,
										  StaticTable ),

	%trace_utils:debug_fmt( "As class-specific attributes, we have ~ts",
	%					   [ attributes_to_string( NewAttributeTable ) ] ),

	ClassInfo#class_info{ attributes=NewAttributeTable,
						  function_exports=NewFunExportTable,
						  functions=ShrunkFunTable,
						  statics=NewStaticTable }.



% Registers (and checks) specified attributes.
% (helper)
%
register_attributes_from_form( AttrListForm, AttributeTable, Classname ) ->

	AttrFormList = try

		ast_generation:form_to_list( AttrListForm )

		catch _:_ ->
			wooper_internals:raise_usage_error( "invalid 'class_attributes' "
				"define: expecting a list (of attribute declarations).",
				[], Classname )

	end,

	%trace_utils:debug_fmt( "Attribute declaration forms:~n  ~p",
	%						[ AttrFormList ] ),

	register_helper( AttrFormList, AttributeTable, Classname ).




% Note: there is no point in trying to obtain a line number from the forms
% related to class_attributes; indeed, this is just a (preprocessor) define, so
% it has no source location, and any line obtained from a related form would
% point to the wooper_get_class_specific_attributes/0 pseudo-function that we
% introduced - not to the location of said attributes in the file.
%
% (helper)
%
register_helper( _AttrFormList=[], AttributeTable, _Classname ) ->
	AttributeTable;

% All attributes are expected to be declared either as a single atom or a
% tuple with 2, 3 or 4 elements:
%
% Single atom:
register_helper( _AttrFormList=[ AttrForm={atom,_,_AttrName} | T ],
				 AttributeTable, Classname ) ->

	% Only the name is specified here:
	NewAttributeTable = register_attribute( _AttrNameForm=AttrForm,
		_TypeForm=undefined, _QualifiersForm=undefined,
		_DescriptionForm=undefined, AttributeTable, Classname ),

	register_helper( T, NewAttributeTable, Classname );

% 4 elements:
register_helper( _AttrFormList=[ _AttrForm={ tuple,_, [ AttrNameForm,
	  TypeForm, QualifiersForm, DescriptionForm ] } | T ], AttributeTable,
				 Classname ) ->

	NewAttributeTable = register_attribute( AttrNameForm, TypeForm,
			  QualifiersForm, DescriptionForm, AttributeTable, Classname ),

	register_helper( T, NewAttributeTable, Classname );

% 3 elements:
register_helper( _AttrFormList=[ _AttrForm={ tuple,_,
					[ AttrNameForm, TypeForm, DescriptionForm ] } | T ],
				 AttributeTable, Classname ) ->

	NewAttributeTable = register_attribute( AttrNameForm, TypeForm,
		_QualifiersForm=undefined, DescriptionForm, AttributeTable, Classname ),

	register_helper( T, NewAttributeTable, Classname );

% 2 elements:
register_helper( _AttrFormList=[ _AttrForm={ tuple,_,
	  [ AttrNameForm, DescriptionForm ] } | T ], AttributeTable, Classname ) ->

	NewAttributeTable = register_attribute( AttrNameForm, _TypeForm=undefined,
		_QualifiersForm=undefined, DescriptionForm, AttributeTable, Classname ),

	register_helper( T, NewAttributeTable, Classname );

% Errors:
register_helper( _AttrForm=[ { tuple,_, Forms } | _T ], _AttributeTable,
				 Classname ) ->
	wooper_internals:raise_usage_error( "invalid attribute declaration "
		"tuple in the 'class_attributes' define (expecting a size of 2, "
		"3 or 4; got ~B elements).", [ length( Forms ) ], Classname );

register_helper( _OtherAttrForm, _AttributeTable, Classname ) ->
	wooper_internals:raise_usage_error( "invalid attribute declaration in "
		"the 'class_attributes' define (neither an atom nor a tuple).", [],
		Classname ).



% (helper)
register_attribute( AttrNameForm, TypeForm, QualifiersForm, DescriptionForm,
					AttributeTable, Classname ) ->

	AttrName = handle_attribute_name( AttrNameForm, Classname ),

	Type = handle_attribute_type( TypeForm, Classname, AttrName ),

	Qualifiers = handle_attribute_qualifiers( QualifiersForm, Classname,
											  AttrName ),

	Description = handle_attribute_description( DescriptionForm, Classname,
												AttrName ),

	AttrInfo = #attribute_info{ name=AttrName,
								type=Type,
								qualifiers=Qualifiers,
								description=Description },

	case table:has_entry( AttrName, AttributeTable ) of

		true ->
			wooper_internals:raise_usage_error( "multiple declarations for "
				   "class attribute '~ts'.", [ AttrName ], Classname );

		false ->
			table:add_entry( AttrName, AttrInfo, AttributeTable )

	end.



% Returns a clause that is compilable, a list of {AttrName, AttrType,
% AttrQualifier, AttrDescription} quadruplets.
%
% Currently:
%
% - AttrName is an atom (ex: 'color')
% - AttrType is 'undefined' (not transformed in a compilable form yet)
% - AttrQualifier is 'undefined' (no detailed "parsing" yet)
% - AttrDescription is the expected string
%
transform_attribute_getter_form( RawForm ) ->

	%trace_utils:debug_fmt( "Raw attribute form: ~p", [ RawForm ] ),

	% Replaces '{ cons,_, {atom,_,foo},..' by '[ {atom,_foo}, ...':

	ListForm = ast_generation:form_to_list( RawForm ),

	%trace_utils:debug_fmt( "Attribute form as list: ~p", [ ListForm ] ),

	FilteredForm = filter_attribute_forms( ListForm, _Acc=[] ),

	%trace_utils:debug_fmt( "Filtered form: ~p", [ FilteredForm ] ),

	FilteredForm.



% Selects only the attribute names (as forms).
%
% (helper)
%
filter_attribute_forms( _ListForm=[], Acc ) ->

	% Each accumulated element is now in an AST form, only have to convert their
	% list (once reversed to match the original order of attribute
	% declarations):
	%
	ast_generation:list_to_form( lists:reverse( Acc ) );


% Just with an attribute name specified (match to distinguish from a tuple):
filter_attribute_forms( _ListForm=[ Name={ atom, _Line, _Name } | T ], Acc ) ->

	AttrName = filter_name( Name ),

	AttrInfo = convert_to_attribute_info_form( AttrName,
		_AttrType=get_default_type_ast(),
		_AttrQualifier=get_default_qualifiers_ast(),
		_AttrDescription=get_undefined_form() ),

	filter_attribute_forms( T, [ AttrInfo | Acc ] );


% Attribute name and description specified:
filter_attribute_forms( _ListForm=[ { tuple, _Line,
									  [ Name, Description ] } | T ], Acc ) ->

	AttrName = filter_name( Name ),

	AttrDescription = filter_description( Description ),

	AttrInfo = convert_to_attribute_info_form( AttrName,
		_AttrType=get_default_type_ast(),
		_AttrQualifier=get_default_qualifiers_ast(),
		AttrDescription ),

	filter_attribute_forms( T, [ AttrInfo | Acc ] );


% Attribute name, type and description specified:
filter_attribute_forms( _ListForm=[
		  { tuple, _Line, [ Name, Type, Description ] } | T ], Acc ) ->

	AttrName = filter_name( Name ),

	AttrType = filter_type( Type ),

	AttrDescription = filter_description( Description ),

	AttrInfo = convert_to_attribute_info_form( AttrName, AttrType,
		_AttrQualifier=get_default_qualifiers_ast(), AttrDescription ),

	filter_attribute_forms( T, [ AttrInfo | Acc ] );


% Attribute name, type, qualifier and description specified:
filter_attribute_forms( _ListForm=[ { tuple, _Line,
				[ Name, Type, Qualifier, Description ] } | T ], Acc ) ->

	AttrName = filter_name( Name ),

	AttrType = filter_type( Type ),

	AttrQualifier = filter_qualifier( Qualifier ),

	AttrDescription = filter_description( Description ),

	AttrInfo = convert_to_attribute_info_form( AttrName, AttrType,
										AttrQualifier, AttrDescription ),

	filter_attribute_forms( T, [ AttrInfo | Acc ] );


filter_attribute_forms( [ OtherForm | _T ], _Acc ) ->
	wooper_internals:raise_usage_error(
	  "invalid class attribute: ~p", [ OtherForm ] ).



% Per attribute metadata filters:

filter_name( NameForm={ atom, _Line, _AttrName } ) ->
	NameForm;

filter_name( OtherNameForm ) ->
	wooper_internals:raise_usage_error(
	  "invalid name for class attribute: ~p", [ OtherNameForm ] ).



% TO-DO: adopt a type language (ex: a type foo( integer(), bar() ) could be
% translated as { foo, [ { integer, [] }, { bar, [] } ] }, i.e. a type T
% depending on types T1, T2, ..., Tn would be described in terms of
% data-structure as a {T,[T1,T2,..,Tn]} pair.
%
filter_type( _Type ) ->

	% Ex: name() can be represented as: '{call,_,{atom,_,name},[]}.'.

	%trace_utils:debug_fmt( "Not translating type ~p.", [ Type ] ),

	get_default_type_ast().



% TO-DO: check and canonicalise the qualifier spec:
filter_qualifier( _Qualifier ) ->

	% Ex: [ const, protected ] is represented as:
	% {cons,_, {atom,_,const}, {cons,_,{atom,_,protected},{nil,_}}}.

	%trace_utils:debug_fmt( "Not translating qualifier ~p.", [ Qualifier ] ),

	Line = 0,

	% Returning the AST form of []:
	{ nil, Line }.



% Filters the specified form corresponding to an attribute description.
filter_description( AttrDescription={ string, Line, _DescString } ) ->
	% Returning a binary version thereof:
	{ bin, Line, [ { bin_element, Line, AttrDescription, default, default } ] };

filter_description( UnexpectedAttrDescription ) ->
	throw( { unexpected_attribute_description, UnexpectedAttrDescription } ).



% Returns a form element corresponding to 'undefined':
get_undefined_form() ->
	{ atom, _Line=0, 'undefined' }.



% Returns a form element corresponding to the default type:
get_default_type_ast() ->

	Line = 0,

	% Default is any(), encoded as a term as { any, [] }, whose AST is:
	{ tuple, Line, [ { atom, Line, any }, { nil, Line } ] }.



% Returns a form element corresponding to the default qualifiers:
get_default_qualifiers_ast() ->
	% Default is []:
	{ nil, _Line=0 }.




% Converts specified quadruplet into the proper AST form of an attribute_info
% record, knowing that the tuple elements are already in an AST form.
%
convert_to_attribute_info_form( ASTName, ASTType, ASTQualifier,
								ASTDescription ) ->
	Line = 0,
	{ tuple, Line, [ _Tag={ atom, Line, attribute_info }, ASTName, ASTType,
					 ASTQualifier, ASTDescription ] }.



% Checks of attribute meta-data:


% Vetting specified attribute name:
handle_attribute_name( NameForm={ atom, _, _AtomName }, _Classname ) ->
	NameForm;

handle_attribute_name( _OtherForm, Classname ) ->
	wooper_internals:raise_usage_error( "invalid name for class attribute.", [],
										Classname ).



% Vetting specified attribute type.
%
% Currently, for any future use, we store the user-specified type in its
% abstract form; for example, if having declared an attribute of type 'color()',
% the corresponding '{call,_,{atom,_,color}}' form will be stored.
%
handle_attribute_type( _TypeForm=undefined, _Classname, _AttrName ) ->
	undefined;

handle_attribute_type( TypeForm, _Classname, _AttrName )
  when is_tuple( TypeForm ) ->

	%trace_utils:warning_fmt( "Storing attribute type as its raw form:~n  ~p",
	%						 [ TypeForm ] ),

	TypeForm;

% Probably never triggered:
handle_attribute_type( _TypeForm, Classname, AttrName ) ->
	wooper_internals:raise_usage_error(
	  "invalid type for class attribute '~ts'.", [ AttrName ], Classname ).



% Vetting specified attribute qualifier(s):
handle_attribute_qualifiers( _Qualifiers=undefined, _Classname, _AttrName ) ->
	[];

handle_attribute_qualifiers( _Qualifiers={atom,_,none}, _Classname,
							 _AttrName ) ->
	[];

handle_attribute_qualifiers( Qualifiers={cons,_,_H,_T}, Classname, AttrName ) ->

	% We have a list of qualifiers here (as a form):
	QualifierList = ast_generation:form_to_list( Qualifiers ),

	% It could be checked that no initial is specified if a const is.
	[ handle_attribute_qualifier( Q, Classname, AttrName )
	  || Q <- QualifierList ];

% A single qualifier shall be promoted to a list:
handle_attribute_qualifiers( Qualifier, Classname, AttrName ) ->
	[ handle_attribute_qualifier( Qualifier, Classname, AttrName ) ].



% Vetting specified attribute qualifier:
handle_attribute_qualifier( {atom,_,public}, _Classname, _AttrName ) ->
	public;

handle_attribute_qualifier( {atom,_,protected}, _Classname, _AttrName ) ->
	protected;

handle_attribute_qualifier( {atom,_,private}, _Classname, _AttrName ) ->
	private;

handle_attribute_qualifier( {tuple,_,[ {atom,_,initial}, ValueForm ]},
							_Classname, _AttrName ) ->

	Value = ast_value:get_immediate_value( ValueForm ),

	% It should be checked also that Value is of the declared type.
	{ initial, Value };

handle_attribute_qualifier( {tuple,_,[ {atom,_,const}, ValueForm ]},
							_Classname, _AttrName ) ->

	Value = ast_value:get_immediate_value( ValueForm ),

	% It should be checked also that Value is of the declared type.
	{ const, Value };

handle_attribute_qualifier( {atom,_,const}, _Classname, _AttrName ) ->
	const;

handle_attribute_qualifier( {atom,_,Other}, Classname, AttrName ) ->
	wooper_internals:raise_usage_error(
	  "invalid qualifier '~ts' for class attribute '~ts'.", [ Other, AttrName ],
	  Classname );

handle_attribute_qualifier( _UnexpectedForm, Classname, AttrName ) ->
	wooper_internals:raise_usage_error(
	  "invalid qualifier for class attribute '~ts'.", [ AttrName ], Classname ).



% Vetting specified attribute description:
handle_attribute_description( _DescriptionForm=undefined, _Classname,
							  _AttrName ) ->
	undefined;

handle_attribute_description( _DescriptionForm={string,_,Description},
							  _Classname, _AttrName ) ->
	Description;

handle_attribute_description( _DescriptionForm, Classname, AttrName ) ->
	wooper_internals:raise_usage_error(
	  "invalid description (not a string) for class attribute '~p'.",
	  [ AttrName ], Classname ).



% Returns a textual description of specified attribute table.
-spec attributes_to_string( attribute_table() ) -> text_utils:ustring().
attributes_to_string( AttributeTable ) ->

	AttrInfos = table:values( AttributeTable ),

	AttrStrings = [ attribute_to_string( AttrInfo )
					|| AttrInfo <- AttrInfos ],

	case length( AttrStrings ) of

		0 ->
			"no attribute defined";

		L ->
			text_utils:format( "~B attributes defined: ~ts",
				[ L, text_utils:strings_to_sorted_string( AttrStrings ) ] )

	end.



% Returns a textual description of specified attribute information.
-spec attribute_to_string( wooper_info:attribute_info() ) ->
								 text_utils:ustring().
attribute_to_string( #attribute_info{ name=Name,
									  type=TypeForm,
									  qualifiers=Qualifiers,
									  description=Description } ) ->

	TypeString = case TypeForm of

		undefined ->
			"undefined type";

		_ ->
			text_utils:format( "type whose form is ~p", [ TypeForm ] )

	end,

	DescString = case Description of

		undefined ->
			"with no associated description";

		_ ->
			text_utils:format( "whose description is '~ts'", [ Description ] )

	end,

	text_utils:format(
	  "attribute named '~ts' of ~ts, with qualifiers ~w, and ~ts",
	  [ Name, TypeString, Qualifiers, DescString ] ).
