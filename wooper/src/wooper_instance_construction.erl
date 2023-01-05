% Copyright (C) 2014-2023 Olivier Boudeville
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


% @doc Centralises, on behalf of the WOOPER parse transform, the support for
% <b>instance construction</b>.
%
-module(wooper_instance_construction).


-export([ manage_constructors/1 ]).


% For the function_info record:
-include_lib("myriad/include/ast_info.hrl").

% For the class_info record:
-include("wooper_info.hrl").


% Shorthands:

-type file_loc() :: ast_base:file_loc().
-type form_element() :: ast_base:form_element().
-type form_location() :: ast_base:form_location().

-type compose_pair() :: wooper_parse_transform:compose_pair().
-type operator_table() :: wooper_parse_transform:operator_table().
-type classname() :: wooper:classname().


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



% @doc Extracts the constructors found in the specified function table, and
% interprets them to enrich the specified class information.
%
% Returns an updated pair thereof.
%
-spec manage_constructors( compose_pair() ) -> compose_pair().
manage_constructors( { FunctionTable, ClassInfo } ) ->

	% To better report errors:
	ClassEntry = ClassInfo#class_info.class,

	Classname = pair:first( ClassEntry ),

	% First element is a list of {arity(), function_info()} pairs corresponding
	% to the defined constructors (construct/N), while the second element is the
	% input function table obtained once these corresponding entries have been
	% removed:
	%
	{ ConstructPairs, ShrunkFunctionTable } =
		extract_constructors_from( FunctionTable, Classname ),

	case ConstructPairs of

		[] ->
			% Better than throw( no_constructor_found ):
			NewErrors = [ no_constructor_found | ClassInfo#class_info.errors ],
			{ FunctionTable, ClassInfo#class_info{ errors=NewErrors } };

		_ ->
			%trace_utils:debug_fmt( "~B constructor(s) found: ~ts",
			%					   [ length( ConstructPairs ),
			%						 text_utils:strings_to_string(
			%						   [ ast_info:function_info_to_string( FI )
			%							 || { _Arity, FI } <- ConstructPairs ] )
			%					   ] ),

			% Returns {NewFunctionTable, NewClassInfo}:
			manage_new_operators( ConstructPairs, ShrunkFunctionTable,
								  ClassInfo )

	end.



% @doc Returns a list of {arity(), function_info()} pairs and the shrunk
% function table from which they were extracted.
%
% (helper)
%
extract_constructors_from( FunctionTable, Classname ) ->

	% We are looking, among the keys (i.e. function ids), for those matching
	% {construct,N}, knowing that the associated values are function_info():

	FunIdInfos = table:enumerate( FunctionTable ),

	filter_constructors( FunIdInfos, Classname, _AccPairs=[], _AccFunInfos=[] ).



% (helper)
filter_constructors( _FunIdInfos=[], _Classname, AccPairs, AccFunInfos ) ->
	{ AccPairs, table:new( AccFunInfos ) };

% Should a constructor have a spec yet not actually being defined:
filter_constructors( _FunIdInfos=[ { { construct, Arity },
		#function_info{
		   clauses=[],
		   spec={ _Loc, _FunSpec={ attribute, SpecFileLoc, spec, _SpecElems } }
		  } } | _T ], Classname, _AccPairs, _AccFunInfos ) ->
	wooper_internals:raise_usage_error( "the constructor construct/~B "
		"has a spec, yet it has never been defined.", [ Arity ], Classname,
		SpecFileLoc );

% Legit constructor:
filter_constructors( _FunIdInfos=[ { { construct, Arity }, FunInfo } | T ],
					 Classname, AccPairs, AccFunInfos ) ->
	filter_constructors( T, Classname, [ { Arity, FunInfo } | AccPairs ],
						 AccFunInfos );

% 'Other' expected to be {{_NonConstructFunName, Arity}, FunInfo}:
filter_constructors( _FunIdInfos=[ Other | T ], Classname, AccPairs,
					 AccFunInfos ) ->
	filter_constructors( T, Classname, AccPairs, [ Other | AccFunInfos ] ).



% @doc Adds the new operators and all their relevant variations for each of the
% specified constructors construct/N.
%
% Returns {NewFunctionTable, NewClassInfo}.
%
manage_new_operators( _ConstructPairs=[], FunctionTable, ClassInfo ) ->
	{ FunctionTable, ClassInfo };

manage_new_operators( _ConstructPairs=[ { Arity, FunInfo } | T ], FunctionTable,
					  ClassInfo=#class_info{ class={ Classname, _ClassLocForm },
											 constructors=Constructors,
											 new_operators=NewOpTable,
											 debug_mode=IsDebugMode,
											 markers=MarkerTable  } ) ->

	%trace_utils:debug_fmt( "Processing constructor of arity ~B: ~ts",
	%			   [ Arity, ast_info:function_info_to_string( FunInfo ) ] ),

	% Where the generated 'new*' operators (and possibly the constructor) will
	% be exported:
	%
	ExportASTLoc = ast_info:get_default_export_function_location( MarkerTable ),

	% First, for the class developer, exporting a constructor is not mandatory
	% (and generally not to be done); so, if this constructor is not exported,
	% let's do it automatically:
	%
	NewFunInfo = wooper_method_management:ensure_exported( FunInfo,
														   MarkerTable ),


	% Registering this (possibly updated) constructor in the dedicated table:
	NewConstructors = table:add_new_entry( _K=Arity, _V=NewFunInfo,
										   Constructors ),

	% Then, for a constructor of arity N, we have to automatically define and
	% export here following 15 functions, which are all new operator variations
	% V; 7 base ones, each doubled to support whether or not an atomic link is
	% wanted as well between the creator process and the created instance, plus
	% one passive:
	%
	% - V1: new/N-1 and new_link/N-1 (N-1, as no State parameter expected here)
	% - V2: synchronous_new/N-1 and synchronous_new_link/N-1
	% - V3: synchronous_timed_new/N-1 and synchronous_timed_new_link/N-1
	% - V4: remote_new/N and remote_new_link/N
	% - V5: remote_synchronous_new/N and remote_synchronous_new_link/N
	% - V6: remote_synchronisable_new/N and remote_synchronisable_new_link/N
	% - V7: remote_synchronous_timed_new/N and
	%       remote_synchronous_timed_new_link/N
	% - V8: new_passive/N-1

	% Where the generated 'new*' operators will be defined:
	DefinitionASTLoc =
		ast_info:get_default_definition_function_location( MarkerTable ),

	V1OpTable = add_v1_operators( Classname, Arity, ExportASTLoc,
								  DefinitionASTLoc, IsDebugMode, NewOpTable ),

	V2OpTable = add_v2_operators( Classname, Arity, ExportASTLoc,
								  DefinitionASTLoc, IsDebugMode, V1OpTable ),

	V3OpTable = add_v3_operators( Classname, Arity, ExportASTLoc,
								  DefinitionASTLoc, IsDebugMode, V2OpTable ),

	V4OpTable = add_v4_operators( Classname, Arity, ExportASTLoc,
								  DefinitionASTLoc, IsDebugMode, V3OpTable ),

	V5OpTable = add_v5_operators( Classname, Arity, ExportASTLoc,
								  DefinitionASTLoc, IsDebugMode, V4OpTable ),

	V6OpTable = add_v6_operators( Classname, Arity, ExportASTLoc,
								  DefinitionASTLoc, IsDebugMode, V5OpTable ),

	V7OpTable = add_v7_operators( Classname, Arity, ExportASTLoc,
								  DefinitionASTLoc, IsDebugMode, V6OpTable ),

	V8OpTable = add_v8_operators( Classname, Arity, ExportASTLoc,
								  DefinitionASTLoc, IsDebugMode, V7OpTable ),

	FinalClassInfo = ClassInfo#class_info{ constructors=NewConstructors,
										   new_operators=V8OpTable },

	manage_new_operators( T, FunctionTable, FinalClassInfo ).




% @doc Adds the V1 operators, that is new/N-1 and new_link/N-1, by updating the
% specified operator table.
%
-spec add_v1_operators( classname(), arity(), form_location(),
			form_location(), boolean(), operator_table() ) -> operator_table().
add_v1_operators( Classname, Arity, ExportASTLocation, DefinitionASTLoc,
				  IsDebugMode, OperatorTable ) ->

	% Let's create from scratch the corresponding operator definition, export
	% and spec:
	%
	% (Arity N is the one of the corresponding constructor)

	NewArity = Arity - 1,

	%trace_utils:debug_fmt( "Adding {new,new_link}/~B.", [ NewArity ] ),

	% Let's start with new/N-1:

	NewName = new,

	NewId = { NewName, NewArity },

	FileGenLoc = ast_utils:get_generated_code_location(),

	% Its definition is, if N=3 (hence NewArity=2):
	%
	% new( A, B ) ->
	%     spawn( fun() ->
	%			   wooper:construct_and_run( class_Foo, [ A, B ] )
	%            end ).


	% Preparing the form elements involved:


	% For the header of the new function, that is for 'new( A, B ) ->'.
	%
	% Ex: [ {var,0,'Myriad_Param_1'}, {var,0,'Myriad_Param_2'} ]
	%
	HeaderParams = ast_generation:get_header_params( NewArity ),

	SpawnExpr = get_spawn_expression_for( IsDebugMode, FileGenLoc ),

	% For the call to wooper:construct_and_run/2:
	RunCall = get_run_call( FileGenLoc ),

	% For the application of the parameters to the later function,
	% that is for 'wooper:construct_and_run( class_Foo, [ A, B ] )'.
	%
	% Ex: [ {atom,0,class_Foo}, { cons, 0, {var,0,'Myriad_Param_1'},
	% { cons, 0, {var,0,'Myriad_Param_2'}, {nil,0} } } ].
	%
	CallParams = [ {atom,FileGenLoc,Classname},
				   ast_generation:enumerated_variables_to_form( NewArity ) ],

	NewClause = { clause, FileGenLoc, HeaderParams, [],
				  [{ call, FileGenLoc, SpawnExpr,
					 [ {'fun', FileGenLoc,
						{ clauses,
						  [ { clause, FileGenLoc, [], [],
							  [ { call, FileGenLoc, RunCall, CallParams } ]
							}]
						}}]
				   }]},


	% We also generate the corresponding spec, which is in this N=3 example:
	%
	% '-spec new( wooper:construction_parameter(),
	%             wooper:construction_parameter() ) -> pid().':

	% First listing the types of the expected parameters:
	ConstructParamTypes = get_construction_types( NewArity, FileGenLoc ),

	Result = forge_pid_type(),

	NewSpecForm = { attribute, FileGenLoc, spec, { NewId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	NewOpInfo = #function_info{ name=NewName,
								arity=NewArity,
								ast_location=DefinitionASTLoc,
								file_location=FileGenLoc,
								clauses=[ NewClause ],
								% Spec close to definition:
								spec={ DefinitionASTLoc, NewSpecForm },
								callback=false,
								exported=[ ExportASTLocation ] },

	% Now, let's do the same for the new_link counterpart:

	NewLinkName = new_link,

	NewLinkId = { NewLinkName, NewArity },

	% Its definition is, if N=3 (hence NewArity=2):
	%
	% new_link( A, B ) ->
	%     spawn_link( fun() ->
	%		  wooper:construct_and_run( class_Foo, [ A, B ] )
	% end ).

	SpawnLinkExpr = get_spawn_link_expression_for( IsDebugMode, FileGenLoc ),

	NewLinkClause = { clause, FileGenLoc, HeaderParams, [],
					  [{ call, FileGenLoc, SpawnLinkExpr,
						 [ {'fun', FileGenLoc,
							{ clauses,
							  [ { clause, FileGenLoc,[],[],
								  [ { call, FileGenLoc, RunCall, CallParams } ]
								}]
							}}]
					   }]},

	NewLinkSpecForm = { attribute, FileGenLoc, spec, { NewLinkId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	NewLinkOpInfo = #function_info{ name=NewLinkName,
									arity=NewArity,
									ast_location=DefinitionASTLoc,
									file_location=FileGenLoc,
									clauses=[ NewLinkClause ],
									% Spec close to definition:
									spec={ DefinitionASTLoc, NewLinkSpecForm },
									callback=false,
									exported=[ ExportASTLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:add_new_entries( [ { NewId, NewOpInfo },
							 { NewLinkId, NewLinkOpInfo } ], OperatorTable ).



% @doc Adds the V2 operators, that is synchronous_new/N-1 and
% synchronous_new_link/N-1, by updating the specified operator table; they
% correspond roughly to the V1 ones, augmented with a receive clause.
%
-spec add_v2_operators( classname(), arity(), form_location(),
			form_location(), boolean(), operator_table() ) -> operator_table().
add_v2_operators( Classname, Arity, ExportASTLocation, DefinitionASTLoc,
				  IsDebugMode, OperatorTable ) ->

	% Same strategy as add_v1_operators/5:

	SyncNewArity = Arity - 1,

	%trace_utils:debug_fmt( "Adding {synchronous_new,synchronous_new_link}/~B.",
	%					   [ SyncNewArity ] ),

	% Let's start with:
	SyncNewName = synchronous_new,

	FileGenLoc = ast_utils:get_generated_code_location(),

	% Its definition is, if N=3 (hence SyncNewArity=2):
	% ('S' standing for statement)
	%
	% synchronous_new( A, B ) ->
	% [S1]  CreatorPid = self(),
	% [S2]  SpawnedPid = spawn( fun() ->
	%		  wooper:construct_and_run_synchronous( class_Foo, [ A, B ],
	%                                               CreatorPid )
	%                       end ),
	%
	% [S3]  receive
	%
	%		  { spawn_successful, SpawnedPid } ->
	%			  SpawnedPid
	%
	% end.


	S1 = { match, FileGenLoc, {var,FileGenLoc,'CreatorPid'},
		   { call, FileGenLoc, {atom,FileGenLoc,self}, [] } },

	SpawnExpr = get_spawn_expression_for( IsDebugMode, FileGenLoc ),

	% For the call to wooper:construct_and_run_synchronous/2:
	SyncRunCall = get_sync_run_call( FileGenLoc ),

	SyncNewId = { SyncNewName, SyncNewArity },

	CallParams = [ {atom,FileGenLoc,Classname},
				   ast_generation:enumerated_variables_to_form( SyncNewArity ),
				   {var,FileGenLoc,'CreatorPid'} ],

	S2 = { match, FileGenLoc, {var,FileGenLoc,'SpawnedPid'},
		   { call, FileGenLoc, SpawnExpr,
			[ {'fun',FileGenLoc,
			  { clauses,
				[ {clause,FileGenLoc,[],[],
				   [ {call,FileGenLoc,SyncRunCall,CallParams} ] } ] } } ] } },

	S3 = get_receive( FileGenLoc ),

	HeaderParams = ast_generation:get_header_params( SyncNewArity ),

	SyncNewClause = { clause, FileGenLoc, HeaderParams, [],
					  [ S1, S2, S3 ] },


	% Then, its spec:

	ConstructParamTypes = get_construction_types( SyncNewArity, FileGenLoc ),

	Result = forge_pid_type(),

	SyncNewSpecForm = { attribute, FileGenLoc, spec, { SyncNewId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	SyncNewOpInfo = #function_info{ name=SyncNewName,
									arity=SyncNewArity,
									ast_location=DefinitionASTLoc,
									file_location=FileGenLoc,
									clauses=[ SyncNewClause ],
									spec={ DefinitionASTLoc, SyncNewSpecForm },
									callback=false,
									exported=[ ExportASTLocation ] },


	% Next, roughly the same but with a link:

	SpawnLinkExpr = get_spawn_link_expression_for( IsDebugMode, FileGenLoc ),

	S2Link = { match, FileGenLoc, {var,FileGenLoc,'SpawnedPid'},
			   { call, FileGenLoc, SpawnLinkExpr,
				 [ {'fun',FileGenLoc,
					{ clauses,
					  [ {clause,FileGenLoc,[],[],
						 [ {call,FileGenLoc,SyncRunCall,CallParams} ]
						} ] } } ] } },

	SyncNewLinkClause = { clause, FileGenLoc, HeaderParams, [],
						  [ S1, S2Link, S3 ] },

	SyncNewLinkName = synchronous_new_link,

	SyncNewLinkId = { SyncNewLinkName, SyncNewArity },

	SyncNewLinkSpecForm = { attribute, FileGenLoc, spec, { SyncNewLinkId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	SyncNewLinkOpInfo = #function_info{
						   name=SyncNewLinkName,
						   arity=SyncNewArity,
						   ast_location=DefinitionASTLoc,
						   file_location=FileGenLoc,
						   clauses=[ SyncNewLinkClause ],
						   spec={ DefinitionASTLoc, SyncNewLinkSpecForm },
						   callback=false,
						   exported=[ ExportASTLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:add_new_entries( [ { SyncNewId, SyncNewOpInfo },
		{ SyncNewLinkId, SyncNewLinkOpInfo } ], OperatorTable ).



% @doc Adds the V3 operators, that is synchronous_timed_new/N-1 and
% synchronous_timed_new_link/N-1, by updating the specified operator table; they
% correspond roughly to the V2 ones, augmented with an after clause.
%
-spec add_v3_operators( classname(), arity(), form_location(),
			form_location(), boolean(), operator_table() ) -> operator_table().
add_v3_operators( Classname, Arity, ExportASTLocation, DefinitionASTLoc,
				  IsDebugMode, OperatorTable ) ->

	% Same strategy as add_v2_operators/5:

	OpArity = Arity - 1,

	%trace_utils:debug_fmt(
	%  "Adding {synchronous_timed_new,synchronous_timed_new_link}/~B.",
	%  [ OpArity ] ),

	% Let's start with:
	OpNewName = synchronous_timed_new,

	OpNewId = { OpNewName, OpArity },

	FileGenLoc = ast_utils:get_generated_code_location(),

	% Its definition is, if N=3 (hence OpArity=2):
	% ('S' standing for statement)
	%
	% synchronous_timed_new( A, B ) ->
	% [S1]  CreatorPid = self(),
	% [S2]  SpawnedPid = spawn( fun() ->
	%		  wooper:construct_and_run_synchronous( class_Foo, [ A, B ],
	%                                               CreatorPid )
	%                           end ),
	%
	% [S3]  receive
	%
	%		  { spawn_successful, SpawnedPid } ->
	%			  SpawnedPid
	%
	% [S4]  after 5000 ->
	%
	%			throw( { synchronous_time_out, ?MODULE } )
	%
	%       end.

	S1 = { match, FileGenLoc, {var,FileGenLoc,'CreatorPid'},
		   { call, FileGenLoc, {atom,FileGenLoc,self}, [] } },


	SpawnExpr = get_spawn_expression_for( IsDebugMode, FileGenLoc ),

	% For the call to wooper:construct_and_run_synchronous/2:
	SyncRunCall = get_sync_run_call( FileGenLoc ),

	CallParams = [ {atom,FileGenLoc,Classname},
				   ast_generation:enumerated_variables_to_form( OpArity ),
				   {var,FileGenLoc,'CreatorPid'} ],

	S2 = { match, FileGenLoc, {var,FileGenLoc,'SpawnedPid'},
		   { call, FileGenLoc, SpawnExpr,
			 [ {'fun', FileGenLoc,
				{ clauses,
				  [ { clause,FileGenLoc,[],[],
					  [ { call,FileGenLoc,SyncRunCall,CallParams } ]
					} ] } } ] } },

	S3 = get_local_receive_with_after( Classname, IsDebugMode, FileGenLoc ),

	HeaderParams = ast_generation:get_header_params( OpArity ),

	OpNewClause = { clause, FileGenLoc, HeaderParams, [], [ S1, S2, S3 ] },


	% Then, its spec:

	ConstructParamTypes = get_construction_types( OpArity, FileGenLoc ),

	Result = forge_pid_type(),

	OpSpecForm = { attribute, FileGenLoc, spec, { OpNewId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								ast_location=DefinitionASTLoc,
								file_location=FileGenLoc,
								clauses=[ OpNewClause ],
								spec={ DefinitionASTLoc, OpSpecForm },
								callback=false,
								exported=[ ExportASTLocation ] },


	% Next, roughly the same but with a link:

	OpNewLinkName = synchronous_timed_new_link,

	SpawnLinkExpr = get_spawn_link_expression_for( IsDebugMode, FileGenLoc ),

	S2Link = { match, FileGenLoc, {var,FileGenLoc,'SpawnedPid'},
			   { call, FileGenLoc, SpawnLinkExpr,
				 [ {'fun',FileGenLoc,
					{ clauses,
					  [ {clause,FileGenLoc,[],[],
						 [ {call,FileGenLoc,SyncRunCall,CallParams } ]
						} ] } } ] } },

	OpNewLinkClause = { clause, FileGenLoc, HeaderParams, [],
						[ S1, S2Link, S3 ] },

	OpNewLinkId = { OpNewLinkName, OpArity },

	OpLinkSpecForm = { attribute, FileGenLoc, spec, { OpNewLinkId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									ast_location=DefinitionASTLoc,
									file_location=FileGenLoc,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionASTLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportASTLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:add_new_entries( [ { OpNewId, OpNewInfo },
		{ OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% @doc Adds the V4 operators, that is remote_new/N and remote_new_link/N by
% updating the specified operator table; they correspond roughly to the V1 ones,
% augmented with a node specification at the spawn call.
%
-spec add_v4_operators( classname(), arity(), form_location(),
			form_location(), boolean(), operator_table() ) -> operator_table().
add_v4_operators( Classname, Arity, ExportASTLocation, DefinitionASTLoc,
				  IsDebugMode, OperatorTable ) ->

	% Overall arity (with node()):
	OpArity = Arity,

	% For the actual parameters (without node()):
	ArgArity = OpArity - 1,

	%trace_utils:debug_fmt( "Adding {remote_new,remote_new_link}/~B.",
	%					   [ OpArity ] ),

	% Let's start with:
	OpNewName = remote_new,

	OpNewId = { OpNewName, OpArity },

	FileGenLoc = ast_utils:get_generated_code_location(),

	% Its definition is, if N=3 (hence OpArity=3):
	% ('S' standing for statement)
	%
	% remote_new( Node, A, B ) ->
	% [S1]  spawn( Node, fun() ->
	%		  wooper:construct_and_run( class_Foo, [ A, B ] )
	%                    end ).

	% First element must be the node parameter:
	HeaderParams = [ {var,FileGenLoc,'Node'}
					 | ast_generation:get_header_params( ArgArity ) ],

	SpawnExpr = get_spawn_expression_for( IsDebugMode, FileGenLoc ),

	RunCall = get_run_call( FileGenLoc ),

	CallParams = [ {atom,FileGenLoc,Classname},
				   ast_generation:enumerated_variables_to_form( ArgArity ) ],

	OpNewClause = { clause, FileGenLoc, HeaderParams, [],
					[{ call, FileGenLoc, SpawnExpr,
					   [ {var,FileGenLoc,'Node'},
						 {'fun', FileGenLoc,
						  { clauses,
							[ { clause, FileGenLoc,[],[],
								[ { call, FileGenLoc, RunCall, CallParams } ]
							  }]
						  }}]
					 }]},


	% Now the spec, which is here:
	%
	% -spec remote_new( net_utils:atom_node_name(),
	%                   wooper:construction_parameter(),
	%                   wooper:construction_parameter() ) -> pid().

	NodeType = forge_node_type( FileGenLoc ),

	ConstructParamTypes =
		[ NodeType | get_construction_types( ArgArity, FileGenLoc ) ],

	Result = forge_pid_type(),

	OpSpecForm = { attribute, FileGenLoc, spec, { OpNewId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								ast_location=DefinitionASTLoc,
								file_location=FileGenLoc,
								clauses=[ OpNewClause ],
								spec={ DefinitionASTLoc, OpSpecForm },
								callback=false,
								exported=[ ExportASTLocation ] },


	% Next, roughly the same but with a link:

	OpNewLinkName = remote_new_link,

	SpawnLinkExpr = get_spawn_link_expression_for( IsDebugMode, FileGenLoc ),

	OpNewLinkClause = { clause, FileGenLoc, HeaderParams, [],
					[{ call, FileGenLoc, SpawnLinkExpr,
					   [ {var,FileGenLoc,'Node'},
						 {'fun', FileGenLoc,
						  { clauses,
							[ { clause, FileGenLoc,[],[],
								[ { call, FileGenLoc, RunCall, CallParams } ]
							  }]
						  }}]
					 }]},

	OpNewLinkId = { OpNewLinkName, OpArity },

	OpLinkSpecForm = { attribute, FileGenLoc, spec, { OpNewLinkId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									ast_location=DefinitionASTLoc,
									file_location=FileGenLoc,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionASTLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportASTLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:add_new_entries( [ { OpNewId, OpNewInfo },
		{ OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% @doc Adds the V5 operators, that is remote_synchronous_new/N and
% remote_synchronous_new_link/N, by updating the specified operator table; they
% correspond roughly to the V4 ones, augmented with a synchronous variant and a
% receive clause.
%
-spec add_v5_operators( classname(), arity(), form_location(),
			form_location(), boolean(), operator_table() ) -> operator_table().
add_v5_operators( Classname, Arity, ExportASTLocation, DefinitionASTLoc,
				  IsDebugMode, OperatorTable ) ->

	% Overall arity (with node()):
	OpArity = Arity,

	% For the actual parameters (without node()):
	ArgArity = OpArity - 1,

	%trace_utils:debug_fmt(
	%  "Adding {remote_synchronous_new,remote_synchronous_new_link}/~B.",
	%  [ OpArity ] ),

	% Let's start with:
	OpNewName = remote_synchronous_new,

	OpNewId = { OpNewName, OpArity },

	FileGenLoc = ast_utils:get_generated_code_location(),

	% Its definition is, if N=3 (hence OpArity=3):
	% ('S' standing for statement)
	%
	% remote_synchronous_new( Node, A, B ) ->
	%
	% [S1] CreatorPid = self(),
	% [S2] SpawnedPid = spawn( Node, fun() ->
	%					   wooper:construct_and_run_synchronous(
	%									 [ A, B ], CreatorPid )
	%								 end ),
	% [S3] receive
	%		  { spawn_successful, SpawnedPid } ->
	%			  SpawnedPid
	%	   end.

	S1 = { match, FileGenLoc, {var,FileGenLoc,'CreatorPid'},
		   { call, FileGenLoc, {atom,FileGenLoc,self}, [] } },

	SpawnExpr = get_spawn_expression_for( IsDebugMode, FileGenLoc ),

	SyncRunCall = get_sync_run_call( FileGenLoc ),

	CallParams = [ {atom,FileGenLoc,Classname},
				   ast_generation:enumerated_variables_to_form( ArgArity ),
				   {var,FileGenLoc,'CreatorPid'} ],

	S2 = { match, FileGenLoc, {var,FileGenLoc,'SpawnedPid'},
		   { call, FileGenLoc, SpawnExpr,
			 [ {var,FileGenLoc,'Node'},
			   {'fun', FileGenLoc,
				{ clauses,
				  [ { clause,FileGenLoc,[],[],
					  [ { call,FileGenLoc,SyncRunCall,CallParams },
						{var,FileGenLoc,'CreatorPid'}
					  ] } ] } } ] } },

	S3 = get_receive( FileGenLoc ),

	% First element must be the node parameter:
	HeaderParams = [ {var,FileGenLoc,'Node'}
					 | ast_generation:get_header_params( ArgArity ) ],

	OpNewClause = { clause, FileGenLoc, HeaderParams, [],
					  [ S1, S2, S3 ] },


	% Now the spec, which is:
	%
	% -spec remote_synchronous_new( net_utils:atom_node_name(),
	%                   wooper:construction_parameter(),
	%                   wooper:construction_parameter() ) -> pid().

	NodeType = forge_node_type( FileGenLoc ),

	ConstructParamTypes =
		[ NodeType | get_construction_types( ArgArity, FileGenLoc ) ],

	Result = forge_pid_type(),

	OpSpecForm = { attribute, FileGenLoc, spec, { OpNewId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								ast_location=DefinitionASTLoc,
								file_location=FileGenLoc,
								clauses=[ OpNewClause ],
								spec={ DefinitionASTLoc, OpSpecForm },
								callback=false,
								exported=[ ExportASTLocation ] },


	% Next, roughly the same but with a link:

	OpNewLinkName = remote_synchronous_new_link,

	SpawnLinkExpr = get_spawn_link_expression_for( IsDebugMode, FileGenLoc ),

	S2Link = { match, FileGenLoc, {var,FileGenLoc,'SpawnedPid'},
			   { call, FileGenLoc, SpawnLinkExpr,
				 [ {var,FileGenLoc,'Node'},
				   {'fun', FileGenLoc,
					{ clauses,
					  [ { clause,FileGenLoc,[],[],
						  [ { call,FileGenLoc,SyncRunCall,CallParams },
							{var,FileGenLoc,'CreatorPid'}
						  ] } ] } } ] } },

	OpNewLinkClause = { clause, FileGenLoc, HeaderParams, [],
						[ S1, S2Link, S3 ] },

	OpNewLinkId = { OpNewLinkName, OpArity },


	OpLinkSpecForm = { attribute, FileGenLoc, spec, { OpNewLinkId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									ast_location=DefinitionASTLoc,
									file_location=FileGenLoc,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionASTLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportASTLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:add_new_entries( [ { OpNewId, OpNewInfo },
		{ OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% @doc Adds the V6 operators, that is remote_synchronisable_new/N and
% remote_synchronisable_new_link/N, by updating the specified operator table;
% they correspond roughly to the V5 ones, except there is no integrated receive,
% as it is left at the hand of the user.
%
-spec add_v6_operators( classname(), arity(), form_location(),
			form_location(), boolean(), operator_table() ) -> operator_table().
add_v6_operators( Classname, Arity, ExportASTLocation, DefinitionASTLoc,
				  IsDebugMode, OperatorTable ) ->

	% Overall arity (with node()):
	OpArity = Arity,

	% For the actual parameters (without node()):
	ArgArity = OpArity - 1,

	%trace_utils:debug_fmt(
	%  "Adding {remote_synchronisable_new,remote_synchronisable_new_link}/~B.",
	%  [ OpArity ] ),

	% Let's start with:
	OpNewName = remote_synchronisable_new,

	OpNewId = { OpNewName, OpArity },

	FileGenLoc = ast_utils:get_generated_code_location(),

	% Its definition is, if N=3 (hence OpArity=3):
	% ('S' standing for statement)
	%
	% remote_synchronisable_new( Node, A, B ) ->
	%
	% [S1] CreatorPid = self(),
	% [S2] spawn( Node, fun() ->
	%					   wooper:construct_and_run_synchronous(
	%									 [ A, B ], CreatorPid )
	%	   end ).

	S1 = { match, FileGenLoc, {var,FileGenLoc,'CreatorPid'},
		   { call, FileGenLoc, {atom,FileGenLoc,self}, [] } },

	SpawnExpr = get_spawn_expression_for( IsDebugMode, FileGenLoc ),

	SyncRunCall = get_sync_run_call( FileGenLoc ),

	CallParams = [ {atom,FileGenLoc,Classname},
				   ast_generation:enumerated_variables_to_form( ArgArity ),
				   {var,FileGenLoc,'CreatorPid'} ],

	S2 = { call, FileGenLoc, SpawnExpr,
			 [ {var,FileGenLoc,'Node'},
			   {'fun', FileGenLoc,
				{ clauses,
				  [ { clause,FileGenLoc,[],[],
					  [ { call,FileGenLoc,SyncRunCall,CallParams },
						{var,FileGenLoc,'CreatorPid'}
					  ] } ] } } ] },

	% First element must be the node parameter:
	HeaderParams = [ {var,FileGenLoc,'Node'}
					 | ast_generation:get_header_params( ArgArity ) ],

	OpNewClause = { clause, FileGenLoc, HeaderParams, [], [ S1, S2 ] },


	% Now the spec, which is:
	%
	% -spec remote_synchronisable_new( net_utils:atom_node_name(),
	%                   wooper:construction_parameter(),
	%                   wooper:construction_parameter() ) -> pid().

	NodeType = forge_node_type( FileGenLoc ),

	ConstructParamTypes =
		[ NodeType | get_construction_types( ArgArity, FileGenLoc ) ],

	Result = forge_pid_type(),

	OpSpecForm = { attribute, FileGenLoc, spec, { OpNewId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								ast_location=DefinitionASTLoc,
								file_location=FileGenLoc,
								clauses=[ OpNewClause ],
								spec={ DefinitionASTLoc, OpSpecForm },
								callback=false,
								exported=[ ExportASTLocation ] },


	% Next, roughly the same but with a link:

	SpawnLinkExpr = get_spawn_link_expression_for( IsDebugMode, FileGenLoc ),

	OpNewLinkName = remote_synchronisable_new_link,

	S2Link = { call, FileGenLoc, SpawnLinkExpr,
				 [ {var,FileGenLoc,'Node'},
				   {'fun', FileGenLoc,
					{ clauses,
					  [ { clause,FileGenLoc,[],[],
						  [ { call,FileGenLoc,SyncRunCall,CallParams },
							{var,FileGenLoc,'CreatorPid'}
						  ] } ] } } ] },

	OpNewLinkClause = { clause, FileGenLoc, HeaderParams, [], [ S1, S2Link ] },

	OpNewLinkId = { OpNewLinkName, OpArity },

	OpLinkSpecForm = { attribute, FileGenLoc, spec, { OpNewLinkId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									ast_location=DefinitionASTLoc,
									file_location=FileGenLoc,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionASTLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportASTLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:add_new_entries( [ { OpNewId, OpNewInfo },
		{ OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% @doc Adds the V7 operators, that is remote_synchronous_timed_new/N and
% remote_synchronous_timed_new/N, by updating the specified operator table; they
% correspond roughly to a version of V5 with an additional after clause.
%
-spec add_v7_operators( classname(), arity(), form_location(),
			form_location(), boolean(), operator_table() ) -> operator_table().
add_v7_operators( Classname, Arity, ExportASTLocation, DefinitionASTLoc,
				  IsDebugMode, OperatorTable ) ->

	% Overall arity (with node()):
	OpArity = Arity,

	% For the actual parameters (without node()):
	ArgArity = OpArity - 1,

	%trace_utils:debug_fmt( "Adding {remote_synchronous_timed_new,"
	%    "remote_synchronous_timed_new_link}/~B.", [ OpArity ] ),

	% Let's start with:
	OpNewName = remote_synchronous_timed_new,

	OpNewId = { OpNewName, OpArity },

	FileGenLoc = ast_utils:get_generated_code_location(),

	% Its definition is, if N=3 (hence OpArity=3):
	% ('S' standing for statement)
	%
	% remote_synchronous_timed_new( Node, A, B ) ->
	%
	% [S1] CreatorPid = self(),
	% [S2] SpawnedPid = spawn( Node, fun() ->
	%					   wooper:construct_and_run_synchronous(
	%									 [ A, B ], CreatorPid )
	%								 end ),
	% [S3] receive
	%		  { spawn_successful, SpawnedPid } ->
	%			  SpawnedPid
	%      after TimeOut ->
	%		  throw( { remote_synchronous_time_out, Node, Classname } )
	%      end.

	S1 = { match, FileGenLoc, {var,FileGenLoc,'CreatorPid'},
		   { call, FileGenLoc, {atom,FileGenLoc,self}, [] } },

	SpawnExpr = get_spawn_expression_for( IsDebugMode, FileGenLoc ),

	SyncRunCall = get_sync_run_call( FileGenLoc ),

	CallParams = [ {atom,FileGenLoc,Classname},
				   ast_generation:enumerated_variables_to_form( ArgArity ),
				   {var,FileGenLoc,'CreatorPid'} ],

	S2 = { match, FileGenLoc, {var,FileGenLoc,'SpawnedPid'},
		   { call, FileGenLoc, SpawnExpr,
			 [ {var,FileGenLoc,'Node'},
			   {'fun', FileGenLoc,
				{ clauses,
				  [ { clause,FileGenLoc,[],[],
					  [ { call,FileGenLoc,SyncRunCall,CallParams },
						{var,FileGenLoc,'CreatorPid'}
					  ] } ] } } ] } },

	S3 = get_remote_receive_with_after( Classname, IsDebugMode, FileGenLoc ),

	% First element must be the node parameter:
	HeaderParams = [ {var,FileGenLoc,'Node'}
					 | ast_generation:get_header_params( ArgArity ) ],

	OpNewClause = { clause, FileGenLoc, HeaderParams, [], [ S1, S2, S3 ] },


	% Now the spec, which is:
	%
	% -spec remote_synchronous_timed_new( net_utils:atom_node_name(),
	%                   wooper:construction_parameter(),
	%                   wooper:construction_parameter() ) -> pid().

	NodeType = forge_node_type( FileGenLoc ),

	ConstructParamTypes =
		[ NodeType | get_construction_types( ArgArity, FileGenLoc ) ],

	Result = forge_pid_type(),

	OpSpecForm = { attribute, FileGenLoc, spec, { OpNewId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewInfo = #function_info{ name=OpNewName,
								arity=OpArity,
								ast_location=DefinitionASTLoc,
								file_location=FileGenLoc,
								clauses=[ OpNewClause ],
								spec={ DefinitionASTLoc, OpSpecForm },
								callback=false,
								exported=[ ExportASTLocation ] },


	% Next, roughly the same but with a link:

	OpNewLinkName = remote_synchronous_timed_new_link,

	SpawnLinkExpr = get_spawn_link_expression_for( IsDebugMode, FileGenLoc ),

	S2Link = { match, FileGenLoc, {var,FileGenLoc,'SpawnedPid'},
			   { call, FileGenLoc, SpawnLinkExpr,
				 [ {var,FileGenLoc,'Node'},
				   {'fun', FileGenLoc,
					{ clauses,
					  [ { clause,FileGenLoc,[],[],
						  [ { call,FileGenLoc,SyncRunCall,CallParams },
							{var,FileGenLoc,'CreatorPid'}
						  ] } ] } } ] } },

	OpNewLinkClause = { clause, FileGenLoc, HeaderParams, [],
						[ S1, S2Link, S3 ] },

	OpNewLinkId = { OpNewLinkName, OpArity },

	OpLinkSpecForm = { attribute, FileGenLoc, spec, { OpNewLinkId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes }, Result ]
		 } ] } },

	OpNewLinkInfo = #function_info{ name=OpNewLinkName,
									arity=OpArity,
									ast_location=DefinitionASTLoc,
									file_location=FileGenLoc,
									clauses=[ OpNewLinkClause ],
									spec={ DefinitionASTLoc, OpLinkSpecForm },
									callback=false,
									exported=[ ExportASTLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:add_new_entries( [ { OpNewId, OpNewInfo },
		{ OpNewLinkId, OpNewLinkInfo } ], OperatorTable ).



% @doc Adds the V8 operator, that is new_passive/N-1 (no other variation makes
% sense).
%
-spec add_v8_operators( classname(), arity(), form_location(),
			form_location(), boolean(), operator_table() ) -> operator_table().
add_v8_operators( Classname, Arity, ExportASTLocation, DefinitionASTLoc,
				  _IsDebugMode, OperatorTable ) ->

	% Same strategy as add_v1_operators/5:

	PassiveNewArity = Arity - 1,

	%trace_utils:debug_fmt( "Adding new_passive/~B.", [ PassiveNewArity ] ),

	PassiveNewName = new_passive,

	PassiveNewId = { PassiveNewName, PassiveNewArity },

	FileGenLoc = ast_utils:get_generated_code_location(),

	% Its definition is, if N=3 (hence PassiveNewArity=2):
	% ('S' standing for statement)
	%
	% new_passive( A, B ) ->
	% [S1] wooper:construct_passive( class_Foo, [ A, B ] ).

	% For the call to wooper:construct_passive/2:
	PassiveRunCall = { remote, FileGenLoc, {atom,FileGenLoc,wooper},
					   {atom,FileGenLoc,construct_passive} },

	VarForm = ast_generation:enumerated_variables_to_form( PassiveNewArity ),

	CallParams = [ {atom,FileGenLoc,Classname}, VarForm ],

	S1 = { call, FileGenLoc, PassiveRunCall, CallParams },

	HeaderParams = ast_generation:get_header_params( PassiveNewArity ),

	PassiveNewClause = { clause, FileGenLoc, HeaderParams, [], [ S1 ] },


	% Then, its spec:

	ConstructParamTypes = get_construction_types( PassiveNewArity, FileGenLoc ),

	PassiveInstanceType = forge_passive_instance_type( FileGenLoc ),

	PassiveNewSpecForm = { attribute, FileGenLoc, spec, { PassiveNewId,
	   [ { type, FileGenLoc, 'fun',
		   [ { type, FileGenLoc, product, ConstructParamTypes },
			 _Result=PassiveInstanceType ]
		 } ] } },

	PassiveNewOpInfo = #function_info{ name=PassiveNewName,
									   arity=PassiveNewArity,
									   ast_location=DefinitionASTLoc,
									   file_location=FileGenLoc,
									   clauses=[ PassiveNewClause ],
									   spec={ DefinitionASTLoc,
											  PassiveNewSpecForm },
									   callback=false,
									   exported=[ ExportASTLocation ] },

	% Ensure not already defined (ex: by an unwary user):
	table:add_new_entry(  PassiveNewId, PassiveNewOpInfo, OperatorTable ).



% @doc Returns the form element corresponding to pid().
forge_pid_type() ->
	ast_type:forge_pid_type().


% @doc Returns the form element corresponding to net_utils:atom_node_name().
forge_node_type( FileGenLoc ) ->
	% Corresponds to node():
	ast_type:forge_remote_type( _ModuleName=net_utils, _TypeName=atom_node_name,
								_TypeVars=[], FileGenLoc ).


% @doc Returns the form element corresponding to wooper:passive_instance().
forge_passive_instance_type( FileGenLoc ) ->
	ast_type:forge_remote_type( _ModuleName=wooper, _TypeName=passive_instance,
								_TypeVars=[], FileGenLoc ).



% @doc Returns the form element corresponding to wooper:construct_and_run/2.
-spec get_run_call( file_loc() ) -> form_element().
get_run_call( FileGenLoc ) ->
	{ remote, FileGenLoc, {atom,FileGenLoc,wooper},
	  {atom,FileGenLoc,construct_and_run} }.



% @doc Returns the form element corresponding to
% wooper:construct_and_run_synchronous/2.
%
-spec get_sync_run_call( file_loc() ) -> form_element().
get_sync_run_call( FileGenLoc ) ->
	{ remote, FileGenLoc, {atom,FileGenLoc,wooper},
	  {atom,FileGenLoc,construct_and_run_synchronous} }.



% @doc Returns the form element corresponding to a receive.
%
% Like:
% ```
%receive
%
%	{spawn_successful, SpawnedPid} ->
%		SpawnedPid
%
% end
% '''
%
-spec get_receive( file_loc() ) -> form_element().
get_receive( FileGenLoc ) ->
	{ 'receive', FileGenLoc, [
		{ clause, FileGenLoc, [ {tuple,FileGenLoc,
								 [ {atom,FileGenLoc,spawn_successful},
								   {var,FileGenLoc,'SpawnedPid'} ] } ],
		  [], [ {var,FileGenLoc,'SpawnedPid'} ] } ] }.



% @doc Returns the form element corresponding to a fixed-timed receive.
%
% Like:
% ```
% receive
%
%	{spawn_successful, SpawnedPid} ->
%		SpawnedPid
%
% after 5000 ->
%
%	throw( { synchronous_time_out, ?MODULE } )
%
% end
% '''
%
-spec get_local_receive_with_after( basic_utils:module_name(), boolean(),
									file_loc() ) -> form_element().
get_local_receive_with_after( ModuleName, IsDebugMode, FileGenLoc ) ->

	TimeOut = wooper:get_synchronous_time_out( IsDebugMode ),

	{ 'receive', FileGenLoc, [
		{ clause, FileGenLoc, [ {tuple,FileGenLoc,
								 [ {atom,FileGenLoc,spawn_successful},
								   {var,FileGenLoc,'SpawnedPid'} ] } ],
		  [], [ {var,FileGenLoc,'SpawnedPid'} ] } ],
	  {integer,FileGenLoc,TimeOut},
	  [ { call, FileGenLoc, {atom,FileGenLoc,throw},
		   [ { tuple, FileGenLoc,
			   [ {atom,FileGenLoc,synchronous_time_out},
				 {atom,FileGenLoc,ModuleName} ] } ] } ] }.



% @doc Returns the form element corresponding to a user-defined timed receive.
%
% Like:
% ```
%
% receive
%
%	{spawn_successful, SpawnedPid} ->
%		SpawnedPid
%
% after TimeOut  ->
%
%	throw( {remote_synchronous_time_out, node(), ?MODULE})
%
% end
% '''
%
-spec get_remote_receive_with_after( basic_utils:module_name(), boolean(),
									 file_loc() ) -> form_element().
get_remote_receive_with_after( ModuleName, IsDebugMode, FileGenLoc ) ->

	TimeOut = wooper:get_synchronous_time_out( IsDebugMode ),

	{ 'receive', FileGenLoc, [
		{ clause, FileGenLoc, [ {tuple,FileGenLoc,
								 [ {atom,FileGenLoc,spawn_successful},
								   {var,FileGenLoc,'SpawnedPid'} ] } ],
		  [], [ {var,FileGenLoc,'SpawnedPid'} ] } ],
	  {integer,FileGenLoc,TimeOut},
	  [ { call, FileGenLoc, {atom,FileGenLoc,throw},
		   [ { tuple, FileGenLoc,
			   [ {atom,FileGenLoc,remote_synchronous_time_out},
				 {var,FileGenLoc,'Node'},
				 {atom,FileGenLoc,ModuleName} ] } ] } ] }.



% @doc Returns a list of adequate types for the specified number of construction
% parameters, that is of type wooper:construction_parameter().
%
-spec get_construction_types( basic_utils:count(), file_loc() ) ->
									form_element().
get_construction_types( Count, FileGenLoc ) ->
	Type = { remote_type, FileGenLoc, [ {atom,FileGenLoc,wooper},
				{atom,FileGenLoc,construction_parameter}, [] ] },
	lists:duplicate( Count, Type ).



% @doc Returns the spawn expression corresponding to the execution mode (target
% is either development or production, resulting in debug mode being activated
% or not).
%
% Corresponds to Myriad's spawn_utils.hrl (the myriad_spawn* macros).
%
-spec get_spawn_expression_for( boolean(), file_loc() ) -> form_element().
get_spawn_expression_for( _IsDebugMode=true, FileGenLoc ) ->

	% To test:
	%{ remote, FileGenLoc, { atom, FileGenLoc, non_existing },
	%  { atom, FileGenLoc, spawn } };

	%{ remote, FileGenLoc, { atom, FileGenLoc, proc_lib },
	%  { atom, FileGenLoc, spawn } };

	% We switched back to a basic spawn (see Myriad's spawn_utils.hrl), as we
	% felt the extra information in error messages was counter-productive:
	%
	{ atom, FileGenLoc, spawn };


get_spawn_expression_for( _IsDebugMode=false, FileGenLoc ) ->
	{ atom, FileGenLoc, spawn }.



% @doc Returns the spawn_link expression corresponding to the execution mode
% (target is either development or production, resulting in debug mode being
% activated or not).
%
% Corresponds to Myriad's spawn_utils.hrl (the myriad_spawn* macros).
%
-spec get_spawn_link_expression_for( boolean(), file_loc() ) -> form_element().
get_spawn_link_expression_for( _IsDebugMode=true, FileGenLoc ) ->

	% To test:
	%{ remote, FileGenLoc, { atom, FileGenLoc, non_existing },
	%  { atom, FileGenLoc, spawn_link } };

	%{ remote, FileGenLoc, { atom, FileGenLoc, proc_lib },
	%  { atom, FileGenLoc, spawn_link } };

	% Some time ago, for dubious reasons, we had switched back to a basic
	% spawn_link (see Myriad's spawn_utils.hrl), as we felt that the extra
	% information in error messages was counter-productive.
	%
	{ atom, FileGenLoc, spawn_link };


get_spawn_link_expression_for( _IsDebugMode=false, FileGenLoc ) ->
	{ atom, FileGenLoc, spawn_link }.
