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

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


-module(class_SemanticServer).


-define( class_description,
		 "Class in charge of managing semantic information. "
		 "Generally instantiated as a singleton." ).



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_EngineBaseObject ] ).


% The class-specific attributes of a semantic server:
-define( class_attributes, [

	{ vocabulary, vocabulary(), "the set of the known semantic elements, for "
	  "fast inclusion look-up" },

	{ string_vocabulary, string_vocabulary(), "the corresponding string-based "
	  "version of vocabulary, for faster distance computations (a list with "
	  "the same elements of the 'vocabulary' attribute, in unspecified order); "
	  "allows to avoid countless conversions from binaries to strings" },

	{ min_distance, distance(), "the minimum lexicographic distance "
	  "allowed between two semantic elements (Levenshtein distance)" } ] ).


% Design notes:
%
% Currently the semantics are only checked for equality and to ensure that their
% texts are not too close one from each other (useful to detect typos, case
% mismatches, etc.).


% A user-level element of a semantics is a symbol (a plain string):
-type user_semantics() :: rdf_utils:string_iri().

% A user-level vocabulary is a list of semantic elements (plain strings):
-type user_vocabulary() :: rdf_utils:user_vocabulary().



% An (internal) element of a semantics is a symbol (a binary string):
-type semantics() :: rdf_utils:subject().

% An (internal) vocabulary is a set of semantic elements (binary strings):
-type vocabulary() :: rdf_utils:vocabulary().


% PID of the semantic server:
-type semantic_server_pid() :: pid().


-export_type([ user_semantics/0, user_vocabulary/0, semantics/0, vocabulary/0,
			   semantic_server_pid/0 ]).


% String version of a vocabulary:
-type string_vocabulary() :: [ rdf_utils:string_iri() ].


% Possible outcomes of a semantic validation (possibly involving multiple
% semantics):
%
-type validation_outcome() :: 'semantics_accepted'
				| { 'semantics_rejected', basic_utils:error_reason() }.


% Helpers:
-export([ to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% To have the trace messages adequately sorted:
-define( trace_emitter_categorization, "Core.Dataflow.Semantics" ).

% Allows to use macros for trace sending:

-include_lib("traces/include/class_TraceEmitter.hrl").

% For registration:
-define( semantic_server_name, sim_diasca_semantic_server ).


% Default minimum lexicographic distance allowed between two semantic elements
% (Levenshtein distance):
%
-define( default_min_distance, 1 ).


% Shorthands:

-type unchecked_data() :: basic_utils:unchecked_data().
-type distance() :: text_utils:distance().
-type ustring() :: text_utils:ustring().


% Implementation notes:
%
% Internal IRIs could have been atoms (rather than binary strings).



% Constructs a new semantic server.
%
% MinDistance is the minimum lexicographic distance allowed between two semantic
% elements (Levenshtein distance).
%
-spec construct( wooper:state(), distance() ) -> wooper:state().
construct( State, MinDistance ) ->

	% First the direct mother class:
	TraceState = class_EngineBaseObject:construct( State,
							?trace_categorize("SemanticServer") ),

	naming_utils:register_as( self(), ?semantic_server_name, global_only ),

	?send_notice_fmt( TraceState, "Semantic server started, relying "
		"on a minimum lexicographic Levenshtein distance of ~B.",
		[ MinDistance ] ),

	% Then the class-specific actions:
	setAttributes( TraceState, [
		{ vocabulary, set_utils:new() },
		{ string_vocabulary, [] },
		{ min_distance, MinDistance } ] ).



% To avoid the Vocabulary variable being reported as unused because of the
% two-liner:

-ifdef(tracing_activated).


-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	Vocabulary = ?getAttr(string_vocabulary),

	?notice_fmt( "Stopping semantic server, vocabulary had following "
		"~B elements: ~ts", [ length( Vocabulary ),
							  text_utils:strings_to_string( Vocabulary ) ] ),

	State.


-endif. % tracing_activated





% Methods section.


% Declares specified vocabulary (expected to be a list of binary strings) to
% this server.
%
-spec declareVocabulary( wooper:state(), unchecked_data() ) -> oneway_return().
declareVocabulary( State, Vocabulary ) when is_list( Vocabulary ) ->

	DeclaredState = declare_semantics( Vocabulary, State ),

	wooper:return_state( DeclaredState ).



% Declares either a single semantic element (expected to be a binary string) or
% a list thereof to this server.
%
-spec declareSemantics( wooper:state(), unchecked_data() ) -> oneway_return().
% Single element clause:
declareSemantics( State, SemanticElement ) when is_binary( SemanticElement ) ->

	case add_semantic_element( SemanticElement, State ) of

		{ semantics_accepted, DeclaredState } ->
			?debug_fmt( "Declaration of semantic element '~ts' accepted.",
						[ SemanticElement ] ),
			wooper:return_state( DeclaredState );

		{ { semantics_rejected, Reason }, _NewState } ->
			?error_fmt( "Declaration of semantic element '~ts' rejected, "
						"reason: ~p", [ SemanticElement, Reason ] ),
			throw( { semantics_rejected, SemanticElement, Reason } )

	end;


% Multiple elements here (to centralise the trace outcomes in the correct
% cases):
%
declareSemantics( State, SemanticElements ) when is_list( SemanticElements ) ->

	% Throws an exception, should an element be rejected:
	DeclaredState = declare_semantics( SemanticElements, State ),

	?debug_fmt( "Declaration of semantic elements '~p' accepted.",
				[ SemanticElements ] ),

	wooper:return_state( DeclaredState ).




% Tells whether specified semantic element (specified as a binary string) or a
% list thereof, are valid.
%
% Note: request, for result and also synchronisation.
%
-spec validateSemantics( wooper:state(), unchecked_data() ) ->
								request_return( validation_outcome() ).
validateSemantics( State, SemanticElement ) when is_binary( SemanticElement ) ->

	%?debug_fmt( "Checking semantic element '~ts'.", [ SemanticElement ] ),
	{ Outcome, NewState } = add_semantic_element( SemanticElement, State ),

	%trace_utils:debug_fmt( "Single-element outcome: ~p.", [ Outcome ] ),
	wooper:return_state_result( NewState, Outcome );


validateSemantics( State, SemanticElements ) when is_list( SemanticElements ) ->

	%?debug_fmt( "Checking semantic elements '~p'.", [ SemanticElements ] ),
	{ Outcome, NewState } = add_semantic_elements( SemanticElements, State ),

	%trace_utils:debug_fmt( "Multiple-element outcome: ~p.", [ Outcome ] ),
	wooper:return_state_result( NewState, Outcome );

validateSemantics( State, Invalid ) ->
	?error_fmt( "Invalid semantics: ~p.", [ Invalid ] ),
	throw( { invalid_semantics, Invalid } ).



% Returns a textual description of the state of this semantic server.
-spec getStatus( wooper:state() ) -> const_request_return( ustring() ).
getStatus( State ) ->
	wooper:const_return_result( to_string( State ) ).





% Static section.


% Launches the semantic server, with default settings.
-spec start() -> static_return( semantic_server_pid() ).
start() ->
	SemServerPid = start( ?default_min_distance ),
	wooper:return_static( SemServerPid ).



% Launches the semantic server, with specified minimum lexicographic distance
% allowed between two semantic elements (Levenshtein distance).
%
-spec start( distance() ) -> static_return( semantic_server_pid() ).
start( MinDistance ) ->
	SemServerPid = new_link( MinDistance ),
	wooper:return_static( SemServerPid ).



% Declares specified user-level vocabulary.
-spec declare_vocabulary( user_vocabulary(), semantic_server_pid() ) ->
								static_void_return().
declare_vocabulary( Vocabulary, SemanticServerPid )
  when is_list( Vocabulary ) ->

	% To reduce messaging payload, switching from user_semantics() to
	% semantics():
	BinVocabulary = [ text_utils:string_to_binary( S ) || S <- Vocabulary ],

	SemanticServerPid ! { declareVocabulary, [ BinVocabulary ] },

	wooper:return_static_void().



% Tells whether specified semantics are compliant.
%
% Currently we consider that this is the case iff the receiver semantics are a
% subset of the emitter ones (i.e. that all semantics demanded by the receiver
% can be found among the ones shown by the emitter).
%
-spec are_semantics_compliant( semantics(), semantics() ) ->
										static_return( boolean() ).
are_semantics_compliant( EmitterSemantics, ReceiverSemantics ) ->

	IsCompliant = set_utils:is_subset( ReceiverSemantics, EmitterSemantics ),

	wooper:return_static( IsCompliant ).




% Transforms a user-level vocabulary into one in internal form.
-spec transform_as_internal( user_vocabulary() ) ->
									static_return( vocabulary() ).
transform_as_internal( UserVocabulary ) when is_list( UserVocabulary ) ->

	BinSemantics = text_utils:strings_to_binaries( UserVocabulary ),

	Vocab = set_utils:from_list( BinSemantics ),

	wooper:return_static( Vocab ).



% Stops (asynchronously) the semantic server.
-spec stop() -> static_void_return().
stop() ->
	SemServerPid = get_server(),
	stop( SemServerPid ),
	wooper:return_static_void().




% Stops (asynchronously) the specified semantic server.
-spec stop( semantic_server_pid() ) -> static_void_return().
stop( SemServerPid ) ->
	SemServerPid ! delete,
	wooper:return_static_void().



% Returns the PID of the semantic server (if any).
-spec get_server() -> static_return( semantic_server_pid() ).
get_server() ->
	Pid = naming_utils:get_registered_pid_for( ?semantic_server_name, global ),
	wooper:return_static( Pid ).





% Helper section.


% Declares a list of semantics (that are binary strings).
-spec declare_semantics( [ semantics() ], wooper:state() ) -> wooper:state().
declare_semantics( _SemanticElements=[], State ) ->
	State;

declare_semantics( _SemanticElements=[ E | T ], State ) when is_binary( E ) ->

	case add_semantic_element( E, State ) of

		{ semantics_accepted, NewState } ->
			declare_semantics( T, NewState );

		{ { semantics_rejected, Reason }, _NewState } ->
			?error_fmt( "Declaration of semantic element '~ts' rejected, "
						"reason: ~p", [ E, Reason ] ),
			throw( { semantics_rejected, E, Reason } )

	end;

declare_semantics( _SemanticElements=[ E | _T ], _State ) ->
	throw( { non_binary_semantics, E } ).



% Adds specified semantic element, if accepted.
%
% Note: the caller shall ensure that this element is a binary string indeed.
%
-spec add_semantic_element( semantics(), wooper:state() ) ->
									{ validation_outcome(), wooper:state() }.
add_semantic_element( SemanticElement, State ) ->

	Vocabulary = ?getAttr(vocabulary),

	case set_utils:member( SemanticElement, Vocabulary ) of

		true ->
			% Already registered, hence accepted:
			{ semantics_accepted, State };

		false ->

			% New element, hence more (string-based) checking:

			% Both structures needed in both cases:
			StringSemanticElement =
				text_utils:binary_to_string( SemanticElement ),

			StringVocabulary = ?getAttr(string_vocabulary),

			case ?getAttr(min_distance) of

				0 ->
					% No restriction here, hence directly included (shortcut):

					%?info_fmt( "Semantic element '~ts' directly accepted.",
					%			[ SemanticElement ] ),

					IncState = include_element( SemanticElement,
						 StringSemanticElement, Vocabulary, StringVocabulary,
						 State ),

					{ semantics_accepted, IncState };


				MinDistance ->

					% We will now check the distance between this element and
					% all the ones in the internal vocabulary:

					case is_valid_semantics( StringSemanticElement,
											 StringVocabulary, MinDistance ) of

						true ->
							%?info_fmt( "Semantic element '~ts' accepted.",
							%			[ SemanticElement ] ),

							IncState = include_element( SemanticElement,
								StringSemanticElement, Vocabulary,
								StringVocabulary, State ),

							{ semantics_accepted, IncState };

						{ false, Reason } ->
							%?info_fmt( "Semantic element '~ts' rejected, "
							% "reason: ~p.", [ SemanticElement, Reason ] ),

							{ { semantics_rejected, Reason }, State }

					end

			end

	end.



% Adds specified semantic elements, if accepted.
%
% Returns an accepted outcome iff all elements are valid.
%
-spec add_semantic_elements( [ semantics() ], wooper:state() ) ->
									{ validation_outcome(), wooper:state() }.
add_semantic_elements( _Elements=[], State ) ->
	{ semantics_accepted, State };

add_semantic_elements( _Elements=[ E | T ], State ) when is_binary( E ) ->
	case add_semantic_element( E, State ) of

		{ semantics_accepted, AcceptedState } ->
			add_semantic_elements( T, AcceptedState );

		Rejection -> % = { { semantics_rejected, Reason }, RejectedState } ->
			% Stop the recursion on first rejection:
			Rejection

	end;

add_semantic_elements( _Elements=[ InvalidElement | _T ], State ) ->

	?error_fmt( "Invalid, non-binary string, semantic element: ~p.",
				[ InvalidElement ] ),

	throw( { invalid_semantic_element, InvalidElement } ).




% Tells whether specified semantics is valid, knowing it is a string here, and
% that it does not belong to the specified vocabulary.
%
% Returns either true or {false, Reason}.
%
-spec is_valid_semantics( ustring(), string_vocabulary(), distance() ) ->
								'true' | { 'false', term() }.
is_valid_semantics( SemanticElement, VocabularyStrings, MinDistance ) ->

	case get_minimal_distance( SemanticElement, VocabularyStrings ) of

		none ->
			% Empty vocabulary here:
			true;

		% Insufficient lexicographic distance:
		{ D, Closest } when D < MinDistance ->
			{ false, { semantics_too_close, { SemanticElement, Closest } } };

		% Sufficient lexicographic distance:
		_ ->
			true

	end.



% (helper)
%
-spec include_element( semantics(), rdf_utils:string_iri(), vocabulary(),
					   string_vocabulary(), wooper:state() ) -> wooper:state().
include_element( SemanticElement, StringSemanticElement, Vocabulary,
				 StringVocabulary, State ) ->

	NewVocabulary = set_utils:add( SemanticElement, Vocabulary ),
	NewStringVocabulary = [ StringSemanticElement | StringVocabulary ],

	setAttributes( State, [ { vocabulary, NewVocabulary },
							{ string_vocabulary, NewStringVocabulary } ] ).



% Returns the minimal lexicographic distance between the specified semantic
% element and vocabulary.
%
% Returns either 'none' if the vocabulary is empty, or {MinimalDistance,
% CorrespondingVocabularyElement}.
%
-spec get_minimal_distance( rdf_utils:string_iri(), string_vocabulary() ) ->
								  'none' | { distance(), semantics() }.
get_minimal_distance( SemanticElement, Vocabulary ) ->
	get_minimal_distance( SemanticElement, Vocabulary, _Min=none ).


% (helper)
get_minimal_distance( _SemanticElement, _Vocabulary=[], Min ) ->
	Min;

% Having a first element allows to have an actual minimum:
get_minimal_distance( SemanticElement, _Vocabulary=[ E | T ], _Min=none ) ->

	Dist = text_utils:get_lexicographic_distance( SemanticElement, E ),

	NewMin = { Dist, E },

	get_minimal_distance( SemanticElement, T, NewMin );


get_minimal_distance( SemanticElement, _Vocabulary=[ E | T ],
					  Min={ MinDist, _MinElem } ) ->

	case text_utils:get_lexicographic_distance( SemanticElement, E ) of

		D when D < MinDist ->
			NewMin = { D, E },
			get_minimal_distance( SemanticElement, T, NewMin );

		_ ->
			get_minimal_distance( SemanticElement, T, Min )

	end.



% Returns a textual description of the state of this semantic server.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	VocString = case ?getAttr(string_vocabulary) of

		[] ->
			"no vocabulary";

		Vocabulary ->
			text_utils:format( "a vocabulary of ~B semantic elements, namely, "
				"in alphabetical order: ~ts",
				[ length( Vocabulary ),
				  text_utils:strings_to_sorted_string( Vocabulary ) ] )

	end,

	text_utils:format( "Semantic server relying on a minimum Levenshtein "
		"distance of ~B and having ~ts",
		[ ?getAttr(min_distance), VocString ] ).
