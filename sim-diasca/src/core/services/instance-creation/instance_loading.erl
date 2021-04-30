% Copyright (C) 2014-2021 EDF R&D

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


% Module dedicated to the loading of instances.
%
% The typical use case is to load from a set of files a description of the
% initial state of the simulation, i.e. the construction parameters of initial
% actors and scenarios.
%
-module(instance_loading).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.LoadBalancing.Instances." ).
-define( loader_cat, ?trace_emitter_categorization ++ "Loading" ).
-define( reader_cat, ?trace_emitter_categorization ++ "Creating" ).


% In milliseconds:

%-define( initialisation_file_timeout, 5000 ).
-define( initialisation_file_timeout, infinity ).


% Design notes:
%
% Now the load-balancing service provides all actor-specific information (ex:
% AAI and seeding) at their creation.
%
% Currently only one initialisation stream (typically: file) can be managed at
% once (N could be simulaneously managed by having the successive AAIs of each
% stream progress by increment of N, so that each stream remains unrestricted).
%
% For a given stream, one reader process can assign creations to a pool of
% creator processes, which is shared among all readers.
%
% These creations will be done in parallel yet will be reproducible, provided
% that there is no nested creation, i.e. provided that the created actors do not
% create themselves (from their constructor) other (initial) actors.
%
% If nested creations are to be done, reproducibility can be preserved by
% limiting the number of creator processes to 1, which is the current default.


% Types related to creation lines.


% To identify a construction line in a file:
-type line_number() :: basic_utils:count().


% Full line being examined (as a binary):
-type line() :: text_utils:bin_string().


% Information about a read creation line:
-type line_info() :: { line_number(), line() }.


% Shorthand:
-type classname() :: wooper:classname().
-type method_argument() :: wooper:method_argument().


% Full information to create a new instance:
-type creation_spec() :: { classname(), [ method_argument() ],
						   class_LoadBalancer:placement_hint(), line_number() }.



% Full context of a parsed creation line (file origin, etc.):
-type line_context() :: { file_utils:bin_file_name(), line_number(), line() }.


-type user_identifier() :: text_utils:bin_string().


-type id_ref() :: { 'user_id', user_identifier() }.


% Either the user identifier itself (as a binary) or 'none'.
-type identifier_info() :: user_identifier() | 'none'.


-export_type([ line_number/0, creation_spec/0, user_identifier/0, id_ref/0,
			   identifier_info/0 ]).


-export([ manage_initialisation/3, get_instance_initialisation_line/3 ]).


% For notify_debug_fmt:
-include_lib("traces/include/traces.hrl").

% For load_balancer_pid() and others:
-include("engine_common_defines.hrl").


% Shorthands:

-type count() :: basic_utils:count().

-type file_name() :: file_utils:file_name().
-type file_path() :: file_utils:file_path().
-type file() :: file_utils:file().

-type bin_string() :: text_utils:bin_string().

-type id_resolver_pid() :: pid().
-type creator_pid() :: pid().
-type reader_pid() :: pid().
-type loader_pid() :: pid().
-type instance_pid() :: pid().


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").


% Implementation notes.

% The reading of files is meant to be parallel, and the instance creations
% stemming from them are themselves also parallel.
%
% The difficulty is that, despite the parallelism and thus the changes in the
% receiving order of the corresponding messages by the (centralised) load
% balancer, each creation should be handled each time identically (ex: same
% AAI).
%
% For that, multiple steps must be taken:
%
% - when a user identifier is *referenced* first, a process will be created (by
% the instance creator for an increased parallelism, not by the load balancer),
% based on the sole user identifier: the id resolver just requests the load
% balancer to tell, based on the user id, the selected computing node; the load
% balancer does not consider any actor created (no AAI assigned, etc.), as we
% cannot know the line number where the user id is *defined*, which is necessary
% for reproducible creations
%
% - when a user identifier is *defined*, either it reuses an already existing
% PID (if having been referenced at least once beforehand), or it is then
% created (still based only on the user identifier); then only the blank
% process, the transformed arguments (w.r.t. referenced user ids) and
% information from the load balancer (typically actor settings like AAI,
% determined also based on the line number of this definition) are used to
% create the actual instance



% Debugging hints:
%
% To the best of our knowledge, these creation mechanisms work correctly and
% report at least more user-level errors, though creation failures.
%
% A remaining corner case could happen if, for any reason, an instance creation
% was not to fail but never to return (ex: blocked because of interacting with
% other actors from its constructor - which should never be done, use
% onFirstDiasca/2 for that instead).
%
% To detect more easily such cases, switch to a small, finite time-out:
%
-define( spawn_embodiement_time_out, infinity ).
% In seconds:
%-define( spawn_embodiement_time_out, 2 ).



% Takes care of creating the initial instances from specified files (if any).
%
% Expected to be spawned from the load balancer, letting it able to answer
% placement requests and al afterwards.
%
% (helper)
%
-spec manage_initialisation( [ file_path() ], count(), load_balancer_pid() ) -> 
								   no_return().
manage_initialisation( InitialisationFiles, NodeCount, LoadBalancerPid ) ->

	LoadStartTimestamp = time_utils:get_precise_timestamp(),

	ShortenInitFiles = [ text_utils:format( "file '~ts'",
		    [ filename:basename( F ) ] ) || F <- InitialisationFiles ],

	trace_utils:info_fmt( "Loading initial instances from: ~ts",
			   [ text_utils:strings_to_string( ShortenInitFiles ) ] ),

	CompressionFormat = xz,

	CompressExt = file_utils:get_extension_for( CompressionFormat ),

	% So that all compressed initialisation files are uncompressed:
	ReadyInitFiles = lists:foldl(

		fun( Filename, AccFilenames ) ->

			case filename:extension( Filename ) of

				CompressExt ->

					%trace_utils:info_fmt( "Decompressing '~ts'.",
					%					   [ Filename ] ),

					DecompressedFilename =
						file_utils:decompress( Filename, CompressionFormat ),

					[ DecompressedFilename | AccFilenames ];


				_ ->
					[ Filename | AccFilenames ]


			end

		end,
		_Acc0=[],
		_List=InitialisationFiles ),


	% Now let's take care of the initialisation files, one reader process per
	% file, all readers making use of a common pool of creator processes (one
	% per core of the user node, each transforming its assigned set of lines
	% into actual actor instances):

	% Needed for synchronisation (beware, closure):
	InstanceLoaderPid = self(),

	%trace_utils:info_fmt( "Creating user_id resolver from ~w.",
	%					   [ InstanceLoaderPid ] ),

	% Let's first create the resolver of user identifiers first, as it is needed
	% by the creators:
	%
	UserIdResolverPid = ?myriad_spawn_link(
			fun() ->
				user_identifier_resolver_loop( InstanceLoaderPid )
			end ),

	% Let's then spawn the creator processes first; as many of them as there are
	% available cores on this user node:
	%
	%CreatorsCount = system_utils:get_core_count(),

	% For an easier sequential debugging:
	%
	% (now enabled to support nested initial actor creations)
	%
	CreatorsCount = 1,

	%trace_utils:info_fmt( "Creating ~B instance creator(s).", 
	%    [ CreatorsCount ] ),

	Creators = [ ?myriad_spawn_link(
		fun() ->
			instance_creator_loop( NodeCount, UserIdResolverPid,
								   LoadBalancerPid, InstanceLoaderPid )
		end )
				 || _CoreCount <- lists:seq( 1, CreatorsCount ) ],

	case ReadyInitFiles of

		[] ->
			?notify_warning( "No initialisation file specified." );

		[ _ ] ->
			% Usual case:
			ok;

		_ ->

			% We will later use operations on AAI based on the modulo of the
			% number of files, in order to provide each of them with its own AAI
			% space and to preserve reproducibility:
			%
			?notify_warning_cat( "Multiple initialisation files have been "
				"specified. Total reproducibility currently cannot be "
				"guaranteed with this setting.", ?loader_cat )

	end,

	%trace_utils:info_fmt( "Reading following initialisation file(s): ~ts",
	%					   [ text_utils:strings_to_string( ReadyInitFiles ) ] ),

	Readers = [ ?myriad_spawn_link(
				fun() ->
					read_init_file( InitFile, Creators, InstanceLoaderPid )
				end )
				|| InitFile <- ReadyInitFiles ],

	% We chose, for this data-based initialisation, to rely on a dedicated
	% process and handle the corresponding applicative protocol from this
	% function, performing ad-hoc receiving and handling messages rather than
	% letting this initialisation be done through asynchronous calls to the load
	% balancer API (simpler, more self-contained, and anyway some requests have
	% to be sent in the course of the loading to the load balancer):
	%
	initialisation_waiting_loop( UserIdResolverPid, _CreationCount=0,
			    Creators, Readers, LoadStartTimestamp, LoadBalancerPid ).




% Main loop driving the loadings.
-spec initialisation_waiting_loop( id_resolver_pid(), count(),
			[ creator_pid() ], [ reader_pid() ],
			time_utils:precise_timestamp(), load_balancer_pid() ) -> void().
% First clause, for termination (no more reader awaited):
initialisation_waiting_loop( UserIdResolverPid, CreationCount, Creators,
					_Readers=[], LoadStartTimestamp, LoadBalancerPid ) ->

	?notify_debug_cat( "All readers terminated, wrapping up.", ?loader_cat ),

	% All readers terminated, thus the initialisation as such is finished too:
	UserIdResolverPid ! wrap_up_and_terminate,

	% Their work is then over as well:
	[ Creator ! instance_creation_over || Creator <- Creators ],

	% For synchronicity:
	receive

		% The identifier resolver terminated:
		all_user_identifiers_resolved ->
			ok

	end,

	LoadDuration = time_utils:get_precise_duration_since( LoadStartTimestamp ),

	LoadDurationString = time_utils:duration_to_string( LoadDuration ),

	Message = text_utils:format( "All ~B initial instances have been "
			"successfully loaded from the initialisation sources, in ~ts.~n~n",
			[ CreationCount, LoadDurationString ] ),

	?notify_debug_cat( Message, ?loader_cat ),
	trace_utils:info( Message ),

	% Calling a real oneway, as the load balancer is not blocked:
	LoadBalancerPid ! onInstancesLoaded;


% Main clause; at least one reader is still waited:
initialisation_waiting_loop( UserIdResolverPid, CreationCount, Creators,
							 Readers, LoadStartTimestamp, LoadBalancerPid ) ->

	%trace_utils:info_fmt( "Waiting for following reader(s), whereas "
	%   "~B creations have already been done: ~w.",
	%   [ CreationCount, Readers ] ),

	receive

		% Readers know when their file has already been fully processed,
		% including by the creators:
		%
		{ initialisation_file_read, InstanceCreationCount, ReaderPid } ->

			%trace_utils:info_fmt( "Instance reader ~w finished "
			%   "(~B new instances created).",
            %   [ ReaderPid, InstanceCreationCount ] ),

			?notify_debug_fmt_cat( "Instance reader ~w finished.",
								   [ ReaderPid ], ?loader_cat ),

			NewReaders = list_utils:delete_existing( ReaderPid, Readers ),

			initialisation_waiting_loop( UserIdResolverPid,
				CreationCount + InstanceCreationCount, Creators,
				NewReaders, LoadStartTimestamp, LoadBalancerPid )

	after ?initialisation_file_timeout ->

		trace_utils:warning_fmt( "Still waiting for following initialisation "
								 "readers: ~w.", [ Readers ] ),

		initialisation_waiting_loop( UserIdResolverPid, CreationCount, Creators,
			Readers, LoadStartTimestamp, LoadBalancerPid )

	end.





% Three main sections below; in chronological order of the loading process:
%
%  - readers read construction data from initialisation files (one reader per
%  file), and as they are each on a critical path (a parallel file reading is
%  not really an option), they do as little as possible and forward the rest of
%  the work to the next processing step, which is a lot more parallel
%
%  - the pool of instance creators (one per core of the user host) is fed by all
%  the previous readers; creators perform most of the line parsing and
%  transformation, communicating with the load balancer (for placement) and
%  exchanging with the identifier resolver about user identifiers and their
%  associated PIDs
%
%  - the identifier resolver checks the set of user identifiers and associates
%  them to the relevant PIDs, creating them when useful




% Section about data readers.



% Code entry point for a reader of a given initialisation file.
-spec read_init_file( file_name(), [ creator_pid() ], loader_pid() ) -> 
							no_return().
read_init_file( Filename, Creators, InstanceLoaderPid ) ->

	% Traces useful, yet not in an actor anymore:
	?notify_debug_fmt_cat( "Reading initialisation file '~ts' from ~w, "
		"using instance creator processes ~p.",
		[ Filename, self(), Creators ], ?loader_cat ),

	% Each filename is by design an absolute path here:
	case file_utils:is_existing_file_or_link( Filename ) of

		true ->
			ok;

		false ->
			trace_utils:error_fmt( "Initialisation file '~ts' could "
				"not be found from node '~ts'.", [ Filename, node() ] ),
			throw( { init_file_not_found, Filename, node() } )

	end,

	% We prefer reading the (potentially huge) file line by line (with some
	% read-ahead for performances), rather than reading it as a whole as a
	% binary: we do not want to saturate the RAM, and anyway we will process it
	% on a per-line basis (and lower-level caching is to happen anyway).
	%
	InitFile = file_utils:open( Filename,
	   _Options=[ read, raw, binary, { read_ahead, _Size=1024*256 } ] ),

	% The target number of construction lines to be gathered in a creation chunk
	% (allows to perform more parallel creations; actual chunk size is often
	% smaller, because of lines that are blank or correspond to comments;
	% notably in the header of initialisation files):
	%
	CreationChunkCount = 64,

	% Rather that using a ring to send blindly (i.e. asynchronously) creation
	% chunks to creators on a round-robin basis, we prefer enforcing some flow
	% control, so that for example a fast SSD cannot overwhelm the creators and
	% exhaust the RAM.
	%
	% So the worst case scenario would be to have each creator having in its
	% message box as many chunks to process as there are file readers. No
	% problem here.
	%
	InstanceCreationCount = 
		read_all_lines( InitFile, CreationChunkCount, Creators, Filename ),

	InstanceLoaderPid ! { initialisation_file_read, InstanceCreationCount, 
						  self() } ,

	% Terminates just afterwards:
	file_utils:close( InitFile ).




% Reads all lines of specified file, by chunks of specified size, and feeds them
% to creators as early as possible.
%
-spec read_all_lines( file(), count(), [ creator_pid() ], file_name() ) -> 
							count().
read_all_lines( File, ChunkCount, Creators, Filename ) ->

	?notify_debug_fmt_cat( "Reading all lines from '~ts'.", [ Filename ],
						   ?loader_cat ),

	BinFilename = text_utils:string_to_binary( Filename ),

	% Creators are considered as free only from the point of view of that reader
	% (they can be kept busy by other readers in the meantime):
	%
	% We maintain a count of initialisation lines to assign AAIs, and also to be
	% able to return informative error messages.
	%
	read_all_lines( File, ChunkCount, _CreationCount=0,
		_RemainingFreeCreators=Creators, _WaitedCreators=[], BinFilename,
		_LineCount=1 ).



% (helper)
read_all_lines( File, ChunkCount, CreationCount, RemainingFreeCreators,
				WaitedCreators, BinFilename, LineCount ) ->

	case read_chunk( File, ChunkCount, LineCount ) of

		{ _Chunk=[], LineCount } ->

			?notify_debug_fmt_cat( "Read all lines, waiting for instance "
				   "creators ~p.", [ WaitedCreators ], ?loader_cat ),

			% We hit the end of file, let's wait for all busy creators and
			% terminate:
			%
			FinalCreationCount = basic_utils:wait_for_summable_acks(
				_WaitedSenders=WaitedCreators, CreationCount,
				_Timeout=infinity, _AckReceiveAtom=chunk_processed,
				% Never triggered anyway:
				_ThrowAtom=chunk_timeout ),

			?notify_debug_fmt_cat( "Read all ~B lines from '~ts', "
				"resulting in ~B instance creations.",
				[ LineCount, text_utils:binary_to_string( BinFilename ),
				  FinalCreationCount ], ?loader_cat ),

			FinalCreationCount;


		{ Chunk, NewLineCount } ->

			ChunkMessage = { process_chunk, { Chunk, BinFilename }, self() },

			?notify_debug_fmt_cat( "Instance reader has a new chunk of "
				"~B lines to process.", [ length( Chunk ) ], ?loader_cat ),

			% We have a chunk, but do we have a creator?
			case RemainingFreeCreators of

				[] ->
					% None left, let's then wait for the first to become free:
					receive

						{ chunk_processed, AdditionalCreationCount, 
						  CreatorPid } ->

							% Reallocate this creator immediately:
							CreatorPid ! ChunkMessage,

							?notify_debug_fmt_cat(
							   "Chunk assigned to instance creator ~p.",
							   [ CreatorPid ], ?loader_cat ),

							read_all_lines( File, ChunkCount,
								CreationCount + AdditionalCreationCount,
								_RemainingFreeCreators=[],
								% CreatorPid already there by design:
								_NewWaited=WaitedCreators, BinFilename, 
								NewLineCount )

					end;


				[ FreeCreatorPid | T ] ->

					% Let's use this free creator then, and record that:
					FreeCreatorPid ! ChunkMessage,

					?notify_debug_fmt_cat(
					   "Chunk assigned to instance creator ~p.",
					   [ FreeCreatorPid ], ?loader_cat ),

					read_all_lines( File, ChunkCount, CreationCount,
						_RemainingFreeCreators=T,
						_NewWaited=[ FreeCreatorPid | WaitedCreators ],
						BinFilename, NewLineCount )

			end

	end.



% Returns {InfoLines,LineCount}, where:
%
% - InfoLines is a list of up to ChunkCount pairs, made of a line number and of
% a line (as a binary), read from specified file
%
% - LineCount is the new current line count
%
% Note: line order is not preserved (each chunk is in reverse order compared to
% the one of the read file), but, as we record the line number anyway, and will
% rely on it to preserve reproducibility, this is not a problem.
%
-spec read_chunk( file(), count(), line_number() ) -> 
						{ [ line_info() ], line_number() }.
read_chunk( File, ChunkCount, LineCount ) ->
	read_chunk( File, ChunkCount, LineCount, _AccLines=[] ).


% Whole chunk read:
read_chunk( _File, _ChunkCount=0, LineCount, AccLines ) ->
	{ AccLines, LineCount };

% Still reading:
read_chunk( File, ChunkCount, LineCount, AccLines ) ->

	case file:read_line( File ) of

		{ ok, Line } ->

			% We will let creators filter out comments and all later:
			read_chunk( File, ChunkCount - 1, LineCount + 1,
						[ { LineCount, Line } | AccLines ] );

		eof ->
			% Returning the list (a partial chunk) as it is:
			{ AccLines, LineCount }

	end.






% Section about instance creators.



% Main loop of an instance creator process.
-spec instance_creator_loop( count(), id_resolver_pid(),
		load_balancer_pid(), loader_pid() ) -> no_return().
instance_creator_loop( NodeCount, IdResolverPid, LoadBalancerPid, 
					   InstanceLoaderPid ) ->

	?notify_debug_fmt_cat( "Instance creator ~w waiting.", [ self() ], 
						   ?reader_cat ),

	receive

		{ process_chunk, _Chunk={ LineInfos, BinFilename }, ReaderPid } ->

			%trace_utils:debug_fmt( "Creator ~w processing chunk of ~B "
			%	"elements:~ts", [ self(), length( LineInfos ),
			%		text_utils:terms_to_string( LineInfos ) ] ),

			?notify_debug_fmt_cat(
			   "Instance creator ~w processing chunk of ~B elements.",
			   [ self(), length( LineInfos ) ], ?reader_cat ),

			% We could, instead of directly creating instances in the course of
			% this function, aggregate a set of fully-prepared creation data per
			% chunk (hence with fewer messages sent to the load balancer, which
			% is a likely bottleneck), however the logic would become very
			% complex, as we want to perform the transformation of arguments in
			% creators (for parallelism) and as a result we have to rely on the
			% load balancer for that anyway (to determine where to spawn the
			% processes that will host the referenced instances); so we create
			% instances on the fly:
			%
			WaitedEmbodiements = [ parse_creation_line( LineInf, BinFilename,
				IdResolverPid, LoadBalancerPid ) || LineInf <- LineInfos ],

			% Filter-out 'no_creation', keep only PIDs:
			FilteredWaited = [ P || P <- WaitedEmbodiements, is_pid( P ) ],

			case FilteredWaited of

				[] ->
					ok;

				_ ->
					LoadBalancerPid !
						{ registerInitialActors, [ FilteredWaited ], self() }

			end,

			%trace_utils:debug_fmt(
			%	"Waiting for embodiement of ~w actors: ~w...",
			%	[ length( FilteredWaited ), FilteredWaited ] ),

			% Anticipated processing:
			NewCreationCount = length( FilteredWaited ),

			basic_utils:wait_for_acks( FilteredWaited,
				?spawn_embodiement_time_out,
				_AckReceiveAtom=spawn_successful,
				_ThrowAtom=embodiement_time_out ),

			%trace_utils:debug_fmt( "...embodiement of ~w finished.",
			%					   [ FilteredWaited ] ),

			% Synchronous:
			ReaderPid ! { chunk_processed, NewCreationCount, self() },

			?notify_debug_fmt_cat( "Instance reader ~p processed its chunk.",
								   [ ReaderPid ], ?reader_cat ),

			% From registerInitialActors:
			case FilteredWaited of

				[] ->
					ok;

				_ ->
					receive

						{ wooper_result, initial_actors_registered } ->
							ok

					end

			end,

			instance_creator_loop( NodeCount, IdResolverPid, LoadBalancerPid,
								   InstanceLoaderPid );


		instance_creation_over ->

			% Terminates:
			?notify_debug_fmt_cat( "Instance creator ~w terminated.",
								   [ self() ], ?reader_cat ),

			ok

	end.



% Parses (supposedly from an instance creator process) the specified creation
% line, possibly resulting on an instance creation and on various blank
% processes to be created.
%
% Synchronicity enforced here, for a better control.
%
-spec parse_creation_line( line_info(), file_utils:bin_file_name(),
			id_resolver_pid(), load_balancer_pid() ) -> instance_pid().
parse_creation_line( _LineInfo={ LineNumber, BinLine }, BinFilename,
					 IdResolverPid, LoadBalancerPid ) ->

	case LineNumber rem 500 of

		0 ->
			ShortenFilename = filename:basename(
						 text_utils:binary_to_string( BinFilename ) ),

			trace_utils:info_fmt(
				" - processing creation line #~B from ~ts at ~ts",
				[ LineNumber, ShortenFilename,
				  time_utils:get_textual_timestamp() ] );

		_ ->
			ok

	end,

	% Removes the ending newline for readability:
	%trace_utils:debug_fmt( "Parsing line #~B: '~ts'.", [ LineNumber,
	%	list_utils:remove_last_element(
	%	  text_utils:binary_to_string( BinLine ) ) ] ),

	% Note: for multi-file reading, the filename as well shall be associated to
	% the line number.

	% We offset as much processing as possible here, on a creator, as there are
	% one of them per core, we are thus in a parallel segment (as opposed to a
	% single reader per file) to perform the parsing.

	% Instead of ad-hoc parsing, we could have used 're', like in:
	%
	% MFA = re:replace( Line, "\r\n$", "", [ { return, list } ] ),
	% { match, [ M, F, A ] } = re:run( MFA,
	%      "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
	%      [ { capture, [ 1, 2, 3 ], list }, ungreedy ] ),
	%      { list_to_atom( M ), list_to_atom( F ), args_to_terms( A ) }.
	%
	% with args_to_terms( RawArgs) ->
	%   { ok, Toks, _Line } = erl_scan:string( "[" ++ RawArgs ++ "]. ", 1 ),
	%   { ok, Args } = erl_parse:parse_term( Toks ),
	%    Args.
	%
	% but we would have less control/error checking.

	% This is the beauty of ad-hoc parsing:
	Line = text_utils:binary_to_string( BinLine ),

	case text_utils:trim_whitespaces( Line ) of

		% Blank line:
		[] ->
			% Empty lines are ignored:
			%trace_utils:debug_fmt( "Empty line at line #~B ignored.",
			%                      [ LineNumber ] ),
			no_creation;


		% Comment:
		[ $% | _T ] ->
			% Starting by '%' means comments, that are ignored:
			%trace_utils:debug_fmt( "Comment '~ts' at line #~B ignored.~n",
			%		   [ Line, LineNumber ] ),
			no_creation;


		% Here we must have the first double quotes of a user identifier being
		% declared (ex: "John" <- ....):
		%
		_L=[ $" | T ] ->

			LineContext = { BinFilename, LineNumber, BinLine },

			% Extracts the user identifier and the actual creation tuple:
			{ BinUserId, CreationClause } = extract_id( T, LineContext ),

			%trace_utils:debug_fmt( "~p processing '~ts': "
			%    "user identifier '~ts' defined", [ self(), L, BinUserId ] ),

			InstancePid = create_instance_from( CreationClause, 
                _IdInfo=BinUserId, LineContext, IdResolverPid, 
                LoadBalancerPid ),

			%trace_utils:debug_fmt( "Instance corresponding to line #~B (user "
			%	"id: '~ts') will have for PID ~w.",
			%	[ LineNumber, BinUserId, InstancePid ] ),

			InstancePid;


		% Here we do not have a user identifier, we must be defining directly an
		% instance creation:
		%
		L=[ ${ | _T ] ->

			LineContext = { BinFilename, LineNumber, BinLine },

			InstancePid = create_instance_from( _CreationClause=L, _IdInfo=none,
								LineContext, IdResolverPid, LoadBalancerPid ),

			%trace_utils:debug_fmt( "Instance corresponding to line #~B will "
			%	"have for PID ~w.", [ LineNumber, InstancePid ] ),

			InstancePid;


		% Unexpected input:
		_Other ->

			LineContext = { BinFilename, LineNumber, BinLine },

			report_parse_error( invalid_start, LineContext )


	end.



% Returns {BinUserId,ActualCreationLine} from specified string, akin to:
% 'John" <- {class_Beatle,[{user_id,"Paul"},{user_id,"George"}]}.'
% (note that there is no leading '"' expected here)
%
% Should return : { <<"John">>,
%   "{class_Beatle,[{user_id,\"Paul\"},{user_id,\"George\"}]}." }
%
% LineContext is {BinFilename,LineNumber,BinLine}:
%
-spec extract_id( string(), line_context() ) -> { bin_string(), string() }.
extract_id( String, LineContext ) ->

	case text_utils:split_at_first( $", String ) of

		none_found ->
			report_parse_error( invalid_user_identifier, LineContext );


		% Ex: {"John", "   <-    {class_Beatle,...."}
		{ UserId, Rest } ->

			% We can now jump to the first '{':
			case text_utils:split_at_first( ${, Rest ) of

				none_found ->
					report_parse_error( no_creation_tuple_after_arrow,
										LineContext );

				% Must be akin to {"  <-   ", "class_Beatle,...."}:
				{ SomeKindOfArrow, AfterArrowWithoutBrace } ->

					case text_utils:trim_whitespaces( SomeKindOfArrow ) of

						"<-" ->
							ok;

						_Other ->
							report_parse_error( arrow_not_found, LineContext )

					end,

					BinUserId = text_utils:string_to_binary( UserId ),

					% Re-adding opening brace ('{') here:
					{ BinUserId, [ ${ | AfterArrowWithoutBrace ] }

			end

	end.



% Creates corresponding instance, from the specified actual creation clause,
% like: {class_Foo, [ "Hello world!", {user_id,"Charles"}, 154.06 ],
% _PlacementHint="greetings"} and user_id information for this instance to
% create.
%
-spec create_instance_from( string(), identifier_info(), line_context(),
		id_resolver_pid(), load_balancer_pid() ) -> instance_pid().
create_instance_from( CreationClause, IdInfo,
		LineContext={ _BinFilename, LineNumber, _BinLine }, IdResolverPid, 
        LoadBalancerPid ) ->

	%trace_utils:debug_fmt( "~p creating instance from '~ts', with id=~p.",
	%					   [ self(), CreationClause, IdInfo ] ),

	case erl_scan:string( CreationClause, _StartLocation=1 ) of

		{ ok, Tokens, _EndLocation } ->

			%trace_utils:debug( "Scan OK." ),

			% Tokens is for example: [{'{',1}, {atom,1,class_Foo}, {',',1},
			%  {'[',1}, {string,1,"Hello world!"}, {',',1}, {float,1,154.06},..

			% We want to obtain the final creation spec:
			case erl_parse:parse_term( Tokens ) of

				% With placement hints:
				{ ok, _CreationInfo={ Class, Args, PlacementHint } }
				  when is_atom( Class ) andalso is_list( Args ) ->

					%trace_utils:debug( "Parse OK, with placement hint." ),

					% A bit of interleaving will not hurt:
					LoadBalancerPid ! { getActorCreationInformationFromHint,
							    [ PlacementHint, LineNumber, Class ], self() },

					case IdInfo of

						none ->
							ok;

						_ ->
							report_parse_error(
							  both_placement_hint_and_id_defined, LineContext )

					end,


					{ ActualConstructionParameters, _FirstUserId } =
						replace_identifiers_by_pid( Args, IdResolverPid,
											LoadBalancerPid, LineContext ),

					%trace_utils:debug_fmt( "using '~p' as placement hint and "
					%                      "'~p' as construction parameters.",
					%		   [ PlacementHint,
					%			 ActualConstructionParameters ] ),

					% Return from getActorCreationInformationFromHint/3:
					{ TargetNode, ActorSettings } = receive

						{ wooper_result, CreationInfo } ->
							CreationInfo

					end,

					BlankPid = wooper:create_hosting_process( _Loc=TargetNode,
										    _ToLinkWithPid=LoadBalancerPid ),

					FullParams = 
						[ ActorSettings | ActualConstructionParameters ],

					%trace_utils:debug_fmt(
					%	   " - embodiement of an instance of class ~ts",
					%		[ Class ] ),

					%trace_utils:debug_fmt( " - embodiement of an instance "
					%       "of class ~ts with parameters ~p.",
					%		[ Class, FullParams ] ),

					BlankPid ! { embody, [ Class, FullParams ], self() },

					% No real need to wait for the completion of embodiement,
					% except for example if the simulation case wants to
					% interact with a loaded instance and, as a consequence,
					% needs to look-up its registered name. Moreover keeping
					% actions synchronous is a safer bet anyway.
					%
					BlankPid;


				{ ok, { Class, _Args, _PlacementHint } }
				  when is_atom( Class ) ->
					report_parse_error( non_list_arguments, LineContext );

				{ ok, { _Class, _Args, _PlacementHint } } ->
					report_parse_error( non_atom_class, LineContext );


				% Without placement hints:
				{ ok, { Class, Args } } 
				  when is_atom( Class ) andalso is_list( Args ) ->

					%trace_utils:debug( "Parse OK, without placement hint." ),

					% Side-effect: processes may be created for instances that
					% are referenced thourgh user_id, if not already created.
					%
					% (we get the first referenced user identifier - if any -
					% that may be needed if no user id was specified)
					%
					{ ActualConstructionParameters, FirstUserId } =
						replace_identifiers_by_pid( Args, IdResolverPid,
											LoadBalancerPid, LineContext ),

					% The target instance might not have a user identifier
					% defined for it; in this case we could thus create it
					% directly with a WOOPER new operator.
					%
					% However we prefer that all these creations are managed the
					% same (ex: w.r.t. to linking); thus we will go in all cases
					% for a two-step creation.

					% First, selects the user id to be used in order to place
					% the upcoming instance:
					%
					IdOfInterest = case IdInfo of

						none ->
							% Possibly still none:
							FirstUserId;

						SpecifiedIdInfo ->
							SpecifiedIdInfo

					end,

					% Then deduce its placement:
					LoadBalancerPid ! { getActorCreationInformation,
							[ IdOfInterest, LineNumber, Class ], self() },

					{ TargetNode, ActorSettings } = receive

						{ wooper_result, CreationInfo } ->
							%trace_utils:info( "Creation info received." ),
							CreationInfo

					end,


					% All created instances will be linked ultimately to the
					% load-balancer, and transiently to either this creator or
					% to the id resolver respectively (not a problem):
					%
					BlankPid = case IdInfo of

						none ->
							% No need to involve the id resolver here:
							wooper:create_hosting_process( _Loc=TargetNode,
										_ToLinkWithPid=LoadBalancerPid );

						_ ->
							% Here the id resolver shall create a corresponding
							% (remote) PID, record its association with
							% specified user id, and return the PID:
							%
							IdResolverPid ! { declare_id, IdInfo, TargetNode,
											  LoadBalancerPid, self() },

							receive

								{ pid_assigned, Pid } ->
									Pid

							end

					end,

					FullParams = 
						[ ActorSettings | ActualConstructionParameters ],

					%trace_utils:debug_fmt(
					%    " - embodiement of an instance of class ~ts",
					%    [ Class ] ),

					%trace_utils:debug_fmt(
					%   " - embodiement of an instance of class ~ts "
					%   " with parameters ~p.",  [ Class, FullParams ] ),

					BlankPid ! { embody, [ Class, FullParams ], self() },

					BlankPid;


				{ ok, { Class, _Args } } when is_atom( Class ) ->
					report_parse_error( non_list_arguments, LineContext );

				{ ok, { _Class, _Args } } ->
					report_parse_error( non_atom_class, LineContext );

				{ error, ErrorInfo } ->
					report_parse_error( { term_parsing, ErrorInfo }, 
										LineContext )

			end;


		{ error, ErrorInfo, ErrorLocation } ->
			report_parse_error( { term_tokenizing,
					    { ErrorInfo, ErrorLocation } }, LineContext )

	end.



% Explores and transforms specified construction arguments, replacing
% {user_id,Id} by the corresponding PID to be used for that identifier, and
% returning a {ActualConstructionParameters, FirstUserIdFound} pair made of the
% transformed parameters and the first user identifier referenced in them (if
% any).
%
-spec replace_identifiers_by_pid( [ method_argument() ], id_resolver_pid(),
			load_balancer_pid(), line_context() ) ->
						{ [ method_argument() ], identifier_info() }.
replace_identifiers_by_pid( Arguments, IdResolverPid, LoadBalancerPid,
							LineContext ) ->

	{ FirstUserIdFound, ReverseParams } = lists:foldl(

		fun( Arg, _Acc={ CurrentFirstId, Args } ) ->

			{ TransformedArg, NewFirstId } = transform_argument( Arg,
				IdResolverPid, LoadBalancerPid, CurrentFirstId, LineContext ),

			% New accumulator:
			{ NewFirstId, [ TransformedArg | Args ] }

		end,

		_InitialAcc={ _InitialFirstIdFound=none, _AccArgs=[] },

		_List=Arguments ),

	% Reversed by accumulation:
	{ lists:reverse( ReverseParams ), FirstUserIdFound }.



% Transforms specified argument, replacing {user_id,Id} by the corresponding PID
% to be used, and updating the first user identifier found.
%
% Returns {TransformedArgument, FirstUserId}.
%
-spec transform_argument( method_argument(), id_resolver_pid(),
		load_balancer_pid(), identifier_info(), line_context() ) ->
								{ method_argument(), identifier_info() }.
transform_argument( Arg, IdResolverPid, LoadBalancerPid, FirstUserId,
					LineContext ) ->

	IdTransformer = fun

		( _Term={ user_id, Id } , CurrentFirstId ) when is_list( Id ) ->

			BinId = text_utils:string_to_binary( Id ),

			IdResolverPid ! { resolve_id, BinId, LoadBalancerPid, self() },

			InstancePid = receive

				{ pid_resolved, Pid } ->
					Pid

			end,

			%trace_utils:debug_fmt( "  + transformed user ID '~ts' "
			%						"into '~p'", [ Id, InstancePid ] ),

			NewCurrentFirstId = case CurrentFirstId of

				none ->
					Id;

				Other ->
					Other

			end,

			{ InstancePid, NewCurrentFirstId };


		( _Term={ user_id, NonStringId }, _CurrentFirstId ) ->

			{ BinFilename, LineNumber, _BinLine } = LineContext,

			Filename = text_utils:binary_to_string( BinFilename ),

			%Line = text_utils:binary_to_string( BinLine ),

			throw( { non_string_user_id, NonStringId, Filename,
					 { line, LineNumber } } );
					 %,Line } );


		( Term, UserData ) ->
			%trace_utils:debug_fmt( "  + kept '~p' as is", [ Term ] ),
			{ Term, UserData }

	end,

	ast_transform:transform_term( _TargetTerm=Arg, _TypeDescription=tuple,
								  IdTransformer, _UserData=FirstUserId ).


% Reports specified error while parsing specified creation line.
-spec report_parse_error( atom() | { atom(), any() }, line_context() ) ->
								no_return().
report_parse_error( Reason,
					_LineContext={ BinFilename, LineNumber, BinLine } ) ->

	Message = text_utils:format( "In file '~ts', the parsing of creation line "
		"#~B (which is: '~ts') failed, reason: ~p. Aborting.",
		[ BinFilename, LineNumber, BinLine, Reason ] ),

	?notify_error_cat( Message, ?loader_cat ),

	% Useless (implied by macro):
	%trace_utils:error( Message ),

	Filename = text_utils:binary_to_string( BinFilename ),

	Line = text_utils:binary_to_string( BinLine ),

	throw( { invalid_creation_line, { Filename, LineNumber }, Line, Reason } ).





% Section about user identifiers.


% Launcher of the resolver of user identifiers.
user_identifier_resolver_loop( InstanceLoaderPid ) ->
	user_identifier_resolver_loop( _IdTable=table:new(), InstanceLoaderPid ).



% Main loop of the resolver of user identifiers.
%
% Its role is to keep track of user identifiers, and associate to each of them
% the PID of a process meant to host, sooner or later, the corresponding
% instance.
%
% Parameters:
%
% - IdTable :: table( user_identifier(), {blank_pid, pid()} | pid() ) is
% a table whose keys are user identifiers (as binary strings), and whose values
% are:
%
%   - either { blank_pid, P } where P is a blank PID that will host the
%   corresponding instance once it will be constructed
%
%   - or a PID already corresponding to that instance (if already created,
%   i.e. constructed and embodied in this process)
%
% - InstanceLoaderPid :: pid(), the PID of the instance loader process, as it
% must be notified of termination
%
-spec user_identifier_resolver_loop( table(), loader_pid() ) -> no_return().
user_identifier_resolver_loop( IdTable, InstanceLoaderPid ) ->

	%trace_utils:info_fmt( "User identifier in main loop." ),

	receive

		% Declaration of a user id (possibly already known):
		{ declare_id, BinId, TargetNode, LoadBalancerPid, CallerPid } ->

			NewIdTable = declare_id( BinId, TargetNode, IdTable,
									 LoadBalancerPid, CallerPid ),

			user_identifier_resolver_loop( NewIdTable, InstanceLoaderPid );


		% New reference to a user id:
		{ resolve_id, BinId, LoadBalancerPid, CallerPid } ->

			NewIdTable = resolve_id( BinId, IdTable, LoadBalancerPid,
									 CallerPid ),

			user_identifier_resolver_loop( NewIdTable, InstanceLoaderPid );


		wrap_up_and_terminate ->

			wrap_up_and_terminate( IdTable ),

			%trace_utils:debug( "User identifier resolver terminated." ),

			InstanceLoaderPid ! all_user_identifiers_resolved

	end.




% Returns an updated identifier table, and sends back a pid_assigned message.
declare_id( BinId, TargetNode, IdTable, LoadBalancerPid, CallerPid ) ->

	% We declare here a user identifier being defined, expected to be new:
	case table:lookup_entry( _Key=BinId, IdTable ) of

		key_not_found ->

			% All new instances shall be linked to the load balancer:
			BlankPid = wooper:create_hosting_process( TargetNode,
									  _ToLinkWithPid=LoadBalancerPid ),

			%trace_utils:debug_fmt(
			%       "Blank process ~w created for user id '~ts'.",
			%		[ BlankPid, BinId ] ),

			CallerPid ! { pid_assigned, BlankPid },

			table:add_entry( _K=BinId, _Value=BlankPid, IdTable );


		{ value, { blank_pid, BlankPid } } ->

			%trace_utils:debug_fmt( "Blank process ~w reused "
            %   "for user id '~ts'.", [ BlankPid, BinId ] ),

			CallerPid ! { pid_assigned, BlankPid },

			% Replacing entry (was blank, will be immediately embodied):
			table:add_entry( _K=BinId, _Value=BlankPid, IdTable );


		{ value, AssociatedPid } ->

			Message = text_utils:format( "Error, the user identifier '~ts' "
				"has been defined more than once (already associated to ~w).",
				[ BinId, AssociatedPid ] ),

			?notify_error_cat( Message, ?loader_cat ),

			throw( { multiply_defined_user_identifiers, BinId, AssociatedPid } )


	end.



% Returns an updated identifier table, and sends back a pid_resolved message.
resolve_id( BinId, IdTable, LoadBalancerPid, CallerPid ) ->

	case table:lookup_entry( _Key=BinId, IdTable ) of

		% Here, this user identifier is not known yet; we just create a blank
		% process for its future, and record that fact:
		%
		key_not_found ->

			LoadBalancerPid ! { getNodeForUserIdentifier, BinId, self() },

			NodeForID = receive

				{ wooper_result, TargetNode } ->
					TargetNode

			end,

			BlankPid = wooper:create_hosting_process( NodeForID,
										_ToLinkWithPid=LoadBalancerPid ),

			CallerPid ! { pid_resolved, BlankPid },

			%trace_utils:debug_fmt(
			%    "Blank process ~w associated to user id '~ts'.", 
            %    [ BlankPid, BinId ] ),

			table:add_entry( _K=BinId, _Value={ blank_pid, BlankPid }, 
							 IdTable );


		% Here this identifier has already been referenced:
		{ value, { blank_pid, BlankPid } } ->

			CallerPid ! { pid_resolved, BlankPid },

			%trace_utils:debug_fmt(
			%          "Already-existing blank process ~w returned for "
			%		   "user id '~ts'.", [ BlankPid, BinId ] ),

			IdTable;


		% Here this identifier has already been defined:
		{ value, AssociatedPid } ->

			CallerPid ! { pid_resolved, AssociatedPid },

			%trace_utils:debug_fmt(
			%       "Already-constructed process ~w returned for "
			%		"user id '~ts'.", [ AssociatedPid, BinId ] ),

			IdTable

	end.




% Does not return any result of interest.
wrap_up_and_terminate( IdTable ) ->

	% Let's scan the table to ensure no blank_pid remains (the accumulator will
	% be the list of still undefined user identifiers):

	CheckFun = fun

				   ( { BinId, { blank_pid, _BlankPid } }, IdAcc ) ->
					   [ BinId | IdAcc ];


				  ( { _BinId, _Pid }, IdAcc ) ->
					   IdAcc

			   end,

	case table:fold_on_entries( CheckFun, _InitialIdAcc=[], IdTable ) of

		[] ->
			ok;

		[ UndefinedId ] ->

			StringID = text_utils:binary_to_string( UndefinedId ),

			?notify_error_fmt_cat( "Error, user identifier ~p was referenced "
			  "at least once, but never defined.", [ StringID ], ?loader_cat ),

			throw( { undefined_user_identifier, StringID } );


		UndefinedIds ->

			% Better order:
			RevUndefinedIds = lists:sort( UndefinedIds ),

			?notify_error_fmt_cat(
			   "Error, ~B user identifiers were referenced "
			   "at least once, but never defined: ~ts",
			   [ length( UndefinedIds ),
				 text_utils:binaries_to_string( RevUndefinedIds ) ],
			   ?loader_cat ),

			StringIds = [ text_utils:binary_to_string( I )
						  || I <- RevUndefinedIds ],

			throw( { undefined_user_identifiers, StringIds } )


	end.



% Returns a string corresponding to the specified initialisation information,
% like:
%
% "my_city" <- {class_City, [ "Paris", "city_pa" ]}.
%
-spec get_instance_initialisation_line( wooper:classname(),
		wooper:method_arguments(), identifier_info() ) -> string().
get_instance_initialisation_line( Classname, Parameters, 
								  _UserIdentifier=none ) ->
	% Here like: { class_City, [ "Paris", "city_pa" ] }.
	get_instance_description( Classname, Parameters );

get_instance_initialisation_line( Classname, Parameters,
								  UserIdentifier ) ->
	text_utils:format( "\"~ts\" <- ", [ UserIdentifier ] )
		++ get_instance_description( Classname, Parameters ).



% (helper)
get_instance_description( Classname, Parameters ) ->

	StringParams = text_utils:format( "~p", [ Parameters ] ),

	NoReturnStringParams = text_utils:filter( $\n, StringParams ),

	text_utils:format( "{ ~ts, ~ts }.\n", [ Classname, NoReturnStringParams ] ).
