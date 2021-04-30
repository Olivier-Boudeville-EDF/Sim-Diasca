% Copyright (C) 2019-2021 EDF R&D

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
% Creation date: Friday, June 14, 2019.


-module(class_WebProbe).


-define( class_description,
		 "Abstract web-based probe class, in charge of generating results "
		 "to be displayed in a (web) browser; depends on the web manager." ).


% See web_result_management_test.erl for a test of these probes.


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ResultProducer ] ).


% Note that, prior to using such probe, the simulation case must have enabled
% the webserver support. This is done thanks to the enable_webmanager field of
% the deployment_settings record.
%
% We deemed that introducing an intermediary WebResultProducer class was not
% useful enough.


% A web probe is a result producer that generates web content in a dedicated
% root, so that the web manager is able to make it available to the user,
% through a browser.

% In terms of life-cycle, as basic probes, web probes that are tracked by the
% result manager are managed by it (i.e. deallocated when appropriate),
% otherwise (if not tracked) their creator keeps their ownership.


% Unlike basic probes (class_Probe), which are generic and meant to be set
% through method calls, web probes are typically meant to be subclassed.  As
% such, the creation helpers defined in the current class are more sources of
% inspirations rather than usable facilities.



% Attributes that are specific to a web probe instance are:
-define( class_attributes, [

  { web_dir, bin_directory_path(), "the directory where the (web)
	content shall be written (i.e. the local, temporary content root for this
	probe)" },

  { web_filename, bin_file_name(), "the (main, local to web content)
	filename (typically with a '.html' extension) in which this probe is to
	write its content" },

  { web_manager_pid, web_manager_pid(),
	"the PID of the overall web manager" },

  { available_content_files, [ bin_file_path() ],
	"a list of the names of the content files that are currently available "
	"(probably after having been generated), notably in the context of "
	"result sending" },

  { meta_data, class_ResultManager:meta_data(), "corresponds to the meta-data "
	"to be added in probe-generated data files" }

  % Commented-out and not used anymore as misleading: both the webserver
  % installation and the engine tree are on the user host, whereas a probe may
  % be on any other host:

  %{ webserver_install_root, file_utils:bin_directory_path(),
  % "the root directory of the webserver runtime install" },

  %{ engine_root_dir, file_utils:bin_directory_path(),
  % "the root directory of the engine" }

						   ] ).




% A probe may not be a wanted result producer (cf. 'non_wanted_probe'):
-type probe_ref() :: class_Probe:probe_ref().

-type probe_pid() :: class_Probe:probe_pid().

-type name_options() :: probe_name() | { probe_name(), web_probe_options() }.


-export_type([ probe_ref/0, probe_pid/0, web_probe_options/0, name_options/0 ]).



% Shorthands:

-type probe_name() :: class_ResultProducer:producer_name().

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().

-type bin_file_name() :: file_utils:bin_file_name().

-type directory_path() :: file_utils:directory_path().

% In attributes:
%-type bin_directory_path() :: file_utils:bin_directory_path().
%-type bin_file_path() :: file_utils:bin_file_path().


% Exported helpers:
-export([ declare_content_file/2, to_string/1 ]).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.ResultManagement.Probe.Web" ).


% For the web probe settings:
-include("class_WebProbe.hrl").


% For getAttr/1, etc.:
-include_lib("wooper/include/wooper.hrl").

% For app_info*:
-include_lib("traces/include/traces.hrl").

% For web_manager_name:
-include("class_WebManager.hrl").




% Constructs a new web probe, from:
%
% - NameInit tells about the name (and possibly categorization) of this probe
%
% - ProbeOptions, if available, specifies the options that shall apply
%
% - MetaData is an option list that corresponds to extra, contextual information
% that can be taken into account in the probe-generated data files
%
-spec construct( wooper:state(), class_Probe:probe_name_init() |
				 { class_Probe:probe_name_init(), web_probe_options() },
				 class_ResultManager:meta_data() ) -> wooper:state().
construct( State, { NameInit, ProbeOptions }, Metadata )
  when is_record( ProbeOptions, web_probe_options ) ->

	%trace_utils:debug_fmt( "Creating a web probe '~ts' from ~p.",
	%					   [ NameInit, ProbeOptions ] ),

	% Early interleaving:

	ProbeName = class_Probe:get_actual_probe_name( NameInit ),

	% The deployment of the *webserver* is currently asynchronous (as it is
	% launched in the background; relevant HTTP connections would have to be
	% done in order to poll when/if it becomes available), so we cannot tell for
	% sure when it is ready; yet the creation of the web manager *is*
	% synchronous, so this manager must already be available and thus no
	% specific waiting is to be to performed here:
	%
	WebManagerPid =
		naming_utils:get_registered_pid_for( ?web_manager_name, global ),

	ProducerState = class_ResultProducer:construct( State,
										?trace_categorize(NameInit) ),

	% Such an instance is both a probe and a web element, so:

	BinProbeName = text_utils:string_to_binary( ProbeName ),

	{ IsTrackedProducer, MaybeBinProbeDir } = interpret_options( ProbeOptions ),

	% Thanks to WOOPER, even if we are here in the mother class of the actual,
	% non-already constructed class (ex: could be class_TestWebProbe), we
	% already know the actual, most precise classname that we will be using in
	% order to trigger, on the web manager side, any relevant code that is
	% specific to this class:
	%
	ActualClassname = wooper:get_classname( ProducerState ),

	WebManagerPid ! { declareWebProbe,
				[ BinProbeName, ActualClassname, MaybeBinProbeDir ], self() },

	ResultManagerPid = getAttribute( ProducerState, result_manager_pid ),

	ResultManagerPid ! { declareWebProbe, [ BinProbeName, IsTrackedProducer ],
						 self() },

	BinFilename = get_filename_for( ProbeName ),

	receive

		% From web manager's declareWebProbe/4:
		{ wooper_result, web_probe_declared } ->

			% If a directory is specified in the options, it supersedes the
			% default one (which is the current deployed directory):
			%
			ActualBinRootDir = case MaybeBinProbeDir of

				undefined ->
					text_utils:string_to_binary(
						file_utils:get_current_directory() );

				_ ->
					MaybeBinProbeDir

			end,

			BinProbeFilePath = file_utils:join( ActualBinRootDir, BinFilename ),

			case file_utils:is_existing_file_or_link( BinProbeFilePath ) of

				true ->

					?send_error_fmt( ProducerState,
						"Already existing probe filename (~ts). "
						"Multiple web probes declared as being named '~ts'?",
						[ BinProbeFilePath, ProbeName ] ),

					throw( { already_existing_web_probe_filename,
							 BinProbeFilePath } );

				false ->
					ok

			end,

			ContentState = setAttributes( ProducerState, [
					{ web_dir, ActualBinRootDir },
					{ web_filename, BinFilename },
					{ web_manager_pid, WebManagerPid },
					{ available_content_files, [] },
					{ meta_data, Metadata } ] ),

			?send_notice_fmt( ContentState, "Created ~ts.",
							  [ to_string( ContentState ) ] ),

			% From result manager:
			class_Probe:wait_result_declaration_outcome( ProbeName,
														 ContentState )

	end;


construct( State, Name, Metadata ) ->
	construct( State, { Name, _DefaultOptions=#web_probe_options{} },
			   Metadata ).



% (helper)
interpret_options( #web_probe_options{
					  register_as_tracked_producer=IsTrackedProducer,
					  probe_directory=undefined } ) ->
	{ IsTrackedProducer, undefined };

interpret_options( #web_probe_options{
					  register_as_tracked_producer=IsTrackedProducer,
					  probe_directory=ProbeDir } ) ->
	{ IsTrackedProducer, text_utils:string_to_binary( ProbeDir ) }.



% Overridden destructor.
-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->

	% Class-specific actions:

	?info( "Deleting web probe." ),

	%?debug( "Probe deleted." ),

	% Then call the direct mother class counterparts and allow chaining:
	State.




% Methods section.



% Sets the content of the main file of this web probe.
-spec setMainContent( wooper:state(), any_string() ) ->
							request_return( 'content_set' ).
setMainContent( State, ContentString ) when is_list( ContentString ) ->

	{ SetState, Res } =
		setMainContent( State, text_utils:string_to_binary( ContentString ) ),

	wooper:return_state_result( SetState, Res );


setMainContent( State, BinContent ) when is_binary( BinContent ) ->

	ContentFilename = ?getAttr(web_filename),

	TargetPath = file_utils:join( ?getAttr(web_dir), ContentFilename ),

	?debug_fmt( "Writing (main) content of web probe in '~ts', namely:~n~ts",
				[ TargetPath, BinContent ] ),

	file_utils:write_whole( TargetPath, BinContent ),

	DeclaredState = declare_content_file( ContentFilename, State ),

	wooper:return_state_result( DeclaredState, content_set ).



% Sends the specified type of (tracked) results to the caller (generally the
% result manager).
%
% (request, notably for synchronous operations)
%
-spec sendResults( wooper:state(), class_ResultProducer:producer_options() ) ->
					request_return( class_ResultProducer:producer_result() ).
sendResults( State, _ProducerOptions ) ->

	%trace_utils:debug_fmt( "sendResults/2 called for web probe ~w",
	%                       [ self() ] ),

	% For web probes, data and rendering are the same: web content is produced,
	% and the webserver is to display them.

	% Check:
	false = ?getAttr(result_collected),

	% Call to a (possibly overridden) empty callback method, so that any
	% finalization of a web content (ex: the addition of a web footer) can be
	% done when needed:
	%
	FinalizedState = executeOneway( State, onFinalizationTime ),

	% An actual (specialised through inheritance) web probe is expected to
	% generate a content (through overridden methods) in all cases:
	%
	Result = case getAttribute( FinalizedState, available_content_files) of

		[] ->
			?debug( "No available content for sending." ),
			{ self(), no_result };


		[ BinFilename ] ->

			?debug_fmt( "Sending a single content file, '~ts'.",
						[ BinFilename ] ),

			BinContentFilename =
						file_utils:join( ?getAttr(web_dir), BinFilename ),

			BinContent = file_utils:read_whole( BinContentFilename ),

			%trace_utils:debug_fmt( "Removing '~ts'.", [ BinContentFilename ] ),

			file_utils:remove_file( BinContentFilename ),

			{ self(), raw, { BinFilename, BinContent } };


		BinFilenames ->

			WebDir = text_utils:binary_to_string( ?getAttr(web_dir) ),

			Filenames = [ text_utils:binary_to_string( F )
							|| F <- BinFilenames ],

			?debug_fmt( "Sending, from ~ts, ~B content file(s): ~ts",
						[ WebDir, length( Filenames ),
						  text_utils:strings_to_string( Filenames ) ] ),

			BinArchive = file_utils:files_to_zipped_term( Filenames, WebDir ),

			FilesToRemove = [ file_utils:join( WebDir, F ) || F <- Filenames ],

			file_utils:remove_files( FilesToRemove ),

			{ self(), archive, BinArchive }

	end,

	LastState = setAttribute( FinalizedState, result_collected, true ),

	wooper:return_state_result( LastState, Result ).



% Empty oneway callback, meant to be overridden if needed.
-spec onFinalizationTime( wooper:state() ) -> const_oneway_return().
onFinalizationTime( State ) ->

	% No-op here.

	wooper:const_return().



% Static methods.


% Creates a facility probe, i.e. a lingering probe, to be created (unilaterally)
% from a test case, and that will not to considered as a result.
%
% NameOptions is either:
%
% - Name :: ustring(); i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
% - or {Name :: ustring(), ProbeOptions :: web_probe_options()}
%
-spec create_facility_probe( name_options() ) -> static_return( probe_ref() ).
create_facility_probe( NameOptions ) ->

	ProbeDirectory = file_utils:get_current_directory(),

	Res = create_facility_probe( NameOptions, ProbeDirectory ),

	wooper:return_static( Res ).



% Creates a facility probe, i.e. a lingering probe, to be created (unilaterally)
% from a test case, and that will not to considered as a result.
%
% The first parameter is either:
%
% - Name :: ustring(); i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
% - or {Name :: ustring(), ProbeOptions :: web_probe_options() }
%
-spec create_facility_probe( name_options(), directory_path() ) ->
									static_return( probe_ref() ).
create_facility_probe( { Name, Options }, ProbeDirectory ) ->

	% Overrides any previous probe directory definition:
	NewOptions = Options#web_probe_options{ register_as_tracked_producer=false,
											probe_directory=ProbeDirectory },

	ProbePid = class_WebProbe:synchronous_new_link( { Name, NewOptions },
													_Metadata=[] ),

	wooper:return_static( ProbePid );


create_facility_probe( Name, ProbeDirectory ) ->

	% Overrides any previous probe directory definition:
	Options = #web_probe_options{ register_as_tracked_producer=false,
								  probe_directory=ProbeDirectory },

	ProbePid = class_WebProbe:synchronous_new_link( { Name, Options },
													_Metadata=[] ),

	wooper:return_static( ProbePid ).



% Declares (synchronously) a new (web) probe, to be seen as a result producer,
% and be created either from an actor or from a test case.
%
% - NameOptions is either:
%
%  - Name :: ustring(); i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions :: web_probe_options()}
%
% Returns either the PID of this newly created probe (if the name of that probe
% is acknowledged as a wanted result by the result manager), or the
% 'non_wanted_probe' atom.
%
-spec declare_result_probe( name_options() ) -> static_return( probe_ref() ).
declare_result_probe( NameOptions ) ->

	case is_wanted( NameOptions ) of

		false ->
			wooper:return_static( non_wanted_probe );

		Metadata ->
			% Created in current directory (i.e. the one for temporary data):
			wooper:return_static(
			  class_WebProbe:synchronous_new_link( NameOptions, Metadata ) )

	end.



% Tells whether the specified web probe is wanted.
%
% (useful factored code for child classes)
%
-spec is_wanted( name_options() ) ->
				static_return( 'false' | class_ResultManager:meta_data() ).
is_wanted( NameOptions ) ->

	%trace_utils:debug_fmt( "Declaring result probe, with ~p.",
	%					   [ NameOptions ] ),

	ActualName = case NameOptions of

		 { Name, _ProbeOptions } ->
			Name;

		 Name when is_list( Name ) ->
			Name

	end,

	ActualBinName = text_utils:string_to_binary( ActualName ),

	ResultManagerPid = class_ResultManager:get_result_manager(),

	ResultManagerPid ! { isResultProducerWanted,
						 [ ActualBinName, _Nature=web_probe ], self() },

	receive

		{ wooper_result, { true, Metadata } } ->
			wooper:return_static( Metadata );

		{ wooper_result, false } ->
			wooper:return_static( false )

	end.



% Deletes specified facility (web) probe (knowing that the other kinds of probes
% are results, and thus their life cycles are managed by the result manager).
%
-spec delete_facility_probe( probe_ref() ) -> static_void_return().
delete_facility_probe( ProbePid ) when is_pid( ProbePid ) ->

	%trace_utils:debug( "Deleting this facility web probe." ),

	% Disable checking for this very specific case:
	ProbePid ! { setResultCollectedStatus, true },

	% This is necessarily a PID, not a 'non_wanted_probe' atom; synchronicity is
	% better here, to detect all failures:
	%
	wooper:delete_synchronously_instance( ProbePid ),

	wooper:return_static_void().



% Returns the filename (not a full path) of the main HTML page corresponding to
% the specified probe.
%
-spec get_filename_for( probe_name() ) -> static_return( bin_file_name() ).
get_filename_for( WebProbeName ) ->

	BinFilename = text_utils:string_to_binary( file_utils:convert_to_filename(
		  text_utils:format( "web-probe-~ts.html", [ WebProbeName ] ) ) ),

	%trace_utils:debug_fmt( "The filename corresponding to the web probe "
	%    "named '~ts' is: '~ts'.", [ WebProbeName, BinFilename ] ),

	wooper:return_static( BinFilename ).



% Helpers.



% Declares the specified content file among the results of that probe.
%
% Implies that this web probe will be considered as having produced a result.
%
% Note that only a filename is to be specified (relative to web_dir), not a
% full, absolute path).
%
-spec declare_content_file( bin_file_name(), wooper:state() ) -> wooper:state().
declare_content_file( BinFilename, State ) ->

	ContentFiles = ?getAttr(available_content_files),

	% Poor man's set:
	NewContentFiles = case lists:member( BinFilename, ContentFiles ) of

		true ->
			ContentFiles;

		false ->
			[ BinFilename | ContentFiles ]

	end,

	setAttributes( State, [ { available_content_files, NewContentFiles },
							{ result_produced, true } ] ).



% Returns a textual description of this web probe.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

	ContentString = case ?getAttr(available_content_files) of

		[] ->
			"no content currently available";

		Files ->
			text_utils:format( "~B content file(s) currently available: ~ts",
							   [ length( Files ), Files ] )

	end,

	% meta_data not taken into account here.

	text_utils:format( "web probe using content web directory '~ts', "
		"producing main web file '~ts', linked to "
		"web manager ~w and to result manager ~w, having ~ts" ,
		[ ?getAttr(web_dir), ?getAttr(web_filename), ?getAttr(web_manager_pid),
		  ?getAttr(result_manager_pid), ContentString ] ).
