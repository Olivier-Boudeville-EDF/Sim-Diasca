% Copyright (C) 2019-2023 EDF R&D

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

% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: Wednesday, June 19, 2019.


% @doc Test web probe.
-module(class_TestWebProbe).


-define( class_description, "Test for web-based probes." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_WebProbe ] ).



% Attributes that are specific to a web probe instance are:
-define( class_attributes, [
	{ counter, count(), "a counter to track state updates" } ] ).


% Must be included before class_TraceEmitter header:
-define( trace_emitter_categorization, "Core.ResultManagement.Probe.Web" ).

% For getAttr/1, etc.:
-include_lib("wooper/include/wooper.hrl").

% For app_info*:
-include_lib("traces/include/traces.hrl").


% For web_manager_name:
-include("class_WebManager.hrl").


% Shorthands:

-type count() :: basic_utils:count().
-type ustring() :: text_utils:ustring().



% @doc Constructs a web probe, from NameInit, which tells about the name (and
% possibly categorization) of this probe.
%
% Note: knowing that the creation of a probe has to be acknowledged by the
% result manager, actors are not expected to create directly such a probe, they
% ought to call declare_result_probe/1 instead.
%
-spec construct( wooper:state(), class_Probe:probe_name_init() |
		{ class_Probe:probe_name_init(), class_WebProbe:web_probe_options() },
				 class_ResultManager:meta_data() ) -> wooper:state().
construct( State, NameInit, Metadata ) ->

	ProbeState = class_WebProbe:construct( State, NameInit, Metadata ),

	setAttribute( ProbeState, counter, 1 ).




% Methods section.



% @doc Declares (synchronously) a new test (web) probe, to be seen as a result
% producer, and to be created either from an actor or from a test case.
%
% - NameOptions is either:
%
%  - Name :: ustring(), i.e. directly the name of this probe (specified as a
%  plain string), which will be used for the generated data and command files
%
%  - or {Name :: ustring(), ProbeOptions :: web_probe_options()}
%
% Returns either the PID of this newly created probe (if the name of that probe
% is acknowledged as a wanted result by the result manager), or the
% 'non_wanted_probe' atom.
%
-spec declare_result_probe( class_WebProbe:name_options() ) ->
								static_return( class_WebProbe:probe_ref() ).
declare_result_probe( NameOptions ) ->

	case class_WebProbe:is_wanted( NameOptions ) of

		false ->
			wooper:return_static( non_wanted_probe );

		Metadata ->
			% Created in current directory (i.e. the one for temporary data):
			wooper:return_static(
				synchronous_new_link( NameOptions, Metadata ) )

	end.



% @doc Emulates the feeding of that probe.
-spec update( wooper:state(), count() ) -> request_return( 'probe_updated' ).
update( State, NewCount ) ->

	NewState = setAttribute( State, counter, NewCount ),

	GenState = generate_html( NewState ),

	?notice_fmt( "Updating probe: ~ts", [ to_string( GenState ) ] ),

	wooper:return_state_result( GenState, probe_updated ).




% Helpers.


% @doc Generates the test HTML.
-spec generate_html( wooper:state() ) -> wooper:state().
generate_html( State ) ->

	Content = "Testing!!!",

	{ SetState, content_set } =
		executeRequest( State, setMainContent, [ Content ] ),

	SetState.



% @doc Returns a textual description of this web probe.
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->
	text_utils:format( "test ~ts", [ class_WebProbe:to_string( State ) ] ).
