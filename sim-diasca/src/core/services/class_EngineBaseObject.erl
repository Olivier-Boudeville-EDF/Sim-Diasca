% Copyright (C) 2017-2021 EDF R&D

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


-module(class_EngineBaseObject).


-define( class_description,
		 "Abstract base class common to all Sim-Diasca instances, whence most "
		 "actual classes are to inherit, directly or not (root of the "
		 "Sim-Diasca class hierarchy). "
		 "Useful to be able to introduce uniformly common, transverse, "
		 "possibly new behaviours such as trace sending, serialization, etc. "
		 "at the engine level." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_TraceEmitter ] ).


% No class-specific attribute defined:
-define( class_attributes, [] ).


% To have the trace messages adequately sorted:
-define( trace_emitter_categorization, "Core" ).


% The PID of an engine base object:
-type object_pid() :: class_TraceEmitter:emitter_pid().

-export_type([ object_pid/0 ]).


% Helpers:
-export([ init/1, to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% For the trace_categorize/1 macro:
-include_lib("traces/include/class_TraceEmitter.hrl").


% Constructs a new, named Sim-Diasca base object.
-spec construct( wooper:state(), class_TraceEmitter:emitter_init() ) ->
					   wooper:state().
construct( State, InstanceName ) ->
	% Only direct mother class; updated state returned directly:
	class_TraceEmitter:construct( State, ?trace_categorize(InstanceName) ).



% Static section.


% Returns, should this base object have been deployed (i.e. should it run on a
% computing node), the (object-local) root directory of the deployment tree.
%
-spec get_deployment_root_directory() ->
				   static_return( file_utils:directory_name() ).
get_deployment_root_directory() ->

	% Returns typically /tmp/sim-diasca-$CASE-$USER-$TIME/deployed-elements",
	% in which all layers (ex: myriad, wooper, etc.) are located:
	%
	wooper:return_static( file_utils:get_current_directory() ).



% Returns the names of all the base state attributes (be they defined by this
% class or inherited).
%
-spec get_all_base_attribute_names() ->
							 static_return( [ wooper:attribute_name() ] ).
get_all_base_attribute_names() ->

	AttrNames =
		wooper_introspection:get_class_specific_attribute_names( ?MODULE )
		++ list_utils:flatten_once(
			 [ wooper_introspection:get_class_specific_attribute_names( C )
			   || C <- ?superclasses ] ),

	wooper:return_static( AttrNames ).



% Helper section.


% Initializes some context-specific information.
-spec init( wooper:state() ) -> wooper:state().
init( State ) ->
	class_TraceEmitter:init( State ).



% Returns a textual description of this instance.
-spec to_string( wooper:state() ) -> string().
to_string( State ) ->
	text_utils:format( "Sim-Diasca base object named '~s'",
					   [ ?getAttr(name) ] ).
