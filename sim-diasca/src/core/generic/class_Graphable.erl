% Copyright (C) 2008-2021 EDF R&D

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


-module(class_Graphable).


% See also: core/mesh/src/class_Mesh.erl
-define( class_description,
		 "Graphable class, base of all instances able to output a textual "
		 "description of their state in the context of graph rendering." ).



% Implementation notes:
%
% We rely here on Dot (see https://www.graphviz.org/).
%
% This class does not inherit from class_Actor, yet, in some cases, it may send
% actor messages. This is a bad practice!
%
% Being a trace emitter is not strictly needed here, as it would lead to useless
% diamond-shaped multiple inheritance.



% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


% The class-specific attributes:
-define( class_attributes, [

			{ node_name, net_utils:string_node_name(),
			  "the node name of interest" } ] ).


% Helper functions.
-export([ forge_node_name/0, forge_node_name/1, graph_options_to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include("sim_diasca_for_spatialised_actors.hrl").


-type dot_option_name() :: 'label' | 'style' | 'height' | 'width' | 'fixedsize'
	 | 'shape' | 'fillcolor' | 'color' | 'bgcolor' | 'penwidth' | 'pencolor'.


-define( dot_option_list, [ label, style, height, width, fixedsize, shape,
							fillcolor, color, bgcolor, penwidth, pencolor ] ).


-type dot_option_value() :: any().


-type options() :: string() | [ { dot_option_name(), dot_option_value() } ].


-type graphable_name() :: net_utils:string_node_name().

-type graphable_label() :: text_utils:label().



% Constructs a new graphable instance.
%
% OptionParameters is:
%
% - either a label, like "hello"
%
% - or a list of option pairs like { dot_option_name, option_value } in which at
% least the label is defined
%
% Ex: [ { label, "hello" }, { color, red } ].
%
% Note that options must be valid dot options.
%
% See the dot_option_list macro.
%
-spec construct( wooper:state(), options() ) -> wooper:state().
construct( State, OptionParameters ) ->

	% First the direct mother classes, then this class-specific actions:
	NamedState = setAttribute( State, node_name, forge_node_name() ),

	interpret_option_list( OptionParameters, NamedState ).




% Methods section.


% Returns a name, derived from current PID, adequate to be a node identifier.
-spec getNodeName( wooper:state() ) ->
						 const_request_return( graphable_name() ).
getNodeName( State ) ->
	wooper:const_return_result( ?getAttr(node_name) ).



% Returns the description of this Graphable.
-spec getLabel( wooper:state() ) -> const_request_return( graphable_label() ).
getLabel( State ) ->
	wooper:const_return_result( ?getAttr(label) ).



% Sets the label of this Graphable.
-spec setLabel( wooper:state(), graphable_label() ) -> oneway_return().
setLabel( State, NewLabel ) ->
	wooper:return_state( setAttribute( State, label, NewLabel ) ).



% Returns { GraphableNodeName, OptionList } where GraphableNodeName is the
% generated name for this graphable, and OptionList is the list of all attribute
% name/value pairs corresponding to dot options for that Graphable.
%
-spec getGraphInformation( wooper:state() ) ->
			const_request_return( { graphable_name(), options() } ).
getGraphInformation( State ) ->
	wooper:const_return_result( { ?getAttr(node_name),
								  select_attributes_from( State ) } ).



% Returns the list of all attribute name/value pairs corresponding to dot
% options for that Graphable.
%
-spec getGraphOptions( wooper:state() ) -> const_request_return( options() ).
getGraphOptions( State ) ->
	wooper:const_return_result( select_attributes_from( State ) ).



% Returns the list of all attribute name/value pairs corresponding to dot
% options for that Graphable.
%
% Triggers back a setGraphOptions actor call.
%
% Note: supposed to be called, through inheritance, on an Actor instance.
%
-spec getGraphOptions( wooper:state(), sending_actor_pid() ) ->
							 actor_oneway_return().
getGraphOptions( State, CallerPid ) ->

	Options = select_attributes_from( State ),

	% Note the double list for options:
	SentState = class_Actor:send_actor_message( CallerPid,
		{ setGraphOptions, [ Options ] }, State ),

	actor:return_state( SentState ).



% Sets the specified option list, regarding graphable parameters.
-spec setGraphOptions( wooper:state(), options() ) -> oneway_return().
setGraphOptions( State, OptionParameters ) ->
	wooper:return_state( interpret_option_list( OptionParameters, State ) ).




% Section for helper functions (not methods).


% Interprets the option list specified for a graphable.
interpret_option_list( _Options=[], State ) ->
	State;

interpret_option_list( _Options=[ { label, Label } | T ], State ) ->
	interpret_option_list( T,
		setAttribute( State, label, transform_label( Label ) ) );

interpret_option_list( [ { OptionName, OptionValue } | T ], State ) ->

	case lists:member( OptionName, ?dot_option_list ) of

		true ->
			interpret_option_list( T,
				setAttribute( State, OptionName, OptionValue ) );

		false ->
			throw( { unknown_dot_option, OptionName } )

	end;

interpret_option_list( Label, State ) ->
	interpret_option_list( _Options=[],
		setAttribute( State, label, transform_label( Label ) )  ).



% Forges a suitable graphable name for the current instance.
-spec forge_node_name() -> graphable_name().
forge_node_name() ->
	forge_node_name( self() ).


-spec forge_node_name( pid() ) -> graphable_name().
forge_node_name( Pid ) when is_pid( Pid ) ->
	% Ex: <0.59.0> becoming "|59|":
	text_utils:pid_to_short_string( Pid ).


% Splits specified label, one word per line.
transform_label( Label ) ->
	separate_in_lines( string:tokens( Label, " " ) ).


separate_in_lines( WordList ) ->
	separate_in_lines( WordList, "" ).


separate_in_lines( _WordList=[], ResultingString ) ->
	ResultingString;

separate_in_lines( _WordList=[ H | T ], ResultingString ) ->
	separate_in_lines( T, ResultingString ++ H ++ "\\n" ).



% Select only the known dot attributes.
%
select_attributes_from( State ) ->

	AttributeList = wooper:get_all_attributes( State ),

	select_attributes_from( AttributeList, _Acc=[] ).


select_attributes_from( _AttributeList=[], Acc ) ->
	Acc;

select_attributes_from( _AttributeList=[ { Name, Value } | T ], Acc ) ->
	case lists:member( Name, ?dot_option_list ) of

		true ->
			select_attributes_from( T, [ { Name, Value } | Acc ] );

		false ->
			select_attributes_from( T, Acc )
	end.



% Returns a string corresponding to specified graph options.
%
-spec graph_options_to_string( options() ) -> string().
graph_options_to_string( OptionsList ) ->
	graph_options_to_string( OptionsList, _Acc=[] ).


graph_options_to_string( _OptionsList=[], Acc ) ->
	Acc;

graph_options_to_string( _OptionsList=[ { label, Label } | T ], Acc ) ->
	graph_options_to_string( T,
		Acc ++ text_utils:format( " label='~s'", [ Label ] ) );

graph_options_to_string( _OptionsList=[ { OptionName, OptionValue } | T ],
						 Acc ) ->
	graph_options_to_string( T,
		Acc ++ text_utils:format( " ~s=~w", [ OptionName, OptionValue ] ) ).
