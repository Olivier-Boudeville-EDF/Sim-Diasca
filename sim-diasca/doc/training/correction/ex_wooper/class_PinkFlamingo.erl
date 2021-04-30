% Copyright (C) 2008-2021 EDF R&D
%
% This file is part of the Sim-Diasca training material.
%
% It has been placed in the public domain.
%
% Author: Olivier Boudeville (olivier.boudeville@edf.fr)
%
-module(class_PinkFlamingo).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ViviparousBeing ] ).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").



-type name() :: ustring().

% Expressed in centimeters:
-type heigth() :: float().

-type color() :: 'pink' | 'yellow' | 'blue'.

-type flamingo_pid() :: class_Actor:actor_pid().

-type filtering_location() :: 'camargue' | 'chile'.


% Shorthands:
-type ustring() :: text_utils:ustring().


% Constructs a new PinkFlamingo.
%
% Note: this is *not* a Sim-Diasca actor.
%
-spec construct( wooper:state(), name(), height() ) -> wooper:state().
construct( State, Name, Height ) when is_list( Name )
		andalso is_float( Height ) ->

	% First the direct mother classes:
	ViviparousBeingState = class_ViviparousBeing:construct( State ),

	% Then the class-specific attributes:
	setAttributes( ViviparousBeingState, [ { name, Name },
										   { height, Height },
										   {feather_color,pink} ] ).



% Requests the flamingo to filter plankton in specified location.
-spec filterPlankton( wooper:state(), filetring_location() ) -> oneway_return().
filterPlankton( State, camargue ) ->

	NewHeight = ?getAttr(height) + 2.5,

	trace_utils:notice( "[~ts] Gobble, gobble, my height is now ~f cm.",
						[ ?getAttr(name), NewHeight ] ),

	wooper:return_state( setAttribute( State, height, NewHeight ) );


filterPlankton( State, chile ) ->

	NewHeight = ?getAttr(height) + 1,

	trace_utils:notice( "[~ts] Gurgle, gurgle, my height is now ~f cm.",
						[ ?getAttr(name), NewHeight ] ),

	wooper:return_state( setAttribute( State, height, NewHeight ) ).



% Returns the feather color of the flamingo.
%
% Could be a static method if we knew for sure that all flamingos were pink.
%
-spec getFeatherColor( wooper:state() ) -> const_request_return( color() ).
getFeatherColor( State ) ->
	wooper:const_return( ?getAttr(feather_color) ).



% Returns the mean children count for that flamingo (actually does not depend on
% any specific flamingo).
%
-spec getMeanChildrenCount( wooper:state() ) ->
								const_request_return( basic_utils:count() ).
getMeanChildrenCount( State ) ->
	wooper:const_return_result( 1.7 ).



% Static section.


% Let's say an average means something here:

% (this is a static method, as it does not depend on a state)
%
get_mean_children_count() ->
	wooper:return_static( 1.7 ).
