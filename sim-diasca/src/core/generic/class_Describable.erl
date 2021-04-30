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


-module(class_Describable).


-define( class_description,
		 "Base of all instances able to output a textual description of "
		 "their state." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [] ).


% Declaration of class-specific attributes:
-define( class_attributes, [

			 { description, description(), "description held by this instance" }

						   ] ).


% Helper functions.
-export([ get_description/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


-type description() :: string().


% Implementation notes:
%
% Being a trace emitter is not strictly needed, as it leads to useless
% diamond-shaped multiple inheritance.


% Constructs a new describable instance, based on a record of an in-world
% location.
%
-spec construct( wooper:state(), description() ) -> wooper:state().
construct( State, Description ) ->
	% First the direct mother classes, then this class-specific actions:
	setAttribute( State, description, Description ).



% Methods section.


% Returns the description of this Describable.
-spec getDescription( wooper:state() ) -> const_request_return( description() ).
getDescription( State ) ->
	wooper:const_return_result( ?getAttr(description) ).



% Sets the description of this Describable.
-spec setDescription( wooper:state(), description() ) -> oneway_return().
setDescription( State, NewDescription ) ->
	wooper:return_state(
	   setAttribute( State, description, NewDescription ) ).



% Section for helper functions (not methods).


% Returns the description of this Describable.
%
% Note: is never and cannot be overloaded.
%
% (helper)
%
-spec get_description( wooper:state() ) -> description().
get_description( State ) ->
	?getAttr(description).
