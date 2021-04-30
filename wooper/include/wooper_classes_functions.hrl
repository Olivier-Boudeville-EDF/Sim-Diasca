% Copyright (C) 2003-2021 Olivier Boudeville
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


% Modular WOOPER header gathering the class-related primitives (function
% definitions).


% Note: functions below (ex: getClassname/1) are "verbatim" methods: rather than
% being generated through a parse-transform (which would bring no added value),
% it is actually more convenient to inject their code thanks to a header file.
% However these verbatim methods shall not be considered as built-in, as we want
% to have them registered (known of the WOOPER parse transforms) as methods (ex:
% so that they are themselves transformed, since they use method terminators).

% Request returning the classname of the instance.
%
% Always accurate, in all constructors, methods and destructors.
%
-spec getClassname( wooper:state() ) -> const_request_return( classname() ).
getClassname( State ) ->
	wooper:const_return_result( State#state_holder.actual_class ).



% Method that returns the (direct) superclasses of the instance.
%
% Always accurate, in all constructors, methods and destructors.
%
-spec getSuperclasses( wooper:state() ) ->
							 const_request_return( [ classname() ] ).
getSuperclasses( State ) ->
	ActualModule = State#state_holder.actual_class,
	SuperClasses = ActualModule:get_superclasses(),
	wooper:const_return_result( SuperClasses ).



-ifdef(wooper_debug_mode).


% Returns a full textual description of this instance, including its state and
% virtual table.
%
% This is a method for debug purpose, only activated if wooper_debug_mode is
% defined.
%
-spec wooper_get_instance_description( wooper:state() ) ->
							 const_request_return( text_utils:ustring() ).
wooper_get_instance_description( State ) ->
	wooper:const_return_result( wooper:instance_to_string( State ) ).


-endif. % wooper_debug_mode
