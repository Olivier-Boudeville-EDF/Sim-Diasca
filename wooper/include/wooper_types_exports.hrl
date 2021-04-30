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


% Modular WOOPER header gathering the main types of interest.


% Allows to define WOOPER base variables and methods for that class.

% We actually define these types in the WOOPER module (wooper.erl), so that they
% are defined only once yet can be referred to with or without the 'wooper:'
% prefix.


% Not module(), which can be also a tuple():
-type classname() :: wooper:classname().


-type method_name() :: wooper:method_name().

-type request_name() :: wooper:request_name().
-type oneway_name()  :: wooper:oneway_name().


% List of arguments, or non-list standalone one:
-type method_argument() :: wooper:method_argument().

% Standalone (non-list) arguments may be specified:
-type method_arguments() :: wooper:method_arguments().

% Special case of construction parameters:
-type construction_parameters() :: wooper:construction_parameters().


-type request_call() :: wooper:request_call().
-type oneway_call()  :: wooper:oneway_call().


-type method_internal_result() :: wooper:method_internal_result().


% The actual value of interest returned by a request:
-type request_result( T ) :: wooper:request_result( T ).


% Describes the outcome of a set of requests: either all succeeded, or some
% failed (that are then specified).
%
-type requests_outcome() :: wooper:requests_outcome().


% The result of the execution of a request:
-type request_result() :: any().



-type request_return( T ) :: wooper:request_result( T ).
-type const_request_return( T ) :: wooper:const_request_return( T ).

-type oneway_return() :: wooper:oneway_return().
-type const_oneway_return() :: wooper:const_oneway_return().

-type static_return( T ) :: wooper:static_return( T ).

-type static_void_return() :: static_return( 'wooper_void_return' ).



-type attribute_name() :: wooper:attribute_name().
-type attribute_value() :: wooper:attribute_value().

-type attribute_entry() :: { attribute_name(), attribute_value() }.


% PID of a WOOPER instance.
-type instance_pid() :: pid().


% A request is typically:
%
% -spec my_request :: fun( wooper:state(), Arg1 :: method_argument(),
%    Arg2 :: method_argument(), ... ) -> request_return( T ).


% A oneway is typically:
%
% -spec my_oneway :: fun( wooper:state(), Arg1 :: method_argument(),
%    Arg2 :: method_argument(), ... ) -> oneway_return().


% We prefer defining these types into an header file (this one) rather than in
% the wooper module, to lighten the syntax (no prefix module):
%
-export_type([ classname/0,
			   method_name/0, request_name/0, oneway_name/0,
			   method_argument/0, method_arguments/0, requests_outcome/0 ]).

-export_type([ request_result/1, request_result/0,
			   request_return/1, oneway_return/0,
			   attribute_name/0, attribute_value/0, attribute_entry/0 ]).

-export_type([ instance_pid/0 ]).
