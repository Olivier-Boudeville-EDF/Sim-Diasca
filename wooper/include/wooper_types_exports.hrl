% Copyright (C) 2007-2026 Olivier Boudeville
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
% Creation date: 2007.


% Modular WOOPER header gathering the main types of interest.


% Allows to define WOOPER base variables and methods for that class.

% We define these actual types in the WOOPER module (wooper.erl), so that they
% are defined only once - yet can be referred to with or without the 'wooper:'
% prefix.


% Note that '-doc' declarations could have been added for types here as well for
% clarity, knowing that all WOOPER-based classes are to include this file
% indirectly, whereas wooper.erl does not (hence there is no risk of
% clash/circular definition); however we prefer not duplicating information, so
% one should directly refer to the wooper module instead.
%
% As a result, we removed on purpose all related comments that were available
% here.


-type classname() :: wooper:classname().


-type method_name() :: wooper:method_name().

-type request_name() :: wooper:request_name().
-type oneway_name()  :: wooper:oneway_name().


-type method_argument() :: wooper:method_argument().


-type method_arguments() :: wooper:method_arguments().


-type construction_parameters() :: wooper:construction_parameters().


-type request_call() :: wooper:request_call().
-type oneway_call()  :: wooper:oneway_call().


-type method_internal_result() :: wooper:method_internal_result().


-type request_result( T ) :: wooper:request_result( T ).


-type requests_outcome() :: wooper:requests_outcome().


-type request_result() :: any().



-type request_return( T ) :: wooper:request_return( T ).


-type const_request_return( T ) :: wooper:const_request_return( T ).


-type oneway_return() :: wooper:oneway_return().


-type const_oneway_return() :: wooper:const_oneway_return().


-type static_return( T ) :: wooper:static_return( T ).


-type static_void_return() :: wooper:static_void_return().

-type static_no_return() :: wooper:static_no_return().


-type attribute_name() :: wooper:attribute_name().
-type attribute_value() :: wooper:attribute_value().

-type attribute_entry() :: wooper:attribute_entry().


-type instance_pid() :: wooper:instance_pid().



% As we prefer defining these types into an header file (this one), hence
% directly in each class (rather than only in the wooper module), in order to
% lighten the syntax (no prefixing module needed for actual references):
%
-export_type([ classname/0,
               method_name/0, request_name/0, oneway_name/0,
               method_argument/0, method_arguments/0, requests_outcome/0 ]).

-export_type([ request_result/1, request_result/0,
               request_return/1, oneway_return/0,
               attribute_name/0, attribute_value/0, attribute_entry/0 ]).

-export_type([ instance_pid/0 ]).
