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


% Modular WOOPER header gathering all general-purposes defines.


% Note: types are defined here but exported in wooper.erl.



% Note: the hashtable type used by WOOPER (not the one exposed as a potential
% attribute) should be a preprocessor define
%
-define( wooper_table_type, table ).


% Approximate average attribute count for a given class instance, including
% inherited ones (ideally should be slightly above the maximum number of actual
% attributes for a given class)
%
-define( wooper_attribute_count_upper_bound, 16 ).


% Tells whether the OTP integration of WOOPER (involving supervisor, gen_server,
% explicit starting, etc.) shall be enabled.
%
-define( wooper_enable_otp_integration, true ).
%-define( wooper_enable_otp_integration, false ).


% Number of milliseconds to wait for, in order to be reasonably sure that the
% warning message could be written to the console, knowing that the operation is
% asynchronous and thus may not be performed should the VM halt immediately:
%
% (otherwise you will not see any stacktrace)
%
-define( wooper_warning_display_waiting, 200 ).


% Number of milliseconds to wait for, in order to be reasonably sure that the
% error message could be written to the console, knowing that the operation is
% asynchronous and thus may not be performed should the VM halt immediately:
%
% (otherwise you will not see any stacktrace)
%
-define( wooper_error_display_waiting, 500 ).




% Records the state of an instance.
% Module is the Erlang module the class is mapped to.
%
% This is the class-specific object state, each instance of this class will have
% its own state_holder, quite similar to the 'C++' this pointer.
%
% Constant data (ex: the virtual table) are referenced by each class instance,
% they are not duplicated (pointer to a virtual table shared by all class
% instances rather than deep copy).
%
% The virtual table holds the method name to module mapping for a given class.
% The attribute table (a hashtable) records all the data members of a given
% instance, including all the inherited ones.
%
% The request sender member is used internally by WOOPER so that a request
% method have a way of retrieving the corresponding caller PID. This avoids the
% caller to specify its PID twice, one for WOOPER, one for the method, as a
% method parameter, in the case the method itself needs the caller PID, for
% example to register it in a list in its own state. Thus a caller does not have
% to specify: 'MyInstance ! {my_request,[self()],self()}', specifying
% 'MyInstance ! {my_request,[],self()}' is enough: the method will be able to
% retrieve the caller PID thanks to the request_sender member, automatically set
% by WOOPER. For non-request methods (oneways), WOOPER will set request_sender
% to the atom 'undefined', to ensure the oneway crashes whenever trying to use
% this request-specific information to send a message.
%
% Therefore when you see the first parameter of a method, 'State', it is
% actually just an instance of the following record:
%
-record( state_holder, {

		   %virtual_table :: maybe( ?wooper_table_type:?wooper_table_type() ),

		   % Just a key in the persistent_term registry:
		   %virtual_table_key :: wooper:class_key(),

		   % Now directly a reference within the persistent_term registry:
		   virtual_table :: ?wooper_table_type:?wooper_table_type(),

		   attribute_table :: maybe( ?wooper_table_type:?wooper_table_type() ),

		   % Only means that we know to access the actual class name:
		   %
		   % (otherwise we could not, for example, report in an intermediate
		   % child class the actual class name of a deleted instance)
		   %
		   % To be used, instead of ?MODULE or alike.
		   %
		   actual_class :: basic_utils:module_name(),

		   request_sender :: maybe( pid() )

}).



% Now that type-checking on the state record is performed in debug mode, in both
% modes method results are sent directly:
%
% (no wooper_result atom added any more in debug mode)
%
%-define( wooper_return_state_result( State, Result ), { State, Result } ).
%-define( wooper_return_state( State ), State ).
%
% Finally, using retrocompatibility macros, resulting in the same code:
%
% (retrocompatibility now shall have been solved, hence these macros are
% disabled)
%
%-define( wooper_return_state_result( S, R ),
%		 wooper:return_state_result( (S), (R) ) ).

%-define( wooper_return_state( S ), wooper:return_state( (S) ) ).



-ifdef(wooper_debug_mode).


	% Uncomment to have all WOOPER recompiled classes output verbosely their
	% information:
	% (useful when everything is compiled without this flag and then
	% uncommenting the flag to recompile only the class(es) to debug)
	%-define(wooper_log_wanted,).

-ifdef(wooper_log_wanted).

	-define( wooper_log( Msg ), io:format( Msg ) ).
	-define( wooper_log_format( Msg, Format ), io:format( Msg, Format ) ).

-else. % wooper_log_wanted

	-define( wooper_log( Msg ), no_wooper_log ).
	-define( wooper_log_format( Msg, Format ), no_wooper_log ).

-endif. % wooper_log_wanted


-else. % wooper_debug_mode


	-define( wooper_log( Msg ), no_wooper_log ).
	-define( wooper_log_format( Msg, Format ), no_wooper_log ).


-endif. % wooper_debug_mode





% A reasonable duration (in milliseconds) before a time-out is triggered after a
% instance (notably, a created one) does not seem to answer properly.
%
% (we could block forever but for at least some cases it would make the
% debugging harder)


-ifndef(synchronous_time_out).


-ifdef(wooper_debug_mode).


% Suitable for most applications (5 seconds):
-define(synchronous_time_out,5000).


-else. % wooper_debug_mode


% Better for applications in production (30 minutes):
-define(synchronous_time_out, (30*60*1000) ).

% Also possible:
%-define(synchronous_time_out,infinity).

-endif. % wooper_debug_mode


-endif. % ifndef(synchronous_time_out)
