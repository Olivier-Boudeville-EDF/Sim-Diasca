% Copyright (C) 2011-2024 Olivier Boudeville
%
% This file is part of the Ceylan-Myriad library.
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
% Creation date: 2011.


% A Myriad-defined record introduced in order to store information regarding any
% application.
%
% This information can be transformed in a map that is a bit less detailed (not
% storing application name, and storing os only, instead of os_family and
% os_name) and that can be used directly by functions in the standard 'filename'
% module, such as filename:basedir/3 in order to return suitable system-specific
% base paths.
%
-record( app_info, {

	% The official name of the corresponding application:
	name :: text_utils:bin_string(),

	% The version (if any) of the corresponding application:
	%
	version :: maybe( basic_utils:any_version() | text_utils:ustring() ),

	% The author description (if any)  of the corresponding application:
	author :: maybe( text_utils:bin_string() ),

	% The current operating system information (see
	% system_utils:get_operating_system_type(/0), if any, on which is this
	% application runs:

	% Coarse categorisation of the current operating system:
	os_family :: maybe( system_utils:os_family() ),

	% More precise categorisation of the current operating system:
	os_name :: maybe( system_utils:os_name() ) } ).
