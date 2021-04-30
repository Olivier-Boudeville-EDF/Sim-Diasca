% Copyright (C) 2003-2021 Olivier Boudeville
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



% Shared records and defines.


% This records stores static information about the host this Erlang node runs
% on.
%
% Used for example by the performance tracker.
%
-record( host_static_info, {

		% The total amount of physical RAM, in megabytes (MB).
		total_ram :: basic_utils:count(),

		% The total amount of physical swap space, in megabytes (MB).
		total_swap :: basic_utils:count(),

		% The number of detected cores:
		core_count :: basic_utils:count(),

		% The version string of this Erlang VM:
		erlang_version :: text_utils:ustring()

} ).



% This records stores dynamic information about the host this Erlang node runs
% on.
%
-record( host_dynamic_info, {


		% The name of the corresponding node on this host:
		node_name :: net_utils:atom_node_name(),


		% The amount of swap used, in gibibytes (GiB, not GB).
		swap_used :: basic_utils:count(),


		% {PercentRamUsedByApplication, PercentRamUsedByOthers} where
		% PercentRamUsedByApplication is the percentage of the total physical
		% RAM of the local computer used by this Erlang VM (including all its
		% processes) and PercentRamUsedByOthers corresponds to the percentage of
		% total RAM used by all other applications; both percentages are floats
		% in the [0,100] range, and the difference between their sum and 100
		% corresponds to the free RAM.
		%
		ram_use :: { float(), float() },


		% The total, aggregated (on all cores of all CPUs on this host)
		% per-usage CPU percentages (floats in [0,100]), since last update, or
		% 'undefined' (if not usage could be computed).
		%
		cpu_usage :: maybe( system_utils:cpu_usage_percentages() ),


		% The number of Erlang processes running on this node:
		process_count :: basic_utils:count()


} ).



% The basic builtin shell commands and related executables (ex: cat, awk, grep,
% df) are not so easy to manage, as, from a distribution to another, they tend
% to sit in different locations (ex: /bin/, /usr/bin) and/or to be aliased or
% symlinked (ex: /usr/bin/awk being gawk). Depending also on the user
% environment, different conventions apply.
%
% These commands could be referred to directly as 'awk' or 'grep', yet it may be
% considered as a risk, with non-standard if not malicious executables being
% potentially thrown in a user's environment.
%
% Here we at least centralise the (unique) versions we prefer, and ensure that
% safer bet are done.
%
% Commands are unaliased; for example, '\\awk' is '\awk' once escaped; a
% backslash allows to unalias the corresponding command, to avoid
% not-so-compliant alternatives to be used.
%
% See https://www.tldp.org/LDP/GNU-Linux-Tools-Summary/html/c1195.htm for more
% information.

-define( awk, " \\awk " ).

% "/bin/cat" should be valid as well:
-define( cat, " \\cat " ).

% "/bin/df" should be valid as well:
-define( df, " \\df " ).

% "/bin/free" should be valid as well:
-define( free, " \\free " ).

% "/bin/grep" should be valid as well:
-define( grep, " \\grep " ).

% "/bin/sed" should be valid as well:
-define( sed, " \\sed " ).

-define( id, " \\id " ).

-define( ulimit, " \\ulimit " ).


% For proper Unicode support (see also the comments in file_utils.erl).
%
% Note:
%
% - if ever setting a non-Unicode default encoding,
% system_utils:force_unicode_support/0 shall be modified
%
% - see the notes in the 'Regarding encodings and Unicode' section of the
% file_utils module, notably about the consequences of specifying an encoding at
% file opening
%
-define( default_encoding, utf8 ).

% Generally not to be used, as directly writing encoded content is safer and
% offers more control:
%
-define( default_encoding_opt, { encoding, ?default_encoding } ).
