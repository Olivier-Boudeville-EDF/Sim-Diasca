% Copyright (C) 2003-2021 Olivier Boudeville
%
% This file is part of the Ceylan-Traces library.
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
% Creation date: Tuesday, January 11, 2011.



% Defines all the macros and exports useful for trace-using applications.
%
% Allows to define exports before functions: macros and functions have been
% split so that overall header files can be defined which start with all macros
% and finish with all function definitions.



% Define functions as late as possible.
%
% app_trace_disabled/{1,2} must be defined whether or not traces are enabled
% (as void traces use them in all cases), but then they might be reported as
% unused (depending on the user code).
%
% Hence this report is silenced here in all cases:


-compile( [ { nowarn_unused_function, [ { app_trace_disabled, 1 },
										{ app_trace_disabled, 2 } ] } ] ).


-spec app_trace_disabled( any() ) -> void().
app_trace_disabled( _ ) ->
	ok.


-spec app_trace_disabled( any(), any() ) -> void().
app_trace_disabled( _, _ ) ->
	ok.
