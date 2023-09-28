% Copyright (C) 2003-2023 Olivier Boudeville
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


% To properly format (as text), typically coordinates:
-define( printout_width, "14" ).


% For floats, precision is the number of digits after the decimal point. The
% default precision is 6.
%
-define( printout_precision, "2" ).


% Finally we do not set a maximum width, we let the necessary one happen and
% adjust accordingly; otherwise, depending on the precision and the part of the
% float before/after the decimal, it may not be correctly represented as a
% string (resulting in "****").
%
% Hopefully these format strings are resolved at compile-time:

-define( coord_float_format,
		 "~" ++ ?printout_width ++ "." ++ ?printout_precision ++ ". f" ).

% Not fixed-width:
%-define( coord_float_format, "~." ++ ?printout_precision ++ ". f" ).


% For integer, the precision corresponds to the base, not of interest here (we
% use of course the default, 10):
%
%-define( coord_integer_format,
%         "~" ++ ?printout_width ++ "." ++ ?printout_precision ++ ". B" ).

-define( coord_integer_format, "~" ++ ?printout_width ++ ".. B" ).


% Relatively aggressive inlining for basic operations:
-define( inline_size, 48 ).

