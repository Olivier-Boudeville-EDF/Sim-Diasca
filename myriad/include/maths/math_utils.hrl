% Copyright (C) 2010-2024 Olivier Boudeville
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
% Creation date: Monday, February 15, 2010.


% Numerical margin to account for rounding/quantisation errors induced by the
% use of floating-point numbers.
%
% For example, when using floating-point coordinates, equality of points is
% defined relatively to the epsilon distance: P1 is deemed equal to P2 iff the
% distance between P1 and P2 is below epsilon.
%
-define( epsilon, 1.0e-6 ).


% Quite small epsilon, for example for series expansions:
-define( smaller_epsilon, 1.0e-20 ).


% A threshold to denote unbounded values:
%
% (corresponds to the maximum value of 32-bit floats, i.e. the maximum
% representable IEEE 754 floating-point value)
%
-define( infinity, 3.402823e+38 ).


% Should these constants be evaluated only at runtime otherwise:

% sqrt(2.Pi): math:sqrt(2*math:pi()):
-define( sqrt_2_pi, 2.5066282746310002 ).


% ln(Pi): math:log(math:pi()).
-define( ln_pi, 1.144729885849400174143427351353058711647 ).
