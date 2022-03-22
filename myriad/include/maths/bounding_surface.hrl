% Copyright (C) 2003-2022 Olivier Boudeville
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


% Common definitions about bounding surfaces.



% Record declarations:


% Rectangle-based bounding surface.
-record( rectangle, {

	% The top-left corner of the rectangle:
	top_left :: point2:any_point2(),

	% The bottom-right corner of the rectangle:
	bottom_right :: point2:any_point2() } ).



% Circle-based bounding surface.
-record( circle, {

	% The center of the circle:
	center :: point2:point2(),

	% The square of the radius (R^2) of this circle:
	square_radius :: linear:square_distance() } ).
