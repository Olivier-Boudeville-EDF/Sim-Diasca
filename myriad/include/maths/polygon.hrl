% Copyright (C) 2010-2023 Olivier Boudeville
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


% Describes a polygon, convex or not, simple or not.
-record( polygon, {

	% The points defining that polygon:
	vertices = [] :: [ point2:any_vertex2() ],


	% Rendering information, if any, as an option list.
	%
	% Supported options:
	% - edge_color :: gui_color:color_by_decimal()
	% - fill_color :: gui_color:color_by_decimal()
	%
	rendering = [] :: option_list:option_list(),


	% Bounding surface information:
	% (can be for example a rectangle or a circle)
	%
	bounding_surface :: maybe( bounding_surface:bounding_surface() ) } ).
