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


% Modular WOOPER header gathering all execute{Const,}{Request,Oneway}*
% primitives provided to implement methods (exports here).


-export([ executeRequest/2, executeRequest/3,
		  executeConstRequest/2, executeConstRequest/3,

		  executeOneway/2, executeOneway/3,
		  executeConstOneway/2, executeConstOneway/3,

		  executeRequestAs/3, executeRequestAs/4,
		  executeConstRequestAs/3, executeConstRequestAs/4,

		  executeOnewayAs/3, executeOnewayAs/4,
		  executeConstOnewayAs/3, executeConstOnewayAs/4 ]).
