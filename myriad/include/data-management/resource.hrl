% Copyright (C) 2022-2024 Olivier Boudeville
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
% Creation date: Sunday, April 3, 2022.


% State of a referential centralising resources.
-record( resource_referential, {

	% The (absolute) root directory (if any) whence file-based resources are
	% loaded: (this can be an actual directory or a symbolic link)
	%
	root_directory :: maybe( file_utils:bin_directory_path() ),

	% The table storing the currently-loaded resources:
	table :: resource:resource_table()

	% Bitmap holder, to be able to control their life-cycle:
	% (this workaround was actually unecessary)
	%dummy_frame :: maybe( gui_frame:frame() )

							   } ).
