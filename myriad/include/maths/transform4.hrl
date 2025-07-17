% Copyright (C) 2022-2025 Olivier Boudeville
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
% Creation date: 2022.


% A 4x4 transformation, storing both its corresponding homogeneous 4x4 reference
% matrix and its inverse.
%
% Typically designated as T12, the transformation between a frame of reference
% R1 to another R2 one, storing both its corresponding reference 4x4 matrix
% (P1->2, transition from R1 to R2) and its inverse (P2->1, transition from R2
% to R1).
%
% We tend to prefer (by convention) "upward" transformations in a reference
% tree, i.e the transformations from child to parent (rather than the opposite -
% although one can be easily deduced from the other), as then it is directly the
% reference matrix of T12 (RefM) that can be used to convert a vector expressed
% in the child (R1) into its representation in the parent (R2), as V2 = RefM.V1.
%
-record( transform4, {

	% The reference homogeneous, 4x4 matrix of that transformation:
	% (corresponds to P1->2 in our conventions)
	%
	reference = 'identity_4' :: matrix4:homogeneous_matrix4(),

	% The inverse of the reference matrix for that transformation:
	% (corresponds to P2->1 in our conventions)
	%
	inverse = 'identity_4' :: matrix4:homogeneous_matrix4() } ).
