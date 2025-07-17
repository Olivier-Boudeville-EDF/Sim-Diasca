% Copyright (C) 2024-2025 Olivier Boudeville
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
% Creation date: Saturday, March 2, 2024.


% For root_ref_id:
-include("reference_tree.hrl").


% A 3D frame of reference, defining notably a coordinate system.
%
% Such frame belongs to a (3D) reference tree, which holds notably an
% associative table of reference frames, based on their identifier.
%
% A shorthand for this record and corresponding type is "ref3".
%
% A parent frame of reference is either explicitly defined (note that it relies
% then on an implicit reference table) or, if not defined, is the global, root,
% absolute one.
%
-record( reference_frame3, {

	% The name (if any) of a reference frame (this is not an identifier).
	name :: option( reference_frame:ref_name() ),


	% The parent reference frame of this one, to be found in an (implicit)
	% reference table.
	%
	% Any reference frame not having a parent specifically defined (which is the
	% default for this record) is considered to be absolutely defined, i.e. it
	% is then defined relative to the root node - as opposed to one having a
	% parent specified, to which it is then relative.
	%
	% Only the root of a reference tree as a parent set to 'undefined'.
	%
	parent = ?root_ref_id :: option( reference_tree:ref_id() ),


	% A list of the (direct) children of this reference frame:
	% (cached precomputation deduced from the parent information)
	%
	children = [] :: reference_tree:child_ids(),


	% An (optional) identifier path from the (implicit) root frame of reference
	% to this frame (cached precomputation)
	%
	path_from_root :: option( reference_tree:id_path() ),


	% The 3D transformation (based on 4x4 matrices) between the parent reference
	% frame (if any, otherwise from the root, absolute reference frame) and this
	% one.
	%
	% The reference matrix held by this transformation is the transition matrix
	% from to this reference frame to its parent (if any, otherwise to the
	% global reference frame).
	%
	% Its associated inverse matrix is therefore the transition matrix from the
	% parent reference frame (if any, otherwise from the root, absolute
	% reference frame) to this frame of reference.
	%
	transform :: transform4:transform4() } ).
