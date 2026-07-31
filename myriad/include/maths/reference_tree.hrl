% Copyright (C) 2024-2026 Olivier Boudeville
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
% Creation date: Saturday, March 16, 2024.


% As, sometimes, defines in header files make sense:
-ifndef(myriad_maths_reference_tree_hrl_guard).
-define(myriad_maths_reference_tree_hrl_guard,).


-define( root_ref_id, 0 ).
% The identifier of the root, absolute reference frame.
%
% Note that this information is in some sense redundant with the fact that we
% expect that a single node (the root one) has no parent.


% A reference tree, also known as a scene graph.
%
% Note that the root, absolute reference frame is to be designated by the null
% (zero) reference frame identifier (see the root_ref_id define).
%
-record( reference_tree, {

    % The main table of this tree, concentrating all known reference frames,
    % based on their identifier.
    %
    ref_table :: reference_tree:ref_table(),

    % A table caching the computed paths from on frame to another:
    path_table :: reference_tree:path_table(),

    % The identifier to be allocated at the next registered frame:
    next_ref_id = ?root_ref_id + 1 :: reference_tree:ref_id() } ).


-endif. % myriad_maths_reference_tree_hrl_guard
