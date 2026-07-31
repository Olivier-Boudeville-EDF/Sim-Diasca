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
% Creation date: Saturday, March 2, 2024.

-module(reference_frame).

-moduledoc """
Base module for the support of **reference frames** (frames of reference) of any
dimension.

A frame of reference is a part of reference tree; it records the
transformation between this frame and its parent frame (if any).

See https://en.wikipedia.org/wiki/Frame_of_reference for further information
""".



% Implementation notes:
% - "referential" is not a proper choice of word here
% - refer to <https://howtos.esperide.org/reference-frame-tree.png> for an
% example



% For the matrix4 record:
-include("matrix4.hrl").

% For the reference_frame3 actual record:
-include("reference_frame3.hrl").



-doc """
A frame of reference, defining a coordinate system, for each of the supported
dimensions.
""".
-type reference_frame() :: reference_frame3().



-doc "Shorthand of reference_frame/0.".
-type ref() :: reference_frame().



-doc """
The PID of any kind of process implementing the ref protocol, ultimately
equivalent in terms of semantics, once resolved, to a reference frame.
""".
-type ref_pid() :: pid().



-doc "Any way of designating an actual reference_frame() instance.".
-type designated_ref() :: ref() | ref_pid().



-doc """
Any user-specified name (not an identifier) of a reference frame.
""".
-type user_ref_name() :: any_string().



-doc """
Any (internal) name (not an identifier) of a reference frame.
""".
-type ref_name() :: bin_string().



-export_type([ reference_frame/0, ref/0,
               ref_pid/0, designated_ref/0,
               user_ref_name/0, ref_name/0 ]).


-export([ is_designated_ref/1, check_designated_ref/1,
          designated_ref_to_string/1 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type reference_frame3() :: reference_frame3:reference_frame3().



-doc "Tells whether the specified term is a reference frame designator.".
-spec is_designated_ref( term() ) -> boolean().
%is_designated_ref( Ref ) when is_record( Ref, reference_frame3 ) ->
is_designated_ref( RefId ) when is_integer( RefId ) ->
    true;

is_designated_ref( RefPid ) when is_pid( RefPid ) ->
    true;

is_designated_ref( _ ) ->
    false.



-doc "Ensures that the specified term is a reference frame designator indeed.".
-spec check_designated_ref( term() ) -> designated_ref().
check_designated_ref( T ) ->
    is_designated_ref( T ) orelse throw( { not_a_designated_ref, T } ).



-doc """
Returns a textual representation of the specified designated reference frame.
""".
-spec designated_ref_to_string( designated_ref() ) -> ustring().
designated_ref_to_string( Ref3 ) when is_record( Ref3, reference_frame3 ) ->
    reference_frame3:to_string( Ref3 );

designated_ref_to_string( DesigPid ) when is_pid( DesigPid ) ->
    text_utils:format( "the reference frame held by ~w", [ DesigPid ] ).
