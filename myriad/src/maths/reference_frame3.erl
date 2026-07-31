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

-module(reference_frame3).

-moduledoc """
Module implementing the support for **3D reference frames** (frames of
references), based on 4x4 transformations.

A 3D frame of reference is a part of a (3D) reference tree, which holds notably
an associative table of reference frames, based on their identifier; a frame
records the transformation to/from its (direct) parent frame (if any).

See <https://en.wikipedia.org/wiki/Frame_of_reference> for further information.

See also the `transform4` module.
""".



% For the corresponding record:
-include("matrix4.hrl").

% For the transform4 record and indirectly the root_ref_id define:
-include("reference_frame3.hrl").



-doc "A 3D frame of reference, defining a coordinate system.".
-type reference_frame3() :: #reference_frame3{}.



-doc "Shorthand of reference_frame3().".
-type ref3() :: reference_frame3().



-doc """
The PID of any kind of process implementing the ref3 protocol, ultimately
equivalent in terms of semantics, once resolved, to a 3D reference frame.
""".
-type ref3_pid() :: reference_frame:ref_pid().



-doc "Any way of designating an actual reference_frame3() instance.".
-type designated_ref3() :: ref3() | ref3_pid().


-export_type([ reference_frame3/0, ref3/0,
               ref3_pid/0, designated_ref3/0 ]).


-export([ new/0, new/1, new_absolute/1,
          new/2, new/3, new_absolute/2,

          get_transform/1, get_inverse_transform/1,

          to_string/1, node_to_string/2 ] ).


% Shorthands:

-type ustring() :: text_utils:ustring().

-type transform4() :: transform4:transform4().

-type user_ref_name() :: reference_frame:user_ref_name().

-type ref_id() :: reference_tree:ref_id().



-doc """
Creates the absolute (3D) reference frame, relying on the identity
transformation; this is thus the (only) root frame.
""".
-spec new() -> reference_frame3().
new() ->
    % Anonymous and parentless:
    #reference_frame3{ name= <<"Root">>,
                       parent=undefined,
                       transform=transform4:identity() }.



-doc """
Creates an absolute (3D) reference frame, relying on the identity transformation
and named as specified.

Its is deemed absolute as it is only relative to the root frame.
""".
-spec new_absolute( user_ref_name() ) -> reference_frame3().
new_absolute( UserRefName ) ->
    % Anonymous and parentless:
    #reference_frame3{ name=text_utils:ensure_binary( UserRefName ),
                       transform=transform4:identity(),
                       parent=?root_ref_id }.



-doc """
Creates an absolute (3D) reference frame, based on the specified transformation.
""".
-spec new( transform4() ) -> reference_frame3().
new( Transf4 ) ->

    cond_utils:if_defined( myriad_debug_ref_frames,
                           transform4:check_type( Transf4 ) ),

    % Parentless:
    #reference_frame3{ transform=Transf4 }.



-doc """
Creates a (3D) reference frame, based on the specified transformation relative
to the designated parent reference frame.
""".
-spec new( transform4(), designated_ref3() ) -> reference_frame3().
new( Transf4, ParentRefDesig3 ) ->

    cond_utils:if_defined( myriad_debug_ref_frames,
        begin
            transform4:check_type( Transf4 ),
            reference_frame:check_designated_ref( ParentRefDesig3 )
        end ),

    #reference_frame3{ parent=ParentRefDesig3, transform=Transf4 }.



-doc """
Creates a (3D) reference frame, based on the specified transformation relative
to the designated parent reference frame, named as specified.
""".
-spec new( user_ref_name(), transform4(), designated_ref3() ) ->
          reference_frame3().
new( UserRefName, Transf4, ParentRefDesig3 ) ->

    cond_utils:if_defined( myriad_debug_ref_frames,
        begin
            transform4:check_type( Transf4 ),
            reference_frame:check_designated_ref( ParentRefDesig3 )
        end ),

    #reference_frame3{ name=text_utils:ensure_binary( UserRefName ),
                       parent=ParentRefDesig3,
                       transform=Transf4 }.



-doc """
Creates a (3D) reference frame, based on the specified absolute transformation,
named as specified.
""".
-spec new_absolute( user_ref_name(), transform4() ) -> reference_frame3().
new_absolute( UserRefName, Transf4 ) ->

    cond_utils:if_defined( myriad_debug_ref_frames,
                           transform4:check_type( Transf4 ) ),

    #reference_frame3{ name=text_utils:ensure_binary( UserRefName ),
                       transform=Transf4 }.



-doc """
Returns the 3D transformation corresponding to the specified reference frame.

Note: this transformaton may be accessed directly instead.
""".
-spec get_transform( reference_frame3() ) -> transform4().
get_transform( #reference_frame3{ transform=Transf4 } ) ->
    Transf4.



-doc """
Returns the inverse of the 3D transformation corresponding to the specified
reference frame.
""".
-spec get_inverse_transform( reference_frame3() ) -> transform4().
get_inverse_transform( #reference_frame3{ transform=Transf4 } ) ->
    transform4:inverse( Transf4 ).



-doc """
Returns a textual representation of the specified (3D) reference frame
designator.
""".
-spec designated_ref3_to_string( designated_ref3() ) -> ustring().
designated_ref3_to_string( DesigId ) when is_integer( DesigId ) ->
    text_utils:format( "the 3D reference frame #~B", [ DesigId ] );

designated_ref3_to_string( DesigPid ) when is_pid( DesigPid ) ->
    text_utils:format( "the 3D reference frame ~w", [ DesigPid ] ).



-doc """
Returns a short textual representation of the specified (3D) reference frame
designator.
""".
-spec designated_ref3_to_short_string( designated_ref3() ) -> ustring().
designated_ref3_to_short_string( DesigId ) when is_integer( DesigId ) ->
    text_utils:format( "frame #~B", [ DesigId ] );

designated_ref3_to_short_string( DesigPid ) when is_pid( DesigPid ) ->
    text_utils:format( "frame ~w", [ DesigPid ] ).



-doc """
Returns a textual representation of the specified (3D) reference frame.
""".
-spec to_string( reference_frame3() ) -> ustring().
to_string( #reference_frame3{ name=MaybeBinRefName,
                              parent=Parent,
                              transform=Transf4 } ) ->

    NameStr = case MaybeBinRefName of

        undefined ->
            "";

        BinRefName ->
            text_utils:format( " named '~ts'", [ BinRefName ] )

    end,

    case Parent of

        ?root_ref_id ->
            text_utils:format( "absolute 3D reference frame~ts", [ NameStr ] );

        ParentDesignator ->
            text_utils:format( "3D reference frame~ts "
                "defined relatively to ~ts",
                [ NameStr, designated_ref3_to_string( ParentDesignator ) ] )

    end ++ ", based on a " ++ transform4:to_string( Transf4 ).



-doc """
Returns a textual representation of the specified node of a reference tree, made
of a (3D) reference frame and its identifier.
""".
-spec node_to_string( ref_id(), reference_frame3() ) -> ustring().
node_to_string( RefId, _Ref3=#reference_frame3{ name=MaybeBinRefName,
                                                parent=MaybeParent,
                                                path_from_root=MaybePath } ) ->

    %trace_utils:debug_fmt( "For id=~B, ref3 = ~p", [ RefId, Ref3 ] ),

    % Almost the same as to_string/1, just bzing more compact, adding its
    % identifier and not its transform:

    NameStr = case MaybeBinRefName of

        undefined ->
            "";

        BinRefName ->
            text_utils:format( " named '~ts'", [ BinRefName ] )

    end,

    PathStr = case MaybePath of

        undefined ->
            "";

        Path ->
            text_utils:format( ", whose path from root is ~w", [ Path ] )

    end,

    case MaybeParent of

        undefined ->
            text_utils:format( "root reference frame #~B~ts~ts",
                               [ RefId, NameStr, PathStr ] );

        ParentDesignator ->
            text_utils:format( "frame #~B~ts, relative to ~ts~ts",
                [ RefId, NameStr,
                  designated_ref3_to_short_string( ParentDesignator ),
                  PathStr ] )

    end.
