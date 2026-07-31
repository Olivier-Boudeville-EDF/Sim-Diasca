% Copyright (C) 2011-2026 EDF R&D
%
% This file is part of Sim-Diasca.
%
% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) edf (dot) fr]
% Creation date: 2011.

-module(extrema_curve_filter).

-moduledoc """
This is an **example of a curve filter**: it simply records the minimum and
maximum values reached by that curves, for which tick(s) (i.e. to each extremum
is associated the list of the ticks at which it was reached).
""".



-export([ create/2 ]).


% Type shorthands:

-type ustring() :: text_utils:ustring().


-doc """
Creates a filter process.

We specify at creation the name of that curve.
""".
-spec create( ustring(), [] ) -> 'ok' | { 'onFilterEnded', pid() }.
create( CurveName, [] ) ->

    % The state of that process is the name of the curve, and respectively
    % {MinValue, MinTicks} and {MaxValue, MaxTicks} pairs.
    %
    filter_loop( CurveName, { undefined, undefined },
                 { undefined, undefined } ).


% (helper)
filter_loop( CurveName, MinE={ MinValue, MinTicks },
             MaxE={ MaxValue, MaxTicks } ) ->

    receive

        { setSample, [ Tick, Value ] } ->

            NewMinEntry = case MinValue of

                undefined ->
                    { Value, [ Tick ] };

                Value ->
                    { Value, [ Tick | MinTicks ] };

                Vi when Vi > Value ->
                    { Value, [ Tick ] };

                _ ->
                    MinE

            end,

            NewMaxEntry = case MaxValue of

                undefined ->
                    { Value, [ Tick ] };

                Value ->
                    { Value, [ Tick | MaxTicks ] };

                Va when Va < Value  ->
                    { Value, [ Tick ] };

                _ ->
                    MaxE

            end,

            filter_loop( CurveName, NewMinEntry, NewMaxEntry );


        { onEndOfCurveData, ScannerPid } ->

            %io:format( "Min entry = ~p, max = ~p.~n", [ MinE, MaxE ] ),

            % Either Min and Max are both defined, or none:
            case MinValue of

                undefined ->
                    io:format( "The curve '~ts' had not recorded value, "
                        "thus no extremum could be determined.~n",
                        [ CurveName ] );

                _ ->
                    io:format( "The curve '~ts' had for minimum value ~f, "
                        "which was reached ~B times, at ticks ~w, and "
                        "for maximum value ~f, "
                        "which was reached ~B times, at ticks ~w.~n~n",
                        [ CurveName, MinValue, length( MinTicks ),
                          lists:reverse( MinTicks ), MaxValue,
                          length( MaxTicks ), lists:reverse( MaxTicks ) ] )

            % Terminating.
            end,

            ScannerPid ! { onFilterEnded, self() };

        delete ->
            ok;

        Other ->
            throw( { unexpected_curve_filter_message, Other } )

    end.
