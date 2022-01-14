% Copyright (C) 2011-2022 EDF R&D

% This file is part of Sim-Diasca.

% Sim-Diasca is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License as
% published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.

% Sim-Diasca is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License for more details.

% You should have received a copy of the GNU Lesser General Public
% License along with Sim-Diasca.
% If not, see <http://www.gnu.org/licenses/>.

% Author: Olivier Boudeville (olivier.boudeville@edf.fr)


% @doc This series filter allows to select which curves are to be kept among the
% ones defined in a time series, and to reorder them (that is to write them in
% the order specified in the corresponding data file).
%
-module(curve_selector_series_filter).


-export([ create/3 ]).


% To avoid unused notifications:
-export([ selection_to_string_as_list/3 ]).


-type curve_index() :: basic_utils:count().

-type ustring() :: text_utils:ustring().



% @doc Creates a corresponding file, for whic are specified at creation:
%
% - the name of the data file to produce
%
% - an ordered list of curve indexes, which tells which curves are to be kept,
% and in which order; for example if CurveNames = ["Foo", "Bar", "Baz",
% "Other"], then to generate a time series containing only ["Baz", "Bar"] (in
% that order) following indexes list should be specified: [3, 2]
%
-spec create( ustring(), [ ustring() ], [ curve_index() ] ) ->
					'ok' | { 'onFilterEnded', pid() }.
create( SeriesName, CurveNames, CurveIndexList ) ->

	CurveCount = length( CurveNames ),

	check_curve_indexes( CurveIndexList, CurveCount ),

	% List like: [true, false, true], telling that only curves #1 and #3 are
	% selected here:
	%
	SelectionList = build_selection( CurveIndexList, CurveCount ),

	TargetFilename = file_utils:convert_to_filename( SeriesName )
		++ "-selectioned.dat",

	trace_utils:notice_fmt( "Among the curves ~ts the ones "
		"corresponding to indexes ~p will be written (in that order) "
		"in file '~ts'.~n",
		[ text_utils:strings_to_string( CurveNames ), CurveIndexList,
		  TargetFilename ] ),

	file_utils:remove_file_if_existing( TargetFilename ),

	File = file_utils:open( TargetFilename,
							[ raw, write, delayed_write, exclusive ] ),

	SelectionString = selection_to_string( CurveNames, SelectionList ),

	file_utils:write_ustring( File,
		"# Created by the selector filter at ~ts,~n"
		"# operating on the '~ts' time series,~n"
		"# with following selection filter:~n"
		"# ~p.~n~n~ts",
		[ time_utils:get_textual_timestamp(), SeriesName,
		  SelectionList, SelectionString ] ),

	filter_loop( TargetFilename, File, SelectionList ).



% Ensures that specified curve indexes are valid.
check_curve_indexes( [], _CurveCount ) ->
	ok;

check_curve_indexes( [ CurveIndex | T ], CurveCount )
  when is_integer( CurveIndex ) andalso CurveIndex > 0
	   andalso CurveIndex =< CurveCount ->
	check_curve_indexes( T, CurveCount );

check_curve_indexes( [ CurveIndex | _T ], CurveCount ) ->

	trace_utils:error_fmt( "'~p' is an invalid curve index, it must be an "
		"integer in [1..~B].", [ CurveIndex, CurveCount ] ),

	basic_utils:stop_on_failure().



% @doc Returns a list of booleans telling, for each position k, whether curve k
% is selected.
%
build_selection( CurveIndexList, CurveCount ) ->
	build_selection( CurveIndexList, _CurrentIndex=1, _MaxIndex=CurveCount+1,
					 _Acc=[] ).


build_selection( _CurveIndexList, _CurrentIndex=MaxIndex, MaxIndex, Acc ) ->
	lists:reverse( Acc );

build_selection( CurveIndexList, CurrentIndex, MaxIndex, Acc ) ->
	IsWanted = lists:member( CurrentIndex, CurveIndexList ),
	build_selection( CurveIndexList, CurrentIndex + 1, MaxIndex,
					 [ IsWanted | Acc ] ).


% (helper)
filter_loop( TargetFilename, File, SelectionList ) ->

	receive

		{ setSample, [ Tick, SampleList ] } ->

			case selector_helper( SelectionList, SampleList ) of

				{} ->
					ok;

				T ->
					class_Probe:write_row( File, Tick, T )

			end,
			filter_loop( TargetFilename, File, SelectionList );


		{ onEndOfSeriesData, ScannerPid } ->

			file_utils:write_ustring( File,
									  "\n# End of selector filter writing." ),

			file_utils:close( File ),

			trace_utils:notice_fmt( "File '~ts' has been generated by the "
				"curve selector filter.~n", [ TargetFilename ] ),

			ScannerPid ! { onFilterEnded, self() };

		delete ->
			ok;

		{ getProducedFilename, [], ScannerPid } ->
			ScannerPid ! { wooper_result, TargetFilename },
			filter_loop( TargetFilename, File, SelectionList );

		Other ->
			throw( { unexpected_series_filter_message, Other } )

	end.



selector_helper( SelectionList, SampleList ) ->
	selector_helper( SelectionList, SampleList, _Acc=[] ).


selector_helper( _SelectionList=[], _SampleList=[], Acc ) ->
	list_to_tuple( lists:reverse( Acc ) );

selector_helper( _SelectionList=[ true | TSel ], _SampleList=[ V | TSam ],
				 Acc ) ->
	selector_helper( TSel, TSam, [ V | Acc ] );

% Checking it is a boolean indeed:
selector_helper( _SelectionList=[ false | TSel ], _SampleList=[ _V | TSam ],
				 Acc ) ->
	selector_helper( TSel, TSam, Acc ).



selection_to_string( CurveNames, SelectionList ) ->

	%trace_utils:debug_fmt( "selection_to_string: CurveNames=~p "
	%   "and SelectionList=~p", [ CurveNames, SelectionList ] ),

	{ Selected, Rejected } = sort_selected_curves( CurveNames, SelectionList,
												   { _AccSel=[], _AccRej=[] } ),

	%selection_to_string( CurveNames, SelectionList, _Acc=[] ).

	Bullet= "# - ",

	SelString = case Selected of

		[] ->
			"No curve was selected.";

		_ ->
			NumberedSelected = number_selected( Selected, _Count=1, _Acc=[] ),
			"Following curves were selected: "
				++ text_utils:strings_to_string( NumberedSelected, Bullet )

	end,

	RejString = case Rejected of

		[] ->
			"No curve was rejected.";

		_ ->
			"Following curves were rejected: "
				++ text_utils:strings_to_string( Rejected, Bullet )

	end,

	text_utils:format( "# ~ts~n# ~ts~n", [ SelString, RejString ] ).



% Adds a comment specifying the number of each selected curve.
number_selected( _Selected=[], _Count, Acc ) ->
	lists:reverse( Acc );

number_selected( _Selected=[ S | T ], Count, Acc ) ->
	number_selected( T, Count + 1,
					 [ text_utils:format( "~ts (curve #~B here)",
										  [ S, Count ] ) | Acc ] ).


% (helper)
sort_selected_curves( _CurveNames=[], _SelectionList=[],
					  { AccSel, AccRej } ) ->
	{ lists:reverse( AccSel ), lists:reverse( AccRej ) };

sort_selected_curves( [ CurveName | Tc ], [ true | Ts ], { AccSel, AccRej } ) ->
	sort_selected_curves( Tc, Ts, { [ CurveName | AccSel ], AccRej } );

sort_selected_curves( [ CurveName | Tc ], [ false | Ts ],
					  { AccSel, AccRej } ) ->
	sort_selected_curves( Tc, Ts, { AccSel, [ CurveName | AccRej ] } ).



% @doc Lists all curves in turn and specifies whether it was selected.
%
% (not currently used anymore)
%
-spec selection_to_string_as_list( [ ustring() ], [ boolean() ],
								   [ ustring() ] ) -> ustring().
selection_to_string_as_list( _CurveNames=[], _SelectionList=[], Acc ) ->
	CommentList = [ E || E <- lists:reverse( Acc ) ],
	text_utils:strings_to_string( CommentList, _Bullet= "# - " );

selection_to_string_as_list( [ CurveName | Tc ], [ true | Ts ], Acc ) ->
	CurveString = text_utils:format( "curve '~ts' was selected",
									 [ CurveName ] ),
	selection_to_string_as_list( Tc, Ts, [ CurveString | Acc ] );

selection_to_string_as_list( [ CurveName | Tc ], [ false | Ts ], Acc ) ->
	CurveString = text_utils:format( "curve '~ts' was not selected",
									 [ CurveName ] ),
	selection_to_string_as_list( Tc, Ts, [ CurveString | Acc ] ).
