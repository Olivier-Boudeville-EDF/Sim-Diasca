% Copyright (C) 2015-2021 Olivier Boudeville
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
% Creation date: Friday, July 31, 2015


% Unit tests for the HDF5 low-level support, i.e. relying directly on our
% erlhdf5 fork (https://github.com/Olivier-Boudeville-EDF/erlhdf5) rather than
% on our higher-level hdf5_support module.
%
% Note: if there was not the initialisation phase (relying here on
% hdf5_support), this test would be directly in the erlhdf5 tree.
%
% This test is mimicked by higher-level hdf5_support_test.erl.
%
-module(raw_hdf5_test).


% For run/0 export and al:
-include("test_facilities.hrl").



% One may use 'make hdf5_support_run && h5dump hdf5_support_test.hdf5' to have a
% look at the HDF5 result (if having commented out the file removal at the end).


% The non-erlhdf5 dependencies (typically, hdf5_support) are used only for
% convenience.


% Returns the initial, the full and the partial updates, and the corresponding
% final data and element size for the specified test case.



get_data( one_dim_integer ) ->

	InitialData =
		[ 500, 501, 502, 503, 504, 505, 506, 507, 508, 509, 510, 511 ],

	FullUpdateData = [ 1, 2, 3 , 4, 5, 6, 7, 8, 9, 10, 11, 12 ],

	PartialUpdateData = [ 100, 101, 102 ],

	FinalExpectedData = [ 1, 2, 3 ] ++ PartialUpdateData
		++ [ 7, 8, 9, 10, 11, 12 ],

	% A priori, the size of a small integer (ex: 500) is 4 bytes according to
	% HDF, yet Erlang (on 64 bit) may see it as 8 bytes. Here we compare with
	% the binding:
	%
	%ElemSize = basic_utils:size( element( 1, hd( InitialData ) ) ),
	ElemSize = hdf5_support:get_size( native_integer ),

	{ InitialData, FullUpdateData, PartialUpdateData, FinalExpectedData,
	  ElemSize };


get_data( one_dim_long_float ) ->

	InitialData = [ nan, 501.7, 502.7, 503.7, 504.7, 505.7, 506.7, 507.7,
					508.7, 509.7, 510.7, 511.7 ],

	FullUpdateData =
		[ 1.8, nan, 3.8, 4.8, 5.8, 6.8, 7.8, 8.8, 9.8, 10.8, 11.8, 12.8 ],

	PartialUpdateData = [ 100.9, inf, 102.9 ],

	FinalExpectedData = [ 1.8, nan, 3.8 ] ++ PartialUpdateData
		++ [ 7.8, 8.8, 9.8, 10.8, 11.8, 12.8 ],

	% A priori, the size of a small integer (ex: 500) is 4 bytes according to
	% HDF, yet Erlang (on 64 bit) may see it as 8 bytes. Here we compare with
	% the binding:
	%
	%ElemSize = basic_utils:size( element( 1, hd( InitialData ) ) ),
	ElemSize = hdf5_support:get_size( native_long_float ),

	{ InitialData, FullUpdateData, PartialUpdateData, FinalExpectedData,
	  ElemSize };


get_data( two_dim_integer ) ->

	InitialData = [ { 500, 501, 502 },
					{ 503, 504, 505 },
					{ 506, 507, 508 },
					{ 509, 510, 511 } ],

	FullUpdateData = [ { 1,   2,  3 },
					   { 4  , 5,  6 },
					   { 7,   8,  9 },
					   { 10, 11, 12 } ],

	PartialUpdateData = [ { 100, 101, 102 } ],

	FinalExpectedData = [ {   1,   2,   3 } ] ++
						   PartialUpdateData ++ [
						   {   7,   8,   9 },
						   {  10,  11,  12 } ],

	% A priori, the size of a small integer (ex: 500) is 4 bytes according to
	% HDF, yet Erlang (on 64 bit) may see it as 8 bytes. Here we compare with
	% the binding:
	%
	%ElemSize = basic_utils:size( element( 1, hd( InitialData ) ) ),
	ElemSize = hdf5_support:get_size( native_integer ),

	{ InitialData, FullUpdateData, PartialUpdateData, FinalExpectedData,
	  ElemSize };


get_data( two_dim_long_float ) ->

	InitialData = [ { 500.1, inf, 502.1 },
					{ 503.1, 504.1, 505.1 },
					{ nan,   507.1, 508.1 },
					{ 509.1, 510.1, 511.1 } ],

	FullUpdateData = [ { 1.2,   2.2, 3.2 },
					   { 4.2,   5.2, 6.2 },
					   { 7.2,   8.2, 9.2 },
					   { 10.2,  nan, inf } ],

	PartialUpdateData = [ { 100.3, 101.3, 102.3 } ],

	FinalExpectedData = [ {   1.2,   2.2,  3.2 } ] ++
						   PartialUpdateData ++ [
						   {   7.2,   8.2,  9.2 },
						   {  10.2,   nan,  inf } ],

	% A priori, the size of a float (ex: 500.2) is 4 bytes according to HDF, yet
	% Erlang (on 64 bit) may see it as 24 bytes (!). Here we compare with the
	% binding:
	%
	%ElemSize = basic_utils:size( element( 1, hd( InitialData ) ) ),
	ElemSize = hdf5_support:get_size( native_long_float ),

	{ InitialData, FullUpdateData, PartialUpdateData, FinalExpectedData,
	  ElemSize }.




% Returns the hyperslab settings corresponding to the specified fixture.
%
get_hyperslab_settings( S, _TupleSize=1 ) when S =:= one_dim_integer
									orelse S =:= one_dim_long_float->

	% From element #4:
	Offset = 3,

	Stride = 1,

	% Until element #6 (included):
	Count = 3,

	Block = 1,

	{ Offset, Stride, Count, Block };


get_hyperslab_settings( S, TupleSize ) when S =:= two_dim_integer
									orelse S =:= two_dim_long_float ->

	% With the two_dim_int example, we now target:
	%
	%       [ {   1,   2,   3 },
	%		  { 100, 101, 102 },
	%		  {   7,   8,   9 },
	%		  {  10,  11,  12 } ],
	%
	% thus we want to replace the second line (tuple) by { 100, 101, 102 }; this
	% corresponds to the following in-file hyperslab:
	%
	LineNumberOffset = 1,

	% Second line:
	Offset = { LineNumberOffset, 0 },

	Stride = { 1, 1 },

	% Full line:
	Count = { 1, TupleSize },

	Block = { 1, 1 },

	{ Offset, Stride, Count, Block }.



-type test_fixture() :: 'one_dim_integer'
					  | 'one_dim_long_float'
					  | 'two_dim_integer'
					  | 'two_dim_long_float'.


-spec run() -> no_return().
run() ->

	hdf5_support:start(),

	test_facilities:start( ?MODULE ),

	Fixtures = [ one_dim_integer, one_dim_long_float,
				 two_dim_integer, two_dim_long_float ],

	test_facilities:display( "Running in turn following test fixtures: ~p.",
							 [ Fixtures ] ),

	lists:map( fun run/1, Fixtures ),

	test_facilities:stop().



-spec run( test_fixture() ) -> no_return().
run( TestFixture ) ->

	FileName = text_utils:format( "raw_hdf5_test-fixture-~p.hdf5",
								  [ TestFixture ] ),

	test_facilities:display( "~nStarting by writing a new '~ts' HDF5 file "
		"for test fixture ~p.", [ FileName, TestFixture ] ),

	{ ok, WriteFile } = erlhdf5:h5fcreate( FileName, 'H5F_ACC_TRUNC' ),

	{ InitialData, FullUpdateData, PartialUpdateData, FinalExpectedData,
	  ElemSize } = get_data( TestFixture ),

	% Dimensions is typically either ElementCount (with mono-dimensional data)
	% or {TupleCount, TupleSize} (for bidimensional one):
	%
	{ MetaDatatype, Dimensions } = hdf5_support:check_data( InitialData ),

	% Guesses from the data the datatype to be used:
	BindingDatatype = hdf5_support:convert_datatype( MetaDatatype ),

	test_facilities:display( "Initial data of type ~p (binding: ~p), "
		"with dimensions of ~w elements of unitary size ~B.",
		[ MetaDatatype, BindingDatatype, Dimensions, ElemSize ] ),

	% For example, BindingDatatype may be 'H5T_NATIVE_INT':
	{ ok, CellType } = erlhdf5:h5tcopy( BindingDatatype ),


	% We have for this test to "deconvert" hdf5_support to feed the lower-level
	% binding:
	%
	{ BindingDims, TupleSize } = case Dimensions of

		Pair={ _TupleCount, TupleElemCount } ->
			{ Pair, TupleElemCount };

		ElementCount ->
			{ { ElementCount }, 1 }

	end,

	% Ex: with bidimensional data, 2D array of TupleSize elements on TupleCount
	% lines (tuples):

	Rank = size( BindingDims ),

	{ ok, Dataspace } = erlhdf5:h5screate_simple( Rank, BindingDims ),

	% Property list:
	{ ok, DatasetPropList } = erlhdf5:h5pcreate( 'H5P_DATASET_CREATE' ),


	DatasetName = "/my-dataset",

	% Creates a new dataset:
	{ ok, Dataset } = erlhdf5:h5dcreate( WriteFile, DatasetName, CellType,
										 Dataspace, DatasetPropList ),

	{ ok, AllocStatus } = erlhdf5:h5d_get_space_status( Dataset ),

	% Typically zero size:
	{ ok, Size } = erlhdf5:h5d_get_storage_size( Dataset ),

	test_facilities:display( "Dataset status: ~p, size: ~p",
							 [ AllocStatus, Size ] ),

	'H5D_SPACE_STATUS_NOT_ALLOCATED' = AllocStatus,
	0 = Size,


	% Writes these data into this dataset:
	ok = erlhdf5:h5dwrite( Dataset, InitialData ),

	{ ok, WriteStatus } = erlhdf5:h5d_get_space_status( Dataset ),

	{ ok, WriteSize } = erlhdf5:h5d_get_storage_size( Dataset ),

	test_facilities:display( "Dataset status after write: ~p, size: ~p",
							 [ WriteStatus, WriteSize ] ),

	'H5D_SPACE_STATUS_ALLOCATED' = WriteStatus,

	% Typically, for native_integer, 4*3=12 elements, 4 bytes each:
	WriteSize = hdf5_support:get_element_count( Dimensions ) * ElemSize,

	% Closes resources (a priori, all of the allocated ones):
	ok = erlhdf5:h5sclose( Dataspace ),
	ok = erlhdf5:h5pclose( DatasetPropList ),
	ok = erlhdf5:h5tclose( CellType ),
	ok = erlhdf5:h5dclose( Dataset ),
	ok = erlhdf5:h5fclose( WriteFile ),


	test_facilities:display( "Reading and updating that HDF5 file." ),

	% Opens this just written file, while planning to update it:
	{ ok, ReadFile } = erlhdf5:h5fopen( FileName, 'H5F_ACC_RDWR' ),

	{ ok, Rank } = erlhdf5:h5ltget_dataset_ndims( ReadFile, DatasetName ),
	test_facilities:display( "Dataset rank: ~p", [ Rank ] ),


	% Checks dataset info:
	{ ok, DatasetInfo } = erlhdf5:h5ltget_dataset_info( ReadFile, DatasetName,
														Rank ),

	test_facilities:display( "Dataset info: ~p", [ DatasetInfo ] ),


	% Reads dataset:
	{ ok, RawReadData } = case MetaDatatype of

			native_integer ->
				erlhdf5:h5lt_read_dataset_int( ReadFile, DatasetName );

			native_long_float ->
				erlhdf5:h5lt_read_dataset_double( ReadFile, DatasetName )

	end,

	ReadData = list_utils:reconstruct_tuples( RawReadData, TupleSize ),

	test_facilities:display( "Read data: ~w", [ ReadData ] ),

	InitialData = ReadData,


	% Let's now test a partial overwriting thanks to an hyperslab:

	test_facilities:display( "Updating first this data first as a whole "
							 "(overwriting in full)." ),

	% For example, in the two_dim_int case, InitialData = [
	%			 { 500, 501, 502 },
	%			 { 503, 504, 505 },
	%			 { 506, 507, 508 },
	%			 { 509, 510, 511 } ],


	% First let's get the target dataset (from the already opened file) that
	% will be created:
	{ ok, ReadDataset } = erlhdf5:h5dopen( ReadFile, DatasetName ),

	{ ok, Rank } = erlhdf5:h5ltget_dataset_ndims( ReadFile, DatasetName ),

	% With the two_dim_int example, FullUpdateData = [
	%				   { 1,   2,  3 },
	%				   { 4  , 5,  6 },
	%				   { 7,   8,  9 },
	%				   { 10, 11, 12 } ],

	% Then fully overwrites it with different data:
	ok = erlhdf5:h5dwrite( ReadDataset, FullUpdateData ),

	test_facilities:display( "Updating then this data partly, "
							 "thanks to an hyperslab." ),


	% Creates a relevant dataspace for this replacement; let's get first a copy
	% of its (global) dataspace:
	%
	{ ok, TargetDataspace } = erlhdf5:h5dget_space( ReadDataset ),

	{ Offset, Stride, Count, Block } =
		get_hyperslab_settings( TestFixture, TupleSize ),

	% Then just updates a part of the data (ex: in two dimensions, the second
	% row, i.e. tuple), thanks to an hyperslab: this is done thanks to
	% H5Sselect_hyperslab, which operates on a dataspace, the one of the in-file
	% dataset (dataspace that is modified by this call):
	%
	% (see
	% www.hdfgroup.org/HDF5/doc/Intro/IntroExamples.html#CheckAndReadExample as
	% an example about updating the selection in that dataspace)
	%
	ok = erlhdf5:h5sselect_hyperslab( TargetDataspace, 'H5S_SELECT_SET', Offset,
									  Stride, Count, Block ),


	% Not needing to specify here the memory hyperslab, nor transfer properties:
	ok = erlhdf5:h5dwrite( ReadDataset, _FileSpaceSelection=TargetDataspace,
						   PartialUpdateData ),

	% We should end up with:
	%
	%       HDF5 "hdf_test.h5" {
	%           %       GROUP "/" {
	%          DATASET "my-dataset" {
	%			  DATATYPE  H5T_STD_I32LE
	%			  DATASPACE  SIMPLE { ( 4, 3 ) / ( 4, 3 ) }
	%			  DATA {
	%			  (0,0): 1, 2, 3,
	%			  (1,0): 100, 101, 102,
	%			  (2,0): 7, 8, 9,
	%			  (3,0): 10, 11, 12
	%			  }
	%          }
	%       }
	%    }

	% Reads dataset again:
	{ ok, FinalRawReadData } = case MetaDatatype of

		native_integer ->
			erlhdf5:h5lt_read_dataset_int( ReadFile, DatasetName );

		native_long_float ->
			erlhdf5:h5lt_read_dataset_double( ReadFile, DatasetName )

	end,

	FinalReadData = list_utils:reconstruct_tuples( FinalRawReadData,
												   TupleSize ),

	test_facilities:display( "Finally read data: ~w", [ FinalReadData ] ),

	%io:format( "expected: ~w~n", [ FinalExpectedData ] ),
	%io:format( "read:     ~w~n", [ FinalReadData ] ),

	FinalExpectedData = FinalReadData,

	% Releases resources:

	ok = erlhdf5:h5sclose( TargetDataspace ),
	ok = erlhdf5:h5dclose( ReadDataset ),
	ok = erlhdf5:h5fclose( ReadFile ),

	file_utils:remove_file( FileName ),

	test_facilities:display( "Successful end of test for fixture ~p.",
							 [ TestFixture ] ).
