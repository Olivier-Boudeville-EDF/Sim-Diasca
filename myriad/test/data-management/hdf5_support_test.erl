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



% Unit tests for the HDF5 support.
%
% See the hdf5_support tested module.
%
% This test mimicks lower-level raw_hdf5_test.erl.
%
-module(hdf5_support_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% One may use 'make hdf5_support_run && h5dump hdf5_support_test.hdf5' to have a
% look at the HDF5 result (if having commented out the file removal at the end).



% Creates and fills a test HDF5 file, returns {BindingDatatype, Dimensions}.
create_test_file( HDFFilename, DatasetName, Data ) ->

	test_facilities:display( "Creating an HDF5 file named '~ts'.",
							 [ HDFFilename ] ),

	% For a test, better determining it at runtime than hardcoding:
	{ BindingDatatype, Dimensions } = hdf5_support:check_data( Data ),

	test_facilities:display( "Specified data is ~p, i.e. dimensions ~p "
		"of type ~p.", [ Data, Dimensions, BindingDatatype ] ),

	Datatype = hdf5_support:create_datatype( BindingDatatype ),

	DatatypeClass = hdf5_support:get_datatype_class( Datatype ),

	ByteOrder = hdf5_support:get_byte_order( Datatype ),

	DataSize = hdf5_support:get_size( Datatype ),

	test_facilities:display( "Created a datatype, of class: ~p, byte order: ~p,"
		" size: ~B bytes.", [ DatatypeClass, ByteOrder, DataSize ] ),


	% Up to 20 triplets:
	FirstDataspace = hdf5_support:create_dataspace( Dimensions ),

	%FirstDataspace = hdf5_support:create_dataspace( _Dims={ 3, 20 } ),

	% We can have to process mono-dimensional data as well:
	%FirstDataspace = hdf5_support:create_dataspace( _Dims=10 ),

	Rank = hdf5_support:get_rank( FirstDataspace ),

	DimExtensions = hdf5_support:get_dimension_extensions( FirstDataspace ),

	test_facilities:display( "Created a dataspace of rank ~B and "
		"dimension extensions ~p.", [ Rank, DimExtensions ] ),

	DatasetCreationList = hdf5_support:create_property_list_for(
							dataset_creation ),


	% Prior tests may have left it:
	file_utils:remove_file_if_existing( HDFFilename ),

	TestFile = hdf5_support:create_file( HDFFilename ),

	test_facilities:display( "Creating a dataset named '~ts'.",
							 [ DatasetName ] ),

	Dataset = hdf5_support:create_dataset( DatasetName, Datatype,
						FirstDataspace, DatasetCreationList, TestFile ),

	test_facilities:display( "Allocation status of the newly created dataset: "
		"~ts; size: ~B bytes.",
		[ hdf5_support:get_allocation_status( Dataset ),
		  hdf5_support:get_storage_size( Dataset ) ] ),

	test_facilities:display( "Writing following data into this "
							 "empty dataset: ~p", [ Data ] ),

	hdf5_support:write( Data, Dataset ),

	test_facilities:display( "Allocation status of the dataset once filled: "
		"~ts; size: ~B bytes.",
		[ hdf5_support:get_allocation_status( Dataset ),
		  hdf5_support:get_storage_size( Dataset ) ] ),

	% Closing in reverse order:

	hdf5_support:close_dataset( Dataset ),

	hdf5_support:close_file( TestFile ),

	hdf5_support:close_property_list( DatasetCreationList ),

	hdf5_support:close_dataspace( FirstDataspace ),

	hdf5_support:close_datatype( Datatype ),

	{ BindingDatatype, Dimensions }.




% Checks that the specified file and dataset have the expected content.
%
check_test_file( HDFFilename, DatasetName, ExpectedData, BindingDatatype,
				 Dimensions ) ->

	test_facilities:display( "Reading now an HDF5 file named '~ts'.",
							 [ HDFFilename ] ),

	test_facilities:display( "Reading a dataset named '~ts'.",
							 [ DatasetName ] ),

	TestFile = hdf5_support:open_file( HDFFilename ),

	TupleSize = case Dimensions of

		{ _TupleCount, TupleElemSize } ->
			TupleElemSize;

		_ ->
			1

	end,

	ReadData = hdf5_support:read( DatasetName, BindingDatatype, TupleSize,
								  TestFile ),

	test_facilities:display( "Read following data: ~p.", [ ReadData ] ),


	ExpectedData = ReadData,

	test_facilities:display( "Got ultimately what had to be written, i.e.: ~p",
							 [ ExpectedData ] ),


	hdf5_support:close_file( TestFile ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	test_facilities:display( "Starting HDF5 support." ),
	hdf5_support:start(),

	test_facilities:display( "We will first write to a new HDF5 file, "
							 "then read from it." ),


	% Preparing all elements necessary for the dataset creation:

	HDFFilename = "hdf5_support_test.hdf5",

	DatasetName = "/my-dataset",


	IntMonoDimData = [ 1, 2, 3, 4, 5 ],
	FloatMonoDimData = [ 11.4, nan, 13.4, inf, 15.4 ],

	IntTwoDimData = [ {1,3,4}, {2,5,5}, {3,6,6}, {4,7,6}, {5,7,7}, {6,8,7},
					  {7,9,10}, {8,9,11}, {9,7,12} ],

	FloatTwoDimData = [ {nan,3.7,4.7},  {2.7,5.7,5.7},  {3.7,6.7,6.7},
						{4.7,inf,6.7},  {5.7,7.7,7.7},  {6.7,8.7,7.7},
						{7.7,9.7,10.7}, {8.7,9.7,nan}, {9.7,7.7,12.7} ],


	DataTests = [ IntMonoDimData, FloatMonoDimData,
				  IntTwoDimData, FloatTwoDimData ],

	[ begin

		  test_facilities:display( "~nTesting for data ~p.", [ Data ] ),

		  { BindingDatatype, Dimensions } =
			  create_test_file( HDFFilename, DatasetName, Data ),

		  check_test_file( HDFFilename, DatasetName, Data, BindingDatatype,
						   Dimensions )

	  end || Data <- DataTests ],

	test_facilities:display( "Stopping HDF5 support." ),
	hdf5_support:stop(),

	file_utils:remove_file( HDFFilename ),

	test_facilities:stop().
