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

% Creation date: Friday, July 31, 2015
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]



% This module centralises the support for HDF5 (Hierarchical Data Format version
% 5), a file format designed to store and organize large amounts of numerical
% data.
%
% More information: https://en.wikipedia.org/wiki/Hierarchical_Data_Format#HDF5
%
% Some installation conventions should have been respected, typically so that
% the libraries can be located. For that, refer to the 'Installation
% Instructions' section of our fork, i.e.
% https://github.com/Olivier-Boudeville-EDF/erlhdf5#installation-instructions
%
% See also raw_hdf5_test.erl for a lower-level, more direct access to HDF5.
%
-module(hdf5_support).



% User API:
%
-export([ get_supported_datatypes/0, check_data/1, get_element_count/1,

		  start/0, stop/0,

		  create_file/1, create_file/2, open_file/1, open_file/2, close_file/1,

		  create_dataspace/1, close_dataspace/1,
		  get_rank/1, get_dimension_extensions/1, get_dimension_extensions/2,

		  create_property_list_for/1, close_property_list/1,

		  create_datatype/1, close_datatype/1, get_datatype_class/1,
		  get_byte_order/1, get_size/1,

		  create_dataset/5, open_dataset/2, close_dataset/1,
		  get_allocation_status/1,

		  get_storage_size/1, write/2, write/3, update/3, read/4, read_strings/2

		]).



% Exports for code that should be used in the future, or for code helping the
% development of tests:
%
-export([ convert_open_options/1, convert_class/1, convert_datatype/1,
		  get_known_datatypes/0, convert_datatype_class_identifier/1,
		  convert_byte_order/1, deconvert_byte_order/1 ]).


% Handle managed by the HDF5 library:
-type handle() :: integer().


% Designates an actual HDF5 file:
%
% (HDF5 handle, not a file_utils:file())
%
-type hdf5_file() :: handle().



% By default HDF5 files are opened as read-only and will not be overwritten:
%
-type open_option() ::
		check_non_existing % 'H5F_ACC_EXCL'   (fail if file already exists)
	  | read_write           % 'H5F_ACC_RDWR'   (otherwise read-only)
	  | overwrite          % 'H5F_ACC_TRUNC'  (overwrite existing files)
	  | debug              % 'H5F_ACC_DEBUG'  (print debug info)
	  | create             % 'H5F_ACC_CREAT'  (create non-existing files)
	  | read_only.         % 'H5F_ACC_RDONLY' (read-only)



% An HDF5 dataspace, i.e. metadata:
-type dataspace() :: handle().


% Rank of a dataspace, i.e. its number of dimensions (dimensionality):
% (up to 32 in the general case; usually 1, 2 or 3)
%
-type rank() :: basic_utils:count().


% Index of a dimension:
-type dimension() :: basic_utils:count().


% Number of elements of a dimension:
-type dimension_size() :: basic_utils:count() | 'unlimited'.


% Current and maximum number of elements of a dimension (of course current
% cannot exceed maximum):
%
-type dimension_extension() :: { dimension_size(), dimension_size() }.


% Dimensions of a dataspace (number of elements of each dimension).
%
% Typically, for monodimensional data (ex: [ 4.0, 5.2 ]), we have dimensions ::
% dimension(), which corresponds to the number of elements (here, 2).
%
% For bidimensional data (ex: [ {1,2,3}, {4,5,6} ], we have Dimensions :: {
% TupleCount :: dimension_size(), TupleSize :: dimension_size() }
% (corresponding here to {2,3}).
%
-type dimensions() :: dimension_size()
					%| tuple( dimension_size() ).
					| tuple().


% A class designates a kind of HDF5 objects:
%
-type class() ::  'attribute_creation' | 'dataset_access'  | 'dataset_creation'
				 | 'dataset_transfer'  | 'datatype_access' | 'datatype_creation'
				 | 'file_access'       | 'file_creation'   | 'file_mounting'
				 | 'group_access'      | 'group_creation'
				 | 'link_access'       | 'link_creation'
				 | 'object_copying'    | 'object_creation'
				 | 'string_creation'.


% A property list is created according to a class and then possibly updated:
%
-type property_list() :: handle().


% Defines the built-in types for the cells of an array:
%
% HDF5 defines a very wide range of types, cf
% https://www.hdfgroup.org/HDF5/doc/RM/PredefDTypes.html
%
-type predefined_datatype() :: 'native_integer'      % i.e. C int
							 | 'native_long_integer' % i.e. C long
							 | 'native_float'        % i.e. C float
							 | 'native_long_float'.  % i.e. C double


% Currently this binding supports only native ints and native long floats.
%
-type supported_datatypes() :: 'native_integer' | 'native_long_float'.


% Corresponds to a double in C, with Not-a-Number and Infinite:
-type native_long_float() :: float() | 'nan' | 'inf'.


% Datatype identifier as such:
-type datatype() :: handle().


% Specified to obtain datatypes:
-type datatype_specifier() :: predefined_datatype() | datatype() | dataset().


% Class identifier of a datatype:
-type datatype_class_identifier() :: 'no_class' | 'integer'   | 'float' | 'time'
								   | 'string'   | 'bitfield'  | 'opaque'
								   | 'compound' | 'reference' | 'enumeration'
								   | 'variable_length' | 'array'.


% Byte order of an atomic datatype:
-type byte_order() :: 'little_endian' | 'big_endian' | 'vax_mixed' | 'mixed'
					| 'none'.


% The name of a HDF5 dataset:
-type dataset_name() :: string().


% An HDF5 dataset, i.e. the actual data:
-type dataset() :: binary().


% Allocation status of a dataset:
-type allocation_status() :: 'not_allocated' | 'partially_allocated'
						   | 'fully_allocated'.


% Binding-level types, mostly for internal use:

-type hdf5_open_option() :: atom().
-type hdf5_class_flag() :: atom().

% Note: such a datatype is an atom here (ex: 'H5T_NATIVE_DOUBLE'), for actual
% HDF5 use it must be translated into an integer (using convert_type, defined in
% erlh5t.c);
-type hdf5_predefined_datatype() :: atom().

-type hdf5_class_identifier() :: integer().
-type hdf5_byte_order() :: integer().


% Array of vectors.
%
% Note that:
%
% - all tuples must have the same number of elements
%
% - that these elements must be all of the same type
%
% - that this must be a supported type for that binding (see
% supported_datatypes())
%
-type data() :: [ tuple() ].


-export_type([ hdf5_file/0, open_option/0,

			   dataspace/0, rank/0, dimension/0, dimension_size/0,
			   dimension_extension/0, dimensions/0,

			   class/0, property_list/0,

			   predefined_datatype/0, native_long_float/0,
			   datatype/0, datatype_specifier/0,
			   datatype_class_identifier/0, byte_order/0,

			   dataset_name/0, dataset/0, allocation_status/0

			 ]).



% For the H5T_INTEGER define and all:
%
-include("erlhdf5.hrl").



% Implementation notes:
%
% To manage HDF5 files, we rely on our fork
% (https://github.com/Olivier-Boudeville-EDF/erlhdf5) of the Erlang binding,
% erlhdf5 ( https://github.com/RomanShestakov/erlhdf5), which itself relies on
% libhdf5 (http://www.hdfgroup.org/HDF5/).
%
% Both are expected to be already built and installed, respectively in
% ~/Software/erlhdf5 and in any location (ex: ~/Software/HDF/) that was found
% during the build of erlhdf5.
%
% There is no specific need to update the code path with regard to HDF5 (done
% automatically here).


% An HDF5 file can be displayed by using the h5dump tool (typically in
% ~/Software/HDF/hdf5-current-install/bin).


% Known binding limitations: see the 'User Notes' in
% https://github.com/Olivier-Boudeville-EDF/erlhdf5



% Returns a list of the datatypes that this binding is expected to support.
%
-spec get_supported_datatypes() -> [ supported_datatypes() ].
get_supported_datatypes() ->
	% In Erlang, floating-point values are based on C double, not C floats:
	[ native_integer, native_long_float ].



% Checks the format and typing of specified data, i.e. ensures that it is a
% non-empty list of tuples of the same size (or unitary elements), with elements
% of the same type, this type being a supported one, and determines its
% structure.
%
% Returns { Datatype, Dimensions } where Datatype designates the type of the
% atomic data elements (type among the ones known of the binding) and Dimensions
% lists the number of elements in each dimension (as soon as there are at least
% two dimensions, it is a tuple; it is just directly the number of elements
% itself for one dimension).
%
-spec check_data( data() ) -> { Datatype :: supported_datatypes(),
								Dimensions :: dimensions() }.
check_data( _Data=[] ) ->
	throw( { invalid_data, empty_list } );

check_data( Data=[ H | _T ] ) when is_tuple( H )  ->

	% Currently we manage only one or two dimensions.
	% Here we are on the 'at least two dimensions' case.

	TupleSize = size( H ),

	FirstElement = element( 1, H ),

	{ BindingElemType, MetaElemType } = get_types_of( FirstElement ),

	case lists:member( BindingElemType, get_supported_datatypes() ) of

		true ->
			ok;

		false ->
			throw( { invalid_data, unsupported_datatype, MetaElemType } )

	end,

	% We use Data rather than T as we want to check the types of the elements of
	% the first tuple as well:
	%
	check_data_helper( Data, TupleSize, BindingElemType, MetaElemType,
					   _Count=0 );


check_data( Data=[ FirstElement | _T ] ) ->

	% Special case of tuples of size 1; of course we prefer '3' to '{3}':

	{ BindingElemType, MetaElemType } = get_types_of( FirstElement ),

	case lists:member( BindingElemType, get_supported_datatypes() ) of

		true ->
			ok;

		false ->
			throw( { invalid_data, unsupported_datatype, MetaElemType } )

	end,

	check_data_helper( Data, _TupleSize=1, BindingElemType, MetaElemType,
					   _Count=0 );


check_data( Data ) ->
	throw( { invalid_data, data_not_list, Data } ).



% Helper; Data known to be a list:
check_data_helper( _Data=[], _TupleSize=1, BindingElemType, _MetaElemType,
				   Count ) ->
	% To avoid returning {Count} (or {Count,1}):
	{ BindingElemType, _Dimensions=Count };

check_data_helper( _Data=[], TupleSize, BindingElemType, _MetaElemType,
				   Count ) ->
	{ BindingElemType, _Dimensions={ Count, TupleSize } };


check_data_helper( _Data=[ Tuple | T ], TupleSize, BindingElemType,
				   MetaElemType, Count ) when is_tuple( Tuple ) ->

	case size( Tuple ) of

		TupleSize ->
			ok;

		_UnexpectedSize ->
			throw( { invalid_data, heterogeneous_tuple_size, TupleSize,
					 Tuple } )

	end,

	case meta_utils:is_homogeneous( Tuple ) of

		{ true, MetaElemType } ->
			ok;

		% Doubles are float() | 'nan' | 'inf':
		{ _EitherTrueOrFalse, Types } when MetaElemType =:= float
						   andalso ( Types =:= { float, atom } orelse
									 Types =:= { atom, float } ) ->

			case is_tuple_of_doubles( Tuple ) of

				true ->
					ok;

				false ->
					throw( { invalid_data, non_double_tuple,
							 { Tuple, Types }, { expected, MetaElemType } } )

			end;

		{ true, AnotherType } ->
			throw( { invalid_data, heterogeneous_tuple_types,
					 { Tuple, AnotherType }, { expected, MetaElemType } } );

		{ false, Types } ->
			throw( { invalid_data, heterogeneous_tuple,
					 { Tuple, Types }, { expected, MetaElemType } } )

	end,

	check_data_helper( T, TupleSize, BindingElemType, MetaElemType, Count + 1 );


% Accept special cased double values:
check_data_helper( _Data=[ 'nan' | T ], TupleSize,
				   BindingElemType=native_long_float,
				   MetaElemType=float, Count ) ->
	check_data_helper( T, TupleSize, BindingElemType, MetaElemType,
					   Count + 1 );

check_data_helper( _Data=[ 'inf' | T ], TupleSize,
				   BindingElemType=native_long_float,
				   MetaElemType=float, Count ) ->
	check_data_helper( T, TupleSize, BindingElemType, MetaElemType,
					   Count + 1 );

check_data_helper( _Data=[ H | T ], TupleSize, BindingElemType,
				   MetaElemType, Count ) ->

	case meta_utils:get_type_of( H ) of

		MetaElemType ->
			check_data_helper( T, TupleSize, BindingElemType, MetaElemType,
							   Count + 1 );

		OtherType ->
				throw( { invalid_data, heterogeneous_element_types,
						 { H, OtherType }, { expected, MetaElemType } } )

	end.



% Returns { BindingElemType, MetaElemType }.
%
% We keep meta_utils conventions to avoid plenty of conversions.
%
% (helper)
%
get_types_of( Element ) ->

	case meta_utils:get_type_of( Element ) of

					  integer ->
						  { native_integer, integer };

					  float ->
						  { native_long_float, float };

					  % Special cases managed by the mapping to double:
					  atom when Element =:= 'nan' orelse Element =:= 'inf' ->
						  { native_long_float, float };

					  % Most probably unsupported:
					  Other ->
						  { Other, Other }

	end.


% Tells whether specified tuple is made of doubles, i.e. either Erlang floats
% and/or special values ('nan' and 'inf').
%
is_tuple_of_doubles( Tuple ) ->

	lists:foldl( fun

					 ( Elem, _Acc=true ) ->
						 case get_types_of( Elem ) of

							 { native_long_float, float } ->
								 true;

							 _ ->
								 false

						 end;

					 ( _Elem, _Acc=false ) ->
						 false

				 end,
				 _Acc0=true,
				 tuple_to_list( Tuple ) ).



% Returns the number of elements corresponding to specified dimensions.
%
-spec get_element_count( dimensions() ) -> basic_utils:count().
get_element_count( Dimensions ) when is_tuple( Dimensions ) ->

	lists:foldl(

	  fun( E, Acc ) -> E * Acc end,
	  _InitialAcc=1,
	  _DimsAsList=tuple_to_list( Dimensions )

	 );

get_element_count( Dimension ) when is_integer( Dimension ) ->
	Dimension.





% General support section, regarding the HDF5 service itself.


% Starts (checks and inits) the HDF5 service support.
%
-spec start() -> void().
start() ->

	% We have to secure the erlhdf5 binding:
	%
	% (typically we want to secure the use of:
	% ~/Software/erlhdf5/ebin/erlhdf5.beam, which will use
	% ~/Software/erlhdf5/priv/erlhdf5.so; this library was itself hard-linked
	% with rpath to the HDF5 ones, typically in
	% ~/Software/HDF/hdf5-1.8.15-install/lib/libhdf5*.so)
	%
	ExpectedBindingRoot = file_utils:join( [
							  system_utils:get_user_home_directory(),
							  "Software", "erlhdf5" ] ),

	% We want to display a proper error message if necessary:
	case file_utils:is_existing_directory( ExpectedBindingRoot ) of

		true ->
			ok;

		false ->
			throw( { no_hdf5_support_found, base_root, ExpectedBindingRoot } )

	end,

	BinRoot = file_utils:join( ExpectedBindingRoot, "ebin" ),

	code_utils:declare_beam_directory( BinRoot ),

	HDF5Beam = file_utils:join( BinRoot, "erlhdf5.beam" ),

	case file_utils:is_existing_file( HDF5Beam ) of

		true ->
			ok;

		false ->
			throw( { no_hdf5_support_found, lib, HDF5Beam } )

	end,

	HDF5Lib = file_utils:join( [ ExpectedBindingRoot, "priv",
								 "erlhdf5.so" ] ),

	case file_utils:is_existing_file( HDF5Lib ) of

		true ->
			ok;

		false ->
			throw( { no_hd5_support_found, lib, HDF5Lib } )

	end.

	% No specific checking here for the HDF5 library, as its (arbitrary)
	% location is hardcoded in erlhdf5.so when it is built.



% Stops the HDF5 support.
%
-spec stop() -> void().
stop() ->
	% No specific action needed:
	ok.



% HDF5 file manipulation section, regarding the overall data file.


% Creates an HDF5 file, of specified name, and expected not to exist already.
%
-spec create_file( file_utils:file_name() ) -> hdf5_file().
create_file( Filename ) ->
	create_file( Filename, _CreateFlags=[ overwrite ] ).



% Creates an HDF5 file with specified options, of specified name, and expected
% not to exist already.
%
-spec create_file( file_utils:file_name(), [ open_option() ] ) -> hdf5_file().
create_file( Filename, CreateFlags ) ->

	% To avoid an unclear error message:
	case file_utils:is_existing_file( Filename ) of

		true ->
			throw( { file_already_exists, Filename } );

		false ->
			ok

	end,

	% Binding would not work ("Cannot create file") if using Createflags=[
	% check_non_existing, read_write, overwrite, create ] ).

	BindingFlags = convert_open_options( CreateFlags ),

	case erlhdf5:h5fcreate( Filename, BindingFlags ) of

		{ ok, File } ->
			File;

		{ error, Reason } ->
			throw( { hdf5_file_creation_failed, Reason, Filename,
					 CreateFlags } )

	end.



% Opens an HDF5 file, of specified name, in read-only mode.
%
-spec open_file( file_utils:file_name() ) -> hdf5_file().
open_file( Filename ) ->
	open_file( Filename, _OpenFlags=[ read_only ] ).


% Opens an HDF5 file, of specified name, with specified options.
%
-spec open_file( file_utils:file_name(), [ open_option() ] ) -> hdf5_file().
open_file( Filename, OpenFlags ) ->

	% To avoid an unclear error message:
	case file_utils:is_existing_file( Filename ) of

		true ->
			ok;

		false ->
			throw( { file_not_found, Filename } )

	end,

	BindingFlags = convert_open_options( OpenFlags ),

	case erlhdf5:h5fopen( Filename, BindingFlags ) of

		{ ok, File } ->
			File;

		{ error, Reason } ->
			throw( { hdf5_file_opening_failed, Reason, Filename, OpenFlags } )

	end.



% Closes specified opened HDF5 file.
%
-spec close_file( hdf5_file() ) -> void().
close_file( HDF5File ) ->

	case erlhdf5:h5fclose( HDF5File ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { hdf5_file_closing_failed, Reason } )

	end.





% Content manipulation section, regarding the information that can be stored in
% an HDF5 file.


% Dataspace subsection.
%
% Dataspaces describe data arrays (they are thus metadata), either to define the
% content of a dataset or the operations (read/write) applied to it.



% Creates specified dataspace, with specified dimensions of specified maximum
% size, and opens it for access.

% Ex: to create a dataspace with two dimensions, of size 5 and 9 (hence with
% 5x9=45 elements), use: hdf5_support:create_dataspace( { 5, 9 } ).
%
% Single-dimension arrays can be declared directly, as in:
% hdf5_support:create_dataspace( 20 ).
%
-spec create_dataspace( dimensions() ) -> dataspace().
create_dataspace( MonoDimension ) when is_integer( MonoDimension ) ->
	create_dataspace( { MonoDimension } );

create_dataspace( Dimensions ) when is_tuple( Dimensions ) ->

	Rank = size( Dimensions ),

	case erlhdf5:h5screate_simple( Rank, Dimensions ) of

		{ ok, Dataspace } ->
			Dataspace;

		{ error, Reason } ->
			throw( { dataspace_creation_failed, Reason, Dimensions } )

	end.



% Closes specified dataspace.
%
-spec close_dataspace( dataspace() ) -> void().
close_dataspace( Dataspace ) ->

	case erlhdf5:h5sclose( Dataspace ) of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { dataspace_closing_failed, Reason } )

	end.



% Returns the rank (dimensionality) of the specified dataspace.
%
-spec get_rank( dataspace() ) -> rank().
get_rank( Dataspace ) ->

	{ ok, Rank } = erlhdf5:h5sget_simple_extent_ndims( Dataspace ),

	Rank.



% Returns, for specified dataspace, an ordered list of, for each dimension, a
% pair of current and maximum size.
%
% Ex: may return, for a dataspace of dimension { 5, 9 }, [ {5,5}, {9,9} ].
%
-spec get_dimension_extensions( dataspace() ) -> [ dimension_extension() ].
get_dimension_extensions( Dataspace ) ->

	Rank = get_rank( Dataspace ),

	get_dimension_extensions( Rank, Dataspace ).



% Returns, for specified dataspace of specified rank, an ordered list of, for
% each dimension, a pair of current and maximum size.
%
% Ex: may return, for a dataspace of dimension { 5, 9 } (and thus rank 2), [
% {5,5}, {9,9} ].
%
-spec get_dimension_extensions( rank(), dataspace() ) ->
									  [ dimension_extension() ].
get_dimension_extensions( Rank, Dataspace ) ->

	{ ok, CurrentDimensions, MaxDimensions } =
		erlhdf5:h5sget_simple_extent_dims( Dataspace, Rank ),

	lists:zip( CurrentDimensions, MaxDimensions ).




% Property list subsection.
%
% Such lists allow to tune various kinds of HDF5 objects.



% Creates a property list for specified class.
%
-spec create_property_list_for( class() ) -> property_list().
create_property_list_for( Class ) ->

	BindingClass = convert_class( Class ),

	{ ok, PropertyList } = erlhdf5:h5pcreate( BindingClass ),

	PropertyList.



% Closes specified property list.
%
-spec close_property_list( property_list() ) -> void().
close_property_list( PropertyList ) ->
	ok = erlhdf5:h5pclose( PropertyList ).




% Datatype subsection.


% Returns the specified datatype, from a specification of a predefined datatype,
% an already existing datatype or a dataset.
%
-spec create_datatype( datatype_specifier() ) -> datatype().
create_datatype( DatatypeSpecifier ) when is_atom( DatatypeSpecifier ) ->

	BindingDatatype = case lists:member( DatatypeSpecifier,
										 get_known_datatypes() ) of

						  true ->
							  convert_datatype( DatatypeSpecifier );

						  false ->
							  DatatypeSpecifier

	end,

	{ ok, Datatype } = erlhdf5:h5tcopy( BindingDatatype ),

	Datatype;

create_datatype( DatatypeSpecifier ) ->
	{ ok, Datatype } = erlhdf5:h5tcopy( DatatypeSpecifier ),
	Datatype.



% Closes specified datatype.
%
-spec close_datatype( datatype() ) -> void().
close_datatype( Datatype ) ->
	ok = erlhdf5:h5tclose( Datatype ).



% Returns the class identifier of the specified datatype.
%
-spec get_datatype_class( datatype() ) -> datatype_class_identifier().
get_datatype_class( Datatype ) ->
	{ ok, BindingClass } = erlhdf5:h5tget_class( Datatype ),
	deconvert_datatype_class_identifier( BindingClass ).



% Returns the byte order of the specified atomic datatype.
%
-spec get_byte_order( datatype() ) -> byte_order().
get_byte_order( Datatype ) ->
	{ ok, BindingOrder } = erlhdf5:h5tget_order( Datatype ),
	deconvert_byte_order( BindingOrder ).



% Returns the size, in bytes, of the specified datatype.
%
-spec get_size( datatype_specifier() ) -> system_utils:byte_size().
get_size( DatatypeHandle ) when is_integer( DatatypeHandle ) ->
	{ ok, Size } = erlhdf5:h5tget_size( DatatypeHandle ),
	Size;

get_size( DatatypeName ) when is_atom( DatatypeName ) ->

	% From our atom to binding one:
	BindingDatatypeName = convert_datatype( DatatypeName ),

	% From the binding atom to an handle:
	{ ok, BindingDatatypeHandle } = erlhdf5:datatype_name_to_handle(
								BindingDatatypeName ),



	get_size( BindingDatatypeHandle ).




% Dataset subsection.
%
% They contain the actual data, they are the arrays themselves.



% Creates specified dataset in specified HDF5 file, with specified name, type
% for its elements, metadata (dataspace) and property list.
%
-spec create_dataset( dataset_name(), datatype(), dataspace(), property_list(),
					  hdf5_file() ) -> dataset().
create_dataset( DatasetName, Datatype, Dataspace, PropertyList, File ) ->

	case erlhdf5:h5dcreate( File, DatasetName, Datatype, Dataspace,
							PropertyList ) of

		{ ok, Dataset } ->
			Dataset;

		{ error, Reason } ->
			throw( { dataset_creation_failed, Reason, DatasetName } )

	end.


% Opens an existing dataset.
%
-spec open_dataset( dataset_name(), hdf5_file() ) -> dataset().
open_dataset( DatasetName, File ) ->

	%io:format( "Opening dataset '~s' from file ~p.~n", [ DatasetName, File ] ),

	case erlhdf5:h5dopen( File, DatasetName ) of

		{ ok, Dataset } ->
			Dataset;

		{ error, Reason } ->
			throw( { dataset_open_failed, Reason, DatasetName } )

	end.



% Closes specified dataset.
%
-spec close_dataset( dataset() ) -> void().
close_dataset( Dataset ) ->
	ok = erlhdf5:h5dclose( Dataset ).



% Returns the allocation status of specified dataset.
%
-spec get_allocation_status( dataset() ) -> allocation_status().
get_allocation_status( Dataset ) ->

	{ ok, Res } = erlhdf5:h5d_get_space_status( Dataset ),

	case Res of

		'H5D_SPACE_STATUS_NOT_ALLOCATED' ->
			not_allocated;

		'H5D_SPACE_STATUS_PART_ALLOCATED' ->
			partially_allocated;

		'H5D_SPACE_STATUS_ALLOCATED' ->
			fully_allocated

	end.



% Returns the size, in bytes, used to store specified dataset.
%
-spec get_storage_size( dataset() ) -> system_utils:byte_size().
get_storage_size( Dataset ) ->

	{ ok, Res } = erlhdf5:h5d_get_storage_size( Dataset ),

	Res.



% Writes specified data (without checking it specifically) in specified dataset.
%
-spec write( data(), dataset() ) -> void().
write( Data, Dataset ) ->
	% By default, no checking performed:
	write( Data, Dataset, _CheckData=false ).



% Writes specified data (after having checked it, if requested) in specified
% dataset.
%
-spec write( data(), dataset(), boolean() ) -> void().
write( Data, Dataset, CheckData ) ->

	case CheckData of

		true ->
			check_data( Data );

		false ->
			ok

	end,

	% We deem the reverse parameter order clearer:
	ok = erlhdf5:h5dwrite( Dataset, Data ).



% Updates the specified dataset at specified index (starting at #0) with
% specified data.
%
-spec update( data(), basic_utils:count(), dataset() ) -> void().
update( Data, Index, Dataset ) ->

	% Gets a copy of the dataspace of this dataset:
	{ ok, UpdateDataspace } = erlhdf5:h5dget_space( Dataset ),

	% Using the update data to determine layout:
	{ ActualData, Dim, TupleCount, TupleSize } = case Data of

		List=[ FirstTuple | _T ] ->
			% We are in two dimensions here, with a (supposedly homogeneous)
			% non-empty list of tuples to update:
			{ Data, 2, length( List ), size( FirstTuple ) };

		Tuple when is_tuple( Data ) ->
			% Two dimensions here, just one tuple to update:
			{ [ Data ], 2, 1, size( Tuple ) };

		% Single element, possibly integer, float, 'nan' or 'inf':
		_ ->
			{ Data, 1, 1, 1 }

	end,


	{ Offset, Stride, Count, Block } = case Dim of

		1 ->
			MonoDimOffset = Index,
			MonoDimStride = 1,
			MonoDimCount  = 1,
			MonoDimBlock  = 1,
			{ MonoDimOffset, MonoDimStride, MonoDimCount, MonoDimBlock };

		2 ->
			% Full tuple update only:
			TwoDimOffset = { Index, 0 },
			TwoDimStride = { 1, 1 },
			TwoDimCount  = { TupleCount, TupleSize },
			TwoDimBlock  = { 1, 1 },

			{ TwoDimOffset, TwoDimStride, TwoDimCount, TwoDimBlock }

	end,

	ok = erlhdf5:h5sselect_hyperslab( UpdateDataspace, 'H5S_SELECT_SET',
									  Offset, Stride, Count, Block ),


	% Not needing to specify here the memory hyperslab, nor transfer properties:
	ok = erlhdf5:h5dwrite( Dataset, _FileSpaceSelection=UpdateDataspace,
						   ActualData ).



% Reads the specified named dataset of specified type and tuple size from
% specified file, returning it as a list of tuples of specified size.
%
-spec read( string(), supported_datatypes(), basic_utils:count(), hdf5_file() )
		  -> data().
read( DatasetName, DataType, TupleSize, File ) ->

	% Reads dataset:
	{ ok, RawReadData } = case DataType of

			native_integer ->
				erlhdf5:h5lt_read_dataset_int( File, DatasetName );

			native_long_float ->
				erlhdf5:h5lt_read_dataset_double( File, DatasetName )

	end,

	list_utils:reconstruct_tuples( RawReadData, TupleSize ).



% Reads the specified named dataset expected to contain strings, and returns an
% (ordered) list of these strings.
%
read_strings( DatasetName, File ) ->

	case erlhdf5:h5lt_read_dataset_string( File, DatasetName ) of

		{ ok, List } ->
			List;

		{ error, Reason } ->
			throw( { read_strings_failed, Reason, DatasetName } )

	end.



% Helper section.


% Abstraction (de)conversion subsection.


% Apparently exactly one option supported by the binding:


% Converts our higher-level identifiers into binding ones:
%
-spec convert_open_options( [ open_option() ] ) -> hdf5_open_option().
convert_open_options( _Opts=[] ) ->
	'H5F_ACC_RDONLY';

convert_open_options( _Opts=[ read_only ] ) ->
	'H5F_ACC_RDONLY';

convert_open_options( _Opts=[ check_non_existing ] ) ->
	'H5F_ACC_EXCL';

convert_open_options( _Opts=[ read_write ] ) ->
	'H5F_ACC_RDWR';

convert_open_options( _Opts=[ overwrite ] ) ->
	'H5F_ACC_TRUNC';

convert_open_options( _Opts=[ debug ] ) ->
	'H5F_ACC_DEBUG';

convert_open_options( _Opts=[ create ] ) ->
	'H5F_ACC_CREAT';

convert_open_options( _Opts=Other ) ->
	throw( { unsupported_open_option, Other } ).


% Apparently OR'combining HDF5 options is not well supported by the binding:

%% convert_open_options( Opts ) ->
%%	convert_open_options( Opts, _Acc=[] ).


%% convert_open_options( _Opts=[], Acc ) ->
%%	Acc;

%% convert_open_options( _Opts=[ check_non_existing | T ], Acc ) ->
%%	convert_open_options( T, [ 'H5F_ACC_EXCL' | Acc ] );

%% convert_open_options( _Opts=[ read_write | T ], Acc ) ->
%%	convert_open_options( T, [ 'H5F_ACC_RDWR' | Acc ] );

%% convert_open_options( _Opts=[ overwrite | T ], Acc ) ->
%%	convert_open_options( T, [ 'H5F_ACC_TRUNC' | Acc ] );

%% convert_open_options( _Opts=[ debug | T ], Acc ) ->
%%	convert_open_options( T, [ 'H5F_ACC_DEBUG' | Acc ] );

%% convert_open_options( _Opts=[ create | T ], Acc ) ->
%%	convert_open_options( T, [ 'H5F_ACC_CREAT' | Acc ] );

%% convert_open_options( _Opts=[ Other | _T ], _Acc ) ->
%%	throw( { unsupported_open_option, Other } ).



% Converts our higher-level identifiers into binding ones:
%
-spec convert_class( class() ) -> hdf5_class_flag().
convert_class( attribute_creation ) ->
	'H5P_ATTRIBUTE_CREATE';

convert_class( dataset_access ) ->
	'H5P_DATASET_ACCESS';

convert_class( dataset_creation ) ->
	'H5P_DATASET_CREATE';

convert_class( dataset_transfer ) ->
	'H5P_DATASET_XFER';

convert_class( datatype_access ) ->
	'H5P_DATATYPE_ACCESS';

convert_class( datatype_creation ) ->
	'H5P_DATATYPE_CREATE';

convert_class( file_access ) ->
	'H5P_FILE_ACCESS';

convert_class( file_creation ) ->
	'H5P_FILE_CREATE';

convert_class( file_mounting ) ->
	'H5P_FILE_MOUNT';

convert_class( group_access ) ->
	'H5P_GROUP_ACCESS';

convert_class( group_creation ) ->
	'H5P_GROUP_CREATE';

convert_class( link_access ) ->
	'H5P_LINK_ACCESS';

convert_class( link_creation ) ->
	'H5P_LINK_CREATE';

convert_class( object_copying ) ->
	'H5P_OBJECT_COPY';

convert_class( object_creation ) ->
	'H5P_OBJECT_CREATE';

convert_class( string_creation ) ->
	'H5P_STRING_CREATE';

convert_class( Other ) ->
	throw( { unsupported_class, Other } ).



% Converts our higher-level identifiers into binding ones (HDF5 ones - as atoms,
% not as their final form as integer handles):
%
-spec convert_datatype( predefined_datatype() ) -> hdf5_predefined_datatype().
convert_datatype( native_integer ) ->
	'H5T_NATIVE_INT';

convert_datatype( native_long_integer ) ->
	'H5T_NATIVE_LONG';

convert_datatype( native_float ) ->
	'H5T_NATIVE_FLOAT';

convert_datatype( native_long_float ) ->
	'H5T_NATIVE_DOUBLE';

convert_datatype( Other ) ->
	throw( { unsupported_datatype, Other } ).



% Returns the high-level datatypes that are known.
%
-spec get_known_datatypes() -> [ predefined_datatype() ].
get_known_datatypes() ->
	[ native_integer, native_long_integer, native_float, native_long_float ].



% Converts our higher-level identifiers into binding ones:
%
-spec convert_datatype_class_identifier( datatype_class_identifier() ) ->
											   hdf5_class_identifier().
convert_datatype_class_identifier( no_class ) ->
	% Looks like an error case:
	%'H5T_NO_CLASS';
	?H5T_NO_CLASS;

convert_datatype_class_identifier( integer ) ->
	%'H5T_INTEGER';
	?H5T_INTEGER;

convert_datatype_class_identifier( float ) ->
	%'H5T_FLOAT';
	?H5T_FLOAT;

convert_datatype_class_identifier( time ) ->
	%'H5T_TIME';
	?H5T_TIME;

convert_datatype_class_identifier( string ) ->
	%'H5T_STRING';
	?H5T_STRING;

convert_datatype_class_identifier( bitfield ) ->
	%'H5T_BITFIELD';
	?H5T_BITFIELD;

convert_datatype_class_identifier( opaque ) ->
	%'H5T_OPAQUE';
	?H5T_OPAQUE;

convert_datatype_class_identifier( compound ) ->
	%'H5T_COMPOUND';
	?H5T_COMPOUND;

convert_datatype_class_identifier( reference ) ->
	%'H5T_REFERENCE';
	?H5T_REFERENCE;

convert_datatype_class_identifier( enumeration ) ->
	%'H5T_ENUM';
	?H5T_ENUM;

convert_datatype_class_identifier( variable_length ) ->
	%'H5T_VLEN';
	?H5T_VLEN;

convert_datatype_class_identifier( array ) ->
	%'H5T_ARRAY';
	?H5T_ARRAY;

convert_datatype_class_identifier( Other ) ->
	throw( { unsupported_datatype_class_identifier, Other } ).



% Converts binding identifiers into our higher-level ones:
%
-spec deconvert_datatype_class_identifier( hdf5_class_identifier() ) ->
												 datatype_class_identifier().
%deconvert_datatype_class_identifier( 'H5T_NO_CLASS' ) ->
deconvert_datatype_class_identifier( ?H5T_NO_CLASS ) ->
	no_class;

%deconvert_datatype_class_identifier( 'H5T_INTEGER' ) ->
deconvert_datatype_class_identifier( ?H5T_INTEGER ) ->
	integer;

%deconvert_datatype_class_identifier( 'H5T_FLOAT' ) ->
deconvert_datatype_class_identifier( ?H5T_FLOAT ) ->
	float;

%deconvert_datatype_class_identifier( 'H5T_TIME' ) ->
deconvert_datatype_class_identifier( ?H5T_TIME ) ->
	time;

%deconvert_datatype_class_identifier( 'H5T_STRING' ) ->
deconvert_datatype_class_identifier( ?H5T_STRING ) ->
	string;

%deconvert_datatype_class_identifier( 'H5T_BITFIELD' ) ->
deconvert_datatype_class_identifier( ?H5T_BITFIELD ) ->
	bitfield;

%deconvert_datatype_class_identifier( 'H5T_OPAQUE' ) ->
deconvert_datatype_class_identifier( ?H5T_OPAQUE ) ->
	opaque;

%deconvert_datatype_class_identifier( 'H5T_COMPOUND' ) ->
deconvert_datatype_class_identifier( ?H5T_COMPOUND ) ->
	compound;

%deconvert_datatype_class_identifier( 'H5T_REFERENCE' ) ->
deconvert_datatype_class_identifier( ?H5T_REFERENCE ) ->
	reference;

%deconvert_datatype_class_identifier( 'H5T_ENUM' ) ->
deconvert_datatype_class_identifier( ?H5T_ENUM ) ->
	enumeration;

%deconvert_datatype_class_identifier( 'H5T_VLEN' ) ->
deconvert_datatype_class_identifier( ?H5T_VLEN ) ->
	variable_length;

%deconvert_datatype_class_identifier( 'H5T_ARRAY' ) ->
deconvert_datatype_class_identifier( ?H5T_ARRAY ) ->
	array;

deconvert_datatype_class_identifier( Other ) ->
	throw( { unsupported_binding_datatype_class_identifier, Other } ).



% Converts our higher-level identifiers into binding ones:
%
-spec convert_byte_order( byte_order() ) -> hdf5_byte_order().
convert_byte_order( little_endian ) ->
	?H5T_ORDER_LE;

convert_byte_order( big_endian ) ->
	?H5T_ORDER_BE;

convert_byte_order( vax_mixed ) ->
	?H5T_ORDER_VAX;

convert_byte_order( mixed ) ->
	?H5T_ORDER_MIXED;

convert_byte_order( none ) ->
	?H5T_ORDER_NONE;

% Includes H5T_ORDER_ERROR:
convert_byte_order( Other ) ->
	throw( { unsupported_byte_order, Other } ).



% Converts binding identifiers to our higher-level ones:
%
deconvert_byte_order( ?H5T_ORDER_LE ) ->
	little_endian;

deconvert_byte_order( ?H5T_ORDER_BE ) ->
	big_endian;

deconvert_byte_order( ?H5T_ORDER_VAX ) ->
	vax_mixed;

deconvert_byte_order( ?H5T_ORDER_MIXED ) ->
	mixed;

deconvert_byte_order( ?H5T_ORDER_NONE ) ->
	none;

% Includes H5T_ORDER_ERROR:
deconvert_byte_order( Other ) ->
	throw( { unsupported_byte_order, Other } ).
