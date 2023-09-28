% Copyright (C) 2017-2023 Olivier Boudeville
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
% Creation date: Sunday, July 30, 2017.


% @doc Gathering of various facilities regarding the management of <b>binary,
% bit-level operations</b>, like cyclic redundancy check (CRC) calculations.
%
% See bin_utils_test.erl for the corresponding test.
%
-module(bin_utils).


% Binary basics (see also the 'binary' standard module):
-export([ create_binary/1, concatenate/1, concatenate/2, concatenate/3,
		  replicate/2 ]).


% Serialisation:
-export([ tuples_to_float32s_binary/1, tuples_to_float32s_binary/2,
		  concatenate_as_float32s/1, concatenate_as_float32s/2,

		  tuples_to_int32s_binary/1, tuples_to_int32s_binary/2,
		  concatenate_as_int32s/1, concatenate_as_int32s/2,

		  tuples_to_uint32s_binary/1, tuples_to_uint32s_binary/2,
		  concatenate_as_uint32s/1, concatenate_as_uint32s/2 ]).


-export([ get_crc8_table/0, compute_crc8_checksum/1 ]).



-type buffer() :: binary().
% A (binary) buffer, a series of bytes.

-type crc8_checksum() :: byte().


-export_type([ buffer/0, crc8_checksum/0 ]).


% Erlang pointers about bit-related operations:
% - http://erlang.org/doc/programming_examples/bit_syntax.html
% - http://erlang.org/doc/reference_manual/expressions.html#bit_syntax
% - http://learnyousomeerlang.com/starting-out-for-real#bit-syntax
% - https://cheatography.com/fylke/cheat-sheets/erlang-binaries/


% Shorthands:

-type count() :: basic_utils:count().

-type tuple( T ) :: type_utils:tuple( T ).

-type byte_size() :: system_utils:byte_size().



% @doc Creates a (binary) buffer of the specified size, containing only zeroes.
%
% Possibly useful in order to provide a buffer to non-allocating functions (like
% gl:getDebugMessageLog/2 was wrongly believed to be).
%
-spec create_binary( byte_size() ) -> buffer().
create_binary( ByteCount ) ->

	% Maybe a better solution exists:
	Bin = <<0:(8*ByteCount)/integer>>,

	cond_utils:if_defined( myriad_check_binaries,
		basic_utils:assert_equal( ByteCount, byte_size( Bin ) ) ),

	cond_utils:if_defined( myriad_debug_binaries,
		trace_utils:debug_fmt( "Created a binary of ~B bytes:~n  ~p",
							   [ ByteCount, Bin ] ) ),

	Bin.



% @doc Concatenates the specified binaries into the returned one.
%
% Note: mostly added for documentation purpose; can/should be inlined by the
% developer.
%
-spec concatenate( binary(), binary() ) -> binary().
concatenate( Bin1, Bin2 ) ->
	<<Bin1/binary, Bin2/binary>>.


% @doc Concatenates the specified binaries into the returned one.
-spec concatenate( [ binary() ] ) -> binary().
concatenate( BinStrs ) ->

	% Could have been instead:
	%erlang:iolist_to_binary( BinStrs ).

	lists:foldr( fun concatenate/2, _InitAcc= <<>>, _List=BinStrs ).



% @doc Concatenates to the specified original binary (on its right) the
% specified number of copies of the second specified binary.
%
-spec concatenate( binary(), count(), binary() ) -> binary().
concatenate( OrigBin, ReplicationCount, ToReplicateBin ) ->
	% Avoids too many transient copies:
	concat_helper( ReplicationCount, ToReplicateBin, OrigBin ).


concat_helper( _ReplicationCount=0, _ToReplicateBin, Bin ) ->
	Bin;

concat_helper( ReplicationCount, ToReplicateBin, Bin ) ->
	NewBin = <<Bin/binary, ToReplicateBin/binary>>,
	concat_helper( ReplicationCount-1, ToReplicateBin, NewBin ).



% @doc Returns the binary obtained when concatenating the specified binary the
% specified number of times.
%
-spec replicate( binary(), count() ) -> binary().
replicate( Bin, Count ) ->
	replicate( Bin, Count, _Acc= <<>> ).


% (helper)
replicate( _Bin, _Count=0, Acc ) ->
	Acc;

replicate( Bin, Count, Acc ) ->
	replicate( Bin, Count-1, <<Bin/binary, Acc/binary>> ).




% Serialisation section.

% Binary comprehensions could be used as well, like in:
%to_buffer( Points ) ->
%   << <<X:?F32, Y:?F32, Z:?F32>> || { X, Y, Z } <- Points >>.



% Float serialisation subsection.


% @doc Returns the binary obtained by serialising in-order all floats specified
% as tuples of arbitrary size, as 32-bit floats, based on the native endianess.
%
% Example: Bin = tuples_to_float32s_binary([{0.0, 1.0}, {0.5, 0.5, 0.5}])
%
% Typically useful to create suitable OpenGL arrays from heterogenous tuples
% aggregating vertices, normals, colors, etc. on a per vertex attribute basis.
%
-spec tuples_to_float32s_binary( [ tuple( float() ) ] ) -> binary().
tuples_to_float32s_binary( Tuples ) ->
	tuples_to_float32s_binary( Tuples, _AccBin= <<>> ).



% @doc Returns the binary obtained by appending (on the right) to the specified
% one the in-order serialisation of all floats specified as tuples of arbitrary
% size, as 32-bit floats, based on the native endianess.
%
% Example: FullBin = tuples_to_float32s_binary([{0.0, 1.0}, {0.5, 0.5, 0.5}],
%                                              Bin)
%
% Typically useful to create suitable OpenGL arrays from heterogenous tuples
% aggregating vertices, normals, colors, etc. on a per vertex attribute basis.
%
-spec tuples_to_float32s_binary( [ tuple( float() ) ], binary() ) -> binary().
tuples_to_float32s_binary( _Tuples=[], Bin ) ->
	Bin;

% Hopefully as fast as reasonably possible:
tuples_to_float32s_binary( _Tuples=[ Tuple | T ], Bin ) ->
	Floats = tuple_to_list( Tuple ),
	NewBin = concatenate_as_float32s( Floats, Bin ),
	tuples_to_float32s_binary( T, NewBin ).



% @doc Concatenates the specified floats as 32-bit floats, based on the native
% endianess.
%
-spec concatenate_as_float32s( [ float() ] ) -> binary().
concatenate_as_float32s( Floats ) ->
	concatenate_as_float32s( Floats, _Bin= <<>> ).


% @doc Concatenates the specified floats after (not before) the specified
% binary.
%
-spec concatenate_as_float32s( [ float() ], binary() ) -> binary().
concatenate_as_float32s( _Floats=[], Bin ) ->
	Bin;

concatenate_as_float32s( _Floats=[ F | T ], Bin ) ->
	% Binaries are best appended (i.e. on their the right):
	%
	% (note that no exception will be thrown if any F is an integer)

	cond_utils:if_defined( myriad_check_binaries,
		basic_utils:assert( is_float( F ) ) ),

	NewBin = <<Bin/binary, F:32/float-native>>,
	concatenate_as_float32s( T, NewBin ).



% Integer serialisation subsection.

% @doc Returns the binary obtained by serialising in-order all integers
% specified as tuples of arbitrary size, as 32-bit signed integers, based on the
% native endianess.
%
% Example: Bin = tuples_to_int32s_binary([{40,50}, {5, 10, -15}]).
%
-spec tuples_to_int32s_binary( [ tuple( integer() ) ] ) -> binary().
tuples_to_int32s_binary( Tuples ) ->
	tuples_to_int32s_binary( Tuples, _AccBin= <<>> ).


% @doc Returns the binary obtained by appending (on the right) to the specified
% one the in-order serialisation of all integers specified as tuples of
% arbitrary size, as 32-bit signed integers, based on the native endianess.
%
% Example: FullBin = tuples_to_int32s_binary([{40,50}, {5, 10, -15}], Bin).
%
-spec tuples_to_int32s_binary( [ tuple( integer() ) ], binary() ) -> binary().
tuples_to_int32s_binary( _Tuples=[], Bin ) ->
	Bin;

% Hopefully as fast as reasonably possible:
tuples_to_int32s_binary( _Tuples=[ Tuple | T ], Bin ) ->
	Ints = tuple_to_list( Tuple ),
	NewBin = concatenate_as_int32s( Ints, Bin ),
	tuples_to_int32s_binary( T, NewBin ).



% @doc Concatenates the specified integers as 32-bit integers, based on the
% native endianess.
%
-spec concatenate_as_int32s( [ integer() ] ) -> binary().
concatenate_as_int32s( Ints ) ->
	concatenate_as_int32s( Ints, _Bin= <<>> ).


% @doc Concatenates the specified integers after (not before) the specified
% binary.
%
-spec concatenate_as_int32s( [ integer() ], binary() ) -> binary().
concatenate_as_int32s( _Ints=[], Bin ) ->
	Bin;

concatenate_as_int32s( _Ints=[ I | T ], Bin ) ->
	% Binaries are best appended (i.e. on their the right):
	%
	% (note that an exception will be thrown if any I is a float)
	%
	NewBin = <<Bin/binary, I:32/integer-signed-native>>,
	concatenate_as_int32s( T, NewBin ).



% Unsigned integer serialisation subsection.


% @doc Returns the binary obtained by serialising in-order all positive or null
% integers specified as tuples of arbitrary size, as 32-bit unsigned integers,
% based on the native endianess.
%
% Example: Bin = tuples_to_uint32s_binary([{40,50}, {5, 10, 15}])
%
% Typically useful to create suitable OpenGL arrays from indices.
%
-spec tuples_to_uint32s_binary( [ tuple( non_neg_integer() ) ] ) -> binary().
tuples_to_uint32s_binary( Tuples ) ->
	tuples_to_uint32s_binary( Tuples, _AccBin= <<>> ).


% @doc Returns the binary obtained by appending (on the right) to the specified
% one the in-order serialisation of all integers specified as tuples of
% arbitrary size, as 32-bit unsigned integers, based on the native endianess.
%
% Example: FullBin = tuples_to_uint32s_binary([{40,50}, {5, 10, 15}], Bin).
%
% Typically useful to create suitable OpenGL arrays from indices.
%
tuples_to_uint32s_binary( _Tuples=[], AccBin ) ->
	AccBin;

% Hopefully as fast as reasonably possible:
tuples_to_uint32s_binary( _Tuples=[ Tuple | T ], AccBin ) ->
	Ints = tuple_to_list( Tuple ),
	NewAccBin = concatenate_as_uint32s( Ints, AccBin ),
	tuples_to_uint32s_binary( T, NewAccBin ).



% @doc Concatenates the specified positive or null integers as 32-bit unsigned
% integers, based on the native endianess.
%
-spec concatenate_as_uint32s( [ non_neg_integer() ] ) -> binary().
concatenate_as_uint32s( Ints ) ->
	concatenate_as_uint32s( Ints, _Bin= <<>> ).


% @doc Concatenates the specified positive or null integers after (not before)
% the specified binary.
%
-spec concatenate_as_uint32s( [ non_neg_integer() ], binary() ) -> binary().
concatenate_as_uint32s( _UInts=[], Bin ) ->
	Bin;

concatenate_as_uint32s( _UInts=[ UI | T ], Bin ) ->
	% Binaries are best appended (i.e. on their the right):
	%
	% (note that an exception will be thrown if any UI is a float, but not if it
	% is a negative integer)

	cond_utils:if_defined( myriad_check_binaries,
						   basic_utils:assert( UI >= 0 ) ),

	NewBin = <<Bin/binary, UI:32/integer-unsigned-native>>,
	concatenate_as_uint32s( T, NewBin ).



% CRC section.


% @doc Returns the table used to compute CRC8.
-spec get_crc8_table() -> tuple().
get_crc8_table() ->

	% This table can be found for example, in https://github.com/, in:
	%  - kipe/enocean/blob/master/enocean/protocol/crc8.py
	%  - eno2mqtt/blob/master/src/main/java/com/tellerulam/eno2mqtt/CRC8.java
	%
	% (they agree)

	% Default of a binary: unsigned, 8-bit integers, here listed as hexadecimal
	% values.

	% 256 values, 32 lines:
	{ 16#00, 16#07, 16#0e, 16#09, 16#1c, 16#1b, 16#12, 16#15,
	  16#38, 16#3f, 16#36, 16#31, 16#24, 16#23, 16#2a, 16#2d,
	  16#70, 16#77, 16#7e, 16#79, 16#6c, 16#6b, 16#62, 16#65,
	  16#48, 16#4f, 16#46, 16#41, 16#54, 16#53, 16#5a, 16#5d,
	  16#e0, 16#e7, 16#ee, 16#e9, 16#fc, 16#fb, 16#f2, 16#f5,
	  16#d8, 16#df, 16#d6, 16#d1, 16#c4, 16#c3, 16#ca, 16#cd,
	  16#90, 16#97, 16#9e, 16#99, 16#8c, 16#8b, 16#82, 16#85,
	  16#a8, 16#af, 16#a6, 16#a1, 16#b4, 16#b3, 16#ba, 16#bd,
	  16#c7, 16#c0, 16#c9, 16#ce, 16#db, 16#dc, 16#d5, 16#d2,
	  16#ff, 16#f8, 16#f1, 16#f6, 16#e3, 16#e4, 16#ed, 16#ea,
	  16#b7, 16#b0, 16#b9, 16#be, 16#ab, 16#ac, 16#a5, 16#a2,
	  16#8f, 16#88, 16#81, 16#86, 16#93, 16#94, 16#9d, 16#9a,
	  16#27, 16#20, 16#29, 16#2e, 16#3b, 16#3c, 16#35, 16#32,
	  16#1f, 16#18, 16#11, 16#16, 16#03, 16#04, 16#0d, 16#0a,
	  16#57, 16#50, 16#59, 16#5e, 16#4b, 16#4c, 16#45, 16#42,
	  16#6f, 16#68, 16#61, 16#66, 16#73, 16#74, 16#7d, 16#7a,
	  16#89, 16#8e, 16#87, 16#80, 16#95, 16#92, 16#9b, 16#9c,
	  16#b1, 16#b6, 16#bf, 16#b8, 16#ad, 16#aa, 16#a3, 16#a4,
	  16#f9, 16#fe, 16#f7, 16#f0, 16#e5, 16#e2, 16#eb, 16#ec,
	  16#c1, 16#c6, 16#cf, 16#c8, 16#dd, 16#da, 16#d3, 16#d4,
	  16#69, 16#6e, 16#67, 16#60, 16#75, 16#72, 16#7b, 16#7c,
	  16#51, 16#56, 16#5f, 16#58, 16#4d, 16#4a, 16#43, 16#44,
	  16#19, 16#1e, 16#17, 16#10, 16#05, 16#02, 16#0b, 16#0c,
	  16#21, 16#26, 16#2f, 16#28, 16#3d, 16#3a, 16#33, 16#34,
	  16#4e, 16#49, 16#40, 16#47, 16#52, 16#55, 16#5c, 16#5b,
	  16#76, 16#71, 16#78, 16#7f, 16#6a, 16#6d, 16#64, 16#63,
	  16#3e, 16#39, 16#30, 16#37, 16#22, 16#25, 16#2c, 16#2b,
	  16#06, 16#01, 16#08, 16#0f, 16#1a, 16#1d, 16#14, 16#13,
	  16#ae, 16#a9, 16#a0, 16#a7, 16#b2, 16#b5, 16#bc, 16#bb,
	  16#96, 16#91, 16#98, 16#9f, 16#8a, 16#8d, 16#84, 16#83,
	  16#de, 16#d9, 16#d0, 16#d7, 16#c2, 16#c5, 16#cc, 16#cb,
	  16#e6, 16#e1, 16#e8, 16#ef, 16#fa, 16#fd, 16#f4, 16#f3 }.



% @doc Returns the CRC8 checksum corresponding to specified binary.
-spec compute_crc8_checksum( binary() ) -> crc8_checksum().
compute_crc8_checksum( Binary ) ->

	CRCTable = get_crc8_table(),

	% Byte-level here (no encoding to be considered):
	compute_crc8_checksum( binary_to_list( Binary ), CRCTable, _InitialCRC=0 ).


% (helper)
compute_crc8_checksum( _BinList=[], _CRCTable, CurrentCRC ) ->
	CurrentCRC;

compute_crc8_checksum( _BinList=[ Byte | T ], CRCTable, CurrentCRC ) ->

	% Arithmetic bitwise XOR:

	% No need to truncate to byte:
	%Index = ( CurrentCRC band 16#ff ) bxor ( Byte band 16#ff ),
	Index = CurrentCRC bxor Byte,

	% As indices start at 1:
	NewCRC = element( Index + 1, CRCTable ),

	%trace_utils:debug_fmt( "With current CRC of ~p, read byte ~p: "
	%   "new index is ~p, new CRC is ~p.",
	%   [ CurrentCRC, Byte, Index, NewCRC ] ),

	compute_crc8_checksum( T, CRCTable, NewCRC ).
