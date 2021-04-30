% Copyright (C) 2017-2021 Olivier Boudeville
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



% Gathering of various facilities regarding the management of binary, bit-level
% operations, like cyclic redundancy check (CRC) calculations.
%
% See bin_utils_test.erl for the corresponding test.
%
-module(bin_utils).


-export([ get_crc8_table/0, compute_crc8_checksum/1 ]).

-type crc8_checksum() :: byte().

-export_type([ crc8_checksum/0 ]).


% Erlang pointers about bit-related operations:
% - http://erlang.org/doc/programming_examples/bit_syntax.html
% - http://erlang.org/doc/reference_manual/expressions.html#bit_syntax
% - http://learnyousomeerlang.com/starting-out-for-real#bit-syntax


% Returns the table used to compute CRC8.
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



% Returns the CRC8 checksum corresponding to specified binary.
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
	%	"new index is ~p, new CRC is ~p.",
	%	[ CurrentCRC, Byte, Index, NewCRC ] ),

	compute_crc8_checksum( T, CRCTable, NewCRC ).
