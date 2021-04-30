% Copyright (C) 2013-2021 Olivier Boudeville
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
% Creation date: Friday, November 1, 2013.


% Gathering of various cipher-related facilities.
%
% We focus on symmetric ciphering here.
%
% See cipher_utils_test.erl for testing.
%
-module(cipher_utils).


-export([ generate_key/2, key_to_string/1,
		  encrypt/3, decrypt/3,
		  generate_mealy_table/1, compute_inverse_mealy_table/1,
		  mealy_table_to_string/1 ]).


% This pseudo-export allows to avoid a spurious Dialyzer warning about the final
% catch-all clause (user-provided arguments, hence they can have any type).
%
-export( [ apply_cipher/3 ]).



% Implementation notes.
%
% To encrypt a file, one shall use a key file, whose extension is by convention
% 'cipher' (ex: "my-key-file.cipher").
%
% The same file can be used to perform the reverse operation.
%
% The mode of operation is to chain a series of elementary transformations that
% can be reversed. These operations are listed and described in the
% aforementioned file, which contains Erlang terms for that.


% When ciphering or deciphering a file of size N, as much as possible:
%
% - the whole file is streamed, hence it will never be loaded fully in memory
%
% - we expect that the free storage capacity is at least 2.N bytes
%
% - we could apply all transformations in-memory once (instead of writing as
% many intermediate files as there are transformations), however it would be
% difficult to implement (as such, and because of streaming, and because, from a
% transformation to another, the access patterns are usually different)




% Available transformations are:
%
% - id: identity (content not changed)
%
% - offset: the specified value is added to all bytes of the file
%
% - compress: the file content is replaced by a compressed version thereof,
% using one of the supported formats (see compress_format/0)
%
% - insert_random: based on the specified seed and on the specified range R, a
% series of strictly positive values is uniformly drawn in [1,R]; these values
% are offsets relative to the last random insertion (initial one is 0); at each
% position determined thanks to offsets, a random value in [0,255] is inserted
%
% - delta_combine: a byte Bk+1 is replaced by its difference with the previous
% byte, with B0=128; hence Bk+1 is replaced by Bk+1 - Bk (Bk having obeyed the
% same rule)
%
% - shuffle: based on the specified seed and length L, each series of up to L
% bytes is uniformly shuffled
%
% - xor: based on the specified list of bytes, the content of the file is XOR'ed
%
% - mealy: based on specified state-transition data, the content of the file is
% modified accordingly; the input and output alphabet are the same, B, the set
% of all bytes (i.e. integers in [0,255]), while the states are strictly
% positive integers; the transition and output function are coalesced into a
% single function: f({ CurrentState, InputByte }) -> {NewState, OutputByte};
% for more information: https://en.wikipedia.org/wiki/Mealy_machine


% Just for testing:
-type id_transform() :: 'id'.


-type offset_transform() :: { 'offset', integer() }.


-type compress_transform() :: { 'compress', compression_format() }.

-type decompress_transform() :: { 'decompress', compression_format() }.


-type insert_random_transform() :: { 'insert_random', seed(), count() }.

-type extract_random_transform() :: { 'extract_random', seed(), count() }.


-type delta_combine_transform() :: 'delta_combine'.
-type delta_combine_reverse_transform() :: 'delta_combine_reverse'.


-type shuffle_transform() :: { 'shuffle', seed(), count() }.

-type reciprocal_shuffle_transform() ::
		{ 'reciprocal_shuffle', seed(), count() }.


-type xor_transform() :: { 'xor', [ integer() ] }.



% Mealy transform section.

% Designates a state of the Mealy machine, starting from 1:
-type state() :: pos_integer().


% A letter (of both alphabets, i.e. input and output):
-type letter() :: byte().


% A cell of any inner array, each of these arrays being relative to a given
% input letter and being indexed by the possible machine states:
%
-type cell() :: { state(), letter() }.


% Each array of this type is relative to an input letter, and its cell are
% indexed by states:
%
-type inner_array() :: array:array( cell() ).


% A Mealy table can be seen as a two-dimensional array, whose first dimension is
% the states of the Machine (in [1,StateCount]) and second is the input alphabet
% (here letters are bytes, in [0,255]).
%
% Each cell is a {NextState, OutputLetter} pair.
%
% The Mealy table is implemented (easier to build) as a fixed-size array, one
% element per possible state (corresponding to a column of the 2D array).
%
% Each element of this table is itself an array, having as many elements as the
% size of the input alphabet, hence 256 of them, in [0,255].
%
% Each element of these inner arrays corresponds to the aforementioned cell.
%
-type mealy_table() :: array:array( inner_array() ).


% A state is defined by a strictly positive integer:
-type mealy_state() :: pos_integer().


-type mealy_transform() :: { 'mealy', mealy_state(), mealy_table() }.



-type cipher_transform() :: id_transform()
						  | offset_transform()
						  | compress_transform()
						  | insert_random_transform()
						  | delta_combine_transform()
						  | shuffle_transform()
						  | xor_transform()
						  | mealy_transform().


% For ciphers which require specific reverse transformations:
-type decipher_transform() :: decompress_transform()
							| extract_random_transform()
							| delta_combine_reverse_transform()
							| reciprocal_shuffle_transform().


-type any_transform() :: cipher_transform() | decipher_transform().


% User may specify either some licit transform, or possibly mistakes:
-type user_specified_transform() :: any().


-type key() :: [ any_transform() ].


-export_type([ cipher_transform/0, decipher_transform/0 ]).


-define( bin_read_opts, [ read, raw, binary, read_ahead ] ).
-define( bin_write_opts, [ write, raw, delayed_write ] ).


-define( list_read_opts, [ read, raw, read_ahead ] ).
-define( list_write_opts, [ write, raw, delayed_write ] ).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type seed() :: random_utils:seed().

-type compression_format() :: file_utils:compression_format().
-type file_path() :: file_utils:file_path().



% Generates a key file.
-spec generate_key( file_path(), [ cipher_transform() ] ) -> void().
generate_key( KeyFilePath, Transforms ) ->

	case file_utils:exists( KeyFilePath ) of

		true ->
			throw( { already_existing_key_file, KeyFilePath } );

		false ->
			ok

	end,

	% No delayed_write wanted:
	KeyFile = file_utils:open( KeyFilePath, _Opts=[ write, raw ] ),

	Header = text_utils:format( "% Key generated on ~ts, by ~ts, on ~ts.~n",
		[ time_utils:get_textual_timestamp(), system_utils:get_user_name(),
		  net_utils:localhost() ] ),

	file_utils:write_ustring( KeyFile, Header ),

	file_utils:write_ustring( KeyFile, "~n~w.~n~n", [ Transforms ] ),

	file_utils:write_ustring( KeyFile, "% End of key file.~n", [] ),

	file_utils:close( KeyFile ).




% Returns a description of the specified key.
-spec key_to_string( key() ) -> ustring().
key_to_string( Key ) ->
	text_utils:format( "Key composed of following ~B cipher(s): ~ts",
		[ length( Key ), text_utils:strings_to_string(
							key_to_strings( Key, _Acc=[] ) ) ] ).


% (helper)
key_to_strings( _Ciphers=[], Acc ) ->
	lists:reverse( Acc );


key_to_strings( [ _Cipher={ mealy, InitialState, Table } | T ], Acc ) ->

	% Much info:
	%CipherString = text_utils:format(
	%	"Mealy cipher with initial state S~B and a ~ts",
	%	[ InitialState, mealy_table_to_string( Table ) ] ),

	% Shorter:

	StateCount = array:size( Table ),
	AlphabetSize = array:size( array:get( 0, Table ) ),

	CipherString = text_utils:format(
		"Mealy cipher with initial state S~B for a table of "
		"~B states and an alphabet of ~B letters",
		[ InitialState, StateCount, AlphabetSize ] ),

	key_to_strings( T, [ CipherString | Acc ] );


key_to_strings( [ Cipher | T ], Acc ) ->
	CipherString = text_utils:format( "~p", [ Cipher ] ),
	key_to_strings( T, [ CipherString | Acc ] ).




% Encrypts specified source file using specified key file, and writes the result
% in specified target file.
%
% The original file is kept as is.
%
-spec encrypt( file_path(), file_path(), file_path() ) -> void().
encrypt( SourceFilePath, TargetFilePath, KeyFilePath ) ->

	case file_utils:is_existing_file_or_link( SourceFilePath ) of

		true ->
			ok;

		false ->
			throw( { non_existing_source_file, SourceFilePath } )

	end,


	case file_utils:exists( TargetFilePath ) of

		true ->
			throw( { already_existing_target_file, TargetFilePath } );

		false ->
			ok

	end,

	KeyInfos = read_key( KeyFilePath ),

	io:format( "Encrypting source file '~ts' with key file '~ts', "
		"storing the result in '~ts'.~n~ts~n",
		[ SourceFilePath, KeyFilePath, TargetFilePath,
		  key_to_string( KeyInfos ) ] ),

	% We may use randomised ciphers:
	random_utils:start_random_source( default_seed ),

	TempFilePath = apply_key( KeyInfos, SourceFilePath ),

	file_utils:rename( TempFilePath, TargetFilePath ).





% Decrypts specified source file using specified key file, and writes the result
% in specified target file.
%
% The ciphered file is kept as is.
%
-spec decrypt( file_path(), file_path(), file_path() ) -> void().
decrypt( SourceFilePath, TargetFilePath, KeyFilePath ) ->

	case file_utils:is_existing_file_or_link( SourceFilePath ) of

		true ->
			ok;

		false ->
			throw( { non_existing_source_file, SourceFilePath } )

	end,


	case file_utils:exists( TargetFilePath ) of

		true ->
			throw( { already_existing_target_file, TargetFilePath } );

		false ->
			ok

	end,


	KeyInfos = read_key( KeyFilePath ),

	io:format( "Decrypting source file '~ts' with key file '~ts', "
		"storing the result in '~ts'.~n~ts~n",
		[ SourceFilePath, KeyFilePath, TargetFilePath,
		  key_to_string( KeyInfos ) ] ),

	% We may use randomised ciphers:
	random_utils:start_random_source( default_seed ),

	ReverseKey = get_reverse_key_from( KeyInfos ),

	io:format( "Determined reverse key:~n~ts~n",
			   [ key_to_string( ReverseKey ) ] ),

	TempFilePath = apply_key( ReverseKey, SourceFilePath ),

	file_utils:rename( TempFilePath, TargetFilePath ).




% Helper section.


% Reads key from specified filename, and returns it.
%
% (helper)
%
-spec read_key( file_path() ) -> [ user_specified_transform() ].
read_key( KeyFilePath ) ->

	case file_utils:is_existing_file_or_link( KeyFilePath ) of

		true ->
			case file_utils:read_terms( KeyFilePath ) of

				[] ->
					throw( { empty_key, KeyFilePath } );

				[ Key ] when is_list( Key ) ->
					Key;

				Invalid ->
					throw( { invalid_multiline_key, Invalid } )

			end;

		false ->
			throw( { non_existing_key_file, KeyFilePath } )

	end.



% Returns the reverse key of specified one.
%
% (helper)
%
-spec get_reverse_key_from( [ user_specified_transform() ] ) ->
									[ any_transform() ].
get_reverse_key_from( KeyInfos ) ->
	get_reverse_key_from( KeyInfos, _Acc=[] ).



get_reverse_key_from( _KeyInfos=[], Acc ) ->
	% Order already reversed by design:
	Acc;

get_reverse_key_from( _KeyInfos=[ K | H ], Acc ) ->
	ReversedK = reverse_cipher( K ),
	get_reverse_key_from( H, [ ReversedK | Acc ] ).





% Applies specified key to specified file.
%
% Returns the filename of the resulting file.
%
apply_key( KeyInfos, SourceFilePath ) ->
	apply_key( KeyInfos, SourceFilePath, _CipherCount=1 ).


apply_key( _KeyInfos=[], SourceFilePath, _CipherCount ) ->
	SourceFilePath;

apply_key( _KeyInfos=[ C | H ], SourceFilePath, CipherCount ) ->


	io:format( " - applying cipher #~B: '~p'~n",
			   [ CipherCount, get_cipher_description( C ) ] ),

	% FilePath of the ciphered version:
	CipheredFilePath = generate_filename(),

	apply_cipher( C, SourceFilePath, CipheredFilePath ),

	case CipherCount of

		1 ->
			ok;

		_ ->
			% Not wanting to saturate the storage space with intermediate files:
			file_utils:remove_file( SourceFilePath )

	end,

	apply_key( H, CipheredFilePath, CipherCount + 1 ).



% Table way too big to be displayed:
get_cipher_description( { mealy, InitialState, _Table } ) ->
	text_utils:format( "Mealy transform, with initial state ~p",
					   [ InitialState ] );


get_cipher_description( OtherCipher ) ->
	OtherCipher.



% Applies specified cipher to specified file.
%
% Some ciphers are better managed if special-cased, whereas others can rely on
% base (yet generic) mechanisms.
%
-spec apply_cipher( any(), file_path(), file_path() ) -> void().
apply_cipher( id, SourceFilePath, CipheredFilePath ) ->
	id_cipher( SourceFilePath, CipheredFilePath );


apply_cipher( { offset, Offset }, SourceFilePath, CipheredFilePath ) ->

	% CypherState is simply the constant offset used:

	OffsetFun = fun( InputByte, CypherState ) ->
						OutputByte = InputByte + Offset,
						{ OutputByte, CypherState }
				end,

	apply_byte_level_cipher( SourceFilePath, CipheredFilePath,
							 _Transform=OffsetFun, _InitialCipherState=Offset );


apply_cipher( { compress, CompressFormat }, SourceFilePath,
			  CipheredFilePath ) ->
	compress_cipher( SourceFilePath, CipheredFilePath, CompressFormat );


apply_cipher( { decompress, CompressFormat }, SourceFilePath,
			  CipheredFilePath ) ->
	decompress_cipher( SourceFilePath, CipheredFilePath, CompressFormat );


apply_cipher( { insert_random, Seed, Range }, SourceFilePath,
			  CipheredFilePath ) ->

	random_utils:reset_random_source( Seed ),

	insert_random_cipher( SourceFilePath, CipheredFilePath, Range );


apply_cipher( { extract_random, Seed, Range }, SourceFilePath,
			  CipheredFilePath ) ->

	random_utils:reset_random_source( Seed ),

	extract_random_cipher( SourceFilePath, CipheredFilePath, Range );


apply_cipher( delta_combine, SourceFilePath, CipheredFilePath ) ->

	% CypherState is simply the last value read:

	DeltaFun = fun( InputByte, CypherState ) ->
						OutputByte = InputByte - CypherState,
						{ OutputByte, InputByte }
				end,

	apply_byte_level_cipher( SourceFilePath, CipheredFilePath,
							 _Transform=DeltaFun, _InitialCipherState=100 );


apply_cipher( delta_combine_reverse, SourceFilePath, CipheredFilePath ) ->

	% CypherState is simply the last value read:

	ReverseDeltaFun = fun( InputByte, CypherState ) ->
						OutputByte = InputByte + CypherState,
						{ OutputByte, OutputByte }
				end,

	apply_byte_level_cipher( SourceFilePath, CipheredFilePath,
					_Transform=ReverseDeltaFun, _InitialCipherState=100 );


apply_cipher( { shuffle, Seed, Length }, SourceFilePath, CipheredFilePath ) ->

	random_utils:reset_random_source( Seed ),

	shuffle_cipher( SourceFilePath, CipheredFilePath, Length, direct );


apply_cipher( { reciprocal_shuffle, Seed, Length }, SourceFilePath,
			  CipheredFilePath ) ->

	random_utils:reset_random_source( Seed ),

	shuffle_cipher( SourceFilePath, CipheredFilePath, Length, reciprocal );


apply_cipher( { 'xor', XORList }, SourceFilePath, CipheredFilePath ) ->

	xor_cipher( SourceFilePath, CipheredFilePath, XORList );


apply_cipher( { mealy, InitialMealyState, MealyTable }, SourceFilePath,
			  CipheredFilePath ) ->

	mealy_cipher( SourceFilePath, CipheredFilePath, InitialMealyState,
				  MealyTable );

apply_cipher( C, _SourceFilePath, _CipheredFilePath ) ->
	throw( { unknown_cipher_to_apply, C } ).



% Returns the reverse cipher of the specified one.
reverse_cipher( C=id ) ->
	C;

reverse_cipher( { offset, Offset } ) ->
	{ offset, 256 - Offset };

reverse_cipher( { compress, CompressFormat } ) ->
	{ decompress, CompressFormat };

reverse_cipher( { insert_random, Seed, Range } ) ->
	{ extract_random, Seed, Range };

reverse_cipher( delta_combine ) ->
	delta_combine_reverse;

reverse_cipher( delta_combine_reverse ) ->
	delta_reverse;

reverse_cipher( { shuffle, _Seed, _Length } ) ->
	{ reciprocal_shuffle, _Seed, _Length };

reverse_cipher( C={ 'xor', _XORList } ) ->
	C;

reverse_cipher( { mealy, InitialState, MealyMachine } ) ->
	{ mealy, InitialState, compute_inverse_mealy_table( MealyMachine ) };

reverse_cipher( C ) ->
	throw( { unknown_cipher_to_reverse, C } ).



% Cipher section.


% For all ciphers that can be expressed by a byte-level, stateful transformation
% fun.
%
apply_byte_level_cipher( SourceFilePath, CipheredFilePath, CipherFun,
						 CipherInitialState ) ->

	% No need for intermediate buffering, thanks to read_ahead and
	% delayed_write;

	SourceFile = file_utils:open( SourceFilePath, ?bin_read_opts ),

	TargetFile = file_utils:open( CipheredFilePath, ?bin_write_opts ),

	apply_byte_level_helper( SourceFile, TargetFile, CipherFun,
							 CipherInitialState ).



% Actual application of a byte-level transform.
apply_byte_level_helper( SourceFile, TargetFile, CipherFun,
						 CipherInitialState ) ->

	Count = 1024 * 8,

	case file_utils:read( SourceFile, Count ) of

		eof ->
			file_utils:close( SourceFile ),
			file_utils:close( TargetFile );

		{ ok, DataBin } ->

			{ NewDataBin, NewCipherState } = transform_bytes( DataBin,
									CipherFun, CipherInitialState ),

			file_utils:write( TargetFile, NewDataBin ),

			apply_byte_level_helper( SourceFile, TargetFile, CipherFun,
									 NewCipherState )

	end.



% There must be a way of folding onto binaries (bitstring comprehensions):
transform_bytes( DataBin, CipherFun, CipherInitialState ) ->
	transform_bytes( DataBin, CipherFun, CipherInitialState, _AccBin = <<>> ).


transform_bytes( <<>>, _CipherFun, CipherState, AccBin ) ->
	{ AccBin, CipherState };

transform_bytes( _A = << InputByte:8, T/binary >>, CipherFun,
				 CipherState, AccBin ) ->

	{ OutputByte, NewCipherState } = CipherFun( InputByte, CipherState ),

	transform_bytes( T, CipherFun, NewCipherState,
					 << AccBin/binary, OutputByte >> ).





% We must though create a new file, as the semantics is to create an additional
% file in all cases.
%
id_cipher( SourceFilePath, CipheredFilePath ) ->
	file_utils:copy_file( SourceFilePath, CipheredFilePath ).



compress_cipher( SourceFilePath, CipheredFilePath, CompressFormat ) ->

	CompressedFilePath = file_utils:compress( SourceFilePath, CompressFormat ),

	% Preserves the caller-naming convention:
	file_utils:rename( CompressedFilePath, CipheredFilePath ).


decompress_cipher( CipheredFilePath, TargetFilePath, CompressFormat ) ->

	% The decompressing function will check for the relevant extension:

	% We must avoid, to decompress X, to rename it to X.bzip2 and then to
	% decompress it, as this would produce a new decompressed file named X,
	% overwriting the initial one.

	%NewCipheredFilePath = CipheredFilePath
	NewCipheredFilePath = generate_filename()
		++ file_utils:get_extension_for( CompressFormat ),

	file_utils:rename( CipheredFilePath, NewCipheredFilePath ),

	DecompressedFilePath = file_utils:decompress( NewCipheredFilePath,
												  CompressFormat ),

	file_utils:rename( NewCipheredFilePath, CipheredFilePath ),

	% Preserves the caller-naming convention:
	file_utils:rename( DecompressedFilePath, TargetFilePath ).



insert_random_cipher( SourceFilePath, CipheredFilePath, Range )
  when Range > 1 ->

	SourceFile = file_utils:open( SourceFilePath, ?bin_read_opts ),

	TargetFile = file_utils:open( CipheredFilePath, ?bin_write_opts ),

	_InsertedCount = insert_helper( SourceFile, TargetFile, Range, _Count=0 ).

	%trace_utils:debug_fmt( "insert_random_cipher: inserted ~B bytes.",
	%						[ InsertedCount ] ).


% We insert at random places random values in the content:
insert_helper( SourceFile, TargetFile, Range, Count ) ->

	NextInsertionOffset = random_utils:get_random_value( Range ),

	case file_utils:read( SourceFile, NextInsertionOffset ) of

		eof ->
			file_utils:close( SourceFile ),
			file_utils:close( TargetFile ),
			Count;

		{ ok, DataBin } when size( DataBin ) =:= NextInsertionOffset ->

			RandomByte = random_utils:get_random_value( 255 ),

			NewDataBin = << DataBin/binary, RandomByte:8 >>,

			file_utils:write( TargetFile, NewDataBin ),

			insert_helper( SourceFile, TargetFile, Range, Count + 1 );

		{ ok, PartialDataBin } ->

			% Drawn offset not reachable, just finished then:
			file_utils:write( TargetFile, PartialDataBin ),
			file_utils:close( SourceFile ),
			file_utils:close( TargetFile ),
			Count

	end.



extract_random_cipher( CipheredFilePath, TargetFilePath, Range )
  when Range > 1 ->

	CipheredFile = file_utils:open( CipheredFilePath, ?bin_read_opts ),

	TargetFile = file_utils:open( TargetFilePath, ?bin_write_opts ),

	_ExtractedCount =
		extract_helper( CipheredFile, TargetFile, Range, _Count=0 ).

	%trace_utils:debug_fmt( "extract_random_cipher: extracted ~B bytes.~n",
	%		   [ ExtractedCount ] ).



% We extract at random places the bytes found in the content:
extract_helper( CipheredFile, TargetFile, Range, Count ) ->

	NextExtractionOffset = random_utils:get_random_value( Range ),

	case file_utils:read( CipheredFile, NextExtractionOffset ) of

		eof ->
			file_utils:close( CipheredFile ),
			file_utils:close( TargetFile ),
			Count;

		{ ok, DataBin } when size( DataBin ) =:= NextExtractionOffset ->

			% We drop on the floor the previously inserted byte:
			case file_utils:read( CipheredFile, 1 ) of

				eof ->
					file_utils:close( CipheredFile ),
					file_utils:close( TargetFile ),
					Count;

				{ ok, <<_ExtractedByte:8>> } ->

					% Dummy operation, needed to reproduce the insertion random
					% state:
					_RandomByte = random_utils:get_random_value( 255 ),

					file_utils:write( TargetFile, DataBin ),

					extract_helper( CipheredFile, TargetFile, Range, Count + 1 )

			end;

		{ ok, PartialDataBin } ->
			% Finished:
			file_utils:write( TargetFile, PartialDataBin ),
			file_utils:close( CipheredFile ),
			file_utils:close( TargetFile ),
			Count

	end.


% Direction is either 'direct' or 'reciprocal':
shuffle_cipher( SourceFilePath, CipheredFilePath, Length, Direction ) ->

	% Wanting to read lists, not binaries:
	SourceFile = file_utils:open( SourceFilePath, ?list_read_opts ),

	TargetFile = file_utils:open( CipheredFilePath, ?list_write_opts ),

	shuffle_helper( SourceFile, TargetFile, Length, Direction ).



shuffle_helper( SourceFile, TargetFile, Length, Direction ) ->

	case file_utils:read( SourceFile, Length ) of

		eof ->
			file_utils:close( SourceFile ),
			file_utils:close( TargetFile );

		% When will hit the end of file, may perform shuffle on a smaller chunk:
		{ ok, ByteList } ->

			ShuffledByteList = case Direction of

				direct ->
					list_utils:random_permute( ByteList );

				reciprocal ->
					list_utils:random_permute_reciprocal( ByteList )

			end,

			file_utils:write( TargetFile, ShuffledByteList ),

			shuffle_helper( SourceFile, TargetFile, Length, Direction )

	end.



xor_cipher( SourceFilePath, CipheredFilePath, XORList ) ->

	% Wanting to read lists, not binaries:
	SourceFile = file_utils:open( SourceFilePath, ?list_read_opts ),

	TargetFile = file_utils:open( CipheredFilePath, ?list_write_opts ),

	XORRing = ring_utils:from_list( XORList ),

	xor_helper( SourceFile, TargetFile, XORRing ).



xor_helper( SourceFile, TargetFile, XORRing ) ->

	case file_utils:read( SourceFile, 1 ) of

		eof ->
			file_utils:close( SourceFile ),
			file_utils:close( TargetFile );

		{ ok, [ Byte ] } ->

			{ H, NewXORRing } = ring_utils:head( XORRing ),

			NewByte = Byte bxor H,

			% In a list, as an integer is not licit, io_list() needed:
			file_utils:write( TargetFile, [ NewByte ] ),

			xor_helper( SourceFile, TargetFile, NewXORRing )

	end.



mealy_cipher( SourceFilePath, CipheredFilePath, InitialMealyState,
			  MealyTable ) ->

	% Wanting to read lists, not binaries:
	SourceFile = file_utils:open( SourceFilePath, ?list_read_opts ),

	TargetFile = file_utils:open( CipheredFilePath, ?list_write_opts ),

	mealy_helper( SourceFile, TargetFile, InitialMealyState, MealyTable ).


mealy_helper( SourceFile, TargetFile, CurrentMealyState, MealyTable ) ->

	case file_utils:read( SourceFile, 1 ) of

		eof ->
			file_utils:close( SourceFile ),
			file_utils:close( TargetFile );

		{ ok, [ InputByte ] } ->

			{ NextMealyState, OutputByte } =
				apply_mealy( InputByte, CurrentMealyState, MealyTable ),

			file_utils:write( TargetFile, [ OutputByte ] ),

			mealy_helper( SourceFile, TargetFile, NextMealyState, MealyTable )

	end.



% Applies the Mealy machine to new input, while in specified state.
%
% Returns {NextMealyState, OutputByte}.
%
apply_mealy( InputByte, CurrentMealyState, MealyTable ) ->

	% Zero-indexed:
	InnerArray = array:get( CurrentMealyState - 1, MealyTable ),

	% Returns the cell:
	array:get( InputByte, InnerArray ).




generate_filename() ->

	FilePath = ".cipher-" ++ id_utils:generate_uuid(),

	case file_utils:is_existing_file( FilePath ) of

		% Rather unlikely:
		true ->
			generate_filename();

		false ->
			FilePath

	end.



% Mealy section.


% We can represent a Mealy table that way:


% Alphabet \ States:
%
%         S1             S2 ...  Sstate_count
% 0       { S44, 135 }
% 1       { S2,   11 }
% .       .
% .       .
% .       .
% 255

% Each inner array corresponds to a column, in charge of the behaviour of the
% machine when it is in the corresponding state Sn.



% Generates a Mealy table for the specified number of states.
%
% Relies on the current random state.
%
generate_mealy_table( StateCount ) ->
	% Default alphabet is all byte values:
	generate_mealy_table( StateCount, _AlphabetSize=256 ).


% Generates a Mealy table for the specified number of states and alphabet size.
%
% We manage index to designate symbols of the alphabet; for example, if the
% alphabet is [ alpha, beta, gamma ], then the alpha symbol is coded by 1, the
% beta one by 2, etc.
%
% Relies on the current random state.
%
generate_mealy_table( StateCount, AlphabetSize ) ->

	io:format( "Generating a Mealy table for ~B states and "
			   "an alphabet of ~B symbols.~n", [ StateCount, AlphabetSize ] ),

	Table = array:new( StateCount ),

	% Prebuilt for easier permutations in [0;255]:
	Alphabet = lists:seq( 0, AlphabetSize - 1 ),

	% Arrays are zero-indexed:
	fill_table( Table, _Index=0, _FinalIndex=StateCount, Alphabet,
				AlphabetSize ).


% Adds the inner arrays:
fill_table( Table, _Index=FinalIndex, FinalIndex, _Alphabet, _AlphabetSize ) ->
	Table;

fill_table( Table, Index, FinalIndex, Alphabet, AlphabetSize ) ->

	% FinalIndex is StateCount:
	InnerArray = create_inner_array( Alphabet, AlphabetSize, FinalIndex ),

	NewTable = array:set( Index, InnerArray, Table ),

	fill_table( NewTable, Index + 1, FinalIndex, Alphabet, AlphabetSize ).



% Will have as many elements as there are input letters:
create_inner_array( Alphabet, AlphabetSize, StateCount ) ->

	Array = array:new( AlphabetSize ),

	% To fill this inner array (corresponding to a given state), we must, for
	% each of the possible input letter, specify the corresponding cell.
	%
	% The corresponding pair is made of a new state (uniformly chosen at random
	% among the possible states - some states can appear multiple times, other
	% none) and an output letter; in each inner array, these output letters must
	% form an exact permutation of the alphabet, for reversibility purpose.

	% Updates also the random state:
	Letters = list_utils:random_permute( Alphabet ),

	% No, a fold would not be clearer:
	fill_inner_array( Array, _Index=0, _FinalIndex=AlphabetSize, Letters,
					  StateCount ).


% We iterate through the permuted letters:
fill_inner_array( Array, _Index=FinalIndex, FinalIndex, _Letters=[],
				  _StateCount ) ->
	Array;

fill_inner_array( Array, Index, FinalIndex, _Letters=[ L | T ], StateCount ) ->

	% Returns a value in [1,StateCount]:
	NextState = random_utils:get_random_value( StateCount ),

	Cell = { NextState, L },

	NewArray = array:set( Index, Cell, Array ),

	fill_inner_array( NewArray, Index + 1, FinalIndex, T, StateCount ).





% Returns the inverse Mealy table of the specified one.
-spec compute_inverse_mealy_table( mealy_table() ) -> mealy_table().
compute_inverse_mealy_table( Table ) ->

	StateCount = array:size( Table ),

	% At least one state defined:
	AlphabetSize = array:size( array:get( 0, Table ) ),

	% To inverse a Mealy table: when we read the first encrypted byte while in
	% initial state S, we look up in the original table to which input byte it
	% corresponded for state S, and write that byte. The state in that cell is
	% the next state. Then we iterate.

	% So the inverse table can be computed simply by finding, for each read
	% byte, what is the input byte which corresponded.

	InverseTable = array:new( StateCount ),

	% Iterate first on inner arrays:

	InnerArrays = array:to_list( Table ),

	fill_reverse_table( InverseTable, InnerArrays, _Index=0,
						_FinalIndex=StateCount, AlphabetSize ).


fill_reverse_table( InverseTable, _InnerArrays=[], _Index=FinalIndex,
						FinalIndex, _AlphabetSize ) ->
	InverseTable;

fill_reverse_table( InverseTable, _InnerArrays=[ A | T ], Index, FinalIndex,
					AlphabetSize ) ->

	ReversedInnerArray = inverse_inner_array( A, AlphabetSize ),

	NewInverseTable = array:set( Index, ReversedInnerArray, InverseTable ),

	fill_reverse_table( NewInverseTable, T, Index + 1, FinalIndex,
						AlphabetSize ).



inverse_inner_array( InnerArray, AlphabetSize ) ->

	Cells = array:to_list( InnerArray ),

	NewInnerArray = array:new( AlphabetSize ),

	inverse_cells( Cells, _Index=0, NewInnerArray ).


inverse_cells( _Cells=[], _Index, AccArray ) ->
	AccArray;

% We will branch to the same next state, but we output what the direct machine
% must have read for that output letter:
%
inverse_cells( _Cells=[ { NextState, OutputLetter } | T ], Index, AccArray ) ->

	NewAccArray = array:set( OutputLetter, { NextState, Index }, AccArray ),

	inverse_cells( T, Index + 1, NewAccArray ).



% Returns a textual representation of this mealy table.
-spec mealy_table_to_string( mealy_table() ) -> ustring().
mealy_table_to_string( Table ) ->

	StateCount = array:size( Table ),

	StateStrings = get_inner_info( Table, _Index=0, _FinalIndex=StateCount,
								   _Acc=[] ),

	AlphabetSize = array:size( array:get( 0, Table ) ),

	text_utils:format( "Mealy table with ~B states and an alphabet of "
		"~B letters: ~ts", [ StateCount, AlphabetSize,
							 text_utils:strings_to_string( StateStrings ) ] ).


get_inner_info( _Table, _Index=FinalIndex, FinalIndex, Acc ) ->
	lists:reverse( Acc );

get_inner_info( Table, Index, FinalIndex, Acc ) ->

	% List of cells:
	InnerList = array:to_list( array:get( Index, Table ) ),

	S = text_utils:format( "for state S~B:~n~ts",
						   [ Index + 1, get_cells_info( InnerList ) ] ),

	get_inner_info( Table, Index + 1, FinalIndex, [ S | Acc ] ).

get_cells_info( InnerList ) ->
	% To avoid many ineffective concatenations:
	get_cells_info( lists:reverse( InnerList ), _StringAcc=[] ).


get_cells_info( _InnerList=[], StringAcc ) ->
	StringAcc;

get_cells_info( _InnerList=[ _C={ NewState, OutputByte } | T ], StringAcc ) ->

	NewAcc = text_utils:format( "{~p,~B} ", [ NewState, OutputByte ] )
		++ StringAcc,

	get_cells_info( T, NewAcc ).
