% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Wednesday, August 24, 2022.


% @doc Unit tests for the <b>hashing-related services</b>.
%
% See the hash_utils.erl tested module.
%
-module(hash_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% Shorthands:

-type hash_algorithm() :: hash_utils:hash_algorithm().
-type binary_hash() :: hash_utils:binary_hash().


% @doc Tests the hashing of files.
-spec test_file_hashing( hash_algorithm(), binary(), binary_hash() ) -> void().
test_file_hashing( HashAlg, ContentToHash, ContentHashBin ) ->

	FileToHash = "hash_utils_test-file-to-hash.txt",

	test_facilities:display( "Testing the hashing of file '~ts', "
		"of content '~ts', with algorithm '~ts'.",
		[ FileToHash, ContentToHash, HashAlg ] ),

	% Any already-existing file at that path will be silently overwritten:
	file_utils:write_whole( FileToHash, ContentToHash ),

	% Match:
	ContentHashBin = hash_utils:get_file_hash( FileToHash, HashAlg ),

	test_facilities:display( "Correct hash (~w) found for file.",
							 [ ContentHashBin ] ),

	file_utils:remove_file( FileToHash ).



% @doc Tests the hashing of terms.
-spec test_term_hashing( hash_algorithm(), binary(), binary_hash() ) -> void().
test_term_hashing( HashAlg, ContentToHash, ContentHashBin ) ->

	test_facilities:display( "Testing the hashing of term '~ts', "
		"with algorithm '~ts'.", [ ContentToHash, HashAlg ] ),

	97427884 = hash_utils:get_insecure_hash( ContentToHash ),

	hash_utils:start_crypto_hashing(),

	% Match:
	ContentHashBin = hash_utils:get_hash( ContentToHash, HashAlg ),

	test_facilities:display( "Correct hash (~w) found for term.",
							 [ ContentHashBin ] ),

	hash_utils:stop_crypto_hashing().




-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	HashAlg = sha2_224,

	ContentToHash = <<"I am a content that will be hashed for testing.">>,

	% For example obtained from: 'printf "I am a content that will be hashed for
	% testing." | sha224sum' as a third tool after the crypto module and
	% OpenSSL; from hexadecimal to binary:
	%
	ContentHashStr = "c2518b23da6f71f744e25477fd05dcf3a9e6b63cc77b21199867ada9",
	ContentHashBin = text_utils:hexastring_to_binary( ContentHashStr ),

	ByteSize = size( ContentHashBin ),

	test_facilities:display( "Testing the hashing service, based on "
		"algorithm '~ts' (~B-bit hashes, i.e. ~B bytes), by hashing the "
		"following content: '~ts'. The corresponding, expected (hexadecimal) "
		"hash is then '~ts' (as a binary: ~w).",
		[ HashAlg, ByteSize * 8, ByteSize, ContentToHash,
		  ContentHashStr, ContentHashBin ] ),


	test_file_hashing( HashAlg, ContentToHash, ContentHashBin ),

	test_term_hashing( HashAlg, ContentToHash, ContentHashBin ),

	test_facilities:stop().
