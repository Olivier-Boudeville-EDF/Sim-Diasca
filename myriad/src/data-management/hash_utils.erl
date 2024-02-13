% Copyright (C) 2022-2024 Olivier Boudeville
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
% Creation date: Tuesday, August 23, 2022.


% @doc Gathering of various <b>hashing-related facilities</b>,
% cryptographically-secure or not, to be applied either to files or directly to
% (Erlang, binary) terms.
%
% See hash_utils_test.erl for testing.
%
-module(hash_utils).


% Current defaults:
-export([ get_recommended_algorithm/0 ]).


% For file hashing:
-export([ get_file_hash/2, get_file_recommended_hash/1 ]).


% For non-cryptographic term hashing:
-export([ get_insecure_hash/1 ]).


% For more cryptographic-related term hashing:
-export([ start_crypto_hashing/0, stop_crypto_hashing/0,
		  get_hash/2, get_recommended_hash/1,
		  list_hash_algorithms/0, check_hash_algorithm/1 ]).



% Implementation notes:
%
% For cryptographic uses:
% - erlang:phash* are insufficient
% - MD5 is obsolete
% - SHA1 is cryptographically broken, but still widely used


% File hashing section.
%
% Could be done also by loading the file contents and hashing the corresponding
% binaries, yet the approach implemented here, executing 'openssl' (possibly
% through the installation of suitable package, like 'libssl'), is expected to
% be more efficient.


% Term hashing section.
%
% We rely here on the 'crypto' module, which will be available if a proper
% libssl support (binaries and header files, typically including
% /usr/include/openssl/ssl.h) has been previously secured (possibly through the
% installation of suitable package, like 'libssl-dev').
%
% Warning: for crypto,the seed and state management are presumably global (not
% per-process), and does not seem controllable from Erlang.
%
% See also our random_utils module, which may still use (as a second choice)
% crypto as well.


% There are two data types of hashes, integer or binary. They can usually be
% two-way converted; for example:
%
% # Not 'echo "abc" > foobar.txt' as a trailing newline would be then added:
% $ printf "abc" > foobar.txt
%
% $ make shell
%  1> hash_utils:get_file_hash("foobar.txt", sha2_384).
% <<203,0,117,63,69,163,94,139,181,160,61,105,154,198,80,7,
 % 39,44,50,171,14,222,209,99,26,139,96,90,67,...>>
% (formerly an integer like 191415658344158766168031473277922803570 was
% returned)
%
%  2> hash_utils:get_hash(<<"abc">>, sha2_384).
% <<203,0,117,63,69,163,94,139,181,160,61,105,154,198,80,7,
%  39,44,50,171,14,222,209,99,26,139,96,90,67,...>>
%
% The best form for a hash is the most compact (easy to store and to compare)
% one, the binary one, i.e. binary_hash().
%
% For clear-text storage, an hexadecimal string (typically obtained from
% text_utils:binary_to_hexastring(MyHashBin, _AddPrefix=false)) is often the
% most appropriate form.
%
% For example, for MD5, the hash, when stored as a binary requires only 16
% bytes, whereas the corresponding (large) integer could be 32 bytes, the
% hexadecimal number as a string would be 520 bytes, and as a binary 32 bytes
% (hence a two-fold increase).


% In terms of (command-line) tools, we relied on the 'sha*sum' ones (including
% 'shasum'), yet now we prefer using 'openssl', which is a lot richer; run
% 'openssl list -digest-algorithms' for a full list, in the form "X => Y"
% (e.g. RSA-SHA512/224 => SHA512-224), where X is the one to be specified with
% OpenSSL; or, more simply, use 'openssl dgst -list'.
%
% Our (non-standard) convention started from the same identifiers, as atoms, in
% lowercase, once '-' and '/' have been replaced with '_' (for example, we
% translated "RSA-SHA512/224" to the 'rsa_sha512_224' atom). Finally we
% hand-crafted them, as we deemed them clearer (otherwise for example it is not
% obvious that 'sha384' belongs to the SHA2 family).
%
% See also https://www.openssl.org/docs/manmaster/man1/openssl-dgst.html


-type hash_algorithm() :: 'md5'

						% Not used: | 'sha0'

						| 'sha1'

						  % SHA2 family:
						| 'sha2_224'
						| 'sha2_256'
						| 'sha2_384'
						| 'sha2_512'
						| 'sha2_512_224'
						| 'sha2_512_256'

						  % SHA3 family:
						| 'sha3_224'
						| 'sha3_256'
						| 'sha3_384'
						| 'sha3_512'.
% Designates (as an atom) a known (cryptographic) hashing algorithm.
%
% Not relying on crypto:hash_algorithm() (not clear/complete enough; moreover
% not exported); mostly translated from 'openssl list -1 -digest-algorithms'.


-type integer_hash() :: non_neg_integer().
% A (positive or null) integer value obtained when hashing a piece of data.


-type portable_integer_hash() :: integer_hash().
% A hash value of a term that will the same regardless of machine architecture
% and ERTS version.


-type binary_hash() :: binary().
% Most hash values are, and should be, internally, binaries.
%
% For example 128-bit (16 bytes) for MD5, 160-bit hash (20 bytes) for SHA1,
% 256-bit hash (32 bytes) for SHA-512/256.
%
% Use text_utils:binary_to_hexastring/2 for clear-text storage thereof.


-type md5_sum() :: binary_hash().
% MD5 sum, a 128-bit hash value (that is 16 bytes).
%
% MD5 is mostly obsolete for cryptographic uses.


-type sha1_sum() :: binary_hash().
% SHA1 sum (Secure Hash Algorithm 1) i, a 160-bit hash value (that is 20 bytes).
%
% SHA1 is a cryptographically broken, but still widely used, hash function.


-type sha2_sum() :: binary_hash().
% SHA-2 (Secure Hash Algorithm 2) is a set of 6 cryptographic hash functions
% whose hash values are 224, 256, 384 or 512 bits: SHA-224, SHA-256, SHA-384,
% SHA-512, SHA-512/224, SHA-512/256 (the last two being computed with eight
% 32-bit and 64-bit words, respectively).


-type sha2_512_sum() :: binary_hash().
% SHA-2 (Secure Hash Algorithm 2) for a cryptographic hash function whose hash
% values are 512 bits (that is 64 bytes).
%
% The corresponding algorithm name is 'sha2_512'.


-type sha3_sum() :: binary_hash().
% SHA-3 (Secure Hash Algorithm 3) is a subset of the broader cryptographic
% primitive family Keccak.
%
% The size of its digests is arbitrary; it is internally different from the
% MD5-like structure of SHA-1 and SHA-2.


-type sha_sum() :: binary_hash().
% SHA sum, a hash value of unspecified size.
%
% See [https://en.wikipedia.org/wiki/SHA-1#Comparison_of_SHA_functions].


-type any_hash() :: integer_hash() | binary_hash().
% Any kind of hash value ("digest").


-type passphrase_hash() :: binary_hash().
% The passphrase (binary) hash value of an account.
%
% Should be salted. May be stored.


-type salt_value() :: binary().
% A salt value used to generate safer hashes of passphrase.
%
% A constant, system-wide salt is pointless to mitigate attacks; it would just
% make passphrases longer.
%
% Instead a random salt should be generated each time a new credential is issued
% (login/passphrase pair); then the passphrase would not be stored, but this
% salt value and the corresponding hash of the salted passphrase would,
% preferably in different locations.


-export_type([ hash_algorithm/0, integer_hash/0, portable_integer_hash/0,
			   binary_hash/0,
			   md5_sum/0, sha1_sum/0, sha2_sum/0, sha2_512_sum/0, sha3_sum/0,
			   sha_sum/0,
			   any_hash/0,
			   passphrase_hash/0, salt_value/0 ]).



% Shorthands:

-type ustring() :: text_utils:ustring().

-type file_path() :: file_utils:file_path().
-type any_file_path() :: file_utils: any_file_path().




% @doc Returns the cryptographic algorithm that we currently recommend for
% hashing.
%
% As a result, that algorithm is a moving target, it will be updated over time.
%
-spec get_recommended_algorithm() -> hash_algorithm().
get_recommended_algorithm() ->
	% Currently:
	sha2_512.


% @doc Returns the hash sum corresponding to the content of the specified file,
% based on the specified cryptographic algorithm for hashing.
%
-spec get_file_hash( file_path(), hash_algorithm() ) -> binary_hash().
get_file_hash( FilePath, Alg ) ->

	%trace_utils:info_fmt( "Computing the '~ts' hash of file '~ts'.",
	%                      [ Alg, FilePath ] ),

	% 'utf8' expected as terminal default:
	%trace_utils:debug_fmt( "FilePath encoding mode: ~ts",
	%                       [ file:native_name_encoding() ] ),

	file_utils:is_existing_file( FilePath ) orelse
		throw( { file_to_hash_not_found, FilePath } ),

	% Using OpenSSL for files; already a full, resolved executable path:
	OpenSSLExecPath = executable_utils:get_default_openssl_executable_path(),

	OpenSSLAlg = get_openssl_algorithm( Alg ),

	% (not using '++' anymore, as (raw) filenames might have to be binaries;
	% using bin_format/2 to follow Unicode hint in open_port/2:
	%
	% ('hex' rather than 'binary'; '-r' to print the digest in the most
	% convenient coreutils format)
	%
	Args = [ "dgst", "-hex", "-r", [ $- | OpenSSLAlg ],
			 % By design no need for quoting here:
			 %shell_utils:protect_from_shell( FilePath )
			 FilePath ],

	%trace_utils:debug_fmt( "OpenSSL executable is: '~ts', arguments are ~p.",
	%                       [ OpenSSLExecPath, Args ] ),

	case system_utils:run_executable( OpenSSLExecPath, Args ) of

		{ _ExitCode=0, OutputString } ->

			% Removes the filename just after the digest:
			{ HashStr, _Rest } =
				text_utils:split_at_first( $ , OutputString ),


			% Formerly an integer hash was returned; now returning a binary
			% (better):
			%
			text_utils:hexastring_to_binary( HashStr );


		{ ExitCode, ErrorOutput } ->
			trace_utils:error_fmt( "Hash computation failed for '~ts' and "
				"algorithm ~ts: ~ts.", [ FilePath, Alg, ErrorOutput ] ),
			throw( { hash_computation_failed, ExitCode, ErrorOutput,
					 FilePath, Alg } )

	end.



% @doc Returns the hash for the specified file, computed from the cryptographic
% algorithm that we currently recommend.
%
% As a result, that algorithm is a moving target, it will be updated over time.
%
-spec get_file_recommended_hash( any_file_path() ) -> binary_hash().
get_file_recommended_hash( FilePath ) ->
	get_file_hash( FilePath, get_recommended_algorithm() ).




% Term hashing section.


% For non-cryptographic term hashing:

% @doc Returns the hash value corresponding to the specified term, using an
% hashing algorithm efficient, fair, yet not suitable for cryptographic uses.
%
-spec get_insecure_hash( term() ) -> portable_integer_hash().
get_insecure_hash( Term ) ->
	% Mostly to protect from any next change/version to happen:
	%
	% (range is 2^27 here)
	%
	erlang:phash2( Term ).




% For cryptographic term hashing:
%
% (strangely enough, starting crypto does not seem mandatory)



% @doc Ensures that a support for cryptographic hashing is available and ready
% to use.
%
-spec start_crypto_hashing() -> void().
start_crypto_hashing() ->
	case crypto:start() of

		ok ->
			ok;

		{ error, Reason } ->
			throw( { cannot_start_crypto_hashing, Reason } )

	end.



% @doc Stops the support for cryptographic hashing.
%
% Nevert fails.
%
-spec stop_crypto_hashing() -> void().
stop_crypto_hashing() ->
	case crypto:stop() of

		ok ->
			ok;

		{ error, Reason } ->
			trace_bridge:error_fmt( "The stopping of the crypto hashing "
									"service failed: ~p,", [ Reason ] )

	end.



% @doc Returns the hash sum corresponding to the content of the specified term,
% based on the specified cryptographic algorithm for hashing.
%
% Requires the support for cryptographic hashing to be started.
%
% Raises a 3-tuples exception on error.
%
-spec get_hash( term(), hash_algorithm() ) -> binary_hash().
get_hash( Term, Alg ) ->

	%trace_utils:info_fmt( "Computing the '~ts' hash of term '~p'.",
	%                      [ Alg, Term ] ),

	CryptoAlg = get_crypto_algorithm( Alg ),

	crypto:hash( _Type=CryptoAlg, _Data=Term ).



% @doc Returns the hash for the specified file, computed from the cryptographic
% algorithm that we currently recommend.
%
% As a result, that algorithm is a moving target, it will be updated over time.
%
-spec get_recommended_hash( term() ) -> binary_hash().
get_recommended_hash( Term ) ->
	get_hash( Term, get_recommended_algorithm() ).



% @doc Returns a list of the supported hashing algorithms.
-spec list_hash_algorithms() -> [ hash_algorithm() ].
list_hash_algorithms() ->
	% See the hash_algorithm() type:
	[ md5, sha1,
	  sha2_224, sha2_256, sha2_384, sha2_512, sha2_512_224, sha2_512_256,
	  sha3_224, sha3_256, sha3_384, sha3_512 ].


% @doc Checks whether the specified atom corresponds to a supported hashing
% algorithm; if yes, returns it, otherwise raises an exception.
%
-spec check_hash_algorithm( atom() ) -> hash_algorithm().
check_hash_algorithm( Alg ) ->
	case lists:member( Alg, list_hash_algorithms() ) of

		true ->
			Alg;

		false ->
			throw( { unsupported_hash_algorithm, Alg } )

	end.


% Helpers.


% @doc Converts an hashing algorithm into one known of OpenSSL (typically used
% for the hashing of files).
%
% (formerly relying on 'shashum', see 'man shasum')
%
-spec get_openssl_algorithm( hash_algorithm() ) -> ustring().
get_openssl_algorithm( _Alg=md5 ) -> "md5";
get_openssl_algorithm( _Alg=sha1 ) -> "sha1";

get_openssl_algorithm( _Alg=sha2_224 ) -> "sha224";
get_openssl_algorithm( _Alg=sha2_256 ) -> "sha256";
get_openssl_algorithm( _Alg=sha2_384 ) -> "sha384";
get_openssl_algorithm( _Alg=sha2_512 ) -> "sha512";
get_openssl_algorithm( _Alg=sha2_512_224 ) -> "sha512-224";
get_openssl_algorithm( _Alg=sha2_512_256 ) -> "sha512-256";

get_openssl_algorithm( _Alg=sha3_224 ) -> "sha3-224";
get_openssl_algorithm( _Alg=sha3_256 ) -> "sha3-256";
get_openssl_algorithm( _Alg=sha3_384 ) -> "sha3-384";
get_openssl_algorithm( _Alg=sha3_512 ) -> "sha3-512";
get_openssl_algorithm( Alg ) ->
	throw( { unsupported_hashing_algorithm_for_openssl, Alg } ).



% @doc Converts an hashing algorithm into one known of the 'crypto' Erlang
% module (typically used for the hashing of terms).
%
-spec get_crypto_algorithm( hash_algorithm() ) ->
	% Not exported yet: crypto:hash_algorithm().
	hash_algorithm().
get_crypto_algorithm( _Alg=md5 ) -> md5;
get_crypto_algorithm( _Alg=sha1 ) -> sha;

get_crypto_algorithm( _Alg=sha2_224 ) -> sha224;
get_crypto_algorithm( _Alg=sha2_256 ) -> sha256;
get_crypto_algorithm( _Alg=sha2_384 ) -> sha384;
get_crypto_algorithm( _Alg=sha2_512 ) -> sha512;
%get_crypto_algorithm( _Alg=sha2_512_224 ) -> ;
%get_crypto_algorithm( _Alg=sha2_512_256 ) -> ;

get_crypto_algorithm( _Alg=sha3_224 ) -> sha3_224;
get_crypto_algorithm( _Alg=sha3_256 ) -> sha3_256;
get_crypto_algorithm( _Alg=sha3_384 ) -> sha3_384;
get_crypto_algorithm( _Alg=sha3_512 ) -> sha3_512;
get_crypto_algorithm( Alg ) ->
	throw( { unsupported_hashing_algorithm_for_crypto, Alg } ).
