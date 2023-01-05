% Copyright (C) 2021-2023 Olivier Boudeville
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
% Creation date: Wednesday, December 15, 2021.


% Unit tests for the <b>audio toolbox</b>.
%
% See the audio_utils.erl tested module.
%
-module(audio_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% The actual test:
run_audio_test() ->

	test_facilities:display( "Testing the audio services." ),

	RelPath = [ "..", "..", "..", "doc" ],

	FrenchSpeechPath =
		file_utils:join( RelPath ++ [ "speech-test-fr-FR.ogg.opus" ] ),

	MaybeAudioStreamSettings = undefined,

	audio_utils:playback_file( FrenchSpeechPath, MaybeAudioStreamSettings,
							   _FirstDoBlock=false ),

	timer:sleep( 2000 ),

	EnglishSpeechPath =
		file_utils:join( RelPath ++ [ "speech-test-en-US.ogg.opus" ] ),

	% The two playbacks shall thus overlap, and this test shall not stop before
	% the end of the second:
	%
	audio_utils:playback_file( EnglishSpeechPath, MaybeAudioStreamSettings,
							   _SecondDoBlock=true ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the audio test, being in batch mode)" );

		false ->
			run_audio_test()

	end,

	test_facilities:stop().
