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
% Creation date: Tuesday, November 30, 2021.


% This is a test of Myriad's <b>speech support</b>, more precisely the
% generation of Text-to-Speech (TTS) audio content.
%
% See the speech_support.erl tested module.
%
-module(speech_support_test).


% Usage notes:
%
% For best quality, we rely here on a third-party neural-based only TTS provider
% (Microsoft Azure, at least currently).
%
% For this test to actually perform TTS, it must be run interactively (not as
% batch, as we do not want to make the user spend cloud credits each time a test
% suite is run), and a suitable Azure account must be found in the user Ceylan
% preferences (see ~/.ceylan-settings.etf).


% For run/0 export and al:
-include("test_facilities.hrl").

% For the voice_info record:
-include("speech_support.hrl").


% Silencing to allow selective test activation:
-export([ test_list_voices/1, test_record_speeches/1 ]).


% Shorthands:

-type bin_locale() :: locale_utils:bin_locale().

-type speech_state() :: speech_support:speech_state().

-type speech_settings() :: speech_support:speech_settings().



% @doc The actual speech test.
-spec run_speech_test() -> void().
run_speech_test() ->

	case speech_support:check_availability() of

		{ true, CfgSpeechState } ->
			test_facilities:display( "Speech support found available: ~ts.",
				[ speech_support:speech_state_to_string( CfgSpeechState ) ] ),

			SpeechState = speech_support:start( CfgSpeechState ),

			run_test_tts( SpeechState ),

			speech_support:stop( SpeechState );

		{ false, ReasonStr } ->
			test_facilities:display( "Speech support not found available: ~ts; "
									 "no test performed.", [ ReasonStr ] )

	end.


% @doc Returns the French locale that shall be used in this test.
-spec get_french_locale() -> bin_locale().
get_french_locale() ->
	<<"fr-FR">>.


% @doc Returns the English locale that shall be used in this test.
-spec get_english_locale() -> bin_locale().
get_english_locale() ->
	% Could be also <<"en-GB">>:
	<<"en-US">>.



% @doc Testing the listing of voices.
-spec test_list_voices( speech_state() ) -> void().
test_list_voices( SpeechState ) ->

	test_facilities:display( "Detecting the available voices." ),

	VoiceTable = speech_support:list_voices( SpeechState ),

	test_facilities:display(
		speech_support:voice_table_to_string( VoiceTable ) ),

	FemaleVoiceTable = speech_support:filter_by_gender( female, VoiceTable ),

	FrenchSpokenLocale = get_french_locale(),

	FrenchVoiceTable =
		speech_support:filter_by_locale( FrenchSpokenLocale, VoiceTable ),

	EnglishSpokenLocale = get_english_locale(),

	EnglishVoiceTable =
		speech_support:filter_by_locale( EnglishSpokenLocale, VoiceTable ),

	test_facilities:display( "On the ~B voices, ~B are female, ~B have "
		"~ts as spoken locale (as primary or secondary), ~B for ~ts.",
		[ table:size( VoiceTable ), table:size( FemaleVoiceTable ),
		  table:size( FrenchVoiceTable ), FrenchSpokenLocale,
		  table:size( EnglishVoiceTable ), EnglishSpokenLocale ] ).



% @doc Returns the French speech settings to apply for testing.
-spec get_french_test_speech_settings() -> speech_settings().
get_french_test_speech_settings() ->

	% FrenchVoiceInfoId = pair:second(
	%                hd( table:enumerate( FrenchVoiceTable ) ) ),
	FrenchVoiceInfoId = { azure, "fr-FR-DeniseNeural" },

	% Implicit for this voice:
	%  - FrenchLangLocale = <<"fr-FR">>,
	%  - FrenchVoiceGender = female
	%  - FrenchSpeechStyle = undefined
	%  - FrenchSpeechRole = undefined
	%
	#speech_settings{ voice_id=FrenchVoiceInfoId,
					  language_locale=get_french_locale() }.



% @doc Returns the English speech settings to apply for testing.
-spec get_english_test_speech_settings() -> speech_settings().
get_english_test_speech_settings() ->
	% EnglishVoiceInfoId = pair:second(
	%                hd( table:enumerate( EnglishVoiceTable ) ) ),
	% EnglishVoiceInfoId = { azure, "en-GB-RyanNeural" },
	EnglishVoiceInfoId = { azure, "en-US-JennyNeural" },

	% For the test of styles (roleplays not tested here, applies only to Chinese
	% voices):
	%
	#speech_settings{ voice_id=EnglishVoiceInfoId,
					  language_locale=get_english_locale(),
					  % Will not be honored (as female):
					  voice_gender=male,
					  speech_style=customer_support }.


% @doc Testing the recording of a French speech.
test_record_french_speech( LogicalSpeechBaseName, MaybeOutputDir,
		SpeechState=#speech_state { audio_settings=AudioSettings }  ) ->

	FrenchSpeechSettings = get_french_test_speech_settings(),

	% Hence an XML document, here created in the "simple-form" (see the
	% "Defining one's XML document" section in xml_utils.erl for more details):
	%
	FrenchSSMLText = [ "Ces paroles ont été générées via la synthèse vocale "
		"mise en place par ",
		{ prosody, [ { volume, "+20.00%" } ], [ "Ceylan Myriad" ] },
		". N'est-ce point merveilleux ?" ],

	test_facilities:display( "Recording French SSML speech:~n ~p~n as a ~ts.",
		[ FrenchSSMLText,
		  audio_utils:audio_stream_settings_to_string( AudioSettings ) ] ),


	FrenchFilePath = speech_support:record_speech( FrenchSSMLText,
		LogicalSpeechBaseName, FrenchSpeechSettings, MaybeOutputDir,
		SpeechState ),

	test_facilities:display( "French speech recorded as '~ts'.",
							 [ FrenchFilePath ] ).



% @doc Testing the recording of an English speech.
test_record_english_speech( LogicalSpeechBaseName, MaybeOutputDir,
		SpeechState=#speech_state { audio_settings=AudioSettings }  ) ->

	EnglishSpeechSettings = get_english_test_speech_settings(),

	EnglishSSMLText = [ "This speech has been generated thanks the ",
		{ prosody, [ { volume, "+20.00%" } ], [ "Ceylan Myriad" ] },
		" support for speech synthesis. Wonderful, isn't it?" ],

	test_facilities:display( "Recording English SSML speech:~n ~p~n as a ~ts.",
		[ EnglishSSMLText,
		  audio_utils:audio_stream_settings_to_string( AudioSettings ) ] ),

	EnglishFilePath = speech_support:record_speech( EnglishSSMLText,
		LogicalSpeechBaseName, EnglishSpeechSettings, MaybeOutputDir,
		SpeechState ),

	test_facilities:display( "English speech recorded as '~ts'.",
							 [ EnglishFilePath ] ).


% @doc Testing the recording of speeches.
-spec test_record_speeches( speech_state() ) -> void().
test_record_speeches( SpeechState ) ->

	LogicalSpeechBaseName = "speech-test",

	test_facilities:display( "Recording actual speeches corresponding to "
		"the '~ts' logical one (warning: may spend cloud credits).",
		[ LogicalSpeechBaseName ] ),

	% Hence writes done in the current directory:
	MaybeOutputDir = undefined,

	test_record_french_speech( LogicalSpeechBaseName, MaybeOutputDir,
							   SpeechState ),

	test_record_english_speech( LogicalSpeechBaseName, MaybeOutputDir,
								SpeechState ).




% @doc Tests the management of speech referentials and logical speeches.
-spec test_referential_management( speech_state() ) -> void().
test_referential_management( SpeechState=#speech_state{
											audio_settings=AudioSettings } ) ->

	test_facilities:display( "Testing the management of speech referentials." ),

	% Using the referential in the speech state to define two logical speeches,
	% each defined for the following two speech settings, which we register
	% first:

	FirstSpeechSettings = get_french_test_speech_settings(),

	{ FirstSpeechSettingsId, WithFrenchSpeechState } =
		speech_support:register_speech_settings( FirstSpeechSettings,
												 SpeechState ),


	SecondSpeechSettings = get_english_test_speech_settings(),

	{ SecondSpeechSettingsId, WithEnglishSpeechState } =
		speech_support:register_speech_settings( SecondSpeechSettings,
												 WithFrenchSpeechState ),

	% Then records on this basis the two test logical speeches:

	FirstLogSpeechBaseName = "hello",

	test_facilities:display( "Recording first a '~ts' logical speech, "
		"for two locales / speech settings.", [ FirstLogSpeechBaseName ] ),

	% SSML texts are XML documents, hence lists of elements:
	FirstActualSpeechInfos = [

		{ FirstSpeechSettingsId,
			[ "Bonjour, étranger. Bienvenue dans ma modeste auberge." ] },

		{ SecondSpeechSettingsId,
			[ "Greetings, stranger. Welcome to my humble country inn." ] } ],

	{ FirstSpeechId, HelloSpeechState } = speech_support:record_logical_speech(
		FirstLogSpeechBaseName, FirstActualSpeechInfos,
		WithEnglishSpeechState ),


	SecondLogSpeechBaseName = "goodbye",

	test_facilities:display( "Then recording a second '~ts' logical speech, "
		"for these two locales / speech settings.",
		[ SecondLogSpeechBaseName ] ),

	SecondActualSpeechInfos = [

		{ FirstSpeechSettingsId,
			[ "Au revoir, étranger. Que ton périple soit aisé." ] },

		{ SecondSpeechSettingsId,
			[ "Farewell, stranger. May your journey be a piece of cake." ] } ],

	{ SecondSpeechId, GoodbyeSpeechState } =
		speech_support:record_logical_speech( SecondLogSpeechBaseName,
			SecondActualSpeechInfos, HelloSpeechState ),


	% Playback now:

	FrenchLocale = get_french_locale(),
	EnglishLocale = get_english_locale(),

	test_facilities:display( "Now playing back in turn these two logical "
		"speeches (of identifiers ~B and ~B) for their two supported "
		"locales: ~ts and ~ts.",
		[ FirstSpeechId, SecondSpeechId, FrenchLocale, EnglishLocale ] ),

	% Not wanting all audio to be played simultaneously:
	DoBlock = true,

	HelloFrenchAudioPath = speech_support:get_audio_path_for( FirstSpeechId,
		get_french_locale(), GoodbyeSpeechState ),

	audio_utils:playback_file( HelloFrenchAudioPath, AudioSettings, DoBlock ),


	HelloEnglishAudioPath = speech_support:get_audio_path_for( FirstSpeechId,
		get_english_locale(), GoodbyeSpeechState ),

	audio_utils:playback_file( HelloEnglishAudioPath, AudioSettings, DoBlock ),


	GoodbyeFrenchAudioPath = speech_support:get_audio_path_for( SecondSpeechId,
		get_french_locale(), GoodbyeSpeechState ),

	audio_utils:playback_file( GoodbyeFrenchAudioPath, AudioSettings, DoBlock ),


	GoodbyeEnglishAudioPath = speech_support:get_audio_path_for( SecondSpeechId,
		get_english_locale(), GoodbyeSpeechState ),

	audio_utils:playback_file( GoodbyeEnglishAudioPath, AudioSettings,
							   DoBlock ).



% @doc The actual TTS test.
-spec run_test_tts( speech_state() ) -> void().
run_test_tts( SpeechState ) ->

	test_facilities:display( "Testing TTS service." ),

	%test_list_voices( SpeechState ),

	%test_record_speeches( SpeechState ),

	test_referential_management( SpeechState ).



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display(
				"(not running the speech support test, being in batch mode)" );

		false ->
			run_speech_test()

	end,

	test_facilities:stop().
