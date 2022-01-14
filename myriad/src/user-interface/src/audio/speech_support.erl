% Copyright (C) 2021-2022 Olivier Boudeville
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
% Creation date: Monday, November 29, 2021.


% @doc Gathering of various facilities regarding <b>speech support</b>, i.e. TTS
% (Text to Speech), in order to obtain an audio content corresponding to a
% specified text.
%
-module(speech_support).


% Implementation notes:
%
% We are here neither generating our own speech (way to difficult) nor using
% open solutions like espeak or Festival. Instead we access to the REST API of a
% (commercial) speech provider offering high-end TTS.
%
% We identified three quality providers:
% - Google Wavenet, see https://cloud.google.com/text-to-speech
% - Amazon Polly, see https://aws.amazon.com/fr/polly
% - Microsoft Azure Neural, see REF1:
% https://azure.microsoft.com/en-us/services/cognitive-services/text-to-speech/
%
% We preferred here the Microsoft offer. See REF1 for all details about the
% supported languages, locales, etc.
%
% We rely on user-defined credentials to one's Microsoft Azure account in order
% to use the TTS services.
%
% In practice a series of HTTP requests are made to interact with the service:
% the text to be spoken and the associated data are sent, and the corresponding
% audio samples are fetched.

% The text to be spoken can be specified either as simple text or, better, as
% SSML (see https://en.wikipedia.org/wiki/Speech_Synthesis_Markup_Language).

% Special characters, such as quotation marks, apostrophes, and brackets must be
% escaped, this will be done automatically here.

% Cloud services operate at a cost, refer to
% https://azure.microsoft.com/en-us/pricing/details/cognitive-services/speech-services/
% for Azure pricing.

% Main elements that can be added in a ssml_text():
%
%  - '<break strength="S" />' with S in ['none', 'x-weak', 'weak', 'medium'
%  (default), 'strong', 'x-strong']; can be added at any place in the text
%
%  - '<break time="D" />' with D in seconds (ex: "2s") or milliseconds (ex:
%  "500ms")
%
%  - '<mstts:silence type="Leading|Tailing|Sentenceboundary" value="D"/>";
%  applies only at the beginning or end of the input text, or between two
%  consecutive sentences
%
%  - '<p>P</p>' to denote a paragraph
%
%  - '<s>S</s>' to denote a sentence
%
%  - '<phoneme alphabet="ipa|sapi|ups" ph="P">W</phoneme>' to specify our a word
%  shall be speeched (we prefer the sapi phonetic alphabet, otherwise ipa);
%  refer to
%  https://docs.microsoft.com/en-us/azure/cognitive-services/speech-service/speech-ssml-phonetic-sets for details
%
%  - <prosody pitch="P" contour="C" range="RG" rate="RT" duration="D"
%  volume="V"></prosody> to modify how the enclosed text is speeched
%
%  - <say-as interpret-as="S" format="F" detail="D">T</say-as>
%
%
% This XML structure can be expressed in "simple form" (see
% xml_utils:xml_document/0); for example:
%
% MySSMLText = ["Hello ", {prosody, [{volume, "+20.00%"}], [" John"]},...

% Neural voices are created from samples that use a 24 khz sample rate.



% About speech referentials.
%
% A multilangual application has to support multiple language locales (ex:
% English, French, Japanese, etc.).
%
% We define here a "logical speech", corresponding to a speech of a given
% semantics (meaning), regardless to its translation into a set of locales.

% For that, generally a reference locale is generally elected (ex: "en-US"), and
% each logical speech is first defined in terms of that locale, as a SSML text
% (ex: "Hello world!"). Then, in the context of a given locale, this logical
% speech is to be translated in a corresponding locale-specific SSML text (ex:
% "Salut le monde !"), and then generated as an audio content.
%
% See for that create_referential/1 and record_logical_speech/5


% Next evolutions could be adding facilities in order to export a speech
% referential to JSON and reciprocally import it from JSON, so that afterwards
% clients may load such files and fetch all appropriate information for a given
% spoken locale.


-type tts_provider() :: 'google' % Google (Wavenet)
					  | 'aws'    % Amazon Polly
					  | 'azure'. % Microsoft Azure (Neural)
% A known provider of TTS.
%
% See also web_utils:cloud_provider/0.


-type voice_id() :: { tts_provider(), voice_id_at_provider() }.
% The absolute (reference) identifier of a voice (ex: {azure,
% `<<"fr-FR-DeniseNeural">>'}.


-type voice_id_at_provider() :: bin_string().
% The identifier of a voice (ex: `<<"fr-FR-DeniseNeural">>',
% `<<"ar-SA-Naayf">>') in the context of a specific TTS provider.


-type voice_name() :: bin_string().
% The full name (just informative) of a voice.
%
% Ex: `<<"Foobar Server Speech Text to Speech Voice (xr-XG, Yoda)">>'.


-type voice_type() :: 'normal'  % Basic.
					| 'neural'. % AI-produced.
% The type of a voice.


-type voice_gender() :: 'male' | 'female'.
% The gender of a voice.


-type language_locale() :: bin_locale().
% The language locale to be used (ex: `<<"fr-FR">>'), knowing that for example a
% voice may speak in multiple languages (ex: "Jenny Multilingual").


% Not existing apparently: 'general' | 'senior' | 'child'
-type supported_style() :: 'assistant'
						 | 'news_reading'
						 | 'news_reading_casual'
						 | 'news_reading_formal'
						 | 'story_narrating'
						 | 'work_narrating'
						 | 'conversing'
						 | 'customer_support'
						 | 'calm'
						 | 'fearful'
						 | 'angry'
						 | 'sad'
						 | 'envious'
						 | 'affectionate'
						 | 'gentle'
						 | 'depressed'
						 | 'serious'
						 | 'disgruntled'
						 | 'cheerful'
						 | 'embarrassed'
						 | 'empathetic'
						 | 'lyrical'.
% Defines a style of speech that may be supported by voices.
%
% See, for a synthesis of the styles supported by each voice,
% https://docs.microsoft.com/en-us/azure/cognitive-services/speech-service/speech-synthesis-markup
% or enable the list_voices/1 the writing of the voice JSON listing.


-type role_played() :: { voice_gender(), age_played() } | 'narrator'.
% A role that a voice may play.


-type age_played() :: 'child' | 'young_adult' | 'older_adult' | 'senior' .
% An age that a voice may roleplay.



% For the speech_state, voice_info, speech_settings and speech_referential
% records:
%
-include("speech_support.hrl").

% For the audio_stream_settings record:
-include("audio_utils.hrl").

% For records like azure_instance_info:
-include("web_utils.hrl").


-type voice_info() :: #voice_info{}.
% Information regarding a voice for TTS.

-type voice_table() :: table( voice_id(), voice_info() ).
% A table associating the information regarding a voice based on its identifier.


-type speech_settings() :: #speech_settings{}.
% Information regarding a speech to be recorded (many of whom are optional).

-type speech_settings_id() :: count().
% The identifier of given speech settings in a table thereof.

-type speech_settings_table() ::
		table( speech_settings_id(), speech_settings() ).


-type ssml_text() :: xml_document().
% A text to be spoken, specified in Speech Synthesis Markup Language (SSML),
% therefore as an XML document.
%
% Special characters, such as quotation marks, apostrophes, and brackets will be
% automatically escaped here.
%
% Refer to https://www.w3.org/TR/2004/REC-speech-synthesis-20040907/ and
% https://docs.microsoft.com/en-us/azure/cognitive-services/speech-service/speech-synthesis-markup
% for further SSML details.


-type user_speech_info() :: { speech_settings_id(), ssml_text() }.
% User-specified information regarding an actual text-to-speech, to allow for
% the instantiation of a logical speech.



-type speech_id() :: count().
% The identifier of a record about a logical speech.


-type speech_base_name() :: bin_path_element().
% A short name to designate a logical speech, able to be used as a prefix of a
% filename; ex: `<<"welcome-new-recruits">>'). Not an identifier, but preferably
% unique.


-type any_speech_base_name() :: any_path_element().
% A short name (any kind of string) to designate a logical speech, able to be
% used as a prefix of a filename; ex: "welcome-new-recruits"). Not an
% identifier, but preferably unique.


-type logical_speech() :: #logical_speech{}.
% All information regarding a logical speech, possibly recorded based on
% multiple, different spoken locales.


-type actual_speech_info() :: #actual_speech_info{}.
% Key information regarding an actual text-to-speech, that is the instantiation
% of a logical speech, i.e. the speech settings that apply to this SSML text and
% the resulting audio file (relative to the base directory of the speech
% referential keeping track of the corresponding logical speech).


-type locale_table() :: table( language_locale(), actual_speech_info() ).
% A table associating, in the context of a given logical speech, for each spoken
% locale, the information of the corresponding text to speech.


-type speech_table() :: table( speech_id(), logical_speech() ).
% A table associating to the identifier of a logical speech the various
% available information about it.


-type speech_referential() :: #speech_referential{}.
% A datastructure collecting information regarding a set of logical speeches.


-opaque speech_state() :: #speech_state{}.
% The state of the speech support, to be carried between calls.


-export_type([ tts_provider/0, voice_id/0, voice_id_at_provider/0,
			   voice_name/0, voice_type/0, voice_gender/0,
			   language_locale/0, supported_style/0,
			   voice_info/0, voice_table/0,
			   speech_settings/0, speech_settings_id/0, speech_settings_table/0,
			   ssml_text/0, user_speech_info/0,
			   speech_id/0, speech_base_name/0, any_speech_base_name/0,
			   logical_speech/0, actual_speech_info/0, locale_table/0,
			   speech_table/0, speech_referential/0, speech_state/0 ]).


-export([ check_availability/0,
		  get_default_audio_settings/0, get_audio_format_string/1,
		  start/1, start/2, stop/1,
		  list_voices/1,
		  record_speech/3, record_speech/4, record_speech/5,

		  register_speech_settings/2,
		  create_referential/1, record_logical_speech/3,
		  get_audio_path_for/3,

		  filter_by_gender/2, filter_by_locale/2,

		  speech_state_to_string/1, voice_id_to_string/1,
		  tts_provider_to_string/1, voice_table_to_string/1,
		  voice_info_to_string/1, role_to_string/1,
		  speech_settings_to_string/1, logical_speech_to_string/1,
		  actual_speech_info_to_string/1, speech_referential_to_string/1 ]).


% Shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type bin_locale() :: locale_utils:bin_locale().
-type any_locale() :: locale_utils:any_locale().

-type json_term() :: json_utils:json_term().

-type bin_file_name() :: file_utils:bin_file_name().
-type file_path() :: file_utils:file_path().
-type bin_file_path() :: file_utils:bin_file_path().
-type any_directory_path() :: file_utils:any_directory_path().
-type bin_path_element() :: file_utils:bin_path_element().
-type any_path_element() :: file_utils:any_path_element().
-type extension() :: file_utils:extension().

-type xml_document() :: xml_utils:xml_document().


-type audio_stream_settings() :: audio_utils:audio_stream_settings().


-define( azure_speech_api_endpoint,
		 "api.cognitive.microsoft.com/sts/v1.0/issuetoken" ).



% @doc Tells whether the speech support is available:
% - if true, returns a preliminary, configured speech state
% - if false, returns an extra textual diagnosis
%
% Side-effect: ensures that a Myriad preferences server is running.
%
-spec check_availability() -> { 'true', speech_state() }
							| { 'false', ustring() }.
check_availability() ->

	case json_utils:is_parser_available() of

		true ->
			ParserState = json_utils:start_parser(),

			SpeechState = #speech_state{
				json_parser_state=ParserState,
				http_options=[ { ssl, web_utils:get_ssl_verify_options() } ],
				audio_settings=get_default_audio_settings() },

			check_tts_provider_availability( SpeechState );

		false ->
			{ false, "no JSON parser found available" }

	end.



% @doc Returns the default settings enforced for the speech audio output.
-spec get_default_audio_settings() -> audio_stream_settings().
get_default_audio_settings() ->
	% Corresponds to 'ogg-48khz-16bit-mono-opus', probably close to the best
	% available quality:
	%
	% (another option could be 'raw-48khz-16bit-mono-pcm' to perform the
	% encoding by oneself afterwards)
	%
	#audio_stream_settings{ sampling_rate=48,
							channel_layout=mono,
							bit_level={ bit, 16 },
							container_format=ogg,
							audio_format=opus }.



% @doc Tells whether the speech support is available:
% - if true, returns a preliminary, configured speech state
% - if false, returns an extra textual diagnosis
%
% Side-effect: ensures that a Myriad preferences server is running.
%
-spec check_tts_provider_availability( speech_state() ) ->
			{ 'true', speech_state() } | { 'false', ustring() }.
check_tts_provider_availability( SpeechState ) ->

	% We look for the relevant credentials, expected to be found in the user
	% preferences:

	PrefServerPid = preferences:start(),

	InstPrefKeyForKey = azure_speech_instance_key,
	InstPrefKeyForLoc = azure_speech_instance_location,

	[ MaybeInstanceKey, MaybeInstanceLocation ] = preferences:get(
		[ InstPrefKeyForKey, InstPrefKeyForLoc ], PrefServerPid ),

	case MaybeInstanceKey of

		undefined ->
			{ false, text_utils:format( "no key found in the user preferences "
				"to designate a Microsoft Azure instance for speech "
				"(no '~ts' entry found)", [ InstPrefKeyForKey ] ) };

		InstKey ->
			case MaybeInstanceLocation of

				undefined ->
					{ false, text_utils:format( "no location found in the "
						"user preferences to designate a Microsoft Azure "
						"instance for speech (no '~ts' entry found)",
						[ InstPrefKeyForLoc ] ) };

				InstLoc ->
					AzureInstInfo = web_utils:get_azure_instance_information(
										InstKey, InstLoc ),
					{ true, SpeechState#speech_state{
								cloud_instance_info=AzureInstInfo } }

			end

	end.



% @doc Returns the audio format description string corresponding to specified
% settings.
%
% Not all combinations are supported:
% - sampling rate is in [8, 16, 24, 48]
% - channel layout can at least currently only be 'mono'
% - bit depth is 8 or 16, otherwise it is a bit rate in [32, 48, 64, 96, 128,
%   160, 192]
% - recommended container formats are ogg or riff
% - strongly recommended audio format is opus
%
% Refer to
% https://docs.microsoft.com/en-us/azure/cognitive-services/speech-service/rest-text-to-speech#audio-outputs for more information.
%
-spec get_audio_format_string( audio_stream_settings() ) -> bin_string().
get_audio_format_string( #audio_stream_settings{
							sampling_rate=StdSamplingRate,
							channel_layout=ChannelLayout,
							bit_level=BitLevel,
							container_format=ContainerFormat,
							audio_format=AudioFormat } ) ->

	BitStr = case BitLevel of

		% 8 or 16 most probably:
		{ bit, BitDepth } ->
			text_utils:format( "~Bbit", [ BitDepth ] );

		{ kbps, BitRate } ->
			text_utils:format( "~tskbitrate", [ BitRate ] )

	end,

	% Hope for the best (ex: <<"ogg-24khz-16bit-mono-opus">>):
	BinFormat = text_utils:bin_format( "~ts-~Bkhz-~ts-~ts-~ts",
		[ ContainerFormat, StdSamplingRate, BitStr, ChannelLayout,
		  AudioFormat ] ),

	%trace_utils:debug_fmt( "Returning audio format '~ts'.", [ BinFormat ] ),

	BinFormat.



% @doc Starts the speech support, initialising the embedded speech referential
% with the current directory.
%
-spec start( speech_state() ) -> speech_state().
start( SpeechState ) ->
	start( SpeechState, _DefBaseDir="." ).


% @doc Starts the speech support, initialising the embedded speech referential
% with the specified directory.
%
-spec start( speech_state(), any_directory_path() ) -> speech_state().
start( SpeechState=#speech_state{ audio_settings=AudioSettings },
	   AnyBaseDir ) ->

	BaseDir = file_utils:ensure_path_is_absolute( AnyBaseDir ),

	SpeechReferential = create_referential( BaseDir, AudioSettings ),

	web_utils:start( _Opts=ssl ),

	SpeechState#speech_state{ speech_referential=SpeechReferential }.



% @doc Returns the available information about all the voices offered by the
% current TTS provider.
%
-spec list_voices( speech_state() ) -> voice_table().
list_voices( #speech_state{ cloud_instance_info=#azure_instance_info{
								instance_key=InstKey,
								instance_location=InstLoc },
							json_parser_state=ParserState,
							http_options=HTTPOptions } ) ->

	Uri = text_utils:format(
		"https://~ts.tts.speech.microsoft.com/cognitiveservices/voices/list",
		[ InstLoc ] ),

	Headers = #{ <<"Ocp-Apim-Subscription-Key">> => InstKey },

	case web_utils:get( Uri, Headers, HTTPOptions ) of

		{ _HTTPStatusCode=200, _Headers, JsonBinBody } ->

			%file_utils:write_whole( "voice-listing.json", JsonBinBody ),

			JsonTerm = json_utils:from_json( JsonBinBody, ParserState ),

			%trace_utils:debug_fmt( "Fetched following decoded JSON "
			%                       "content:~n ~p", [ JsonTerm ] ),

			register_voices( _TTSProvider=azure, JsonTerm,
							 _InitTable=table:new() );

		{ HTTPErrorCode, _Headers, BinBody } ->
			trace_utils:error_fmt( "Failed to list voices (~ts; body: ~p)",
				[ web_utils:interpret_http_status_code( HTTPErrorCode ),
				  BinBody ] ),

			throw( { unable_to_list_voices, HTTPErrorCode } );

		{ error, ErrorReason } ->
			% Ex: no Internet access
			trace_utils:error_fmt( "Failed to list voices; reason:~n ~p",
								   [ ErrorReason ] ),

			throw( { unable_to_list_voices, ErrorReason } )

	end.



% @doc Registers the specified (non-deprecated, non-preview) voices of specified
% TTS provider in the specified voice table.
%
-spec register_voices( tts_provider(), json_term(), voice_table() ) ->
															voice_table().
register_voices( _TTSProvider, _JsonTerm=[], VoiceTable ) ->
	VoiceTable;

register_voices( TTSProvider, _JsonTerm=[ VoiceMap | T ], VoiceTable ) ->

	%trace_utils:debug_fmt( "Registering voice ~p", [ VoiceMap ] ),

	% Extract first all base (always_-pecified) information:
	{ [ VIdAtProvider, ReadType, ReadGender, ReadSampleRate, ReadStatus,
		ReadName, ReadLocale, ReadDisplayName, ReadLocalName, ReadLocDesc ],
	  ExtractedVoiceMap } = table:extract_entries(
		[ <<"ShortName">>, <<"VoiceType">>, <<"Gender">>, <<"SampleRateHertz">>,
		  <<"Status">>, <<"Name">>, <<"Locale">>, <<"DisplayName">>,
		  <<"LocalName">>, <<"LocaleName">> ], VoiceMap ),

	VId = { TTSProvider, VIdAtProvider },

	Type = case ReadType of

		<<"Neural">> ->
			neural

	end,

	Gender = case ReadGender of

		<<"Male">> ->
			male;

		<<"Female">> ->
			female

	end,

	SampleRate = text_utils:binary_to_integer( ReadSampleRate ),

	% Now managing optional information:

	{ Styles, StyleShrunkVMap } = case table:extract_entry_if_existing(
						<<"StyleList">>, ExtractedVoiceMap ) of

		false ->
			{ undefined, ExtractedVoiceMap };

		{ ReadStyles, StVMap } ->
			{ convert_styles_from_azure( ReadStyles ), StVMap }

	end,

	{ SecLocales, SecLocShrunkVMap } = case table:extract_entry_if_existing(
						<<"SecondaryLocaleList">> , StyleShrunkVMap ) of

		false ->
			{ [], StyleShrunkVMap };

		P -> %{ SecLocs, LocVMap } ->
			P

	end,

	{ Roles, RoleShrunkVMap } = case table:extract_entry_if_existing(
						<<"RolePlayList">>, SecLocShrunkVMap ) of

		false ->
			{ [], SecLocShrunkVMap };

		{ ReadRoles, RlVMap } ->
			{ convert_roles_played_from_azure( ReadRoles ), RlVMap }

	end,

	FinalVMap = RoleShrunkVMap,

	case table:enumerate( FinalVMap ) of

		[] ->
			ok;

		UnexpectedEntries ->
			trace_utils:warning_fmt( "For voice '~ts', ~B unexpected (ignored) "
				"entries:~n ~p", [ VIdAtProvider, length( UnexpectedEntries ),
								   UnexpectedEntries ] )

	end,

	case ReadStatus of

		<<"GA">> ->
			VoiceRec = #voice_info{
					name=ReadName,
					id=VId,
					type=Type,
					gender=Gender,
					styles=Styles,
					roles_played=Roles,
					locale=ReadLocale,
					locale_description=ReadLocDesc,
					secondary_locales=SecLocales,
					display_name=ReadDisplayName,
					local_name=ReadLocalName,
					sample_rate=SampleRate },

			NewVoiceTable = table:add_new_entry( VId, VoiceRec, VoiceTable ),

			register_voices( TTSProvider, T, NewVoiceTable );

		<<"Preview">> ->
			register_voices( TTSProvider, T, VoiceTable );

		<<"Deprecated">> ->
			register_voices( TTSProvider, T, VoiceTable )

	end.



% @doc Returns a voice table corresponding to the specified one where only the
% voice of the specified gender would be kept.
%
-spec filter_by_gender( voice_gender(), voice_table() ) -> voice_table().
filter_by_gender( Gender, VoiceTable ) ->
	table:fold( fun( VId, VInfo=#voice_info{ gender=G }, Acc )
									when G =:= Gender ->
					table:add_entry( VId, VInfo, Acc );

					( _VId, _VInfo, Acc ) ->
						Acc
				end,
				table:new(),
				VoiceTable ).



% @doc Returns a voice table corresponding to the specified one where only the
% voice of the specified spoken locale (as primary or secondary) would be kept.
%
-spec filter_by_locale( any_locale(), voice_table() ) -> voice_table().
filter_by_locale( SpokenLocale, VoiceTable ) ->

	BinLocale = text_utils:ensure_binary( SpokenLocale ),

	table:fold( fun( VId, VInfo=#voice_info{ locale=Loc,
											 secondary_locales=SecLocs },
					 Acc ) ->
						case Loc =:= BinLocale
								orelse lists:member( BinLocale, SecLocs ) of

							true ->
								table:add_entry( VId, VInfo, Acc );

							false ->
								Acc
						end;

					( _VId, _VInfo, Acc ) ->
						Acc
				end,
				table:new(),
				VoiceTable ).



% @doc Records the speech corresponding to the specified SSML message, according
% to the (supposedly set) current speech settings, using the specified base name
% to forge the filename in which the generated audio will be stored, in the
% current directory; the corresponding audio path is returned; the corresponding
% filename (relative to the elected directory) is returned.
%
% Ex: if BaseName is "hello-world", the current directory is
% /home/bond/my-speeches", and the audio settings imply a Ogg container format
% with an Opus audio format and the fr-FR locale, the specified speech will be
% stored in "/home/bond/my-speeches/hello-world-fr-FR.ogg.opus".
%
% A SSML message is an XML document, we recommend using the so-called "simple
% form" to define it, refer to the "Defining one's XML document" in
% xml_utils.erl for further details. Also spaces shall exist around tags.
%
% Ex: record_speech(["Hello ", {prosody, [{volume, "+20.00%"}], [" John"]},...
%
% See record_speech/5 for further details.
%
-spec record_speech( ssml_text(), any_speech_base_name(), speech_state() ) ->
													file_path().
record_speech( SSMLText, BaseName, SpeechState ) ->
	record_speech( SSMLText, BaseName, _MaybeOutputDir=undefined, SpeechState ).



% @doc Records the speech corresponding to the specified SSML message, according
% to the (supposedly set) current speech settings, using the specified base name
% to forge the filename in which the generated audio will be stored, in the
% specified directory (otherwise in the current one); the corresponding filename
% (relative to the elected directory) is returned.
%
% Ex: if BaseName is "hello-world", the specified directory is
% /home/bond/my-speeches", and the audio settings imply a Ogg container format
% with an Opus audio format and the fr-FR locale, the specified speech will be
% stored in "/home/bond/my-speeches/hello-world-fr-FR.ogg.opus".
%
% A SSML message is an XML document, we recommend using the so-called "simple
% form" to define it, refer to the "Defining one's XML document" in
% xml_utils.erl for further details.
%
% Ex: record_speech(["Hello ", {prosody, [{volume, "+20.00%"}], [" John"]},...
%
% See record_speech/5 for further details.
%
-spec record_speech( ssml_text(), any_speech_base_name(),
			maybe( any_directory_path() ), speech_state() ) -> bin_file_name().
record_speech( _SSMLText, _BaseName, _MaybeOutputDir,
			   #speech_state{ speech_settings=undefined } ) ->
	throw( no_speech_settings_stored );

record_speech( SSMLText, BaseName, MaybeOutputDir,
			   SpeechState=#speech_state{ speech_settings=SpeechSettings } ) ->
	record_speech( SSMLText, BaseName, SpeechSettings, MaybeOutputDir,
				   SpeechState ).



% @doc Records the speech corresponding to the specified SSML message, according
% to the specified speech settings, using the specified base name to forge the
% filename in which the generated audio will be stored, in the specified
% directory (otherwise in the current one); the corresponding filename (relative
% to the elected directory) is returned.
%
% Ex: if BaseName is "hello-world", the specified directory is
% "/home/bond/my-speeches", and the audio settings imply a Ogg container format
% with an Opus audio format and the fr-FR locale, the specified speech will be
% stored in "/home/bond/my-speeches/hello-world-fr-FR.ogg.opus".
%
% A SSML message is an XML document, we recommend using the so-called "simple
% form" to define it, refer to the "Defining one's XML document" in
% xml_utils.erl for further details.
%
% Ex: record_speech(["Hello ", {prosody, [{volume, "+20.00%"}], [" John"]},...
%
% Restrictions:
% - the text-to-speech must be an XML document, yet may be as simple as "Hello
% world!"
% - this is a single-voice speech (no multiple texts per recording, as expecting
% to split these)
% - style adjustments: 'styledegree' (stronger or softer style, to make the
% speech more expressive or subdued) is not used (not offered by the voices
% of interest, and anyway the supported 'style' conveys more meaning)
%
-spec record_speech( ssml_text(), any_speech_base_name(), speech_settings(),
			maybe( any_directory_path() ), speech_state() ) -> bin_file_name().
record_speech( SSMLText,
			   AnyBaseName,
			   #speech_settings{
					voice_id={ azure, BinVoiceProviderId },
					language_locale=MaybeLangLoc,
					voice_gender=MaybeGender,
					speech_style=MaybeStyle,
					role=MaybeRole },
			   MaybeOutputDir,
			   #speech_state{
					cloud_instance_info=#azure_instance_info{
											instance_key=InstKey,
											instance_location=InstLoc },
					http_options=HTTPOptions,
					requester_app_name=BinAppName,
					audio_settings=AudioSettings } ) ->

	BaseName = text_utils:ensure_binary( AnyBaseName ),

	Uri = text_utils:format(
		"https://~ts.tts.speech.microsoft.com/cognitiveservices/v1",
		[ InstLoc ] ),

	BinAudioFormatStr = get_audio_format_string( AudioSettings ),

	Headers = #{ <<"Ocp-Apim-Subscription-Key">> => InstKey,
				 <<"X-Microsoft-OutputFormat">> => BinAudioFormatStr,
				 <<"User-Agent">> => BinAppName },

	% We create here the string containing the XML message expected by Azure. We
	% now use a real XML parser (through xml_utils) rather than forging XML by
	% ourselves as we used to, to avoid possibly complex escaping of SSML text.

	% We use lists of XML attributes for flexibility, as a maybe-type (empty
	% list or, often, single-element one).

	% Text expected to be expressed in the same locale as the speaker:
	{ LangLoc, RootDocLangAttrs, LangLocAttrs } = case MaybeLangLoc of

		undefined ->
			% No clue here, so use a default for document, and implicit for
			% voice:
			%
			DefaultDocLangLoc = <<"en-US">>,

			DocLangLocXMLAttrs = [ { 'xml:lang', DefaultDocLangLoc } ],

			{ DefaultDocLangLoc, DocLangLocXMLAttrs, [] };

		BinLangLoc ->
			LangLocXMLAttrs = [ { 'xml:lang', BinLangLoc } ],
			% Same for document and voice:
			{ BinLangLoc, LangLocXMLAttrs, LangLocXMLAttrs }

	end,

	BinOutputFilename = text_utils:bin_format( "~ts-~ts.~ts",
		[ BaseName, LangLoc, get_extension_for( AudioSettings ) ] ),

	OutputDir = case MaybeOutputDir of

		undefined ->
			file_utils:get_current_directory();

		_ ->
			MaybeOutputDir

	end,

	BinOutputFilePath = file_utils:join( OutputDir, BinOutputFilename ),

	% For Azure:
	BinVoiceName = BinVoiceProviderId,

	% Targeting a resulting XML string akin to:
	%
	% <speak version="1.0"
	%        xmlns="http://www.w3.org/2001/10/synthesis" xml:lang="en_US">
	%     <voice name="XXX" xml:gender="male">
	%         <mstts:express-as style="YYY">
	%             Properly XML-escaped, tag-rich SSML text here.
	%         </mstts:express-as>
	%     </voice>
	% </speak>

	GenderXMLAttrs = case MaybeGender of

		undefined ->
			% Let it be implicit then:
			[];

		Gender ->
			[ { 'xml:gender', convert_gender_to_azure( Gender ) } ]

	end,

	StyleXMLAttrs = case MaybeStyle of

		undefined ->
			% Let it be implicit then:
			[];

		Style ->
			[ { style, convert_style_to_azure( Style ) } ]

	end,

	RoleXMLAttrs = case MaybeRole of

		undefined ->
			% Let it be implicit then:
			[];

		Role ->
			[ { role, convert_roleplay_to_azure( Role ) } ]

	end,

	SSMLElement = case { StyleXMLAttrs, RoleXMLAttrs } of

		{ [], [] } ->
			SSMLText;

		_ ->
			[ { _Tag='mstts:express-as', _Attrs=StyleXMLAttrs++RoleXMLAttrs,
				_Content=SSMLText } ]

	end,

	VoiceAttrs = [ { name, BinVoiceName } | LangLocAttrs ++ GenderXMLAttrs ],

	VoiceContent = [ { voice, VoiceAttrs, SSMLElement } ],


	SpeakAttrs = [ { version, "1.0" },
				   { xmlns, "http://www.w3.org/2001/10/synthesis" },
				   { 'xmlns:mstts', "https://www.w3.org/2001/mstts" },
				   { 'xmlns:emo', "http://www.w3.org/2009/10/emotionml" } ]
												++ RootDocLangAttrs,

	XMLContent = [ { speak, SpeakAttrs, VoiceContent } ],

	%trace_utils:debug_fmt( "XML content for speech record:~n ~p",
	%                       [ XMLContent ] ),

	XMLStr = xml_utils:xml_to_string( XMLContent ),

	% Specifying a binary body is strictly necessary (one as a plain string
	% would result in encoding issues with non-Latin1 characters):
	%
	BinBody = text_utils:string_to_binary( XMLStr ),

	%trace_utils:debug_fmt( "Resulting XML string:~n ~ts", [ BinXMLStr ] ),

	%basic_utils:crash(),

	ContentType = "application/ssml+xml",

	case web_utils:post( Uri, Headers, HTTPOptions, BinBody, ContentType ) of

		{ _HTTPStatusCode=200, _Headers, BinAudio } ->

			%trace_utils:debug_fmt( "Fetched following audio content "
			%   "of type ~ts:~n ~p",
			%   [ type_utils:interpret_type_of( BinAudio ), BinAudio ] ),

			file_utils:write_whole( BinOutputFilePath, BinAudio ),
			BinOutputFilePath;

		{ HTTPErrorCode, _Headers, BinBody } ->
			trace_utils:error_fmt( "Failed to record speech (~ts; body: ~p)",
				[ web_utils:interpret_http_status_code( HTTPErrorCode ),
				  BinBody ] ),

			throw( { unable_to_record_speech, HTTPErrorCode } );

		{ error, ErrorReason } ->
			trace_utils:error_fmt( "Failed to record speech, reason:~n ~p",
								   [ ErrorReason ] ),

			throw( { unable_to_record_speech, ErrorReason } )

	end.



% @doc Registers the specified speech settings in the specified state, and
% returns it once updated, along with the identifier assigned to these settings.
%
-spec register_speech_settings( speech_settings(), speech_state() ) ->
						{ speech_settings_id(), speech_state() }.
register_speech_settings( SpeechSettings,
		SpeechState=#speech_state{ speech_settings_table=SpeechSettingsTable,
								   next_speech_settings_id=NextId } ) ->

	NewTable = table:add_new_entry( _K=NextId, _V=SpeechSettings,
									SpeechSettingsTable ),

	NewState = SpeechState#speech_state{ speech_settings_table=NewTable,
										 next_speech_settings_id=NextId+1 },

	{ NextId, NewState }.



% @doc Returns an empty speech referential with the default "en-US" locale and
% default audio settings, that is a referential not keeping track of any logical
% speech defined (yet).
%
-spec create_referential( any_directory_path() ) -> speech_referential().
create_referential( BaseDir ) ->
	create_referential( BaseDir, get_default_audio_settings() ).


% @doc Returns an empty speech referential with the default "en-US" locale and
% the specified audio settings, that is a referential not keeping track of any
% logical speech defined (yet).
%
-spec create_referential( any_directory_path(), audio_stream_settings() ) ->
											speech_referential().
create_referential( BaseDir, AudioStreamSettings ) ->
	create_referential( BaseDir, _RefLocal= <<"en-US">>, AudioStreamSettings ).


% @doc Returns an empty speech referential with the specified reference locale
% and audio settings, that is a referential not keeping track of any logical
% speech defined (yet).
%
-spec create_referential( any_directory_path(), any_locale(),
						  audio_stream_settings() ) -> speech_referential().
create_referential( BaseDir, RefLocale, AudioSettings )
				when is_record( AudioSettings, audio_stream_settings ) ->

	BinBaseDir = text_utils:ensure_binary( BaseDir ),

	case file_utils:is_existing_directory_or_link( BinBaseDir ) of

		true ->
			ok;

		false ->
			throw( { non_existing_referential_base_dir, BaseDir } )

	end,

	#speech_referential{ speech_table=table:new(),
						 reference_locale=text_utils:ensure_binary( RefLocale ),
						 base_dir=BinBaseDir }.



% @doc Returns the identifier of the logical speech requested to be recorded,
% together with the specified speech state whose speech referential has been
% updated with that speech, created from a speech base name associated to a list
% of actual speech information, so that the corresponding per-locale audio files
% are recorded, each with their specified SSML text and speech settings, and
% referenced.
%
% Note that all speech settings referenced for the generation have to have their
% locale defined.
%
-spec record_logical_speech( any_speech_base_name(),
		% A list of per-locale {speech_settings_id(), ssml_text()}:
		[ user_speech_info() ], speech_state() ) ->
			{ speech_id(), speech_state() }.
record_logical_speech( AnyBaseName, UserSpeechInfos,
		SpeechState=#speech_state{
			speech_settings_table=SpeechSettingsTable,
			audio_settings=AudioSettings,
			speech_referential=SpeechRef=#speech_referential{
					speech_table=SpeechTable,
					base_dir=BinBaseDir,
					next_speech_id=ThisLogSpeechId } } ) ->

	% Preferably done once for all:
	BinBaseName = text_utils:ensure_binary( AnyBaseName ),

	LocTable = record_speeches( UserSpeechInfos, BinBaseName,
		AudioSettings, BinBaseDir, _LocTable=table:new(),
		SpeechSettingsTable, SpeechState ),

	LogSpeech = #logical_speech{ id=ThisLogSpeechId,
								 base_name=BinBaseName,
								 locale_table=LocTable },

	NewSpeechTable =
		table:add_new_entry( ThisLogSpeechId, LogSpeech, SpeechTable ),

	NewSpeechRef = SpeechRef#speech_referential{
						speech_table=NewSpeechTable,
						next_speech_id=ThisLogSpeechId+1 },

	{ ThisLogSpeechId,
	  SpeechState#speech_state{ speech_referential=NewSpeechRef } }.



% @doc Records and references all speeches described in the specified list of
% speech information.
%
record_speeches( _UserlSpeechInfos=[], _BinBaseName, _AudioSettings,
		_BinBaseDir, LocTable, _SpeechSettingsTable, _SpeechState ) ->
	LocTable;

record_speeches( _UserlSpeechInfos=[ { SpeechSettingsId, SSMLText } | T ],
		BinBaseName, AudioSettings, BinBaseDir, LocTable, SpeechSettingsTable,
		SpeechState ) ->

	SpeechSettings = table:get_value( SpeechSettingsId, SpeechSettingsTable ),

	% Needing its locale to be set, as will be a key:
	SpeechLocale = case SpeechSettings#speech_settings.language_locale of

		undefined ->
			throw( { no_locale_defined_for, SpeechSettings } );

		Loc ->
			Loc

	end,

	% Returns filename relative to BinBaseDir:
	BinSpeechFilename = record_speech( SSMLText, BinBaseName, SpeechSettings,
									   BinBaseDir, SpeechState ),

	ActualSpeechInfo = #actual_speech_info{ ssml_text=SSMLText,
											speech_settings_id=SpeechSettingsId,
											audio_filename=BinSpeechFilename },

	NewLocTable = table:add_new_entry( _K=SpeechLocale, ActualSpeechInfo,
									   LocTable ),

	record_speeches( T, BinBaseName, AudioSettings, BinBaseDir, NewLocTable,
					 SpeechSettingsTable, SpeechState ).



% @doc Returns the absolute path of the audio file corresponding to the logical
% speech specified through its identifier, for the specified locale.
%
-spec get_audio_path_for( speech_id(), language_locale(), speech_state() ) ->
											bin_file_path().
get_audio_path_for( LogSpeechId, LangLocale,
					#speech_state{ speech_referential=#speech_referential{
										speech_table=SpeechTable,
										base_dir=BaseDir } } ) ->

	LocaleTable = case table:lookup_entry( _K=LogSpeechId, SpeechTable ) of

		key_not_found ->
			throw( { logical_speech_not_found, LogSpeechId } );

		{ value, #logical_speech{ locale_table=LocT } } ->
			LocT

	end,

	case table:lookup_entry( LangLocale, LocaleTable ) of

		key_not_found ->
			throw( { language_locale_not_found, LangLocale, LogSpeechId } );

		{ value, #actual_speech_info{ audio_filename=AudioFilename } } ->
			file_utils:bin_join( BaseDir, AudioFilename )

	end.



% Section to obtain textual descriptions of the speech-related elements.


% @doc Returns a textual description of the specified speech state.
-spec speech_state_to_string( speech_state() ) -> ustring().
speech_state_to_string( #speech_state{
		cloud_instance_info=InstInfo,
		speech_settings_table=SpeechSettingsTable,
		speech_referential=MaybeSpeechReferential } ) ->

	RefStr = case MaybeSpeechReferential of

		undefined ->
			"no";

		_ ->
			"a"

	end,

	text_utils:format( "speech taken in charge by a ~ts, "
		"referencing ~B speech settings and ~ts speech referential",
		[ web_utils:cloud_instance_info_to_string( InstInfo ),
		  table:size( SpeechSettingsTable ), RefStr ] ).



% @doc Returns a textual description of the specified voice identifier.
-spec voice_id_to_string( voice_id() ) -> ustring().
voice_id_to_string( _VoiceId={ TTSProvider, BinVoiceName } ) ->
	text_utils:format( "voice '~ts' from ~ts",
		[ BinVoiceName, tts_provider_to_string( TTSProvider ) ] ).



% @doc Returns a textual description of the specified TTS provider.
-spec tts_provider_to_string( tts_provider() ) -> ustring().
tts_provider_to_string( _TTSProvider=google ) ->
	"Google Wavenet";

tts_provider_to_string( _TTSProvider=aws ) ->
	"Amazon Polly";

tts_provider_to_string( _TTSProvider=azure ) ->
	"Microsoft Azure".



% @doc Returns a textual description of the specified voice table.
-spec voice_table_to_string( voice_table() ) -> ustring().
voice_table_to_string( VoiceTable ) ->

	case table:values( VoiceTable ) of

		[] ->
			"no voice registered";

		[ SingleVoice ] ->
			text_utils:format( "a single voice registered: ~ts",
				[ voice_info_to_string( SingleVoice ) ] );

		Voices ->
			text_utils:format( "~B voices registered: ~ts",
				[ length( Voices ), text_utils:strings_to_string(
					[ voice_info_to_string( V ) || V <- Voices ] ) ] )

	end.



% @doc Returns a textual description of the specified voice.
-spec voice_info_to_string( voice_info() ) -> ustring().
voice_info_to_string( #voice_info{
						name=FullName,
						id=Id,
						type=Type,
						gender=Gender,
						styles=MaybeStyles,
						roles_played=MaybeRoles,
						locale=Locale,
						locale_description=LocaleDesc,
						secondary_locales=SecondaryLocales,
						display_name=DisplayName,
						local_name=LocalName,
						sample_rate=SampleRate } ) ->

	SecLocStr = case SecondaryLocales of

		[] ->
			"";

		[ SecLoc ] ->
			text_utils:format( "and supporting '~ts' as secondary locale",
							   [ SecLoc ] );

		SecLocs ->
			text_utils:format( "and supporting ~B secondary locales: ~ts",
							   [ length( SecLocs ),
				text_utils:binaries_to_listed_string( SecLocs ) ] )

	end,

	StyleStr = case MaybeStyles of

		undefined ->
			"";

		[] ->
			"";

		[ SingleStyle ] ->
			text_utils:format( ", supporting the '~ts' style",
							   [ SingleStyle ] );

		Styles ->
			text_utils:format( ", supporting ~B styles: ~ts",
							   [ length( Styles ),
				text_utils:atoms_to_listed_string( Styles ) ] )

	end,

	RoleStr = case MaybeRoles of

		undefined ->
			"";

		[] ->
			"";

		[ SingleRole ] ->
			text_utils:format( ", supporting the '~ts' role",
							   [ role_to_string( SingleRole ) ] );

		Roles ->
			text_utils:format( ", supporting ~B roles: ~ts",
				[ length( Roles ),
				  text_utils:strings_to_listed_string(
					[ role_to_string( R ) || R <- Roles ] ) ] )

	end,


	text_utils:format( "~ts (full name is '~ts', display one is '~ts', "
		"local one is '~ts') ~ts of type ~ts, speaking the ~ts locale (~ts)"
		"~ts~ts~ts, with a sampling rate of ~B Hz",
		[ voice_id_to_string( Id ), FullName, DisplayName, LocalName, Gender,
		  Type, Locale, LocaleDesc, SecLocStr, StyleStr, RoleStr,
		  SampleRate ] ).



% @doc Returns a textual description of the specified role.
-spec role_to_string( role_played() ) -> ustring().
role_to_string( { _Gender=male, _AgePlayed=child } ) ->
	"boy";

role_to_string( { _Gender=female, _AgePlayed=child } ) ->
	"girl";


role_to_string( { _Gender=male, _AgePlayed=young_adult } ) ->
	"young man";

role_to_string( { _Gender=female, _AgePlayed=young_adult } ) ->
	"young woman";


role_to_string( { _Gender=male, _AgePlayed=senior } ) ->
	"senior man";

role_to_string( { _Gender=female, _AgePlayed=senior } ) ->
	"senior woman";


role_to_string( { _Gender=male, _AgePlayed=older_adult } ) ->
	"older man";

role_to_string( { _Gender=female, _AgePlayed=older_adult } ) ->
	"older woman";

role_to_string( narrator ) ->
	"narrator".



% @doc Returns a textual description of the specified speech settings.
-spec speech_settings_to_string( speech_settings() ) -> ustring().
speech_settings_to_string( #speech_settings{ voice_id=VoiceId,
											 language_locale=MaybeLoc,
											 voice_gender=MaybeGender,
											 speech_style=MaybeStyle,
											 role=MaybeRole } ) ->

	GenderStr = case MaybeGender of

		undefined ->
			"with no gender specified";

		Gender ->
			text_utils:format( "of the ~ts gender", [ Gender ] )

	end,

	LocStr = case MaybeLoc of

		undefined ->
			"with no spoken locale specified";

		Loc ->
			text_utils:format( "with the ~ts spoken locale", [ Loc ] )

	end,

	StyleStr = case MaybeStyle of

		undefined ->
			"no style specified";

		Style ->
			text_utils:format( "the ~ts style", [ Style ] )

	end,

	RoleStr = case MaybeRole of

		undefined ->
			"no role specified";

		Role ->
			text_utils:format( "the ~ts role", [ role_to_string( Role ) ] )

	end,

	text_utils:format( "speech settings about ~ts, ~ts, ~ts, "
		"with ~ts and ~ts",
		[ voice_id_to_string( VoiceId ), GenderStr, LocStr, StyleStr,
		  RoleStr ] ).



% @doc Returns a textual description of the specified logical speech.
-spec logical_speech_to_string( logical_speech() ) -> ustring().
logical_speech_to_string( #logical_speech{ id=Id,
										   base_name=BaseName,
										   locale_table=LocaleTable } ) ->
	text_utils:format( "logical speech of ID #~B, whose base name is '~ts', "
		"recorded for the following locales: ~ts",
		[ Id, BaseName,
		  text_utils:strings_to_listed_string( table:keys( LocaleTable ) ) ] ).



% @doc Returns a textual description of the specified information about an
% actual speech.
%
-spec actual_speech_info_to_string( actual_speech_info() ) -> ustring().
actual_speech_info_to_string( #actual_speech_info{
								ssml_text=SSMLText,
								speech_settings_id=SpeechSettingsId,
								audio_filename=AudioFilename } ) ->
	text_utils:format( "actual speech whose SSML is ~p, based on speech "
		"settings ~B, recorded in '~ts'",
		[ SSMLText, SpeechSettingsId, AudioFilename ] ).



% @doc Returns a textual description of the specified speech referential.
-spec speech_referential_to_string( speech_referential() ) -> ustring().
speech_referential_to_string( #speech_referential{
		speech_table=SpeechTable,
		reference_locale=RefLocale,
		base_dir=BaseDir
		%next_speech_id=NextSpeechId
								} ) ->
	text_utils:format( "speech referential of ~B logical speeches, "
		"whose reference locale is '~ts' and "
		"whose base directory is '~ts'",
		[ table:size( SpeechTable ), RefLocale, BaseDir ] ).




% @doc Stops the speech support.
-spec stop( speech_state() ) -> void().
stop( _SpeechState=#speech_state{ json_parser_state=ParserState } ) ->
	json_utils:stop_parser( ParserState ),
	web_utils:stop().



% Helpers


% @doc Returns the file extension corresponding to the specified audio settings.
%
% Ex: "ogg.opus".
%
-spec get_extension_for( audio_stream_settings() ) -> extension().
get_extension_for( #audio_stream_settings{ container_format=ContainerFormat,
										   audio_format=AudioFormat } ) ->
	text_utils:format( "~ts.~ts", [ ContainerFormat, AudioFormat ] ).



% @doc Converts Azure styles into our own.
convert_styles_from_azure( [] ) ->
	[];

% First, translations:
convert_styles_from_azure( [ <<"assistant">> | T ] ) ->
	[ assistant | convert_styles_from_azure( T ) ];

convert_styles_from_azure( [ <<"chat">> | T ] ) ->
	[ conversing | convert_styles_from_azure( T ) ];

convert_styles_from_azure( [ <<"narration">> | T ] ) ->
	[ story_narrating | convert_styles_from_azure( T ) ];

convert_styles_from_azure( [ <<"narrative">> | T ] ) ->
	[ story_narrating | convert_styles_from_azure( T ) ];

convert_styles_from_azure( [ <<"narration-professional">> | T ] ) ->
	[ work_narrating | convert_styles_from_azure( T ) ];

convert_styles_from_azure( [ <<"customerservice">> | T ] ) ->
	[ customer_support | convert_styles_from_azure( T ) ];

convert_styles_from_azure( [ <<"newscast">> | T ] ) ->
	[ news_reading | convert_styles_from_azure( T ) ];

convert_styles_from_azure( [ <<"newscast-casual">> | T ] ) ->
	[ news_reading_casual | convert_styles_from_azure( T ) ];

convert_styles_from_azure( [ <<"newscast-formal">> | T ] ) ->
	[ news_reading_formal | convert_styles_from_azure( T ) ];

convert_styles_from_azure( [ <<"Envy">> | T ] ) ->
	[ envious | convert_styles_from_azure( T ) ];

% Then either literal re-use or not known:
convert_styles_from_azure( [ Other | T ] ) ->
	AtomOther = text_utils:binary_to_atom( Other ),
	case lists:member( AtomOther, get_supported_styles() ) of

		true ->
			[ AtomOther | convert_styles_from_azure( T ) ];

		false ->
			trace_utils:warning_fmt( "Unknown speech style: '~ts'; ignored.",
									 [ Other ] ),
			convert_styles_from_azure( T )

	end.



% @doc Returns a list of the known supported styles.
-spec get_supported_styles() -> [ supported_style() ].
get_supported_styles() ->
	[ assistant, news_reading, news_reading_casual, news_reading_formal,
	  story_narrating, work_narrating, conversing, customer_support, calm,
	  fearful, angry, sad, envious, affectionate, gentle, depressed, serious,
	  disgruntled, cheerful, embarrassed, empathetic, lyrical ].



% @doc Converts Azure role plays into our own.
-spec convert_roles_played_from_azure( [ bin_string() ] ) ->
												[ supported_style() ].
convert_roles_played_from_azure( [] ) ->
	[];

convert_roles_played_from_azure( [ <<"Girl">> | T ] ) ->
	[ { female, child } | convert_roles_played_from_azure( T ) ];

convert_roles_played_from_azure( [ <<"Boy">> | T ] ) ->
	[ { male, child } | convert_roles_played_from_azure( T ) ];

convert_roles_played_from_azure( [ <<"YoungAdultFemale">> | T ] ) ->
	[ { female, young_adult } | convert_roles_played_from_azure( T ) ];

convert_roles_played_from_azure( [ <<"YoungAdultMale">> | T ] ) ->
	[ { male, young_adult } | convert_roles_played_from_azure( T ) ];

convert_roles_played_from_azure( [ <<"SeniorFemale">> | T ] ) ->
	[ { female, senior }  | convert_roles_played_from_azure( T ) ];

convert_roles_played_from_azure( [ <<"SeniorMale">> | T ] ) ->
	[ { male, senior }  | convert_roles_played_from_azure( T ) ];

convert_roles_played_from_azure( [ <<"OlderAdultFemale">> | T ] ) ->
	[ { female, older_adult } | convert_roles_played_from_azure( T ) ];

convert_roles_played_from_azure( [ <<"OlderAdultMale">> | T ] ) ->
	[ { male, older_adult } | convert_roles_played_from_azure( T ) ];

convert_roles_played_from_azure( [ <<"Narrator">> | T ] ) ->
	[ narrator | convert_roles_played_from_azure( T ) ];

convert_roles_played_from_azure( [ Other | T ] ) ->
	trace_utils:warning_fmt( "Unknown roleplay: '~ts'; ignored.", [ Other ] ),
	convert_roles_played_from_azure( T ).



% Bijective tables would have little interest (as we merged a few notions).


% @doc Converts back our notion of gender into the one of Azure:
-spec convert_gender_to_azure( voice_gender() ) -> ustring().
convert_gender_to_azure( _Gender=male ) ->
	"Male";

convert_gender_to_azure( _Gender=female ) ->
	"Female".


% @doc Converts back our notion of style into the one of Azure:
-spec convert_style_to_azure( supported_style() ) -> ustring().
% First the different translations:
convert_style_to_azure( _Style=conversing ) ->
	"chat";

convert_style_to_azure( _Style=story_narrating ) ->
	% Could have also be "narrative":
	"narration";

convert_style_to_azure( _Style=work_narrating ) ->
	"narration-professional";

convert_style_to_azure( _Style=customer_support ) ->
	"customerservice";

convert_style_to_azure( _Style=news_reading ) ->
	"newscast";

convert_style_to_azure( _Style=news_reading_casual ) ->
	"newscast-casual";

convert_style_to_azure( _Style=news_reading_formal ) ->
	"newscast-formal";

convert_style_to_azure( _Style=envious ) ->
	"Envy";

% Then the identical ones:
convert_style_to_azure( Style ) ->
	case lists:member( Style, get_supported_styles() ) of

		true ->
			text_utils:atom_to_string( Style );

		false ->
			throw( { unknown_supported_style, Style } )

	end.


% @doc Converts back our notion of age played into the one of Azure:
-spec convert_age_played_to_azure( age_played() ) -> ustring().
% Not defined: convert_age_played_to_azure( child ) ->
convert_age_played_to_azure( young_adult ) ->
	"YoungAdult";

convert_age_played_to_azure( older_adult ) ->
	"OlderAdult";

convert_age_played_to_azure( senior ) ->
	"Senior".



% @doc Converts back our notion of roleplay into the one of Azure:
-spec convert_roleplay_to_azure( role_played() ) -> ustring().
convert_roleplay_to_azure( narrator ) ->
	"Narrator";

convert_roleplay_to_azure( { _Gender=male, _AgePlayed=child } ) ->
	"Boy";

convert_roleplay_to_azure( { _Gender=female, _AgePlayed=child } ) ->
	"Girl";

convert_roleplay_to_azure( { Gender, AgePlayed } ) ->
	convert_age_played_to_azure( AgePlayed )
		++ convert_gender_to_azure( Gender ).
