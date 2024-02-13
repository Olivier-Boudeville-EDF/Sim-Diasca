% Copyright (C) 2021-2024 Olivier Boudeville
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


% @doc This module concentrates <b>audio-related elements</b>.
%
% See speech_support.erl for TTS.
%
-module(audio_utils).



-type sample_rate() :: unit_utils:integer_hertz() .
% The sample rate of an audio content (e.g. 16000 Hz).


% Generally in [8, 16, 24, 48]:
-type standard_sampling_rate() :: pos_integer().
% A standard sample rate, in kHz (e.g. 24 kHz).


-type channel_layout() :: 'mono' | 'stereo' | '5.1'.
% The number and layout of audio channels.
%
% See the 'Standard speaker channels' section of
% https://en.wikipedia.org/wiki/Surround_sound for further details.


-type bit_rate() :: pos_integer().
% The bit rate of an audio content, in kilobits per second (e.g. 192 kbps).


-type bit_depth() :: pos_integer().
% The number of bits to which each audio sample is quantized.
%
% For example 8-bit or 16-bit.


-type bit_level() :: { 'bit', bit_depth() } | { 'kbps', bit_rate() }.
% Either a bit depth or a bit rate.



-type container_format() :: 'raw' | 'ogg' | 'webm' | 'riff'.
% Describes how to store metadata and possibly multiple audio streams in a
% binary stream.
%
% See the 'Audio coding formats support' of
% https://en.wikipedia.org/wiki/Comparison_of_video_container_formats for
% further details.


-type audio_format() :: 'raw' | 'pcm' | 'mp3' | 'vorbis' | 'aac' | 'flac'
					  | 'opus' | 'mulaw' | 'alaw' | 'truesilk'.
% A content representation format for storage or transmission of digital audio,
% i.e. how audio content is encoded.
%
% Refer to https://en.wikipedia.org/wiki/Comparison_of_audio_coding_formats for
% further details.


% For the audio_stream_settings record:
-include("audio_utils.hrl").

-type audio_stream_settings() :: #audio_stream_settings{}.


-type codec() :: bin_string().
% An implementation in charge of coding/decoding an audio format (e.g. the
% codecs of FFmpeg).


-export_type([ sample_rate/0, standard_sampling_rate/0,
			   channel_layout/0,
			   bit_rate/0, bit_depth/0, bit_level/0,
			   container_format/0, audio_format/0, audio_stream_settings/0,
			   codec/0 ]).


-export([ playback_file/2, playback_file/3,
		  audio_stream_settings_to_string/1 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().

-type any_file_path() :: file_utils:any_file_path().



% @doc Performs a playblack of the specified audio file, with specified
% settings (if any), in a non-blocking (in the background) way.
%
-spec playback_file( any_file_path(), maybe( audio_stream_settings() ) ) ->
											void().
playback_file( AudioFilePath, MaybeAudioStreamSettings ) ->
	playback_file( AudioFilePath, MaybeAudioStreamSettings, _DoBlock=false ).


% @doc Performs a playblack of the specified audio file, with specified
% settings, either in a blocking or in a non-blocking (in the background) way.
%
-spec playback_file( any_file_path(), maybe( audio_stream_settings() ),
					 boolean() ) -> void().
playback_file( AudioFilePath, _MaybeAudioStreamSettings, DoBlock ) ->
	executable_utils:playback_audio_file( AudioFilePath, DoBlock ).



% @doc Returns a textual description of the specified audio stream settings.
-spec audio_stream_settings_to_string( audio_stream_settings() ) -> ustring().
audio_stream_settings_to_string( #audio_stream_settings{
							sampling_rate=SamplingRate,
							channel_layout=ChannelLayout,
							bit_level=BitLevel,
							container_format=ContainerFormat,
							audio_format=AudioFormat } ) ->

	BitStr = case BitLevel of

		{ bit, BitDepth } ->
			text_utils:format( "~B-bit", [ BitDepth ] );

		{ kbps, BitRate } ->
			text_utils:format( "~B kbps", [ BitRate ] )

	end,

	text_utils:format( "~ts ~ts stream whose sampling rate is ~B kHz, "
		"as ~ts-encoded audio in a ~ts container",
		[ ChannelLayout, BitStr, SamplingRate, AudioFormat, ContainerFormat ] ).
