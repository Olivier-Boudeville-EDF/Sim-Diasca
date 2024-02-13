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


% The state of our speech service:
-record( speech_state, {

	% Information regarding any clould instance that would typically used for
	% TTS:
	%
	cloud_instance_info :: maybe( web_utils:cloud_instance_info() ),

	% The state of the JSON parser used to interact with the cloud instance:
	json_parser_state :: maybe( json_utils:parser_state() ),

	% The HTTP options that all requests shall use:
	http_options :: web_utils:http_options(),

	% The name of the application requesting speech services (less than 255
	% characters).
	%
	requester_app_name = <<"Myriad speech support">> :: text_utils:bin_string(),

	% May store, as a convenience, current speech settings, for an easier reuse:
	speech_settings :: maybe( speech_support:speech_settings() ),

	% An associated table referencing all known speech settings:
	speech_settings_table = table:new() ::
								speech_support:speech_settings_table(),

	% The next identifier of speech settings to be used:
	next_speech_settings_id = 1 :: basic_utils:count(),

	% The audio settings regarding the generated output:
	audio_settings :: audio_utils:audio_stream_settings(),

	% Any associated speech referential:
	speech_referential :: maybe( speech_support:speech_referential() ) } ).



% Information regarding a voice for TTS:
-record( voice_info, {

	% The full name (just informative) of this voice:
	name :: speech_support:voice_name(),

	% The actual (reference) identifier of this voice:
	id :: speech_support:voice_id(),

	% The type of this voice:
	type :: speech_support:voice_type(),

	% The (main) gender of this voice:
	gender :: speech_support:voice_gender(),

	% The styles of speech (if any known) supported by this voice:
	styles :: maybe( [ speech_support:supported_style() ] ),

	% The specific roles that this voice may play:
	roles_played :: maybe( [ speech_support:role_played() ] ),


	% The (main) locale corresponding to the language spoken by this voice:
	locale :: locale_utils:bin_locale(),

	% The description of the (main) locale corresponding to the language spoken
	% by this voice:
	%
	locale_description :: locale_utils:bin_locale_description(),


	% The extra locales (if any) that this voices may speak:
	secondary_locales :: [ locale_utils:bin_locale() ],


	% The name for simple display (e.g. <<"Hoda">>):
	display_name :: text_utils:bin_string(),

	% The name according to its locale (e.g. <<"هدى">>):
	local_name :: text_utils:bin_string(),

	% The sample rate of the rendering of this voice:
	sample_rate :: audio_utils:sample_rate() } ).



% Information regarding a speech to be recorded (many of whom are optional):
-record( speech_settings, {

	% The voice that is to speak:
	voice_id :: speech_support:voice_id(),

	% Corresponds to the language to be spoken by this voice (some voices may
	% speak multiple language):
	%
	language_locale :: maybe( speech_support:language_locale() ),

	% At least usually the voice identifier already implies a gender:
	voice_gender :: maybe( speech_support:voice_gender() ),

	% Any style this voice may support:
	speech_style :: maybe( speech_support:supported_style() ),

	% Any role this voice might be able to play:
	role :: maybe( speech_support:role_played() ) } ).



% All information regarding a logical speech, probably supporting multiple,
% different spoken locales thanks to translation:
%
-record( logical_speech, {

	% The identifier of that speech (intentional duplicate of the corresponding
	% key in the speech table of any containing referential):
	%
	id :: speech_support:speech_id(),

	% A short name to designate this logical speech (e.g. as a prefix of its
	% filename); e.g. <<"welcome-new-recruits">>). Depending on user choice,
	% this may or may not be also an identifier.
	%
	base_name :: speech_support:speech_base_name(),

	% Records all per-locale text information for the current logical speech.
	%
	% The text information of reference (probably from which the others are
	% translated) corresponds to the entry whose key is the reference locale of
	% the overall speech referential.
	%
	locale_table :: speech_support:locale_table() } ).



% Information regarding an actual speech, i.e. the translation for a given
% spoken locale of a given logical speech.
%
-record( actual_speech_info, {

	% The SSML corresponding to this actual speech:
	ssml_text :: speech_support:ssml_text(),

	% Identifies the speech settings (e.g. which voice) for this actual speech:
	speech_settings_id :: speech_support:speech_settings_id(),

	% The filename, relative to the base directory of the underlying speech
	% referential, of the corresponding audio file:
	%
	audio_filename :: file_utils:bin_file_name() } ).



% A datastructure collecting information regarding a set of logical speeches.
-record( speech_referential, {

	% A table associating to each identifier of a logical speech the various
	% available information about it.
	%
	speech_table :: speech_support:speech_table(),

	% The reference, primary spoken locale, from which other locales may derive:
	%
	% (could be useful to better identify a logical speech from such a
	% "reference" SSML rather than just its base name)
	%
	reference_locale = <<"en-US">> :: speech_support:language_locale(),

	% The base directory where speech record files shall be stored:
	base_dir :: file_utils:bin_directory_path(),

	% The next identifier of logical speech to be assigned:
	next_speech_id = 1 :: speech_support:speech_id() } ).
