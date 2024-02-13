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
% Creation date: Thursday, December 2, 2021.


% Specification of an audio stream:
-record( audio_stream_settings, {

	% The standard (integer) sampling rate (in kHz) of this stream:
	sampling_rate :: audio_utils:standard_sampling_rate(),

	% The number and layout of audio channels:
	channel_layout :: audio_utils:channel_layout(),

	% Either a bit rate or a bit depth:
	bit_level :: audio_utils:bit_level(),

	% How to store metadata and possibly multiple audio streams:
	container_format :: audio_utils:container_format(),

	% How audio content is encoded:
	audio_format :: audio_utils:audio_format() } ).
