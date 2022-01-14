% Copyright (C) 2020-2022 Olivier Boudeville
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
% Creation date: Thursday, July 16, 2020.


% @doc Gathering of various convenient facilities regarding the support of
% various <b>locales</b> (ex: for the management of per-country bank holidays).
%
-module(locale_utils).


-type country() :: 'france' | 'united_kingdom'.
% Type to designate all known countries.



-type string_locale() :: ustring().
% A locale as a BCP 47 plain string, like "fr-FR", corresponding to "French
% (France)" here.
%
% See http://cldr.unicode.org/ and
% https://en.wikipedia.org/wiki/IETF_language_tag for further details.


-type bin_locale() :: bin_string().
% A locale as a BCP 47 binary, like `<<"fr-FR">>', corresponding to "French
% (France)" here.
%
% See http://cldr.unicode.org/ and
% https://en.wikipedia.org/wiki/IETF_language_tag for further details.


-type any_locale() :: any_string().
% A locale as a BCP 47 plain string or binary, like "fr-FR" or `<<"fr-FR">>',
% corresponding to "French (France)" here.
%
% See http://cldr.unicode.org/ and
% https://en.wikipedia.org/wiki/IETF_language_tag for further details.


-type locale_description() :: ustring().
% A description of a locale (ex: "Afrikaans (South Africa)").


-type bin_locale_description() :: bin_string().
% A description of a locale (ex: `<<"Afrikaans (South Africa)">>').


-export_type([ country/0,
			   string_locale/0, bin_locale/0, any_locale/0,
			   locale_description/0, bin_locale_description/0 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().
