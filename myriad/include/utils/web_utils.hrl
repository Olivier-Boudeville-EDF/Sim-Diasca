% Copyright (C) 2015-2021 Olivier Boudeville
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


% Describes fully an URL, like: 'https://www.foo.org:8081/access/login'.
%
% Note: not to be used anymore, the Erlang-native uri_string module shall be
% preferred now.
%
-record( url_info, {

		   % Protocol (scheme) of the URL (ex: 'https'):
		   protocol = 'http' :: web_utils:protocol_type(),

		   % Host of the server (ex: 'www.foo.org'):
		   host_identifier :: net_utils:host_identifier(),

		   % Ex: 8081; possibly undefined.
		   port = 80 :: maybe( net_utils:net_port() ),

		   % Ex: 'access/login'.
		   path :: web_utils:path()

} ).
