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
% Creation date: Monday, November 8, 2021.



% Shared records and defines.


-define( default_database_port, 5432 ).

% The settings needed to designate a database instance.
-record( database_connection_settings, {

	% Designates the hostname on which a target database instance is running:
	host_name :: sql_support:database_host_name(),

	% The (TCP) port at which the target database instance is running:
	port = ?default_database_port :: maybe( sql_support:database_port() ),

	% The name of the target database instance. Ex: "acme_stock_db".
	name :: sql_support:database_name() } ).



% The settings corresponding to a given database user:
-record( database_user_settings, {

	% The name of a database user. Ex: "john_smith".
	user_name :: sql_support:database_user_name(),

	% The password of a database user:
	user_password :: sql_support:database_user_password() } ).


% The description of a field (a column).
-record( field_description, {

	% Integrate a variation of epgsql:column().

} ).
