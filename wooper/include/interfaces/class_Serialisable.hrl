% Copyright (C) 2022-2023 Olivier Boudeville
%
% This file is part of the Ceylan-WOOPER library.
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
% Creation date: Wednesday, July 27, 2022.


% Shared elements regarding instance serialisation.


% The recommended extension for files containing serialisations of WOOPER
% instances:
%
% (for 'WOOPER Serialisation File')
%
-define( wooper_serialisation_extension, "wsf" ).


% The conventional atom to mark internal, local processes that must escape the
% serialisation/deserialisation processes:
%
-define( process_restoration_marker,
		 'wooper_serialisable_process_restoration_marker' ).


% The conventional atom to mark internal, local open files (akin to file
% descriptors) that must escape the serialisation/deserialisation processes:
%
-define( file_restoration_marker,
		 'wooper_serialisable_file_restoration_marker' ).


% The conventional atom to mark internal, local terms that must escape the
% serialisation/deserialisation processes (ex: typically because they are large
% and may be recreated afterwards, or because they are transient terms yet do
% not have a corresponding, more specialised marker):
%
-define( term_restoration_marker,
		 'wooper_serialisable_term_restoration_marker' ).



-record( wooper_serialisation_instance_record, {

	class_name :: wooper:classname(),
	% The classname of that instance.

	attributes :: [ wooper:attribute_entry() ],
	% The serialisation-ready information about all instance attributes.


	% Of course no virtual table is stored here, it will recreated on loading.

	% Any extra process-level element mentioned in the 'About serialised
	% elements' section can be managed here.


	extra_data :: term()
	% Any extra that needs to be kept around (e.g. to preserve the content of
	% generated files that would be problematic to recreate).

} ).
