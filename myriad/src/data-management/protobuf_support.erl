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
% Creation date: Friday, November 5, 2021.


% @doc Support for the <b>Protocol Buffer</b> facilities, a.k.a. Protobuf.
%
% We generally prefer using the proto3 version (rather than the proto2 one).
%
% See protobuf_support_test.erl for the corresponding test, and
% http://myriad.esperide.org/#about-protobuf for further information.
%
-module(protobuf_support).


-type spec_name() :: atom().
% The name of a Protobuf specification (ex: foobar), corresponding both to a
% specification file (ex: "foobar.proto") and the corresponding module name
% generated from it ('foobar').


-type spec_file_path() :: file_utils:any_file_path().
% A path to a Protobuf specification file respecting our conventions (ex:
% "/home/joe/foobar.proto").


-type package_name() :: ustring().
% The name of a Protobuf package, as a plain string, such as the user-defined
% "myriad.protobuf.test" one.

-type bin_package_name() :: bin_string().
% The name of a Protobuf package, as a binary string, such as the user-defined
% `<<"myriad.protobuf.test">>' one.


-type message_type() :: atom().
% The name of a type of Protobuf messages, as defined in a specification file.
%
% For example, if defining a "Person" message type in a "myriad.protobuf.test"
% package, the resulting record is #myriad_protobuf_test_person{}, corresponding
% to a myriad_protobuf_test_person() type.
%
% See myriad_example.proto for a test example.


-type message() :: type_utils:record().
% A (non-serialised, i.e. as an Erlang term) message instance.


-type serialisation() :: binary().
% A binary, serialised form, expected to contain Protobuff messages respecting
% our conventions.


-export_type([ spec_name/0, spec_file_path/0,
			   package_name/0, bin_package_name/0,
			   message_type/0, message/0, serialisation/0 ]).

-export([ encode/2, decode/3 ]).


% Implementation notes:
%
% The role of this module is to provide any higher-level service needed, and to
% shelter the user code from any particular Protobuf backend.
%
% The currently-used Protobuf backend is gpb
% (https://github.com/tomas-abrahamsson/gpb).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().



% @doc Encodes specified message (record instance) defined in specified
% specification, and returns the corresponding serialised form.
%
-spec encode( spec_name(), message() ) -> serialisation().
encode( SpecName, MessageTerm ) ->

	% The message type is deduced from the record.

	% Relies on gpb:
	SpecName:encode_msg( MessageTerm ).



% @doc Decodes specified serialised form into a message of specified type, as
% defined in specified specification.
%
-spec decode( spec_name(), message_type(), serialisation() ) -> message().
decode( SpecName, MessageType, BinSerialisation ) ->

	% Relies on gpb:
	Msg = SpecName:decode_msg( BinSerialisation, MessageType ),

	cond_utils:switch_execution_target(
		% In developement mode, unknown fields are collected by gpb, in the
		% '$unknowns' field. Without introspection, we cannot request a field by
		% name, yet a safe assumption is that this field is the last one; so, as
		% poorly efficient as it may be:
		%
		begin

			case list_utils:get_last_element( tuple_to_list( Msg ) ) of

				[] ->
					Msg;

				UnknownFields ->
					trace_utils:warning_fmt( "For following serialised "
						"message: ~n~p "
						"to be decoded as type '~ts' of specification '~ts', "
						"following unknown fields were reported:~n ~p~n"
						"(full decoded message: ~p)",
						[ BinSerialisation, MessageType, SpecName,
						  UnknownFields, Msg ] )

			end,
			Msg
		end,
		% Production mode just ignores any unknown field:
		Msg ).
