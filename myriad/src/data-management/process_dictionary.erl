% Copyright (C) 2018-2021 Olivier Boudeville
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
% Creation date: Monday, April 30, 2018.



% Process dictionary-related support.
%
% While using the process dictionary is usually regarded with contempt for good
% reasons (impure, prone to unwanted side-effects, etc.), there are a few
% specific cases where it might be nevertheless useful/relevant (ex: to make the
% state of a user interfaces implicit, rather than adding a parameter to
% virtually all functions of views, in the sense of the MVC pattern).
%
% We provide a basic encapsulation for the ways of interacting with the process
% dictionary, notably so that it is easier to locate (ex: thanks to 'grep') the
% places where the process dictionary is used.
%
% This module could have been named 'impure_table' as well, and could have
% obeyed a table-like API applying only to a singleton.
%
% Note: associating to a key the 'undefined' value is, here, semantically the
% same as not defining at all such an entry.
%
-module(process_dictionary).


% Actually any term:
-type key() :: hashtable:key().

-type value() :: hashtable:value().

-type entry() :: hashtable:entry().

-type entries() :: [ entry() ].

-type entry_count() :: basic_utils:count().


% Explicit form thereof, as a term:
-type process_dictionary() :: list_table:list_table().


-export_type([ key/0, value/0, entry/0, entries/0, entry_count/0,
			   process_dictionary/0 ]).



-export([ put/2, put_as_new/2, get/1, get_existing/1,
		  remove/1, remove_existing/1,
		  get_dictionary/0, get_keys/0, get_keys_for/1,
		  blank/0, to_string/0 ]).


% Puts specified entry in the process dictionary; returns any value that was
% previously associated to that key.
%
-spec put( key(), value() ) -> maybe( value() ).
put( Key, Value ) ->
	erlang:put( Key, Value ).



% Puts specified entry in the process dictionary; raises an exception if ever
% the specified key was already registered in the process dictionary.
%
-spec put_as_new( key(), value() ) -> void().
put_as_new( Key, Value ) ->

	case erlang:get( Key ) of

		undefined ->
			erlang:put( Key, Value );

		Other ->
			throw( { already_registered_key, Key, Other } )

	end.



% Returns the value (if any) associated to the specified key in the process
% dictionary, otherwise the 'undefined' atom.
%
-spec get( key() ) -> maybe( value() ).
get( Key ) ->
	%trace_utils:debug_fmt( "Getting key '~ts' (as ~p).", [ Key, self() ] ),
	erlang:get( Key ).



% Returns the value expected to be associated to the specified key in the
% process dictionary, otherwise throws an exception.
%
-spec get_existing( key() ) -> value().
get_existing( Key ) ->

	case erlang:get( Key ) of

		undefined ->
			throw( { non_registered_key, Key } );

		Other ->
			Other

	end.




% Removes any entry in the process dictionary corresponding to specified key,
% returning any value that was associated to it.
%
-spec remove( key() ) -> maybe( value() ).
remove( Key ) ->
	erlang:erase( Key ).



% Removes the entry in the process dictionary expected to correspond to the
% specified key and returns, otherwise throws an exception, should no value be
% associated to specified key.
%
-spec remove_existing( key() ) -> value().
remove_existing( Key ) ->

	case erlang:erase( Key ) of

		undefined ->
			throw( { non_registered_key, Key } );

		Other ->
			Other

	end.



% Returns the full process dictionary, as a list of {Key,Value} pairs.
-spec get_dictionary() -> process_dictionary().
get_dictionary() ->
	erlang:get().



% Returns a list of all keys present in the process dictionary.
-spec get_keys() -> [ key() ].
get_keys() ->
	erlang:get_keys().



% Returns a list of keys that are associated to the specified value in the
% process dictionary.
%
-spec get_keys_for( value() ) -> [ key() ].
get_keys_for( Value ) ->
	erlang:get_keys( Value ).



% Blanks the process dictionary (erases all entries), and returns its past
% content (as a term).
%
-spec blank() -> process_dictionary().
blank() ->
	erlang:erase().



% Returns a textual description of the current state of the process dictionary.
-spec to_string() -> text_utils:ustring().
to_string() ->

	case erlang:get() of

		[] ->
			text_utils:format( "the process dictionary of ~p is empty",
							   [ self() ] );

		Pairs ->
			Strings = lists:sort( [ text_utils:format(
									  "key '~ts' associated to value '~p'",
									  [ K, V ] ) || { K, V } <- Pairs ] ),

			text_utils:format( "the process dictionary of ~p contains "
				"~B pair(s): ~ts", [ self(), length( Pairs ),
									text_utils:strings_to_string( Strings ) ] )

	end.
