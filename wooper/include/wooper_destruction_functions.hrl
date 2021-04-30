% Copyright (C) 2003-2021 Olivier Boudeville
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


% Modular WOOPER header gathering the facilities for instance destruction.


% Debug or not:
-spec wooper_destruct( wooper:state() ) -> wooper:state().


-ifdef(wooper_debug_mode).


% Calls recursively the destructors through the inheritance tree.
%
% Each destructor (destruct/1 function) is purely local to the current module.
%
% Initial specified state is always valid (comes from the main loop), but states
% returned by user-defined destructors must be checked in debug mode.
%
wooper_destruct( State ) ->

	% If a class-specific destruct/1 is defined, executes it, otherwise does
	% nothing.
	%
	% Then recurses with higher-level destructors (maybe just storing destruct/1
	% in the method table would be more efficient, see
	% wooper_class_manager:get_virtual_table_key_for):

	% We should never rely on 'wooper:get_classname(State)' here, as it would
	% always return the leaf class. We use ?MODULE (even for embodied
	% instances):
	%
	Exports = module_info( exports ),

	%io:format( "**** Deleting ~w (destructor for class ~w/~w).~n",
	%		   [ self(), ?MODULE, wooper:get_classname( State ) ] ),

	DestructedState = case lists:member( { destruct, 1 }, Exports ) of

		true ->

			% All destructors, including user-defined ones, must return a
			% (possibly updated) state:

			%io:format( "Deleting ~w (overridden destructor for ~w).~n",
			%		   [ self(), ?MODULE ] ),


			% ?MODULE is always the real class name here:
			%
			%try apply( ActualClassname, destruct, [ State ] ) of
			try ?MODULE:destruct( State ) of

				ReturnedState when is_record( ReturnedState, state_holder ) ->
					ReturnedState;

				Other ->

					wooper:log_error( "~nWOOPER error for PID ~w of class ~ts: "
					  "user-defined destructor did not return a state, but "
					  "returned '~p' instead.", [ self(), ?MODULE, Other ] ),

					throw( { invalid_destructor, ?MODULE } )

			catch

				Reason:ErrorTerm:StackTrace ->
					trigger_destruct_error( Reason, ErrorTerm, StackTrace,
											State )

			end;

		false ->

			% Destructor not overridden, using default one:

			%io:format( "Deleting ~w (default do-nothing destructor "
			%		   "for class ~w).~n", [ self(), ?MODULE ] ),

			% State unchanged here:
			State

	end,

	chain_parent_destructors( DestructedState ).



-else. % if wooper_debug_mode



% Calls recursively the destructors through the inheritance tree.
%
% Each destructor (destruct/1 function) is purely local to the current module.
%
wooper_destruct( State ) ->

	% If a class-specific destruct is defined, executes it, otherwise does
	% nothing.
	%
	% Then recurses with higher-level destructors (maybe just storing destruct/1
	% in the method table would be more efficient, see
	% wooper_class_manager:get_virtual_table_key_for):
	%
	DestructedState = case lists:member( { destruct, 1 },
										 module_info( exports ) ) of

		true ->

			% All destructors, included user-defined ones, must return a
			% (possibly updated) state:
			%
			try

				?MODULE:destruct( State )

			catch

				Reason:ErrorTerm:StackTrace ->
					trigger_destruct_error( Reason, ErrorTerm, StackTrace,
											State )

			end;

		false ->

			% Destructor not overridden, using default one:

			%io:format( "Deleting ~w (default do-nothing destructor "
			%   "for class ~w).~n", [ self(), ?MODULE ] )

			% State unchanged:
			State

	end,

	chain_parent_destructors( DestructedState ).


-endif. % wooper_debug_mode




% Triggers a destruction-related error.
%
% (helper)
%
-spec trigger_destruct_error( basic_utils:exception_class(),
		basic_utils:error_term(), code_utils:stack_trace(), wooper:state() ) ->
									no_return().
trigger_destruct_error( Reason, ErrorTerm, StackTrace, State ) ->

	% Destruction failed:
	% (error term would often be unreadable with ~p)

	ActualClassname = wooper:get_classname( State ),

	wooper:log_error( "~nWOOPER error for PID ~w, "
		"destructor (~ts:destruct/1) failed (cause: ~p):~n~n"
		" - with error term:~n  ~p~n~n"
		" - stack trace was (latest calls first):~n~ts~n"
		" - instance state was: ~ts~n~n",
		[ self(), ActualClassname, Reason, ErrorTerm,
		  code_utils:interpret_stacktrace( StackTrace ),
		  wooper:state_to_string( State ) ] ),

	% Terminates the process:
	throw( { wooper_destructor_failed, self(), ActualClassname, ErrorTerm } ).



% Calls recursively the destructor of all direct superclasses.
%
% (helper)
%
-spec chain_parent_destructors( wooper:state() ) -> wooper:state().
chain_parent_destructors( State ) ->

	% Then automatically call the direct mother destructors.
	%
	% The final state is returned.

	% This would be incorrect, as it would return the superclasses of the
	% current instance, never recursing back in the inheritance graph:
	%
	%{ _State, Superclasses } = executeRequest( State, getSuperclasses ),
	%
	% Note also that auto-generating get_superclasses/0 leads to have to
	% auto-generate in turn
	% wooper_destruction_functions:chain_parent_destructors/1, and thus
	% wooper_destruct/1, etc. - unless we prefix this call with ?MODULE, as in :
	%
	Superclasses = ?MODULE:get_superclasses(),

	% Using foldr, not foldl: the destructors of mother classes are called in
	% the reverse order compared to the order that was used for construction,
	% for the sake of symmetry.
	%
	lists:foldr(

		fun( Class, NewState ) ->

				% More efficient than using apply/3:
				Class:wooper_destruct( NewState )

		end,

		_InitialAcc=State,

		_List=Superclasses ).
