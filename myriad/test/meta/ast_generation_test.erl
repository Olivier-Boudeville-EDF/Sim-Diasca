% Copyright (C) 2014-2021 Olivier Boudeville
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
% Creation date: Friday, December 19, 2014.


% Unit tests for the AST generation services.
%
% See the ast_generation.erl tested module.
%
-module(ast_generation_test).


% For run/0 export and al:
-include("test_facilities.hrl").



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	AtomList = [ a, b, c ],

	AtomListForm = ast_generation:atoms_to_form( AtomList ),

	% Check:
	AtomListForm = { cons, 0, {atom,0,a}, { cons, 0, {atom,0,b},
					   { cons, 0, {atom,0,c}, { nil,0 } } } },


	test_facilities:display( "The form version of ~p is:~n~p",
							 [ AtomList, AtomListForm ] ),

	% Check:
	AtomList = ast_generation:form_to_atoms( AtomListForm ),


	ParamCount = 4,

	HeaderParams = ast_generation:get_header_params( ParamCount ),

	test_facilities:display( "Testing ~B-parameter generation: ~p.",
							 [ ParamCount, HeaderParams ] ),


	VarCount = 2,

	Vars = ast_generation:enumerated_variables_to_form( VarCount ),


	test_facilities:display( "Listing ~B variables:~n~p", [ VarCount, Vars ] ),

	% Check:
	Vars = { cons, 0, {var,0,'Myriad_Param_1'},
			 { cons, 0, {var,0,'Myriad_Param_2'}, {nil,0} } },

	test_facilities:stop().
