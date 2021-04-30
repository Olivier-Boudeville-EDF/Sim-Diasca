% Copyright (C) 2003-2021 Olivier Boudeville
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



% Unit tests for the ring management utilities.
%
% See the ring_utils.erl tested module.
%
-module(ring_utils_test).


% For run/0 export and al:
-include("test_facilities.hrl").


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	Ring = ring_utils:from_list( [ a, b, c, d, e, f, g ] ),

	{ a, FirstRing } = ring_utils:head( Ring ),

	{ b, SecondRing } = ring_utils:head( FirstRing ),

	{ [ c, d, e, f, g, a, b, c ], _ThirdRing } =
		ring_utils:get_next( _RingCount=8, SecondRing ),

	[ c, d, e, f, g, a, b ] = ring_utils:to_list( SecondRing ),

	test_facilities:display( "Second ring: ~ts",
							 [ ring_utils:to_string( SecondRing ) ] ),

	test_facilities:stop().
