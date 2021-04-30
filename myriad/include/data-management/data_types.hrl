% Copyright (C) 2011-2021 Olivier Boudeville
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


% This header tends to be included in a lot of places:
-ifndef(data_types_hrl_guard).
-define(data_types_hrl_guard,).


% The *_impl macros (ex: list_impl, set_impl) allow to select once for all the
% datatypes whose use is promoted internally, possibly with additional
% operations offered (see {list,set}_utils.erl).


%
% List section.
%

% When needing random access to a potentially long list (ex: removing a specific
% element), using a plain list is less efficient than using more advanced
% containers.


% Expected supported API-related conventions from any list_impl variant:
%
% list_impl_type()
%
% new/0
% is_empty/1
% from_list/1
% to_list/1
% is_member/2
% is_element/2
% iterator/1
% next/1
% add/2
% add_element/2
% delete/2
% del_element/2


% Note: this list_impl type should respect the implicit contracts of a list,
% such as being ordered, allowing duplicates, etc.
%
% As a consequence, ordsets and gb_sets cannot be used (they neither respect
% user-defined order nor support duplicates); for example:

% 1> A=gb_sets:new().
% 2> gb_sets:to_list(A).
% []
% 3> B=gb_sets:add_element(1,A).
% 4> gb_sets:to_list(B).
% [1]
% 5> C=gb_sets:add_element(1,B).
% 6> gb_sets:to_list(C).
% [1]


% So we ultimately switched back to plain list, until finding better-scaling
% replacements:
%
% (specifying a module here)
%
-define( list_impl, lists ).


% For homogeneous lists (at least to declare them as such):
% -type ?list_impl( T ) :: ?list_impl().


% The type of list_impl, for specifications:
%
-define( list_impl_type, list() ).

-define( list_impl_type( T ), [ T ] ).


% Any kind of lists:
%
-type any_list() :: list() | ?list_impl_type.




%
% Set section.
%


% Note: the default support for set is provided by the set_utils module, which
% provides a more complete, base implementation.

% Here the user-defined order may not be respected, and any element can be
% included only once (next additions will not change the container).

% Note: this set_impl type should respect the gb_sets API and implicit
% contracts.


% The actual module used for our sets:
%
% (we prefer 'ordsets' to provide a different choice than the one set_utils is
% based on)
%
%-define( set_impl, gb_sets ).
-define( set_impl, ordsets ).


% The type of set_impl, for specifications:
%
-define( set_impl_type, ordsets:ordset( any() ) ).

-define( set_impl_type(T), ordsets:ordset( T ) ).



-endif. % data_types_hrl_guard
