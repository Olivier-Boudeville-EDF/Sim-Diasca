% Copyright (C) 2019-2021 Olivier Boudeville
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


% Module containing some introspection-related facilities.
-module(wooper_introspection).


-export([ get_class_specific_attribute_names/1 ]).


-include("wooper_info.hrl").


% Returns the list of the names of the class-specific attributes of specified
% class, in their declaration order.
%
-spec get_class_specific_attribute_names( wooper:classname() ) ->
												[ wooper:attribute_name() ].
get_class_specific_attribute_names( Classname ) ->

	Attrinfos = Classname:get_class_specific_attributes(),

	[ AttrName || #attribute_info{ name=AttrName } <- Attrinfos ].
