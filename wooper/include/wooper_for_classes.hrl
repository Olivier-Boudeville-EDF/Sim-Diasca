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

% Modular WOOPER header gathering elements used to define a class.


% We define here functions whose only purpose is to make available a
% preprocessor define (like in: '-define(superclasses,[A,B]).') to the WOOPER
% parse transform (and possibly others):
%
% - a define is invisible to it as long it has been specified yet not used
%
% - the function there may be discarded, once it will have allowed the parse
% transform to determine said define (ex: [A,B] here)
%
% Such functions may be of use also for other needs (ex: introspection), so some
% of them are exported.
%
% We used to prefer defining such an helper pseudo-function iff a define had
% been specified; now such functions (ex: wooper_get_superclasses/0,
% wooper_get_class_specific_attributes/0) are defined in all cases, as uses like
% introspection should not have to deal with possibly undefined functions.
%
% Finally, we define here:
%
% - get_superclasses/0 (and not wooper_get_superclasses/0), as this function
% will be kept as is, once checked (its definition is already relevant)

% - wooper_get_class_specific_attributes/0 (and not
% get_class_specific_attributes/0), as its definition is temporary (we prefer a
% canonicalised version thereof; it would not compile anyway, types being
% understood as undefined functions); the only resulting exported function, with
% the signature we prefer, is thus get_class_specific_attributes/0

% get_superclasses/0 not exported, as is a (static) method:
-export([ wooper_get_class_specific_attributes/0 ]).


% Returns the direct mother classes of the current one.
%
% Note: available to the user.
%
-spec get_superclasses() -> static_return( [ wooper:classname() ] ).


-ifdef(superclasses).

% Reuse that define, since it has been specified:
%
% Note: if your compiler points to these lines, you must have introduced a parse
% (syntax) error in your 'superclasses' define.
%
get_superclasses() ->
	wooper:return_static( ?superclasses ).


-else. % superclasses


% Specifying nothing means no superclass:
get_superclasses() ->
	wooper:return_static( [] ).


-endif. % superclasses



% Returns the user specification of the class-specific attributes.
%
% Note: function that will be discarded before the actual compilation, just
% allowing to generate a proper get_class_specific_attributes/0 static method,
% that will be thus available to the user.
%
-spec wooper_get_class_specific_attributes() ->
						   [ wooper_info:attribute_spec() ].


-ifdef(class_attributes).


% Reuse that define, since it has been specified.
%
% Note that the body of this function will be rewritten by the WOOPER parse
% transform, as a typical class_attributes define would not compile (references
% to types such as float() would be undertood as calls of non-existing
% functions).
%
% In practice, the spec of the wooper_get_class_specific_attributes/0 function
% will still be respected; currently, only a list of attribute names will be
% returned (so the extra information like qualifiers, descriptions and types
% will not be returned here).
%
wooper_get_class_specific_attributes() ->

	% Note: if your compiler points to these lines, you must have introduced a
	% parse (syntax) error in your 'class_attributes' define.
	%
	% Check that your brackets and parentheses are balanced, that commas are at
	% their expected places (and only them), etc.
	%
	?class_attributes.


-else. % class_attributes


% Specifying nothing means no superclass:
wooper_get_class_specific_attributes() ->
	[].


-endif. % class_attributes




% Returns the description (if any) specified for this class.
%
% Note: available to the user.
%
-spec get_class_description() ->
			static_return( maybe( wooper_info:class_description() ) ).



-ifdef(class_description).


% Reuse that define, since it has been specified.
%
% Note that the body of this function will be rewritten by the WOOPER parse
% transform.
%
get_class_description() ->

	% Note: if your compiler points to these lines, you must have introduced a
	% parse (syntax) error in your 'class_description' define (expected to be a
	% string).
	%
	wooper:return_static( ?class_description ).


-else. % class_attributes


% Nothing specified:
get_class_description() ->
   wooper:return_static( undefined ).


-endif. % class_attributes
