% Copyright (C) 2007-2025 Olivier Boudeville
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
% Creation date: 2007.


% Modular WOOPER header gathering the primitives (functions) to manage the state
% of an instance.


% This header mostly defines functions, so it should be included late in source
% files, not to prevent them from declaring exports afterwards.

% Note: a @headerfile "wooper_state_functions.hrl" tag was added in various
% places of wooper.erl with no luck (doc tags were always ignored).


% To avoid overspecialisation of its result:
-dialyzer( { nowarn_function, is_wooper_debug/0 } ).



-doc """
Tells whether the debug mode of WOOPER is enabled.

Voluntary underspecification, to be able to toggle.
""".
-spec is_wooper_debug() -> boolean().



% On debug mode, various additional checkings are enabled:
%
% (put in an header, as different settings might apply to different classes)
%
-ifdef(wooper_debug_mode).

is_wooper_debug() ->
	true.

-else. % wooper_debug_mode

is_wooper_debug() ->
	false.

-endif. % wooper_debug_mode



% These frequent operations must be as fast as possible:
%
% (not recommended functions, i.e. hasAttribute/2 and removeAttribute/2, shall
% not be inlined)
%
-compile( { inline, [ setAttribute/3, setAttributes/2,
					  swapInAttribute/3,
					  getAttribute/2, getAttributes/2, getMaybeAttribute/2,
					  addToAttribute/3, subtractFromAttribute/3,
					  incrementAttribute/2, decrementAttribute/2,
					  toggleAttribute/2,
					  appendToAttribute/3, concatToAttribute/3,
					  deleteFromAttribute/3,
					  addKeyValueToAttribute/4, popFromAttribute/2 ] } ).



% Below are listed the correct function-based version (to be inlined), as
% opposed to the faulty macro-based implementations (see
% wooper_state_exports.hrl):



-doc """
Sets the specified attribute of the instance to the specified value, based on
the specified state.

Returns an updated state.

Always succeeds.

See also: setAttributes/3, to set more than one attribute at a time.
""".
-spec setAttribute( wooper:state(), attribute_name(), attribute_value() ) ->
						wooper:state().
setAttribute( State, AttributeName, AttributeValue ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:add_entry(
			AttributeName,
			AttributeValue,
			State#state_holder.attribute_table ) }.



-doc """
Sets a list of attribute/value pairs in the specified state.

The expected parameter is a list of pairs (2-element tuples), each pair
containing in first position the attribute name and in second one the attribute
value.

Returns an updated state.

Always succeeds.

See also: the setAttribute function.
""".
-spec setAttributes( wooper:state(), [ attribute_entry() ] ) ->
						wooper:state().
setAttributes( State, ListOfAttributePairs ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:add_entries(
			ListOfAttributePairs,
			State#state_holder.attribute_table ) }.



-doc """
Swaps in the specified state the current value of the specified attribute
with the specified value.

Returns an updated state and the previous value of that attribute.

For example, if the 'color' attribute happened to be previously set to 'red' in
SomeState, then: `{NewState, red} = swapInAttribute(SomeState, color, blue)` and
in NewState 'color' is set to 'blue'.
""".
-spec swapInAttribute( wooper:state(), attribute_name(), attribute_value() ) ->
										{ wooper:state(), attribute_value() }.
swapInAttribute( State=#state_holder{ attribute_table=AttrTable },
				 AttributeName, NewAttributeValue ) ->

	{ PreviousValue, NewAttrTable } = ?wooper_table_type:swap_value(
		AttributeName, NewAttributeValue, AttrTable ),

	{ State#state_holder{ attribute_table=NewAttrTable }, PreviousValue }.



-doc """
Tells whether the specified attribute exists.

Note: the best practise is certainly to set all possible attributes from the
constructor, either to an appropriate value or to `undefined`, instead of having
instances with or without a given attribute.

Note: not expected to be ever used by user code, as, except when relying on
composition over inheritance (where an attribute may not be even defined), all
attributes should be defined directly in the constructor (hence no attribute
could appear later).
""".
-spec hasAttribute( wooper:state(), attribute_name() ) -> boolean().
hasAttribute( State, AttributeName ) ->
	?wooper_table_type:has_entry( AttributeName,
								  State#state_holder.attribute_table ).



-doc """
Returns the value associated to the specified name-designated attribute, if
found, otherwise triggers a case clause error.

Note: not used very frequently verbatim, as either the attribute value can be
obtained with the `getAttr/1` macro, using the original state, named as `State`
(as externally defined) or the value is already bound by design to an available
variable.

See also: the `getAttr/1` macro shorthand.
""".
-spec getAttribute( wooper:state(), attribute_name() ) -> attribute_value().
getAttribute( State, AttributeName ) ->
	?wooper_table_type:get_value( AttributeName,
								  State#state_holder.attribute_table ).



-doc """
Returns the value associated to each of the specified name-designated attributes
(if found, otherwise triggers a case clause error), in the order of their
specification.

For example: `[MyCount, MyAge, MyIdeas] = getAttribute(SomeState, [count, age,
ideas])`.

See also: the `getAttrList/1` macro shorthand.
""".
-spec getAttributes( wooper:state(), [ attribute_name() ] ) ->
							[ attribute_value() ].
getAttributes( State, AttributeNameList ) ->
	?wooper_table_type:get_values( AttributeNameList,
								   State#state_holder.attribute_table ).



-doc """
Returns the value, if any, associated to the specified named-designated
attribute, if found, otherwise returns `undefined`.

Note that an ambiguity exists if the attribute value belongs to a type that
comprises the `undefined` atom, in the sense that an attribute set to a value
equal to 'undefined' cannot be then discriminated from an attribute not set at
all. For example, instead of a option-type, use then a safe_option-type.

Useful only in specific contexts, like when preferring composition over
inheritance, where an attribute may not be even defined.
""".
-spec getMaybeAttribute( wooper:state(), attribute_name() ) ->
								option( attribute_value() ).
getMaybeAttribute( State, AttributeName ) ->
	case ?wooper_table_type:lookup_entry( AttributeName,
			State#state_holder.attribute_table ) of

		key_not_found ->
			undefined;

		{ value, V } ->
			V

	end.



-doc """
Returns an updated state not having anymore the specified attribute.

No error is triggered if the specified attribute was not existing.

Note: this operation is not recommended, as attributes should always be
defined. Better keep it defined, but set to `undefined`.
""".
-spec removeAttribute( wooper:state(), attribute_name() ) -> wooper:state().
removeAttribute( State, AttributeName ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:remove_entry( AttributeName,
			State#state_holder.attribute_table ) }.



-doc """
Adds the specified value to the specified attribute, supposed to be a number.

Returns an updated state.

A case clause is triggered if the attribute did not exist, a bad arithm is
triggered if no addition can be performed on the attribute value.
""".
-spec addToAttribute( wooper:state(), attribute_name(), attribute_value() ) ->
							wooper:state().
addToAttribute( State, AttributeName, Value ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:add_to_entry(
			AttributeName,
			Value,
			State#state_holder.attribute_table ) }.



-doc """
Subtracts the specified value from the specified attribute, supposed to be a
number.

Returns an updated state.

A case clause is triggered if the attribute did not exist, a bad arithm is
triggered if no subtraction can be performed on the attribute value.
""".
-spec subtractFromAttribute( wooper:state(), attribute_name(),
							 attribute_value() ) -> wooper:state().
subtractFromAttribute( State, AttributeName, Value ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:subtract_from_entry(
			AttributeName,
			Value,
			State#state_holder.attribute_table ) }.



-doc """
Increments the specified attribute, supposed to be a number.

Returns an updated state.

A case clause is triggered if the attribute did not exist, a bad arithm is
triggered if no addition can be performed on the attribute value.
""".
-spec incrementAttribute( wooper:state(), attribute_name() ) -> wooper:state().
incrementAttribute( State, AttributeName ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:add_to_entry(
			AttributeName,
			_Value=1,
			State#state_holder.attribute_table ) }.



-doc """
Decrements the specified attribute, supposed to be a number.

Returns an updated state.

A case clause is triggered if the attribute did not exist, a bad arithm is
triggered if no addition can be performed on the attribute value.
""".
-spec decrementAttribute( wooper:state(), attribute_name() ) -> wooper:state().
decrementAttribute( State, AttributeName ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:add_to_entry(
			AttributeName,
			_Value=-1,
			State#state_holder.attribute_table ) }.



-doc """
Returns an updated state in which specified boolean attribute is toggled: if
true will be false, if false will be true.

A case clause is triggered if the attribute does not exist or it is not a
boolean value.
""".
-spec toggleAttribute( wooper:state(), attribute_name() ) -> wooper:state().
toggleAttribute( State, BooleanAttributeName ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:toggle_entry(
			BooleanAttributeName,
			State#state_holder.attribute_table ) }.



-doc """
Appends the specified element to specified attribute, supposed to be a list. A
case clause is triggered if the attribute did not exist.

Returns an updated state.

Note: no check is performed to ensure the attribute is a list indeed, and the
operation will not complain if not.
""".
-spec appendToAttribute( wooper:state(), attribute_name(),
						 attribute_value() ) -> wooper:state().
appendToAttribute( State, AttributeName, Element ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:append_to_entry(
			AttributeName,
			Element,
			State#state_holder.attribute_table ) }.



-doc """
Concatenes (on the left) the specified prefix list to specified attribute,
supposed to be a list as well. A case clause is triggered if the attribute did
not exist.

If that attribute is not already defined, it will be created and associated to
the specified list (as if beforehand it was associated to an empty list).

Returns an updated state.

Note: no check is performed to ensure the attribute is a list indeed, and the
operation will not complain if not.
""".
-spec concatToAttribute( wooper:state(), attribute_name(),
						 attribute_value() ) -> wooper:state().
concatToAttribute( State, AttributeName, List ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:concat_to_entry(
			AttributeName,
			List,
			State#state_holder.attribute_table ) }.



-doc """
Deletes the first match of the specified element from the specified attribute,
supposed to be a list.

A case clause is triggered if the attribute did not exist.
If the element is not in the specified list, the list will not be modified.

Returns an updated state.
""".
-spec deleteFromAttribute( wooper:state(), attribute_name(),
						   attribute_value() ) -> wooper:state().
deleteFromAttribute( State, AttributeName, Element ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:delete_from_entry(
			AttributeName,
			Element,
			State#state_holder.attribute_table ) }.



-doc """
Assumes the specified attribute is an hashtable, and adds the specified entry
(key/value pair) to it.

Returns an updated state.

Several lines compacted into a bit impressive one-liner.

Note: to be used with much caution, as a class may use a type of table unrelated
to the one used by WOOPER (on the other hand we do not want to force all classes
to define 'table_type').
""".
-spec addKeyValueToAttribute( wooper:state(), attribute_name(),
			?wooper_table_type:key(), ?wooper_table_type:value() ) ->
									wooper:state().
addKeyValueToAttribute( State, AttributeName, Key, Value ) ->
	State#state_holder{
		attribute_table=?wooper_table_type:add_entry(

			AttributeName,

			?wooper_table_type:add_entry( Key, Value,
				?wooper_table_type:get_value( AttributeName,
					State#state_holder.attribute_table ) ),

			State#state_holder.attribute_table ) }.



-doc """
Removes the head from the specified attribute, supposed to be a list, and
returns a {NewState, PoppedHead} pair.

For example, if the attribute 'my_list' contains [5,8,3], executing:
`{PoppedState, Head} = ?popFromAttribute(State, my_list)` returns a state whose
my_list attribute is [8,3] and a value Head=5.

A case clause is triggered if the attribute did not exist.
""".
-spec popFromAttribute( wooper:state(), attribute_name() ) ->
							{ wooper:state(), attribute_value() }.
popFromAttribute( State, AttributeName ) ->

	{ Head, PoppedAttributeTable } = ?wooper_table_type:pop_from_entry(
		AttributeName, State#state_holder.attribute_table ),

	{ State#state_holder{ attribute_table=PoppedAttributeTable }, Head }.
