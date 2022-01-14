% Copyright (C) 2021-2022 Olivier Boudeville
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
% Creation date: Sunday, December 5, 2021.


% @doc Gathering of management facilities for <b>XML</b> processing.
%
% See xml_utils_test.erl for the corresponding test.
%
% Mostly a wrapper around the standard, always available xmerl modules.
%
-module(xml_utils).


% Implementation notes:
%
% This XML support is based on xmerl.
%
% A good way of understanding xmerl is to read lib/xmerl/include/xmerl.hrl.
%
% See also its User's Guide: https://www.erlang.org/doc/apps/xmerl/xmerl_ug.html
%
% One may refer to src/scripts/show-xml-file.escript in order to pretty-print
% XML.



% Defining one's XML document:

% The "simple form" is the lightest, less verbose way of defining one's XML
% document out of simple-tags (see xml_simple_tag/0); we recommend using it.
%
% So for example:
%
% XMLSimpleContent = [
%   myFirstTag,
%   { mySecondTag, [ myNestedTag ] },
%   { myThirdTag,  [ { color, "red" }, { age, 71 } ],
%       [ "This is a text!" ] } ],
%
% is to produce an XML string like:
%
% <?xml version="1.0" encoding="utf-8" ?>
% <myFirstTag/>
% <mySecondTag><myNestedTag/></mySecondTag>
% <myThirdTag color="red" age="71">This is a text!</myThirdTag>
%
% Refer to xml_simple_tag/0; the other options to define XML elements is to use
% IOLists and/or XML records (see xml_element/0).
%
% Refer to xml_utils_test.erl for more examples.


% For xmerl's records:
-include_lib("xml_utils.hrl").


-type xml_text() :: ustring().
% A raw string containing XML (including markup), typically at least an extract
% of an XML document.
%
% Ex: "<birds> <bird species="crow">Arthur</bird> </birds>".


-type xml_document() :: xml_content().
% A full, standalone definition of a XML document.


-type xml_tag() :: atom().
% An XML tag; we recommend to use Upper CamelCase (PascalCase) for their name.
%
% Ex: 'AccountLog' in: <AccountLog level="red">Hello</AccountLog>.


-type xml_attribute_name() :: atom().
% The name of an attribute of a XML tag; we recommend to use (lower) camelCase
% for them.
%
% Ex: 'refCount' in: <Account refCount="4"> ...</Account>


-type xml_attribute_value() :: io_list() | atom() | integer().
% The value of an attribute of a XML tag.
%
% Ex: "4" in: <Account refCount="4"> ...</Account>


-type attribute_record() :: #xmlAttribute{}.
% Stores all information regarding an attribute in an XML tag.


-type xml_attribute() :: { xml_attribute_name(), xml_attribute_value() }
						 | attribute_record().
% The description of an attribute of a XML tag.
%
% Ex: {refCount,"4"} in: <Account refCount="4"> ...</Account>



-type xml_simple_tag() :: xml_tag()
					  | { xml_tag(), xml_content() }
					  | { xml_tag(), [ xml_attribute() ], xml_content() }.
% An XML tag, expressed in xmerl's simple form.


-type xml_iolist() :: io_list().
% An XML element defined by an iolist.


% xmerl records (refer to lib/xmerl/include/xmerl.hrl):

-type text_record() :: #xmlText{}.
% Stores all information regarding a direct text in an XML document.


-type element_record() :: #xmlElement{}.
% Stores all information regarding an element (notably having a content) in an
% XML document.


-type instruction_record() :: #xmlPI{}.
% Stores all information regarding a processing instruction in an XML document.


-type comment_record() :: #xmlComment{}.
% Stores all information regarding a comment in an XML document.


-type declaration_record() ::#xmlDecl{}.
% Stores all information regarding an XML declaration (XML version, encoding,
% etc.).


-type xml_record() :: text_record()
					| element_record()
					| instruction_record()
					| comment_record()
					| declaration_record().
% The internal records introduced by xmerl in order to denote XML elements.


-type xml_element() :: xml_simple_tag() | xml_iolist() | xml_record().
% An element of an XML document (top-level or nested).
%
% It can be expressed in simple form (one of the three options based on a
% xml_tag()), as an iolist, or based on one of the five xmerl's records.



-type xml_content() :: [ xml_element() ].
% An in-memory representation of an XML content, mostly as a tree of markup
% elements; note that this is always a flat list.
%
% See our 'Usage of xmerl' section for more details and examples.


-type xml_simple_content() :: [ xml_simple_tag() ].
% An in-memory representation of an XML content, only based on simple tags
% (hence a special case of xml_content/0).


-type xml_root_attributes() :: [ xml_attribute() ].
% The XML attributes for the root element of an XML document, the implicit
% parent of the user-specified content.
%
% May notably include a XML prolog attribute.



-type xml_prolog_value() :: xml_attribute_value().
% The value associated to the 'prolog' attribute within root attributes,
% typically an (XML-escaped string).
%
% Allows to define the header of the XML document, so that we becomes for
% example:
%
% `<?xml version="1.0" encoding="utf-8" ?>'
%
% or:
%
% `<?xml version="1.0" encoding="utf-8" ?>
% <!DOCTYPE birds SYSTEM "bird.dtd">'
%
% or:
%
% `<?xml version="1.0"?>
% <birds xmlns="http://foo.org/species" xmlns:c="http://bar.net/colors">'
%
% Another option to set the prolog is to alter the root XML element with
% attributes like: `[{xmlns, SpeciesStr}, {'xmlns:c', ColorStr}]'
%
% If the root element is an xmlElement record, then a (single) namespace may be
% specified thanks to its 'namespace' field:
% RootElem = #xmlElement{name=birds,
%                        namespace=#xmlNamespace{default=SpeciesStr}, ...

% Usage of xmerl

% Example on the shell:

% Showcasing the three simple-forms of tags:
%
% FirstParentTagAttrs = [{color,"red"}, {age,71}].
%
% % {Tag, Content} here:
% FirstChildTag = {childTag1, ["Content of child tag 1"]}.
%
% % {Tag, Attributes, Content} and a nested "just Tag" here:
% FirstParentTag = {myFirstParentTag, FirstParentTagAttrs,
%   [FirstChildTag, secondChildTag]}.
%
% MyXMLContent = [FirstParentTag, anotherParentTag].
%
% xmerl:export_simple([MyXMLContent],xmerl_xml).

% % If needing xmerl's records:
% 1> rr(code:lib_dir(xmerl) ++ "/include/xmerl.hrl").

% See xml_utils_test.erl for actual user code based on xml_utils.



-export_type([ xml_document/0, xml_text/0, xml_tag/0,

			   attribute_record/0,
			   xml_attribute_name/0, xml_attribute_value/0, xml_attribute/0,

			   % xmerl records:
			   text_record/0, element_record/0, instruction_record/0,
			   comment_record/0, declaration_record/0,

			   xml_simple_tag/0, xml_iolist/0, xml_record/0, xml_element/0,
			   xml_content/0, xml_simple_content/0,
			   xml_root_attributes/0, xml_prolog_value/0 ]).


-export([ to_xml_text/1,
		  get_default_prolog/0,
		  xml_to_string/1, xml_to_string/2, string_to_xml/1,
		  parse_xml_file/1 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().
-type any_string() :: text_utils:any_string().
-type io_list() :: text_utils:io_list().

-type any_file_path() :: file_utils:any_file_path().


% @doc Escapes specified text (not expected to contain markup), so that it can
% be included safely within an XML content.
%
-spec to_xml_text( any_string() ) -> ustring().
to_xml_text( Text ) ->
	% As our HTML escaping goes a little beyond the strictly necessary and
	% matches the XML requirements:
	%
	web_utils:escape_as_html_content( Text ).



% @doc Returns the default prolog value, corresponding to:
% `<?xml version="1.0" encoding="utf-8" ?>.'
%
-spec get_default_prolog() -> xml_prolog_value().
get_default_prolog() ->
	"<?xml version=\"1.0\" encoding=\"utf-8\" ?>".



% @doc Returns a string corresponding to the specified XML content structure,
% using the default XML prolog, corresponding to:
% `<?xml version="1.0" encoding="utf-8" ?>.'
%
% The returned XML serialised form is ready to be written on a file, sent over
% the network, etc.
%
-spec xml_to_string( xml_content() ) -> io_list().
xml_to_string( XMLContent ) ->
	xml_to_string( XMLContent, get_default_prolog() ).


% @doc Returns a string corresponding to the specified XML content structure,
% using the specified prolog.
%
% The returned XML serialised form is ready to be written on a file, sent over
% the network, etc.
%
-spec xml_to_string( xml_content(), xml_prolog_value() ) -> ustring().
xml_to_string( XMLContent, PrologValue ) ->

   % See
   % https://www.erlang.org/doc/apps/xmerl/xmerl_ug.html#example--create-xml-out-of-arbitrary-data:

	%trace_utils:debug_fmt( "XML content to serialise:~n ~p", [ XMLContent ] ),

	IOList = xmerl:export_simple( XMLContent, xmerl_xml,
								  [ { prolog, PrologValue } ] ),

	text_utils:flatten( IOList ).



% @doc Returns an XML content structure resulting from the scan of the specified
% string expected to contain an XML document.
%
% The returned XML content is a simple one, i.e. it is exclusively made of
% simple tags (no XML records).
%
-spec string_to_xml( ustring() ) -> xml_content().
string_to_xml( XMLStr ) ->

	% See parse_xml_file/1.

	SpaceFlag = normalize, % rather than preserve
	{ Document, _Rest } = xmerl_scan:string( XMLStr, [ { space, SpaceFlag } ] ),

	[ CleanedContent ] = xmerl_lib:remove_whitespace( [ Document ] ),

	xmerl_lib:simplify_element( CleanedContent ).



% @doc Returns an XML content structure resulting from the scan of the specified
% file expected to contain an XML document.
%
% The returned XML content is a simple one, i.e. it is exclusively made of
% simple tags (no XML records).
%
-spec parse_xml_file( any_file_path() ) -> xml_content().
parse_xml_file( FilePath ) ->

	% Directly deriving from
	% https://medium.com/erlang-battleground/the-hidden-xml-simplifier-a5f66e10c928:

	SpaceFlag = normalize, % rather than preserve
	{ Document, _Rest } = xmerl_scan:file( FilePath, [ { space, SpaceFlag } ] ),

	[ CleanedContent ] = xmerl_lib:remove_whitespace( [ Document ] ),

	xmerl_lib:simplify_element( CleanedContent ).
