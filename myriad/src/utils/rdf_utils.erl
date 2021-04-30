% Copyright (C) 2016-2021 Olivier Boudeville
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
% Creation date: Monday, September 19, 2016.




% Gathering of various facilities in relation with RDF, i.e. 'Resource
% Description Framework'.
%
% See also:
% - https://en.wikipedia.org/wiki/Resource_Description_Framework
% - https://www.w3.org/TR/rdf11-primer/
%
% See rdf_utils_test.erl for the corresponding test.
%
-module(rdf_utils).


% See also:
% - a RDF N-Triples reader: https://github.com/tim/erlang-rdf-ntriples
% - a distributed framework for large scale graph processing:
% https://github.com/xslogic/phoebus
% - bindings for the Raptor RDF Parser Library:
% https://github.com/jonasp/erlang-raptor



% International Resource Identifier, in charge of, well, identifying a resource.
%
% This is a generalization of the URIs (Uniform Resource Identifier), allowing
% non-ASCII characters to be used; specified in RFC 3987 (see
% http://www.ietf.org/rfc/rfc3987.txt).
%
% (note: type to be used internally, for efficiency)
%
-type iri() :: text_utils:bin_string().


% Version of an IRI specified (typically by the user) as a plain (Unicode)
% string.
%
-type string_iri() :: text_utils:ustring().


% Any type of IRI:
-type any_iri () :: iri() | string_iri().


% Designates all basic values that are not IRIs, often in a textual version.
%
% Examples of literals include strings such as "La Joconde", dates such as "the
% 4th of July, 1990" and numbers such as "3.14159".
%
% See: https://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/#section-Datatypes
%
-type literal() :: any().


% Identifies uniquely what resource a RDF triplet is describing (input node).
-type subject() :: iri().


% Defines some attribute of the subject, the nature of the relationship between
% the subject and the object (directed edge).
%
-type predicate() :: iri().


% The actual value specified in the triplet; defines how the subject and object
% are related (output edge).
%
-type object() :: iri() | literal().


% A RDF statement, a property triplet (named triple as more usual here):
-type triple() :: { subject(), predicate(), object() }.


-type content() :: [ triple() ].


% Vocabulary, typically to be used for internal purposes.
-type vocabulary() :: set_utils:set( iri() ).


% Vocabulary, typically to be specified by the user (simpler form).
-type user_vocabulary() :: [ string_iri() ].


-export_type([ iri/0, string_iri/0, any_iri/0,
			   literal/0, subject/0, predicate/0, object/0, triple/0,
			   content/0, vocabulary/0, user_vocabulary/0 ]).


-export([ is_iri/1, is_literal/1, is_subject/1, is_predicate/1, is_object/1,
		  evaluate_statement/3, implies/2,
		  vocabulary_to_string/1, vocabulary_to_string/2 ]).


% Shorthands:
-type ustring() :: text_utils:ustring().



% Tells whether specified term is an IRI.
-spec is_iri( term() ) -> boolean().
is_iri( Term ) ->
	text_utils:is_bin_string( Term ).



% Tells whether specified term is a (RDF) literal.
-spec is_literal( term() ) -> boolean().
is_literal( _Term ) ->
	%text_utils:is_bin_string( Term ).
	% May not even be a text?
	true.



% Tells whether specified term is a RDF subject.
-spec is_subject( term() ) -> boolean().
is_subject( Term ) ->
	is_iri( Term ).



% Tells whether specified term is a RDF predicate.
-spec is_predicate( term() ) -> boolean().
is_predicate( Term ) ->
	is_iri( Term ).



% Tells whether specified term is a RDF object.
-spec is_object( term() ) -> boolean().
is_object( Term ) ->
	is_iri( Term ) orelse is_literal( Term ).



% Evaluates specified statement, and returns whether it holds.
-spec evaluate_statement( subject(), predicate(), object() ) -> boolean().
evaluate_statement( _Subject, _Predicate= <<"is_a">>, _Object ) ->
	% Not implemented yet:
	true;

evaluate_statement( _Subject, Predicate, _Object ) ->
	throw( { cannot_evaluate_rdf_statement, Predicate } ).



% Tells whether the first vocabulary is a subset of the second, i.e. whether the
% first properties imply that the second ones are met.
%
-spec implies( vocabulary(), vocabulary() ) -> boolean().
implies( FirstVocabulary, SecondVocabulary ) ->
	set_utils:is_subset( SecondVocabulary, FirstVocabulary ).



% Returns a textual representation of specified vocabulary.
-spec vocabulary_to_string( vocabulary() ) -> ustring().
vocabulary_to_string( Vocabulary ) ->
	vocabulary_to_string( Vocabulary, _IndentationLevel=0 ).



% Returns a textual representation of specified vocabulary, with specified
% indentation level.
%
-spec vocabulary_to_string( vocabulary(), text_utils:indentation_level() ) ->
									ustring().
vocabulary_to_string( Vocabulary, IndentationLevel ) ->

	case set_utils:to_list( Vocabulary ) of

		[] ->
			"an empty vocabulary";

		[ Elem ] ->
			text_utils:format( "a vocabulary comprising a single term, '~ts'",
							   [ Elem ] );

		SemList ->

			SemString = text_utils:binaries_to_string( SemList,
													   IndentationLevel ),

			text_utils:format( "a vocabulary comprising ~B terms: ~ts",
							   [ length( SemList ), SemString ] )

	end.
