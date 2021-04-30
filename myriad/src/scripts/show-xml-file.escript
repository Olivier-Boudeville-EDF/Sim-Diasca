#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname show_xml_file

% This script shows how to use xmerl and is intended to be used as an help to
% understand the structure of an XML file. With a good text editor, we recommend
% redirecting its output to a .erl file, in which it is then possible to
% navigate freely.
%
% The algorithm is mostly taken from Dave Kuhlman's personal page and credits go
% to him: http://www.davekuhlman.org/notes-on-xml-and-xmerl.html


% Includes the xmerl tools:
-include_lib("xmerl/include/xmerl.hrl").


% Message to display as usage information:
display_usage() ->
	io:format( "~cUsage: show_xml_file.escript XML_FILE_PATH~n~n"
		"~cDisplays sequentially in a {name,Value} tree the structure of"
		" specified XML file (XML elements along with their XML attributes).~n",
		[ 9, 9 ] ).



% Entry point of the script.
main( [ "-h" ] ) ->
	display_usage();

main( [ "--help" ] ) ->
	display_usage();

main( [ FileName ] ) ->

	io:format( "~cParsing XML file: ~s~n~n", [ 9, FileName ] ),

	case exists( FileName ) of

		true ->

			{ FileContent, _Misc } = xmerl_scan:file( FileName ),

			show_node( _Level=0, _Node=FileContent );

		false ->

			io:format( "~cError: file '~s' could not be found.",
					   [ 9, FileName ] ),

			throw( { file_not_found, FileName } )

	end;


main( _FileName ) ->
	io:format( "~cError, exactly one parameter should be specified.~n",
			   [ 9 ] ),
	 display_usage().




% Shows a node/element and then the children of that node.
show_node( Level, Node) ->

	case Node of

		#xmlElement{ name=Name, attributes=Attributes, content=Content } ->
			show_indent( Level ),
			io:format( "name: ~s~n", [ Name ] ),
			show_attributes( Level + 1, Attributes ),
			show_children( Level + 1, Content );

		#xmlText{ value=Value } ->
			if
				hd( Value ) =/= hd( "\n" ) ->
					show_indent( Level ),
					io:format( "Text: ~p~n", [ Value ] );

				true ->
					ok

			end;

		_ ->
			ok

	end.



% Shows all direct children of a node.
show_children( _Level, [] ) ->
	ok;

show_children( Level, [ Node | MoreNodes ] ) ->
	show_node( Level, Node ),
	show_children( Level, MoreNodes ).



% Shows the attributes of a node.
show_attributes( _Level, [] ) ->
	ok;

show_attributes( Level,
		   [ #xmlAttribute{ name=Name, value=Value } | MoreAttributes ] ) ->
	show_indent( Level ),
	io:format( "Attribute -- ~s: ~s~n", [ Name, Value ] ),
	show_attributes( Level, MoreAttributes ).

show_indent( Level ) ->
	Seq = lists:seq( 1, Level ),
	F = fun(_) ->
			io:format( "~c", [ 9 ] )
		end,
	lists:foreach( F, Seq ).




% Verbatim duplication section.
%
% Probably that using myriad/src/utils/script_utils.erl would be better than
% duplicating code here.


% We want this escript to be standalone, thus we copy here "verbatim" (module
% references fixed) the functions of interest, so that there is no prerequisite.


% Taken from myriad/src/utils/file_utils.erl:


% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Tells whether specified file entry exists, regardless of its type.
exists( EntryName ) ->
	case file:read_file_info(EntryName) of

		{ok,_FileInfo} ->
			true;

		{error,_Reason} ->
			false

	end.


% End of the myriad/src/utils/file_utils.erl section.
