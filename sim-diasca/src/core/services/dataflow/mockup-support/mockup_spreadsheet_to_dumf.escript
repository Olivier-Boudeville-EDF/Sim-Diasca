#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname mockup_spreadsheet_to_dumf

% Author: Robin Huart (robin-externe.huart@edf.fr)

%
% ==============================================================================
%
% This script translates a spreadsheet obeying the 'Sim-Diasca Mockup
% Spreadsheet Conventions' to a file respecting the DUMF format, which is the
% native format for mockup specification files in Sim-Diasca.
%
% The terminology relevant here defines a spreadsheet as a set of worsheets
% (sometimes simply referred to as "sheets"). One .xlsx or .ods file stores one
% spreadsheet.
%
% Please refer to the 'SimDiasca Dataflow HOWTO' for more information about
% these formats.
%
% ------------------------------------------------------------------------------
%
% Notes to the user:
%
%  - Modifying the layout of the reference spreadsheet should be avoided as much
%    as possible. We do not guarantee this script will work otherwise.
%
%  - The format of the input spreadsheet can be either Excel (".xlsx") or
%    OpenDocument Spreadsheet (".ods").
%
%  - The Excel version currently tested is Excel 2013.
%
%  - The output DUMF file is named simply by replacing the original extension of
%    the input file with ".dumf".
%
% ------------------------------------------------------------------------------
%
% Notes to the developper:
%
%  - In the XLSX format, the XML sheetData sections do not seem to mention cells
%    lying outside the table created by the user, hence we can safely increment
%    the column counter while building an intermediate 'raw array' data struct.
%
%  - In the ODS format, however, such unwanted cells are listed in the XML table
%    sections. Hence a great care has to be taken when trying to build an
%    intermediate 'raw array' if we do not want the width of the left margin (in
%    the sheet) to have any impact on the final indices.
%
%  - In both formats, blank left margins are correctly ignored by the present
%    version of this script, but we do not really know if this behaviour will be
%    robust against changes in the layouts of sheets. Fortunately, this should
%    not (hopefully) impact the parsing of intermediate 'raw arrays'.
%
%  - During a debug process involving playing with XML files, a very useful
%    script can be used to understand their organisation much more easily:
%    show_xml_file.escript (gets rid of a big part of the useless information)
%
% ==============================================================================



% Includes the xmerl tools (Erlang base XML parser):
-include_lib("xmerl/include/xmerl.hrl").



%%
%%
%% -------- SPREADSHEET CONSTANTS --------
%%
%%

% The static list of expected sheet titles for mockup definitions:
expected_sheet_titles() ->
	[ "Unit Mockup Metadata", "Input Port Specifications",
	  "Output Port Specifications", "Mockup Clauses" ].




%%
%%
%% -------- MAIN SECTION --------
%%
%%



% Displays usage information.
%
get_usage() ->
	io:format( "Usage: mockup_spreadsheet_to_dumf.escript my_file.[xlsx|ods]~n"
			   "~nTurns a user-provided mockup specification spreadsheet into "
			   "a DUMF mockup specification file (e.g. MyUnit.xlsx or "
			   "MyUnit.ods into MyUnit.dumf).~nPlease refer to the 'SimDiasca "
			   "Dataflow HOWTO' for information about the Sim-Diasca Mockup "
			   "Spreadsheet Conventions or the native DUMF file format.~n~n" ).



% Entry point of the script:
main( [ "-h" ] ) ->
	get_usage();

main( [ "--help" ] ) ->
	get_usage();

main( [ "--debug", FileName ] ) ->
	main( [ FileName, true ] );

main( [ FileName, "--debug" ] ) ->
	main( [ FileName, true ] );

main( [ FileName ] ) ->
	main( [ FileName, false ] );

main( [ FileName, DebugMode ] ) ->

	case filename:extension( FileName ) of

		".xlsx" ->
			main_xlsx( FileName, expected_sheet_titles(), DebugMode );

		".ods" ->
			main_ods( FileName, expected_sheet_titles(), DebugMode );

		OtherExtension ->
			io:format( "~n~cError: file format with extension ~p not supported "
					   "~n~n", [ 9, OtherExtension ] ),
			get_usage()

	end;

main( _ArgList ) ->
	io:format( "~cError: exactly one parameter should be specified.~n~n",
			   [ 9 ] ),
	get_usage().




% Excel, XLSX-specific execution path:
main_xlsx( FileName, ExpectedSheetTitles, DebugMode ) ->

	io:format( "Generating a DUMF file from the Excel spreadsheet stored in "
			   "the file: ~s...~n", [ FileName ] ),

	% Checks the existence of the input '.xlsx' file:
	check_file_exists( FileName, DebugMode ),

	% Unzips the .xlsx file and finds the .xml files to parse:
	{ TmpDir, [ SharedStringsFile | Worksheets ], DUMFFileName } =
		unzip_xlsx_file( FileName, ExpectedSheetTitles, DebugMode ),

	% Note: the resulting ordering in Worksheets follows ExpectedSheetTitles.

	% Reads the table of "shared strings", which stores all the formatted data
	% originally contained in the Excel document, without duplicates:
	%
	display_debug( "Parsing the XML files...~n", DebugMode ),
	SharedStrings = read_shared_strings_list( SharedStringsFile ),

	% Reads all the worksheets and, for each of them, generates a raw array
	% listing its content in a { Row, Column, Value } format:
	%
	display_debug( "Parsing now ~B worksheets:~n", [ length( Worksheets ) ],
				   DebugMode ),

	TitledWorksheets = lists:zip( Worksheets, ExpectedSheetTitles ),
	SheetsData = [ parse_worksheet( WS, SharedStrings, Title, DebugMode )
				   || { WS, Title } <- TitledWorksheets ],

	% Parsed raw data can be displayed in debug mode:
	[ display_debug( "Sheet: ~p~n~n", [ SheetData ], DebugMode )
	  || SheetData <- SheetsData ],

	display_debug( "Worksheets parsed.~n", [], DebugMode ),

	% Converts the SheetsData arrays to raw data close to the DUMF format:
	display_debug( "Parsing the extracted content...~n", DebugMode ),
	DUMFRawData = parse_raw_data_arrays( SheetsData, ExpectedSheetTitles,
										 DebugMode ),

	% Formats these raw data to the DUMF format and writes the result:
	display_debug( "Generating the DUMF file...~n", DebugMode ),
	write_DUMF_file( DUMFFileName, DUMFRawData ),

	% Removes all the temporarily extracted files and TmpDir itself:
	display_debug( "Cleaning...~n", DebugMode ),
	remove_directory( TmpDir ).



% Unzips the .xlsx archive file into a temporary work directory and tries to
% find the XML files that are to be parsed:
%
unzip_xlsx_file( FileName, ExpectedSheetTitles, DebugMode ) ->

	% Sets the name of the directory in which to unzip the file:
	AbsFileName = filename:absname( FileName ),
	RootFileName = filename:rootname( AbsFileName, ".xlsx" ),
	TmpDirName = RootFileName ++ "_xlsx_tmp_dir",

	% Creates the temporary work directory and unzips the .xlsx file in it:
	file:make_dir( TmpDirName ),
	display_debug( "Unzipping the Excel file in the '~s' directory...~n",
				   [ TmpDirName ], DebugMode ),

	UnzipCommand = get_unzip() ++ io_lib:format( " ~s -d ~s",
												 [ FileName, TmpDirName ] ),
	os:cmd( UnzipCommand ),

	% Finds the workbook referencing worksheets:
	Workbook = filename:join( [ TmpDirName, "xl", "workbook.xml" ] ),
	check_file_exists( Workbook, DebugMode ),

	% Finds the sheet files whose titles match an expected one:
	WorksheetBaseNames =
		extract_sheets_from_workbook( Workbook, ExpectedSheetTitles ),

	Worksheets = [ filename:join( [ TmpDirName, "xl", "worksheets", WSBN ] )
				   || WSBN <- WorksheetBaseNames ] ,

	[ check_file_exists( WS, DebugMode ) || WS <- Worksheets ],

	% Finds the sharedStrings file storing the formatted contents of all sheets:
	SharedStrings = filename:join( [ TmpDirName, "xl", "sharedStrings.xml" ] ),
	check_file_exists( SharedStrings, DebugMode ),

	% Generates the path to the DUMF file created in the end of this script:
	DUMFFileName = RootFileName ++ ".dumf",

	% Returns the temporary directory (for final deletion) and the paths to the
	% files to parse:
	%
	{ TmpDirName, [ SharedStrings | Worksheets ], DUMFFileName }.




% ODS-specific execution path:
main_ods( FileName, ExpectedSheetTitles, DebugMode ) ->

	io:format( "Generating a DUMF file from the ODF spreadsheet stored in "
			   "the file: ~s...~n", [ FileName ] ),

	% Checks the existence of the input '*.ods' file:
	check_file_exists( FileName, DebugMode ),

	% Unzips the .ods file and finds the content.xml file to parse:
	{ TmpDir, ContentFile, DUMFFileName } =
		unzip_ods_file( FileName, DebugMode ),

	% Reads the content file and generates raw arrays listing the contents of
	% each sheet in a { Title, Array } format, where each array is in a { Row,
	% Column, Value } format:
	%
	display_debug( "Parsing the XML content file...~n", DebugMode ),
	RawDataArrays = parse_ods_content_file( ContentFile, DebugMode ),

	% Parsed raw data can be displayed in debug mode:
	[ display_debug( "Array: ~p~n~n", [ RDA ], DebugMode )
	  || RDA <- RawDataArrays ],

	% Converts the SheetsData arrays to raw data close to the DUMF format:
	display_debug( "Parsing the extracted content...~n", DebugMode ),
	DUMFRawData = parse_raw_data_arrays( RawDataArrays, ExpectedSheetTitles,
										 DebugMode ),

	% Formats these raw data to the DUMF format and writes the result:
	display_debug( "Generating the DUMF file...~n", DebugMode ),
	write_DUMF_file( DUMFFileName, DUMFRawData ),

	% Removes all the temporarily extracted files and TmpDir itself:
	display_debug( "Cleaning...~n", DebugMode ),
	remove_directory( TmpDir ).



% Unzips the .ods archive file into a temporary work directory and tries to find
% the content.xml file that is to be parsed:
%
unzip_ods_file( FileName, DebugMode ) ->

	% Sets the name of the directory in which to unzip the file:
	AbsFileName = filename:absname( FileName ),
	RootFileName = filename:rootname( AbsFileName, ".ods" ),
	TmpDirName = RootFileName ++ "_ods_tmp_dir",

	% Creates the temporary work directory and unzips the .ods file in it:
	file:make_dir( TmpDirName ),

	display_debug( "Unzipping the ODS file in ~s...~n", [ TmpDirName ],
				   DebugMode ),

	UnzipCommand = get_unzip() ++ io_lib:format( " ~s -d ~s",
												 [ FileName, TmpDirName ] ),
	os:cmd( UnzipCommand ),

	% Finds the workbook referencing worksheets:
	ContentFile = filename:join( [ TmpDirName, "content.xml" ] ),
	check_file_exists( ContentFile, DebugMode ),

	% Generates the path to the DUMF file created in the end of this script:
	DUMFFileName = RootFileName ++ ".dumf",

	% Returns the temporary directory (for final deletion) and the paths to the
	% files to parse:
	%
	{ TmpDirName, ContentFile, DUMFFileName }.



%%
%%
%% -------- XLSX: WORKBOOK PARSING SECTION --------
%%
%%



% Extracts xmlElements associated with sheets and looks for files matching their
% expected titles:
%
extract_sheets_from_workbook( WorkbookFileName, ExpectedTitles ) ->

	% Scans the workbook with xmerl:
	{ FileContent, _Misc } = xmerl_scan:file( WorkbookFileName ),

	% Gets the xmlElement storing the list of available sheets:
	SheetsElement = lists:keyfind( 'sheets', #xmlElement.name,
								   FileContent#xmlElement.content ),

	% Builds the ordered list of sheet file names associated with these titles:
	SheetElements = SheetsElement#xmlElement.content,
	[ identify_sheet_from_title( SheetElements, Title )
	  || Title <- ExpectedTitles ].



% Tries to find, with an expected (mandatory) sheet title, a sheet xmlElement
% whose 'name' attribute is matching. If successful, returns the associated
% filename deduced from the rank stored in the 'r:id' attribute:
%
identify_sheet_from_title( _SheetElements=[], Title ) ->
	io:format( "~cError: no worksheet matches the title '~s'.~n",
			   [ 9, Title ] ),
	throw( { unmatched_title, Title } );

identify_sheet_from_title( [ SheetElement | T ], Title ) ->

	% Gets the name of the current sheet (as registered in the workbook):
	SheetNameAttr = lists:keyfind( 'name', #xmlAttribute.name,
								   SheetElement#xmlElement.attributes ),
	SheetName = SheetNameAttr#xmlAttribute.value,

	% Tries to match the sheet name with the expected title:
	case string:equal( SheetName, Title ) of

		% If matched, returns the filename (which has the form "sheet" ++ Id ++
		% ".xml", Id being deduced from an attribute) of the sheet:
		true ->
			SheetRIdAttr = lists:keyfind( 'r:id', #xmlAttribute.name,
										  SheetElement#xmlElement.attributes ),
			SheetRId = SheetRIdAttr#xmlAttribute.value,
			SheetIdString = string:substr( SheetRId, 4 ),
			"sheet" ++ SheetIdString ++ ".xml";

		% If not matched, tries the next worksheet registered in the workbook:
		false ->
			identify_sheet_from_title( T, Title )

	end.



%%
%%
%% -------- XLSX: SHARED STRINGS PARSING SECTION --------
%%
%%



% Reads the list of shared strings from the associated file:
read_shared_strings_list( SharedStringsFile ) ->

	% Gets the whole content of the file:
	{ FileContent, _Misc } = xmerl_scan:file( SharedStringsFile ),

	% Gets the list from a recursion on content singletons, and reverses it in
	% order to preserve the original ordering used by references in sheets:
	%
	lists:reverse( parse_shared_strings( FileContent#xmlElement.content,
										 _Acc=[] ) ).



% Tail-recursively extracts the strings from text elements:
parse_shared_strings( _SIElements=[], Acc ) ->
	Acc;

parse_shared_strings( [ SIElement | T ], Acc ) ->

	% Extracts the single content, a text element, from the current SIElement:
	[ TextElement ] = SIElement#xmlElement.content,

	% Extracts the single content, a #xmlText record, from this TextElement:
	[ XmlText ] = TextElement#xmlElement.content,

	% Appends the actual text to Acc and continues the recursion:
	NewAcc = [ XmlText#xmlText.value | Acc ],

	parse_shared_strings( T, NewAcc ).



%%
%%
%% -------- XLSX: WORKSHEETS PARSING SECTION --------
%%
%%



% Parses a worksheet in order to build a row array indexing all its contents:
parse_worksheet( Worksheet, SharedStrings, Title, DebugMode ) ->

	display_debug( " - parsing worksheet '~s'...~n", [ Worksheet ], DebugMode ),

	% Extracts the part of the XML file containing data that matter:
	{ SheetContent, _Misc } = xmerl_scan:file( Worksheet ),

	% Extracts the single SheetData section forming SheetContent:
	[ SheetData ] = pick_elements_with_name( 'sheetData', SheetContent,
											 DebugMode ),

	% Gets the row xmlElements of the current sheet (expectedly its only
	% elements, yet using a filter in order to increase the robustness of this
	% script):
	%
	SheetRows = pick_elements_with_name( 'row', SheetData, DebugMode ),

	% Parses these rows before returning the extracted list(s) of data with the
	% corresponding sheet title aside:
	%
	ReversedRawData = parse_sheet_rows( SheetRows, SharedStrings, 0, _Acc=[],
										DebugMode ),

	{ Title, lists:reverse( ReversedRawData ) }.



% Parses the row xmlElements in order to extract data of their cells:
parse_sheet_rows( _SheetRows=[], _SharedStrings, _RowIndex, Acc,
				  DebugMode ) ->
	display_debug( "Sheet rows parsed.~n", [], DebugMode ),
	Acc;

% A row xmlElement without any content denotes an empty row:
parse_sheet_rows( [ SheetRow | T ], SharedStrings, RowIndex, Acc,
				  DebugMode )
  when SheetRow#xmlElement.content == [] ->
	parse_sheet_rows( T, SharedStrings, RowIndex, Acc, DebugMode );

parse_sheet_rows( [ SheetRow | T ], SharedStrings, RowIndex, Acc,
				  DebugMode ) ->

	%display_debug( "Parsing sheet row '~p'.~n", [ SheetRow ], DebugMode ),
	display_debug( "Parsing a sheet row.~n", [], DebugMode ),

	% Gets the cell xmlElements from the current row (expectedly its only
	% element, yet using a filter in order to increase the robustness of this
	% script):
	%
	RowCells = pick_elements_with_name( 'c', SheetRow, DebugMode ),

	% Parses these cell xmlElements in order to build a raw data array:
	NewAcc = parse_row_cells( RowCells, SharedStrings, RowIndex, 0, Acc,
							  DebugMode ),

	% Detects the empty rows declaring a content (unlikely but still, to do so,
	% simply checks if a row modified the Acc array) and parses the next rows:
	%
	case NewAcc of

		Acc ->
			parse_sheet_rows( T, SharedStrings, RowIndex, Acc, DebugMode );

		_NonEmptyRow ->
			parse_sheet_rows( T, SharedStrings, RowIndex+1, NewAcc, DebugMode )

	end.



% Parses the cell xmlElements in order to build the array storing their values
% along with their relative coordinates:
%
parse_row_cells( _RowCells=[], _SharedStrings, _RowIndex, _ColumnIndex, Acc,
				 _DebugMode ) ->
	Acc;

% A cell xmlElement without any content denotes an empty cell:
parse_row_cells( [ RowCell | T ], SharedStrings, RowIndex, ColumnIndex,
				 Acc, DebugMode ) when RowCell#xmlElement.content == [] ->

	parse_row_cells( T, SharedStrings, RowIndex, ColumnIndex+1, Acc,
					 DebugMode );

parse_row_cells( [ RowCell | T ], SharedStrings, RowIndex, ColumnIndex, Acc,
				 DebugMode ) ->

	% Gets the only text xmlElement forming the content of non-empty cells:
	[ TextElement ] = pick_elements_with_name( 'v', RowCell, DebugMode ),

	%io:format( "TextElement: '~p'~n", [ TextElement ] ),

	% Checks the 't' attribute that is either not defined or whose value is 's',
	% depending on the original type of the cell value:
	%
	CellValue = case get_attribute_value( 't', RowCell ) of

		% When the 't' attribute is not defined or its value is "str", the
		% string storing the cell value correctly represents the original value:
		%
		SimpleAttr when SimpleAttr =:= undefined
						orelse SimpleAttr =:= "str" ->
			get_text_value( TextElement );

		% When it is defined and its value is "s", the string storing the cell
		% value has to be interpreted as an index referring to another string
		% stored in the SharedStrings list, this latter correctly representing
		% the original value:
		%
		"s" ->
			SharedStringRefString = get_text_value( TextElement ),
			SharedStringReference = list_to_integer( SharedStringRefString ),
			lists:nth( SharedStringReference+1, SharedStrings );

		% When defined with the value "b", the string storing the cell value
		% represents a boolean type, whereafter the coming text value is either
		% "1" for TRUE or "0" for FALSE. We will change here this type to a
		% classical string: TRUE and FALSE will become "true" and "false", resp.
		"b" ->
			BooleanStr = get_text_value( TextElement ),
			case BooleanStr of

				"1" ->
					"true";

				"0" ->
					"false"

			end;

		% We do not know of any other possible case:
		OtherAttrValue ->
			io:format( "~cerror: unknown data type encountered in the cell "
					   "located at relative coordinates (~B,~B). This type is "
					   "referred to as ~p in XML, where only \"s\" (string) "
					   "and \"b\" (boolean) are known.~n",
					   [ 9, RowIndex, ColumnIndex, OtherAttrValue ] ),
			throw( { invalid_cell_attribute_value, OtherAttrValue } )

	end,

	% Creates a new array cell aggregated to the previously formed ones, and
	% goes to the next cells:
	%
	NewAcc = [ { RowIndex, ColumnIndex, CellValue } | Acc ],
	parse_row_cells( T, SharedStrings, RowIndex, ColumnIndex+1, NewAcc,
					 DebugMode ).




%%
%%
%% -------- ODS: XML PARSING SECTION --------
%%
%%



% Parses the content.xml file in order to build a raw array of data:
parse_ods_content_file( FileName, DebugMode ) ->

	% Extracts the (part of) content that matters in the XML file:
	{ FileContent, _Misc } = xmerl_scan:file( FileName ),

	% Gets the 'body' part of the file content:
	[ BodyPart ] = pick_elements_with_name( 'office:body', FileContent,
											DebugMode ),

	% Gets the 'spreadsheet' part of 'body' (actually its only part) :
	[ SpreadsheetPart ] = pick_elements_with_name( 'office:spreadsheet',
												   BodyPart, DebugMode ),

	% Gets the 'table' part of 'spreadsheet', which contains the sought data:
	TablesPart = pick_elements_with_name( 'table:table', SpreadsheetPart,
										  DebugMode ),

	% Parses the listed 'table' parts in order to extract the sought data:
	[ parse_ods_table( Tab, DebugMode ) || Tab <- TablesPart ].



% Parses a table (actually corresponding to a sheet):
parse_ods_table( Table, DebugMode ) ->

	% Finds out the name of the current table:
	TableTitle = get_attribute_value( 'table:name', Table ),

	% Gets the rows of the current table:
	RowElements = pick_elements_with_name( 'table:table-row', Table,
										   DebugMode ),

	% Aggregates the data contained in the rows of the table:
	ReversedTableContent = parse_ods_table_rows( RowElements, 0, _Acc=[],
												 DebugMode ),

	TableContent = lists:reverse( ReversedTableContent ),

	% Returns the content of the table with its sheet title aside:
	{ TableTitle, TableContent }.



% Parses the row xmlElements of a table:
parse_ods_table_rows( _Rows=[], _RowIndex, Acc, _DebugMode ) ->
	Acc;

parse_ods_table_rows( [ RowElement | T ], RowIndex, Acc, DebugMode ) ->

	% The only xmlElements that rows contain are cells (covered or not):
	RowCells = RowElement#xmlElement.content,

	% Parses the cells that are part of the current row:
	NewAcc = parse_ods_row_cells( RowCells, RowIndex, 0, Acc, DebugMode ),

	% Checks if the current row was actually empty (by simply checking if the
	% current state of the array, NewAcc, did change) and parses the next ones:
	%
	case NewAcc of

		Acc ->
			parse_ods_table_rows( T, RowIndex, Acc, DebugMode );

		_NotEmptyRow ->
			parse_ods_table_rows( T, RowIndex+1, NewAcc, DebugMode )

	end.



% Parses the cell xmlElements of a row:
parse_ods_row_cells( _Cells=[], _RowInd, _ColInd, Acc, _DebugMode ) ->
	Acc;

% A cell xmlElement with no content and no attribute denotes an empty cell that
% has to be ignored in the count of columns (expectedly only a cell belonging to
% the column offset of the true array in the sheet):
%
parse_ods_row_cells( [ Cell | T ], RowIndex, ColumnIndex, Acc, DebugMode )
  when Cell#xmlElement.content == [], Cell#xmlElement.attributes == [] ->

	parse_ods_row_cells( T, RowIndex, ColumnIndex, Acc, DebugMode );

parse_ods_row_cells( [ Cell | T ], RowIndex, ColumnIndex, Acc, DebugMode )
  when Cell#xmlElement.content == [] ->

	% Looks for an attribute apparently specific to empty cell xmlElements
	% giving the number of empty columns that follow the cell:
	%
	NumberOfEmptyColumnsStr =
		get_attribute_value( 'table:number-columns-repeated', Cell ),

	case NumberOfEmptyColumnsStr of

		% When this attribute is not defined, it means the next cell belongs to
		% next column. Parses the next cells:
		%
		undefined ->
			parse_ods_row_cells( T, RowIndex, ColumnIndex+1, Acc, DebugMode );

		% Otherwise, parses the next cells by taking into account the offset:
		ValueStr ->
			NumberOfEmptyColumns = list_to_integer( ValueStr ),
			parse_ods_row_cells( T, RowIndex,
								 ColumnIndex + NumberOfEmptyColumns, Acc,
								 DebugMode )

	end;

parse_ods_row_cells( [ Cell | T ], RowIndex, ColumnIndex, Acc, DebugMode ) ->

	% Gets the (apparently) only possible content in a cell xmlElement, that is
	% a text xmlElement (yet for robustness, uses a filter):
	%
	[ TextElement ] = pick_elements_with_name( 'text:p', Cell, DebugMode ),

	% Very rarely, according to some extra typings found in a few cells, there
	% can be an extra level of encapsulation. Thus we have to look for this
	% extra level:
	%
	[ RealTextElement ] = case TextElement#xmlElement.content of

		[ #xmlText{} ] ->
			[ TextElement ];

		[ #xmlElement{ name='text:a' } ] ->
			pick_elements_with_name( 'text:a', TextElement, DebugMode )

	end,

	% Gets the actual (text) value out of the cell:
	CellText = get_text_value( RealTextElement ),

	% Creates a new cell added to the array under construction, then parses the
	% next cells:
	%
	NewAcc = [ { RowIndex, ColumnIndex, CellText } | Acc ],
	parse_ods_row_cells( T, RowIndex, ColumnIndex+1, NewAcc, DebugMode ).




%%
%%
%% -------- DUMF: ARRAYS PARSING SECTION --------
%%
%%



% Extracts, from the data arrays obtained after reading the XML file(s), all the
% data that are to be written in the final DUMF file:
%
% Extracts every title from the list:
parse_raw_data_arrays( SheetsData,
		 _Titles=[ MetadataTitle, InputPortSpecsTitle, OutputPortSpecsTitle,
				   MockupClausesTitle ],
		 DebugMode ) ->

	% Extracts metadata from the global data:
	MetadataArray = find_raw_content_from_title( MetadataTitle, SheetsData ),
	Metadata = extract_metadata( MetadataArray ),

	% Extracts input port specifications from the global data:
	IPSArray = find_raw_content_from_title( InputPortSpecsTitle, SheetsData ),
	InputPortSpecs = extract_input_port_specs( IPSArray ),

	% Extracts output port specifications from the global data:
	OPSArray = find_raw_content_from_title( OutputPortSpecsTitle, SheetsData ),
	OutputPortSpecs = extract_output_port_specs( OPSArray ),

	% Extracts mockup clauses from the global data:
	MockupClausesArray = find_raw_content_from_title( MockupClausesTitle,
													  SheetsData ),
	MockupClauses = extract_mockup_clauses( MockupClausesArray ),

	% Displays the lists of extracted data in case of debug execution:
	[ display_debug( "~p~n~n", [ ED ], DebugMode )
	  || ED <- [ Metadata, InputPortSpecs, OutputPortSpecs, MockupClauses ] ],

	% Gathers these extracted data in a global tuple:
	{ Metadata, InputPortSpecs, OutputPortSpecs, MockupClauses }.



% Tries to find an array content by looking for an array title to match:
find_raw_content_from_title( ExpectedTitle, SheetsData ) ->

	% Checks if the expected array title matches any of the parsed ones ...
	case lists:keyfind( ExpectedTitle, 1, SheetsData ) of

		% ... if so, returns the associated array content:
		{ _Title, ContentArray } ->
			ContentArray;

		% ... if not, tries to match a reformatted title (sometimes, e.g. when
		% exporting the spreadsheet to the ODF (.ods) file format from Excel,
		% blanks in the titles are replaced by underscores):
		%
		false ->
			TitleTokens = string:tokens( ExpectedTitle, " " ),
			ReformattedTitle = string:join( TitleTokens, "_" ),

			case lists:keyfind( ReformattedTitle, 1, SheetsData ) of

				{ _Title, ContentArray } ->
					ContentArray;

				% If even this test fails, throws an error:
				false ->
					ParsedTitles = [ T || { T, _RC } <- SheetsData ],
					io:format( "~cError: the expected title '~p' could not be "
							   "found.~n~c       The parsed titles are: ~p~n",
							   [ 9, ExpectedTitle, 9, ParsedTitles ] ),
					throw( { unmatched_sheet_title , ExpectedTitle } )

			end

	end.



% Extracts metadata of the mock-up unit for the associated array:
extract_metadata( RawMetadata ) ->

	% Finds the rank of the "Metadata Value" cell, which serves as reference:
	RefRank = get_cell_rank_from_value( "Metadata Value", RawMetadata ),

	% Extracts in a row the cells which contain values:
	MainValueCells = [ lists:nth( RefRank + 2*Rank, RawMetadata )
					   || Rank <- lists:seq( 1, 6 ) ],

	% Finds the rank of the "DUMF version" cell, after which comes the version
	% number, and which is located outside of the main array:
	%
	DUMFCellRank = get_cell_rank_from_value(
					"Version of the Dataflow Unit Mockup format", RawMetadata ),

	% Extracts the targeted DUMF version:
	DUMFVersion = lists:nth( DUMFCellRank+1, RawMetadata ),

	% Extracts the values from the cells:
	[ Value || { _Row, _Col, Value } <- [ DUMFVersion | MainValueCells ] ].



% Extracts the input port specs of the mock-up unit from the associated array:
extract_input_port_specs( RawIPS ) ->

	% Gets some information guiding the parsing of RawIPS:
	HeadersStartRank = get_cell_rank_from_value( "Input Port Name", RawIPS ),
	HeadersLastRank = get_cell_rank_from_value( "Constraints", RawIPS ),
	NumberOfColumns = HeadersLastRank - HeadersStartRank + 1,

	% Extracts in a row the list of input port specifications for all ports:
	AllIPSCells = lists:sublist( RawIPS, HeadersStartRank+NumberOfColumns,
								 length( RawIPS ) ),

	% Extracts the "per-port" specs by splitting the previous big array:
	split_array_as_full_rows( AllIPSCells, NumberOfColumns ).



% Extracts the output port specs of the mock-up unit from the associated array:
extract_output_port_specs( RawOPS ) ->

	% Gets some information guiding the parsing of RawOPS:
	HeadersStartRank = get_cell_rank_from_value( "Output Port Name", RawOPS ),
	HeadersLastRank = get_cell_rank_from_value( "Constraints", RawOPS ),
	NumberOfColumns = HeadersLastRank - HeadersStartRank + 1,

	% Extracts in a row the list of input port specifications for all ports:
	AllOPSCells = lists:sublist( RawOPS, HeadersStartRank+NumberOfColumns,
								 length( RawOPS ) ),

	% Extracts the "per-port" specs by splitting the previous big array:
	split_array_as_full_rows( AllOPSCells, NumberOfColumns ).



% Extracts the mock-up clause(s) of the mock-up unit from the associated array,
% after having first isolated each of these clauses:
%
extract_mockup_clauses( RawMockupClauses ) ->

	% Sets some known static information guiding the interpretation of RawOPS:
	HeadersStartRank = get_cell_rank_from_value( "Time Specification",
												 RawMockupClauses ),

	HeadersLastRank = get_cell_rank_from_value( "C", RawMockupClauses ),

	NumberOfColumns = HeadersLastRank - HeadersStartRank + 1,

	% Extracts the cells containing the values of mock-up clauses:
	AllMockupClauseCells = lists:sublist( RawMockupClauses,
										  HeadersStartRank + NumberOfColumns,
										  length( RawMockupClauses ) ),

	% Splits the array in order to get exactly one array per mock-up clause:
	MockupClauseArrays = split_mockup_clauses_array( AllMockupClauseCells ),

	% Applies the same extraction algorithm to the next clause(s):
	parse_mockup_clause_arrays( MockupClauseArrays ).



% Splits the global array representing all mock-up clauses in a row, in order to
% isolate each of them inside a returned list of clauses:
%
split_mockup_clauses_array( BigArray ) ->
	ReversedResult = split_mockup_clauses_array( BigArray, _Acc=[] ),
	lists:reverse( ReversedResult ).


split_mockup_clauses_array( _MockupClausesArray=[], Acc ) ->
	Acc;

split_mockup_clauses_array( [ HeadCell | TailArray ], Acc ) ->

	% Detects the column index of the time specification cell (head element):
	{ _RowIndex, TimeSpecColumnIndex, _Value } = HeadCell,

	% Starting just after the head time specification cell, goes up to the next
	% time specification cell and then cuts to form a single clause array:
	%
	SplitArrays = lists:splitwith( fun( _Cell={ _Row, Col, _Val } ) ->
											  Col =/= TimeSpecColumnIndex
								   end, TailArray ),

	{ MockupClauseBodyArray, OtherMockupClausesArray } = SplitArrays,

	% Puts back the head time spec cell to complete the new clause array:
	MockupClauseArray = [ HeadCell | MockupClauseBodyArray ],

	% Aggregates the new clause array to the list of previously formed ones and
	% repeats the same process for clauses remaining in the tail array:
	%
	NewAcc = [ MockupClauseArray | Acc ],

	split_mockup_clauses_array( OtherMockupClausesArray, NewAcc ).



% Once the clauses are isolated, extracts the data of the mock-up clauses out of
% each clause-specific array:
%
parse_mockup_clause_arrays( MockupClauseArrays ) ->
	ReversedResult = parse_mockup_clause_arrays( MockupClauseArrays, _Acc=[] ),
	lists:reverse( ReversedResult ).


parse_mockup_clause_arrays( _MockupClauseArrays=[], Acc ) ->
	Acc;

parse_mockup_clause_arrays( [ ClauseArray | T ], Acc ) ->
	MockupClause = parse_mockup_clause_array( ClauseArray ),
	parse_mockup_clause_arrays( T, [ MockupClause | Acc ] ).



% Extracts the sought data out of a clause-specific array:
parse_mockup_clause_array( [ TimeSpecCell | BodyClauseArray ] ) ->

	% Detects the column index of the time specification cell (head element):
	{ _TimeSpecRowIndex, TimeSpecColumnIndex, TimeSpecValue } = TimeSpecCell,

	% Separates the input and output parts of the body of the current clause:
	{ InputMatchSpecsArray, OutputStateSpecsArray } =
		lists:partition( fun( _Cell={ _Row, Col, _Val } ) ->
								 Col =< TimeSpecColumnIndex + 4
						 end, BodyClauseArray ),

	% Parses these separated spec lists to extract their array values:
	InputMatchSpecs = parse_IO_port_specs_array( InputMatchSpecsArray ),
	OutputStateSpecs = parse_IO_port_specs_array( OutputStateSpecsArray ),

	% Returns the current clause in its canonical form:
	{ TimeSpecValue, InputMatchSpecs, OutputStateSpecs }.



% Parses an input or output spec array to extract its sought values:
parse_IO_port_specs_array( SpecsArray ) ->
	ReversedPortSpecs = parse_IO_port_specs_array( SpecsArray, _Acc=[] ),
	lists:reverse( ReversedPortSpecs ).

parse_IO_port_specs_array( _SpecsArray=[], Acc ) ->
	Acc;

parse_IO_port_specs_array(
  SpecsArray=[ { HeadRowIndex, _HeadColIndex, _HeadValue } | _ ], Acc ) ->

	% HeadRowIndex is the starting row index of the array.

	% Gathers all the cells in the same row, hence belonging to the same spec:
	{ PortSpecArray, MorePortSpecsArrays } =
		lists:splitwith( fun( _Cell={ Row, _Col, _Val } ) ->
								 Row == HeadRowIndex
						 end, SpecsArray ),

	% Extracts the value of each cell in this row:
	PortSpec = [ Value || { _Row, _Col, Value } <- PortSpecArray ],

	% Applies the same algorithm to the next rows of the IO specs array:
	parse_IO_port_specs_array( MorePortSpecsArrays, [ PortSpec | Acc ] ).



%%
%%
%% -------- DUMF: FILE GENERATION SECTION --------
%%
%%



% Generates all the DUMF formatted data and writes them into the target file:
write_DUMF_file( FileName, _RawData={ Metadata, InputPortSpecs,
									  OutputPortSpecs, MockupClauses } ) ->

	% Extracts the pieces of metadata and gets good types when needed:
	%
	[ DUMFVersion, UnitTypeStr, Author, ContactInfo, UnitVersion, FileDate,
	  ActivationPolicyStr ] = enforce_ascii_strings( Metadata ),

	UnitType = list_to_atom( UnitTypeStr ),

	RealDate = check_date_format( FileDate ),

	ActivationPolicy = list_to_atom( ActivationPolicyStr ),

	{ { Year, Month, Day }, { Hour, Minute, Second } } = erlang:localtime(),

	Timestamp = io_lib:format( "~2..0B/~2..0B/~B ~B:~2..0B:~2..0B",
							   [ Day, Month, Year, Hour, Minute, Second ] ),

	User = os:getenv( "USER" ),

	% Assembles the header comments of the file:
	%
	% (maybe adding timestamp and user is not a good idea, as it breaks
	% reproducibility; or the host and source filename should be added as well)
	%
	Header = io_lib:format( "%~n% This DUMF file defines the mock-up version "
							"of the unit named:~n% ~s~n%~n%~n% Please refer to "
							"the 'Sim-Diasca Dataflow HOWTO' for more "
							"information about~n% Mock-up Units.~n%~n% "
							"Generated on ~s, by user '~s'.~n%~n~n",
							[ UnitType, Timestamp, User ] ),

	% Assembles the metadata section of the file:
	MetadataSection = io_lib:format( "{ dumf_version, ~p }.~n~n{ unit_type, "
							"'~p' }.~n~n{ mockup_author, ~p }.~n{ mockup_author"
							"_contact, ~p }.~n~n{ mockup_version, ~p }.~n{ "
							"mockup_date, ~c~s~c }.~n~n{ activation_policy, "
							"'~p' }.~n~n~n",
							[ DUMFVersion, UnitType, Author, ContactInfo,
							  UnitVersion, $", RealDate, $",
							  ActivationPolicy ] ),

	% Assembles the input_port_specs section of the file:
	IPS_Section = [ io_lib:format( "{ input_port_specs, [~n~n", [] ),
					format_input_port_specs( InputPortSpecs ),
					io_lib:format( "~n~n] }.~n~n~n", [] ) ],

	% Assembles the output_port_specs section of the file:
	OPS_Section = [ io_lib:format( "{ output_port_specs, [~n~n", [] ),
					format_output_port_specs( OutputPortSpecs ),
					io_lib:format( "~n~n] }.~n~n~n", [] ) ],

	% Gathers the types of all ports in order to turn the strings storing values
	% into the actual values expected by the user:
	%
	InputPortTypes = [ extract_input_port_type( IPS ) ||
						 IPS <- InputPortSpecs ],

	OutputPortTypes = [ extract_output_port_type( OPS ) ||
						  OPS <- OutputPortSpecs ],

	% Assembles the mockup_clauses section of the file:
	ClausesSection = [ io_lib:format( "{ mockup_clauses, [~n~n", [] ),
					   format_mockup_clauses( MockupClauses, InputPortTypes,
											  OutputPortTypes ),
					   io_lib:format( "~n~n] }.~n", [] ) ],

	% Associates the strings in an IO list and writes the file:
	GlobalIOList = [ Header, MetadataSection, IPS_Section, OPS_Section,
					 ClausesSection ],

	case exists( FileName ) of

		true ->
			display_warning( "the DUMF file already exists and is being "
							 "overwritten" );

		false ->
			ok

	end,

	ok = file:write_file( FileName, GlobalIOList ),

	io:format( "The DUMF file '~s' has been successfully generated.~n~n",
			   [ filename:basename( FileName ) ] ).



% Reformats the date string if it does not follow the DD/MM/YYYY format:
check_date_format( FileDateStr ) ->

	case string:chr( FileDateStr, $/ ) of

		% If no '/' character was found, we should be in the case of a .xlsx
		% file, since Excel stores dates (thus FileDateStr) as a number of days
		% since its chosen origin, which is 1900/01/01.
		%
		0 ->
			ExcelReferenceDate = { 1900, 1, 1 },
			FileDate = list_to_integer( FileDateStr ),
			DaysOffset = calendar:date_to_gregorian_days( ExcelReferenceDate ),
			DateTokens = tuple_to_list( calendar:gregorian_days_to_date(
										  DaysOffset + FileDate - 2 ) ),
			io_lib:format( "~2..0B/~2..0B/~B", lists:reverse( DateTokens ) );

		% If we found one '/', we suppose there are actually two and the format
		% is correct.
		_AnyOccurrence ->
			FileDateStr

	end.



% Generates formatted strings representing each input port spec of the unit:
format_input_port_specs( InputPortSpecs ) ->
	ReversedFormattedIPSs = format_input_port_specs( InputPortSpecs, _Acc=[] ),
	lists:reverse( ReversedFormattedIPSs ).

format_input_port_specs( _InputPortSpecs=[], Acc ) ->
	Acc;

format_input_port_specs( [ InputPortSpec | T ], Acc ) ->

	% Extracts the pieces of input port specifications and checks that they are
	% valid strings:
	%
	[ InputPortName, Comment, IsIterationStr, ValueSemanticsStr, ValueUnit,
	  ValueTypeDescription, ValueConstraintsStr ] =
		enforce_ascii_strings( InputPortSpec ),

	% Converts the specs that are not intended to remain as strings:
	IsIteration = list_to_atom( IsIterationStr ),
	ValueSemantics = list_to_atom( ValueSemanticsStr ),
	ValueConstraints = case get_list_from_string( ValueConstraintsStr ) of

		string_not_depicting_list ->
			[ list_to_atom( ValueConstraintsStr ) ];

		[] ->
			[];

		ConstraintStrings ->
			[ list_to_atom( CS ) || CS <- ConstraintStrings ]

	end,

	% Formats these information pieces:
	CoreFormattedIPS = io_lib:format( "~c[~n~c~c{ input_port_name, ~p },~n~c~c{"
						  " comment, ~p },~n~c~c{ is_iteration, '~p' },~n~c~c{ "
						  "value_semantics, ~p },~n~c~c{ value_unit, ~p },~n~c"
						  "~c{ value_type_description, ~p },~n~c~c{ "
						  "value_constraints, ~p }~n~c]",
						  [ 9, 9, 9, InputPortName, 9, 9, Comment, 9, 9,
							IsIteration, 9, 9, ValueSemantics, 9, 9, ValueUnit,
							9, 9, ValueTypeDescription, 9, 9, ValueConstraints,
							9 ] ),
	FormattedIPS = format_and_append_if_no_tail( ",~n~n", CoreFormattedIPS, T ),

	% Aggregates these formatted specs with those previously formed:
	NewAcc = [ FormattedIPS | Acc ],
	format_input_port_specs( T, NewAcc ).



% Generates formatted strings representing each output port spec of the unit:
format_output_port_specs( OutputPortSpecs ) ->
	ReversedFormattedOPSs =
		format_output_port_specs( OutputPortSpecs, _Acc=[] ),
	lists:reverse( ReversedFormattedOPSs ).

format_output_port_specs( _OutputPortSpecs=[], Acc ) ->
	Acc;

format_output_port_specs( [ OutputPortSpec | T ], Acc ) ->

	% Extracts the pieces of output port specifications:
	[ OutputPortName, Comment, IsIterationStr, ValueSemanticsStr, ValueUnit,
	  ValueTypeDescription, ValueConstraintsStr ] =
		enforce_ascii_strings( OutputPortSpec ),

	% Converts some specs that are not expected to be strings:
	IsIteration = list_to_atom( IsIterationStr ),
	ValueSemantics = list_to_atom( ValueSemanticsStr ),
	ValueConstraints = case get_list_from_string( ValueConstraintsStr ) of

		string_not_depicting_list ->
			[ list_to_atom( ValueConstraintsStr ) ];

		[] ->
			[];

		ConstraintStrings ->
			[ list_to_atom( CS ) || CS <- ConstraintStrings ]

	end,

	% Formats these information pieces:
	CoreFormattedOPS = io_lib:format( "~c[~n~c~c{ output_port_name, ~p },~n~c~c"
						  "{ comment, ~p },~n~c~c{ is_iteration, '~p' },~n~c~c{"
						  " value_semantics, ~p },~n~c~c{ value_unit, ~p },~n~c"
						  "~c{ value_type_description, ~p },~n~c~c{ "
						  "value_constraints, ~p }~n~c]",
						  [ 9, 9, 9, OutputPortName, 9, 9, Comment, 9, 9,
							IsIteration, 9, 9, ValueSemantics, 9, 9, ValueUnit,
							9, 9, ValueTypeDescription, 9, 9, ValueConstraints,
							9 ] ),
	FormattedOPS = format_and_append_if_no_tail( ",~n~n", CoreFormattedOPS, T ),

	% Aggregates these formatted specs with those previously formed:
	NewAcc = [ FormattedOPS | Acc ],

	format_output_port_specs( T, NewAcc ).



% Generates formatted strings representing each entire mockup clause:
format_mockup_clauses( MockupClauses, InputPortTypes, OutputPortTypes ) ->

	ReversedFormattedClauses = format_mockup_clauses( MockupClauses,
								  InputPortTypes, OutputPortTypes, _Acc=[] ),

	lists:reverse( ReversedFormattedClauses ).


format_mockup_clauses( _MockupClauses=[], _IPTypes, _OPTypes, Acc ) ->
	Acc;

format_mockup_clauses(
  [ _Clause={ TimeSpecStr, InputMatchSpecs, OutputStateSpecs } | T ],
  IPTypes, OPTypes, Acc ) ->

	% Main blocks of the current clause extracted.

	% Converts the time specification to the right type:
	TimeSpec = case TimeSpecStr of

		"any_time" ->
			any_time;

		OtherString ->
			list_to_integer( OtherString )

	end,

	% Gets the formatted blocks of InputMatchSpecs and OutputStateSpecs:
	FormattedIMSs = format_input_match_specs( InputMatchSpecs, IPTypes ),
	FormattedOSSs = format_output_state_specs( OutputStateSpecs, OPTypes ),

	% Builds the formatted block corresponding to the entire current clause:
	CoreFormattedClause = [ io_lib:format( "  { ~p,~n~c[~n", [ TimeSpec, 9 ] ),
							FormattedIMSs, io_lib:format( "~n~c],~n~c[~n",
														  [ 9, 9 ] ),
							FormattedOSSs, io_lib:format( "~n~c] }", [ 9 ] ) ],

	FormattedClause = format_and_append_if_no_tail( ",~n~n",
													CoreFormattedClause, T ),

	% Aggregates this formatted clause with those previously formed:
	NewAcc = [ FormattedClause | Acc ],
	format_mockup_clauses( T, IPTypes, OPTypes, NewAcc ).



% Generates formatted strings representing each input match spec of a clause:
format_input_match_specs( InputMatchSpecs, IPTypes ) ->
	ReversedSpecList = format_input_match_specs( InputMatchSpecs, IPTypes,
												 _Acc=[] ),
	lists:reverse( ReversedSpecList ).


format_input_match_specs( _InputMatchSpecs=[], _IPTypes, Acc ) ->
	Acc;


format_input_match_specs( [ _InputMatchSpec = [ PortName, AtomStr ]
							| T ], IPTypes, Acc ) ->

	CoreFormattedIMS = io_lib:format( "~c~c{ ~p, ~p }",
							  [ 9, 9, PortName, list_to_atom( AtomStr ) ] ),

	FormattedIMS =
		format_and_append_if_no_tail( ",~n", CoreFormattedIMS, T ),

	NewAcc = [ FormattedIMS | Acc ],
	format_input_match_specs( T, IPTypes, NewAcc );


format_input_match_specs(
  [ _InputMatchSpec = [ PortName, "set", ValueStr ] | T ], IPTypes, Acc ) ->

	Value = convert_value_string_with_port_type( PortName, ValueStr, IPTypes ),

	CoreFormattedIMS = io_lib:format( "~c~c{ ~p, { set, ~p } }",
									  [ 9, 9, PortName, Value ] ),

	FormattedIMS = format_and_append_if_no_tail( ",~n", CoreFormattedIMS, T ),

	NewAcc = [ FormattedIMS | Acc ],

	format_input_match_specs( T, IPTypes, NewAcc );


format_input_match_specs(
  [ _InputMatchSpec = [ PortName, "around", ValueStr ] | T ], IPTypes, Acc ) ->

	Value = convert_value_string_with_port_type( PortName, ValueStr, IPTypes ),

	CoreFormattedIMS = io_lib:format( "~c~c{ ~p, { around, ~p } }",
									  [ 9, 9, PortName, Value ] ),

	FormattedIMS = format_and_append_if_no_tail( ",~n", CoreFormattedIMS, T ),

	NewAcc = [ FormattedIMS | Acc ],

	format_input_match_specs( T, IPTypes, NewAcc );


format_input_match_specs(
  [ _InputMatchSpec = [ PortName, "around", ValueStr1, ValueStr2 ] | T ],
  IPTypes, Acc ) ->

	Val1 = convert_value_string_with_port_type( PortName, ValueStr1, IPTypes ),
	Val2 = convert_value_string_with_port_type( PortName, ValueStr2, IPTypes ),

	CoreFormattedIMS = io_lib:format( "~c~c{ ~p, { around, ~p, ~p } }",
									  [ 9, 9, PortName, Val1, Val2 ] ),

	FormattedIMS = format_and_append_if_no_tail( ",~n", CoreFormattedIMS, T ),

	NewAcc = [ FormattedIMS | Acc ],

	format_input_match_specs( T, IPTypes, NewAcc );


format_input_match_specs(
  [ _InputMatchSpec=[ PortName, "between", ValueStr1, ValueStr2 ] | T ],
  IPTypes, Acc ) ->

	Val1 = convert_value_string_with_port_type( PortName, ValueStr1, IPTypes ),
	Val2 = convert_value_string_with_port_type( PortName, ValueStr2, IPTypes ),

	CoreFormattedIMS = io_lib:format( "~c~c{ ~p, { between, ~p, ~p } }",
									  [ 9, 9, PortName, Val1, Val2 ] ),

	FormattedIMS = format_and_append_if_no_tail( ",~n", CoreFormattedIMS, T ),

	NewAcc = [ FormattedIMS | Acc ],

	format_input_match_specs( T, IPTypes, NewAcc );


format_input_match_specs(
  [ InputMatchSpec = [ PortName, "among", ValListStr ] | T ], IPTypes, Acc ) ->

	% Gets the list of numbers (floats by default) from the string representing
	% this list:
	%
	ValuesList = case get_list_from_string( ValListStr ) of

		string_not_depicting_list ->
			io:format( "~cError: ill-formed 'among' specification encountered: "
					   "~p~n~c The 'among' specification expects a list of "
					   "values enclosed in squared brackets (e.g. [ 1, 2, 3.14 "
					   "]).~n~c Please refer to the 'Sim-Diasca Dataflow HOWTO'"
					   " documentation for more information about the mock-up "
					   "clause specifications.~n",
					   [ 9, InputMatchSpec, 9, 9 ] ),

			throw( { invalid_among_input_match_spec, InputMatchSpec } );

		[] ->
			io:format( "~cError: an empty 'among' specification has been "
					   "encountered, which is not considered meaningful.~n~c "
					   "Please use the 'unset' specification instead, or refer "
					   "to the 'Sim-Diasca Dataflow HOWTO' documentation for "
					   "more information about the available choices.~n",
					   [ 9, 9 ] ),
			throw( { empty_among_input_match_spec, InputMatchSpec } );

		ValueStrings ->
			[ convert_value_string_with_port_type( PortName, VS, IPTypes )
			  || VS <- ValueStrings ]

	end,

	% Makes the associated formatted line and passes to the next IMS:
	CoreFormattedIMS = io_lib:format( "~c~c{ ~p, { ~p, ~p } }",
									  [ 9, 9, PortName, among, ValuesList ] ),

	FormattedIMS = format_and_append_if_no_tail( ",~n", CoreFormattedIMS, T ),

	NewAcc = [ FormattedIMS | Acc ],

	format_input_match_specs( T, IPTypes, NewAcc );


% Catches most of the ill-formed input match specs:
format_input_match_specs( InputMatchSpec, _IPTypes, _Acc ) ->
	io:format( "~cError: unknown input match specification encountered: ~p~n~c "
			   "Please refer the 'Sim-Diasca Dataflow HOWTO' documentation for "
			   "more information about the available mock-up clause "
			   "specifications.~n", [ 9, InputMatchSpec, 9 ] ),
	throw( { unknown_input_match_spec, InputMatchSpec } ).



% Generates formatted strings representing each output state spec of a clause:
format_output_state_specs( OutputStateSpecs, OPTypes ) ->
	ReversedSpecList =
		format_output_state_specs( OutputStateSpecs, OPTypes, _Acc=[] ),
	lists:reverse( ReversedSpecList ).


format_output_state_specs( _OutputStateSpecs=[], _OPTypes, Acc ) ->
	Acc;


format_output_state_specs( [ _OutputStateSpec = [ PortName, AtomStr ] | T ],
						   OPTypes, Acc ) ->

	CoreFormattedOSS = io_lib:format( "~c~c{ ~p, ~p }",
							  [ 9, 9, PortName, list_to_atom( AtomStr ) ] ),

	FormattedOSS = format_and_append_if_no_tail( ",~n", CoreFormattedOSS, T ),

	NewAcc = [ FormattedOSS | Acc ],

	format_output_state_specs( T, OPTypes, NewAcc );


format_output_state_specs(
  [ _OutputStateSpec = [ PortName, "set", ValueStr ] | T ], OPTypes, Acc ) ->

	Value = convert_value_string_with_port_type( PortName, ValueStr, OPTypes ),

	CoreFormattedOSS = io_lib:format( "~c~c{ ~p, { set, ~p } }",
									  [ 9, 9, PortName, Value ] ),

	FormattedOSS = format_and_append_if_no_tail( ",~n", CoreFormattedOSS, T ),

	NewAcc = [ FormattedOSS | Acc ],
	format_output_state_specs( T, OPTypes, NewAcc );


format_output_state_specs(
  [ _OutputStateSpec = [ PortName, "state_of", IPName ] | T ], OPTypes, Acc ) ->

	CoreFormattedOSS = io_lib:format( "~c~c{ ~p,~n~c~c  { state_of, ~p } }",
									  [ 9, 9, PortName, 9, 9, IPName ] ),

	FormattedOSS = format_and_append_if_no_tail( ",~n", CoreFormattedOSS, T ),

	NewAcc = [ FormattedOSS | Acc ],
	format_output_state_specs( T, OPTypes, NewAcc );


% Catches most of the ill-formed output state specs:
format_output_state_specs( OutputStateSpec, _OPTypes, _Acc ) ->
	io:format( "~cError: unknown output state specification encountered: ~p~n~c"
			   "Please refer the 'Sim-Diasca Dataflow HOWTO' documentation "
			   "for more information about the available mock-up clause "
			   "specifications.~n", [ 9, OutputStateSpec, 9 ] ),
	throw( { invalid_clause_output_state_spec, OutputStateSpec } ).




%%
%%
%% -------- UTILITIES SECTION --------
%%
%%



% Gets the value of an XML attribute identified by its name:
-spec get_attribute_value( atom(), #xmlElement{} ) -> term().
get_attribute_value( AttributeName, ParentElement ) ->

	Attribute = lists:keyfind( AttributeName, #xmlAttribute.name,
							   ParentElement#xmlElement.attributes ),
	case Attribute of

		false ->
			undefined;

		#xmlAttribute{ value=Value } ->
			Value

	end.



% Picks in ParentElement's content only the child sharing a given name:
-spec pick_elements_with_name( string(), #xmlElement{}, boolean() ) ->
									 [ #xmlElement{} ].
pick_elements_with_name( ElementsName, ParentElement, DebugMode ) ->

	display_debug( "Picking elements with name '~s'.~n",
				   [ ElementsName ], DebugMode ),

	lists:filter( fun( XmEl ) ->
					  XmEl#xmlElement.name == ElementsName
				  end, ParentElement#xmlElement.content ).



% Extracts the actual text from an XML text element:
-spec get_text_value( #xmlElement{} ) -> string().
get_text_value( TextElement ) ->
	[ XmlText ] = TextElement#xmlElement.content,
	XmlText#xmlText.value.



% Finds the position in an array (being a list of { Row, Column, Value } cells)
% of a cell containing a test value:
%
get_cell_rank_from_value( Value, RawDataList ) ->
	get_cell_rank_from_value( Value, RawDataList, _Rank=1 ).

get_cell_rank_from_value( Value, _RawDataList=[], _Rank ) ->
	io:format( "~cError: value not found in the list of parsed raw data: ~p~n",
			   [ 9, Value ] ),
	throw( { unknwon_cell_value, Value } );

get_cell_rank_from_value( Value, [ Cell | T ], Rank ) ->

	case Cell of

		{ _RowIndex, _ColIndex, Value } ->
			Rank ;

		_AnythingElse ->
			get_cell_rank_from_value( Value, T, Rank+1 )

	end.



% Splits an array in a list of rows, while making sure that there are no empty
% cells, and extracts at the same time the values from these cells:
%
split_array_as_full_rows( RowsOfCells, NumberOfColumns ) ->
	split_array_as_full_rows( RowsOfCells, NumberOfColumns, [] ).

split_array_as_full_rows( [], _NumberOfColumns, Acc ) ->
	lists:reverse( Acc );

split_array_as_full_rows( RowsOfCells, NumberOfColumns, Acc ) ->

	% Splits the next row if possible (i.e. if enough cells remain):
	{ Row, MoreRows } = case length( RowsOfCells ) < NumberOfColumns of

		true ->
			{ RowsOfCells, [] };

		false ->
			lists:split( NumberOfColumns, RowsOfCells )

	end,

	% Gets both the expected and the real column indices:
	ExpectedColumnIndices = lists:seq( 0, NumberOfColumns-1 ),
	RowColumnIndices = [ ColIndex || { _RowIndex, ColIndex, _Value } <- Row ],

	% Checks if there are no missing column indices, otherwise fills the gaps
	% with default values:
	%
	{ NewRow, OtherRows } = case RowColumnIndices == ExpectedColumnIndices of

		true ->
			{ Row, MoreRows };

		false ->
			{ FullRow, ExtraCells } = fill_empty_cells( Row, NumberOfColumns ),
			{ FullRow, ExtraCells ++ MoreRows }

	end,

	% Extracts the values from the (well-formed) new row before recursion:
	RowValues = [ Value || { _RowIndex, _ColIndex, Value } <- NewRow ],

	split_array_as_full_rows( OtherRows, NumberOfColumns, [ RowValues | Acc ] ).



% Fills as many cells as necessary to make sure that the final row has only
% consecutive column indices, from 0 to NumberOfColumns-1. The exposed "gaps"
% between two cells are filled with default-valued new cells.
%
fill_empty_cells( RowCells, NumberOfColumns ) ->
	fill_empty_cells( RowCells, NumberOfColumns, _ExpectedColIndex=0, _Acc=[] ).


% Algorithm final case: once ExpectedColIndex reaches NumberOfColumns, a full
% row has been aggregated in Acc. Then the extra cells (if any) are returned
% separately to be joined back inside the big unsplit array.
%
fill_empty_cells( ExtraCells, NumberOfColumns, ExpectedColIndex, Acc )
  when ExpectedColIndex == NumberOfColumns ->
	{ lists:reverse( Acc ), ExtraCells };

% Algorith loop case 1: if the current cell contains the column index currently
% looked for (ExpectedColIndex in other clauses), it can be passed to Acc. We
% will now look for the next column index, in the next cells (T).
%
fill_empty_cells( [ Cell={ _RowIndex, ColIndex, _Value } | T ],
				  NumberOfColumns, ColIndex, Acc ) ->
	fill_empty_cells( T, NumberOfColumns, ColIndex+1, [ Cell | Acc ] );

% Algorithm loop case 2: if the current cell does not contain the column index
% currently looked for (ExpectedColIndex), we create the missing cell and add it
% to Acc. We will now look for the next column index, in the same current Cell.
%
fill_empty_cells( Cells=[ { RowIndex, _ColIndex, _Value } | _T ],
				  NumberOfColumns, ExpectedColIndex, Acc ) ->
	NewCell = { RowIndex, ExpectedColIndex,
				set_default_cell_value( ExpectedColIndex ) },
	fill_empty_cells( Cells, NumberOfColumns, ExpectedColIndex+1,
					  [ NewCell | Acc ] ).



% Defines some default values whose meanings are consistent with the ones of
% each column:
%
% Port name:
set_default_cell_value( _ColIndex=0 ) ->
	io:format( "~cError: an empty cell was detected where a port name was "
			   "expected. We cannot set a default value for such a critical "
			   "field, since it must match an existing port.~n", [ 9 ] ),
	throw( empty_port_name_not_allowed );

% Comment:
set_default_cell_value( _ColIndex=1 ) ->
	"None";

% Port iteration:
set_default_cell_value( _ColIndex=2 ) ->
	"false";

% Value semantics:
set_default_cell_value( _ColIndex = 3 ) ->
	DefaultSemantics = "http://foo.org./any",
	display_warning( "an empty semantics cell has been detected. It will be "
					 "filled with the default semantics: '~s'.~n",
					 [ DefaultSemantics ] ),
	DefaultSemantics;

% Value unit:
set_default_cell_value( _ColIndex=4 ) ->
	"dimensionless";

% Value type:
set_default_cell_value( _ColIndex=5 ) ->
	"any";

% Value constraint(s):
set_default_cell_value( _ColIndex=6 ) ->
	"[]".



% Turns a string representing a list into a list of strings:
% ( example: "[foo bar,baz ]" -> ["foo","bar","baz"] )
%
get_list_from_string( "[]" ) ->
	[];

get_list_from_string( MyString ) ->
	case hd( MyString ) == $[ andalso lists:last( MyString ) == $] of
		true ->
			TempString1 = string:strip( MyString, left, $[ ),
			TempString2 = string:strip( TempString1, right, $] ),
			string:tokens( TempString2, ", " );
		false ->
			string_not_depicting_list
	end.



% Extracts the name and the type of an input port from its read specifications:
extract_input_port_type( RawInputPortSpec ) ->
	[ InputPortName, _Comment, _IsIteration, _ValueSemantics, _ValueUnit,
	  ValueTypeDescription, _ValueConstraints ] = RawInputPortSpec,
	{ InputPortName, ValueTypeDescription }.



% Extracts the name and the type of an output port from its read specifications:
extract_output_port_type( RawOutputPortSpec ) ->
	[ OutputPortName, _Comment, _IsIteration, _ValueSemantics, _ValueUnit,
	  ValueTypeDescription, _ValueConstraints ] = RawOutputPortSpec,
	{ OutputPortName, ValueTypeDescription }.



% Appends a formatted sequence to another, only when a tail list related to the
% calling function is not empty.
%
format_and_append_if_no_tail( _StringToFormat, SourceIOList, _TestTail=[] ) ->
	SourceIOList;

format_and_append_if_no_tail( StringToFormat, SourceIOList, _TestTail ) ->
	[ SourceIOList, io_lib:format( StringToFormat, [] ) ].



% Checks that all the characters of the passed strings belong to the ASCII
% table, warns the user otherwise:
%
enforce_ascii_strings( Strings ) ->
	enforce_ascii_strings( Strings, [] ).

enforce_ascii_strings( [], Acc ) ->
	lists:reverse( Acc );

enforce_ascii_strings( [ String | MoreStrings ], Acc ) ->

	CheckingFun = fun( Char ) ->
					  Char > 31 andalso Char < 127
				  end,

	CheckingResult = lists:all( CheckingFun, String ),

	case CheckingResult of

		true ->
			enforce_ascii_strings( MoreStrings, [ String | Acc ] );

		false ->
			Replacement = "(non-ASCII string)",
			display_warning( "non-ASCII characters are not supported. "
							 "The string '~p' will be replaced by '~s'.~n",
							 [ String, Replacement ] ),
			enforce_ascii_strings( MoreStrings, [ Replacement | Acc ] )

	end.



% Converts a string expected to store a port value to this actual value,
% according to the type associated with this port.
%
convert_value_string_with_port_type( PortName, ValueStr, PortTypes ) ->

	% Takes the root name of the port if it is detected to be an iteration:
	TestPortName = case string:str( PortName, "_iterated_" ) of

		0 ->
			PortName;

		Position ->
			string:sub_string( PortName, 1, Position-1 )

	end,

	% Looks for this port in the PortTypes list:
	{ TestPortName, TypeStr } = lists:keyfind( TestPortName, 1, PortTypes ),

	% Converts the value according to the type found in PortTypes:
	case TypeStr of

		"atom" ->
			list_to_atom( ValueStr );

		"boolean" ->
			list_to_atom( ValueStr );

		"string" ->
			ValueStr;

		"integer" ->
			get_integer_from( ValueStr, PortName );

		FloatTypeStr when FloatTypeStr =:= "float" orelse
						  FloatTypeStr =:= "percent" ->
			get_float_from( ValueStr, PortName );

		"any" ->
			ValueStr

	end.



% Returns the integer corresponding to specified value of specified port.
-spec get_integer_from( string(), string() ) -> integer().
get_integer_from( ValueStr, PortName ) ->

	try

		list_to_integer( ValueStr )

	catch
		_:_ ->
			display_error( "Port name '~s' declared as expecting integer "
						   "values, whereas the value '~s' has been specified.",
						   [ PortName, ValueStr ] ),
			throw( { invalid_type, ValueStr } )

	end.



% Returns the float corresponding to specified value of specified port.
-spec get_float_from( string(), string() ) -> float().
get_float_from( ValueStr, PortName ) ->

	try

		string_to_float( ValueStr )

	catch
		_:_ ->
			display_error( "Port name '~s' declared as expecting "
						   "floating-point values, whereas the value "
						   "'~s' has been specified.",
						   [ PortName, ValueStr ] ),
			throw( { invalid_type, ValueStr } )

	end.



% Displays a text message iff in debug mode.
-spec display_debug( string(), boolean() ) -> basic_utils:void().
display_debug( Message, _DebugMode=true ) ->
	io:format( Message );

display_debug( _Message, _DebugMode=false ) ->
	ok.



% Displays a formatted text message iff in debug mode.
-spec display_debug( string(), list(), boolean() ) -> basic_utils:void().
display_debug( MessageFormat, Values, _DebugMode=true ) ->
	io:format( MessageFormat, Values );

display_debug( _Message, _Values, _DebugMode=false ) ->
	ok.



% Displays a warning message.
-spec display_warning( string() ) -> basic_utils:void().
display_warning( Message ) ->
	io:format( "~cWarning: ~s~n", [ 9, Message ] ).


% Displays a formatted warning message.
-spec display_warning( string(), list() ) -> basic_utils:void().
display_warning( MessageFormat, Values ) ->
	display_warning( io_lib:format( MessageFormat, Values ) ).



% Displays an error message.
-spec display_error( string() ) -> basic_utils:void().
display_error( Message ) ->
	io:format( "~cError: ~s~n", [ 9, Message ] ).


% Displays a formatted error message.
-spec display_error( string(), list() ) -> basic_utils:void().
display_error( MessageFormat, Values ) ->
	display_error( io_lib:format( MessageFormat, Values ) ).




% Returns the path to the 'unzip' tool.
get_unzip() ->

	case os:find_executable( "unzip" ) of

		false ->
			throw( unzip_tool_not_found );

		Exec ->
			Exec

	end.



% For the file_info record:
-include_lib("kernel/include/file.hrl").


% Checks the existence of a file entry, and fails if the file does not exist.
-spec check_file_exists( string(), boolean() ) -> basic_utils:void().
check_file_exists( EntryName, DebugMode ) ->

	case file:read_file_info( EntryName ) of

		{ ok, _FileInfo } ->
			display_debug( io_lib:format( "~cFile '~s' was found.~n",
										  [ 9, EntryName ] ), DebugMode );

		{ error, Reason } ->
			io:format( "~cError: file '~s' not found.~n", [ 9, EntryName ] ),
			throw( { Reason, EntryName } )

	end.



% Removes the specified directory.
-spec remove_directory( string() ) -> basic_utils:void().
remove_directory( DirectoryPath ) ->

	case is_existing_directory( DirectoryPath ) of

		true ->

			CleaningCommand = io_lib:format( "/bin/rm -rf '~s'",
											 [ DirectoryPath ] ),

			case os:cmd( CleaningCommand ) of

				"" ->
					ok;

				Msg ->
					display_warning( "removing directory '~s' led to "
									 "following message:~n~s",
									 [ DirectoryPath, Msg ] )

			end;

		false ->
			display_warning( "directory to remove '~s' not found.",
							 [ DirectoryPath ] )

	end.



% Taken verbatim from myriad/src/utils/file_utils.erl:

% Tells whether specified file entry exists, regardless of its type.
%
%-spec exists( file_name() ) -> boolean().
exists( EntryName ) ->

	case file:read_file_info( EntryName ) of

		{ ok, _FileInfo } ->
			true;

		{ error, _Reason } ->
			false

	end.


% Returns the type of the specified file entry.
%
%-spec get_type_of( file_name() ) -> entry_type().
get_type_of( EntryName ) ->

	% We used to rely on file:read_file_info/1, but an existing symlink pointing
	% to a non-existing entry was triggering the enoent error, while we just
	% wanted to know that the specified entry is an existing (yet dead) symlink.

	% Some tools (e.g. emacs) used thus to get in the way, as apparently they
	% create dead symlinks on purpose, to store information.

	case file:read_link_info( EntryName ) of

		{ ok, FileInfo } ->
			#file_info{ type=FileType } = FileInfo,
			FileType;

		{ error, eloop } ->
			% Probably a recursive symlink:
			throw( { too_many_symlink_levels, EntryName } );

		{ error, enoent } ->
			throw( { non_existing_entry, EntryName } )

	end.


% Returns whether the specified entry exists and is a directory.
%
% Returns true or false, and cannot trigger an exception.
%
%-spec is_existing_directory( directory_name() ) -> boolean().
is_existing_directory( EntryName ) ->

	case exists( EntryName ) andalso get_type_of( EntryName ) of

		directory ->
			true ;

		_ ->
			false

	end.


% End of verbatim section for myriad/src/utils/file_utils.erl:



% Taken verbatim from myriad/src/utils/text_utils.erl:


% Returns a float which corresponds to the specified text, not depending on its
% being defined as an integer or as a float.
%
% Throws an exception if the conversion failed.
%
%-spec string_to_float( ustring() ) -> float().
string_to_float( String ) ->

	% Erlang is very picky (too much?) when interpreting floats-as-a-string: if
	% there is an exponent, it shall be 'e' (preferably that 'E' which is
	% nevertheless tolerated), and the mantissa must be a floating-point number
	% (hence with a point, such as 3.0e2, not 3e2) and at least one figure must
	% exist after the point (ex: 1.0e2 is accepted, 1.e2 not). Moreover the
	% decimal mark must be '.' (ex: not ',').

	% We overcome all these limitations here, so that for example -1,2E-4, 40E2
	% and 1,E3 are accepted and interpreted correctly.

	% Indeed, 'list_to_float("1e-4")' will raise badarg, whereas
	% 'list_to_float("1.0e-4")' will be accepted.
	%
	% So: if there is no dot on the left of a 'e' or a 'E', add ".0".
	% Moreover, "1.E-4" is also rejected, it must be fixed as well.

	% First, normalise the string, by transforming any 'E' into 'e', and by
	% converting any comma-based decimal mark into a dot:
	%
	LowerString = substitute( _SourceChar=$E, _TargetChar=$e, String ),

	DotString = substitute( $,, $., LowerString ),

	CandidateString = case split_at_first( $e, DotString ) of

		none_found ->
			% There was no exponent here:
			String;

		{ Left, Right } ->
			NewLeft = case split_at_first( $., Left ) of

				none_found ->
					Left ++ ".0";

				% Here there is a dot, yet there is no number afterward (ex:
				% 1.E2), we fix it (to have 1.0E2):
				%
				{ DotLeft, _DotRight="" } ->
					DotLeft ++ ".0";

				{ _DotLeft, _DotRight } ->
					% Already a dot, continue as is:
					Left

			end,
			NewLeft ++ "e" ++ Right

	end,

	try list_to_float( CandidateString ) of

		F ->
			F

	catch

		error:badarg ->

			try list_to_integer( String ) of

				I ->
					float( I )

			catch

				error:badarg ->
					throw( { float_conversion_failed, String } )

			end

	end.



% Splits the specified string according to the first occurrence of specified
% character: returns a pair of two strings, containing respectively all
% characters strictly before and strictly after the first occurrence of the
% marker (which thus is not kept).
%
% Ex: split_at_first( $x, "  aaaxbbbxccc" ) shall return { "  aaa", "bbbxccc" }.
%
%spec split_at_first( uchar(), ustring() ) ->
%							'none_found' | { ustring(), ustring() }.
split_at_first( Marker, String ) ->
	split_at_first( Marker, String, _Acc=[] ).


% Helper:
split_at_first( _Marker, _ToRead=[], _Read ) ->
	none_found;

split_at_first( Marker, _ToRead=[ Marker | T ], Read ) ->
	{ lists:reverse( Read ), T };

split_at_first( Marker, _ToRead=[ Other | T ], Read ) ->
	split_at_first( Marker, T, [ Other | Read ] ).



% Substitutes in specified string the source character with the target one.
%
% Note: simpler and probably more efficient that a regular expression.
%
%spec substitute( uchar(), uchar(), ustring() ) -> ustring().
substitute( SourceChar, TargetChar, String ) ->
	substitute( SourceChar, TargetChar, String, _Acc=[] ).


substitute( _SourceChar, _TargetChar, _String=[], Acc ) ->
	lists:reverse( Acc );

substitute( SourceChar, TargetChar, _String=[ SourceChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ TargetChar | Acc ] );

substitute( SourceChar, TargetChar, _String=[ OtherChar | T ], Acc ) ->
	substitute( SourceChar, TargetChar, T, [ OtherChar | Acc ] ).


% End of verbatim section for myriad/src/utils/text_utils.erl:
