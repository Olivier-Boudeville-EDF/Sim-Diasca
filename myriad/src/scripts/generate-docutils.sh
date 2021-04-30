#!/bin/sh

# Copyright (C) 2010-2021 Olivier Boudeville
#
# This file is part of the Ceylan-Myriad library.
# Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]

# This file is part of the Ceylan-Myriad library (although it does not depend on
# any of our own prerequisites), as it is used by this Myriad layer and the
# layers upward, and we do not want to introduce an extra dependency to
# Ceylan-Hull.


# Notes:
#
#  - docutils has been finally preferred to txt2tags
#
#  - beware of not running more than one instance of this script against the
# same RST source file: they would trigger at the same time (file saved) and
# each would remove the temporary files of the others, leading to spurious
# errors...


usage="Usage: $(basename $0) <target rst file> [--pdf|--all|<comma-separated path(s) to CSS file to be used, ex: common/css/XXX.css,other.css>] [--icon-file ICON_FILENAME]

Generates a final document from specified docutils source file (*.rst).
By default, only the HTML output will be enabled (using any specified CSS file).
If the '--pdf' command-line option is specified, a PDF will be created instead (and thus no HTML output will be generated), whereas, if '--all' is specified, all output formats (i.e. HTML and PDF) will be created.
"


# Left out: --warnings=rst-warnings.txt --traceback --verbose --debug
# Can be removed for debugging: --quiet
docutils_common_opt="--report=error --tab-width=4 --no-generator --no-datestamp --no-source-link --strip-comments --syntax-highlight=short"


# Obtained from 'rst2html -h':
docutils_html_opt="${docutils_common_opt} --cloak-email-addresses --link-stylesheet --no-section-numbering"


# Obtained from 'rst2latex -h':
doc_class=article
#doc_class=report

# Allows to directly add inlined graphics:
docutils_pdf_opt="${docutils_common_opt} --documentclass=${doc_class} --compound-enumerators --latex-preamble=\\usepackage{graphicx}"


latex_to_pdf_opt="-interaction nonstopmode"

begin_marker="---->"

# By default, generate HTML and not PDF:
do_generate_html=0
do_generate_pdf=1


css_base_opt="--link-stylesheet"

docutils_opt="${docutils_html_opt}"

docutils_html="$(which rst2html 2>/dev/null)"

#echo "arguments received: $*"

if [ -z "$1" ]; then
	echo "${begin_marker} Error: no parameter given.
${usage}" 1>&2
	exit 1
fi


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

   echo "${usage}"
   exit 0

fi


rst_file="$1"

if [ -e "${rst_file}" ]; then

	shift

	if [ "$1" = "--pdf" ]; then

		do_generate_html=1
		do_generate_pdf=0
		shift

	elif [ "$1" = "--all" ]; then

		do_generate_html=0
		do_generate_pdf=0
		shift

		css_file_spec="$1"

		# Might be for example "pygments-default.css,foobar.css":
	   if [ -n "${css_file_spec}" ]; then

		   shift

		   echo "  Using CSS file spec ${css_file_spec}."
		   #css_opt="--stylesheet-path=${css_file_spec}"
		   css_opt="--stylesheet=${css_file_spec} ${css_base_opt}"

		fi

		docutils_html_opt="${docutils_html_opt} ${css_opt}"

	else

		css_file_spec="$1"

		if [ -n "${css_file_spec}" ]; then

		   shift

		   #echo "Using CSS file spec ${css_file_spec}."
		   #css_opt="--stylesheet-path=${css_file_spec}"
		   css_opt="--stylesheet=${css_file_spec} ${css_base_opt}"

		fi

		docutils_html_opt="${docutils_html_opt} ${css_opt}"

	fi

else

	echo "${begin_marker} Error: file '$1' not found.
${usage}" 1>&2
	exit 2

fi


if [ "$1" = "--icon-file" ]; then

   shift

   icon_file="$1"
   shift

   if [ ! -f "${icon_file}" ]; then

	   echo "${begin_marker} Error, specified icon file (${icon_file}) does not exist." 1>&2
	   exit 55

   else

	   echo "  Using icon file '${icon_file}'."

   fi

else

   if [ -n "$1" ]; then

	   echo "${begin_marker} Error, unexpected parameter ($1).
${usage}" 1>&2
	   exit 60

   fi

fi



if [ $do_generate_html -eq 0 ]; then

	docutils_html="$(which rst2html 2>/dev/null)"

	if [ -z "${docutils_html}" ]; then

		echo "${begin_marker} Error: unable to find an executable tool to convert docutils files to HTML (rst2html)." 1>&2
		exit 10

	fi

fi

if [ $do_generate_pdf -eq 0 ]; then

	docutils_latex="$(which rst2latex 2>/dev/null)"

	if [ -z "${docutils_latex}" ]; then

		echo "${begin_marker} Error: unable to find an executable tool to convert docutils files to LaTeX (rst2latex)." 1>&2
		exit 11

	fi

	latex_to_pdf="$(which pdflatex 2>/dev/null)"
	if [ -z "${latex_to_pdf}" ]; then

		echo "${begin_marker} Error: unable to find an executable tool to convert LaTeX files to PDF (pdflatex)." 1>&2
		exit 12

	fi

fi




manage_rst_to_html()
# $1: name of the .rst file to convert to HTML.
# $2: name of the HTML target file
{

	source="$1"
	target="$2"

	echo "${begin_marker} building HTML target ${target} from source"


	#echo ${docutils_html} ${docutils_html_opt} "${source}" "${target}"
	${docutils_html} ${docutils_html_opt} "${source}" "${target}"


	if [ ! $? -eq 0 ]; then
		echo "${begin_marker} Error: HTML generation with ${docutils_html} failed for ${source}." 1>&2
		exit 5
	fi

	if [ ! -f "${target}" ]; then

		echo "  Error, no target file '${target}' generated." 1>&2
		exit 60

	fi

	# Better suffixed than prefixed as there may be a leading directory:
	tmp_file="${target}.tmp"

	if [ -n "${icon_file}" ]; then

		echo "  Referencing the icon"

		/bin/cat "${target}" | sed "s|</head>$|<link href=\"${icon_file}\" rel=\"icon\">\n</head>|1" > "${tmp_file}"

		if [ ! -f "${tmp_file}" ]; then

			echo "  Error, no temporary file '${tmp_file}' obtained." 1>&2
			exit 61

		fi

		/bin/mv -f "${tmp_file}" "${target}"

	fi

	echo "  Adding the viewport settings"
	#echo "  Adding the viewport settings (from '${target}' to '${tmp_file}')."

	# Apparently the viewport settings are strongly recommended in all cases,
	# for mobile support:
	#
	/bin/cat "${target}" | sed 's|</head>$|<meta name="viewport" content="width=device-width, initial-scale=1.0">\n</head>|1' > "${tmp_file}"

	if [ ! -f "${tmp_file}" ]; then

		echo "  Error, no temporary file '${tmp_file}' obtained." 1>&2
		exit 62

	fi

	/bin/mv -f "${tmp_file}" "${target}"

	echo "  Generated file is: file://${PWD}/${target}"

}



manage_rst_to_pdf()
# $1: name of the .rst file to convert to PDF.
{

	source="$1"
	target="$2"

	echo "${begin_marker} building PDF target ${target} corresponding to source ${source}"

	# Never triggered:
	if [ -f "${target}" ]; then

		echo "  (removing pre-existing target '${target}')"

		/bin/rm -f "${target}"

	fi

	# Input extension is generally '.rst' (allows to remove only the final
	# extension, even if there were dots in the base name):
	tex_file=$(echo ${source} | sed 's|\.[^\.]*$|.tex|1')


	#echo "Docutils command: ${docutils_latex} ${docutils_pdf_opt} ${source} ${tex_file}"

	${docutils_latex} ${docutils_pdf_opt} ${source} ${tex_file}
	res=$?

	if [ ! ${res} -eq 0 ]; then

		if [ ${res} -eq 1 ]; then
			echo "${begin_marker} Warning: LaTeX generation returned code 1 for ${source}." 1>&2
		else
			echo "${begin_marker} Error: LaTeX generation failed for ${source}." 1>&2
			exit 6
		fi

	fi

	if [ ! -e "${tex_file}" ]; then
		echo "${begin_marker} Error: generated TeX file '${tex_file}' could not be found, probably due to RST errors." 1>&2
		exit 8

	fi

	rubber="$(which rubber 2>/dev/null)"

	#prefer_rubber=0
	prefer_rubber=1

	if [ ${prefer_rubber} -eq 0 ] && [ -x "${rubber}" ]; then

		# Our preferred build method; however it may enter infinite loops and/or
		# output spurious errors like: '[depend] while making XX.pdf: file
		# contents does not seem to settle':
		#
		echo "Using rubber for PDF generation"

		${rubber} --pdf -v "${tex_file}"

		res=$?

		if [ ! ${res} -eq 0 ]; then

			if [ ${res} -eq 1 ] || [ ${res} -eq 2 ]; then
				echo "${begin_marker} Warning: PDF generation returned code ${res} for ${source}." 1>&2
			else
				echo "${begin_marker} Error: PDF generation failed for ${source} (error code: ${res})." 1>&2
				exit 7
			fi

		fi

	else

		echo "Warning: using 'pdflatex' directly, instead of 'rubber'." 1>&2

		# Run thrice on purpose, to fix links:
		echo "LaTeX command: ${latex_to_pdf} ${latex_to_pdf_opt} ${tex_file}"

		${latex_to_pdf} ${latex_to_pdf_opt} "${tex_file}" && \
		${latex_to_pdf} ${latex_to_pdf_opt} "${tex_file}" && \
		${latex_to_pdf} ${latex_to_pdf_opt} "${tex_file}"

		res=$?

		if [ ! ${res} -eq 0 ]; then

			if [ ${res} -eq 1 ]; then
				echo "${begin_marker} Warning: PDF generation returned code 1 for ${source}." 1>&2
			else
				echo "${begin_marker} Error: PDF generation failed for ${source} (error code: ${res})." 1>&2
				exit 7
			fi

		fi

	fi

	if [ -f "${target}" ]; then

		abs_target="$(realpath ${target})"
		echo "  PDF generated, '${abs_target}' ready!"

	else

		echo "  Error, the '${abs_target}' PDF could not be generated." 1>&2
		exit 35

	fi

}



if [ ${do_generate_html} -eq 0 ]; then

	target_html_file="$(echo ${rst_file} | sed 's|.rst$|.html|1')"
	#echo "target_html_file = ${target_html_file}"

	manage_rst_to_html "${rst_file}" "${target_html_file}"

fi


if [ ${do_generate_pdf} -eq 0 ]; then

	target_pdf_file="$(echo ${rst_file} | sed 's|.rst$|.pdf|1')"
	#echo "target_pdf_file = ${target_pdf_file}"

	# PDF generator will not find includes (ex: images) if not already
	# in target dir:
	#
	current_dir="$(pwd)"
	target_dir="$(dirname ${target_pdf_file})"

	source_file="$(basename ${rst_file})"
	target_file="$(basename ${target_pdf_file})"

	cd "${target_dir}"
	manage_rst_to_pdf "${source_file}" "${target_file}"
	cd "${current_dir}"

fi
