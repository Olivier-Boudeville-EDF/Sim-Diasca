#!/bin/sh


# We can see whiptail is not a drop-in replacement, and that dialog is much
# nicer:
#
target_tool_name="dialog"
#target_tool_name="whiptail"

echo "  Testing '${target_tool_name}'..."

target_tool=$(which ${target_tool_name} 2>/dev/null)

if [ ! -x "${target_tool}" ] ; then

	echo " Error, no '${target_tool_name}' tool found." 1>&2
	exit 5

fi

locale=""

export "LANG=${locale}"

# Instead of a result file, one may use:
# result=$(dialog --stdout --some_box ARGS)
# (see https://fossies.org/linux/Xdialog/samples/buildlist for an example)
#
result_file="/tmp/test.result"


test_msgbox()
{

	${target_tool} --backtitle "Testing msgbox" --title "Hello" --msgbox 'Hello world!' 6 20
	echo "(validated after Enter is pressed)"

}


test_yesno()
{

	${target_tool} --backtitle "Testing yesno" --title "Message" --yesno "Are you having\n fun?" 6 25
	echo "(validated with $?)"

}


test_infobox()
{

	${target_tool} --backtitle "Testing infobox" --infobox "Please wait" 10 30 ; sleep 1
	echo "(disappears after sleep)"

}


test_pause()
{

	${target_tool} --backtitle "Testing pause" --pause "Pausing" 10 30 4
	echo "(disappears once pause is over)"

}


test_inputbox()
{

	${target_tool} --backtitle "Testing inputbox" --inputbox "Enter your name:" 8 40 2>${result_file}
	echo "(input written to standard error, possibly redirected, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_passwordbox()
{

	${target_tool} --backtitle "Testing passwordbox" --passwordbox "Enter some password:" 8 40 2>${result_file}
	echo "(no input echoed, butwritten to standard error, possibly redirected, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_textbox()
{

	${target_tool} --backtitle "Testing textbox" --textbox /etc/profile 22 70
	echo "(file displayed, one may use the cursor keys, Page Up, Page Down, Home, etc. and can exit by pressing <esc> or <enter>)"

}


test_menu()
{

	${target_tool} --backtitle "Testing menu" --menu "Choose one:" 10 30 3 1 red 2 green 3 blue 2>${result_file}
	echo "(allows to select one option, index written to standard error, possibly redirected, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_radiolist()
{

	${target_tool} --backtitle "Testing radiolist" --radiolist "Select CPU type:" 10 40 4 1 386SX off 2 386DX on 3 486SX off 4 486DX off 2>${result_file}
	echo "(allows to select one option, indexes written to standard error, possibly redirected, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_treeview()
{

	${target_tool} --backtitle "Testing treeview" --treeview "Select tree element:" 10 40 5 1 a on 1 2 b on 2 3 c off 1 4 d on 3 2>${result_file}
	echo "(allows to select a tree element, written to standard error, possibly redirected, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_checklist()
{

	${target_tool} --backtitle "Testing checklist" --checklist "Choose toppings:" 10 40 3 1 Cheese on 2 "Tomato Sauce" on 3 Anchovies off 2>${result_file}
	echo "(allows to select non-exclusive options, indexes written to standard error, possibly redirected, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_buildlist()
{

	res=$(${target_tool} --backtitle "Testing buildlist" --stdout --buildlist "Move elements:" 10 40 2 1 "aaa" off 2 "bbb" on 3 "ccc" off 4 "ddd" on)
	echo "(allows to select non-exclusive options, indexes written to standard error, possibly redirected)"
	retval=$?
	case $retval in

		0)
			echo "The user-built list is '$res'.";;
		1)
			echo "Cancel pressed.";;
		255)
			echo "Box closed.";;
	esac
}


test_fselect()
{

	${target_tool} --backtitle "Testing fselect" --fselect / 10 40 2>${result_file}
	echo "(allows to select a file, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_dselect()
{

	${target_tool} --backtitle "Testing dselect" --dselect / 10 40 2>${result_file}
	echo "(allows to select a directory, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_calendar()
{

	${target_tool} --backtitle "Testing calendar" --calendar "Select a date:"  10 40 17 5 1977 2>${result_file}
	echo "(allows to select a date, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_timebox()
{

	${target_tool} --backtitle "Testing timebox" --timebox "Select a time:"  10 40 2>${result_file}
	echo "(allows to select a time, result: '$(cat ${result_file})')"
	/bin/rm -f ${result_file}

}


test_gauge()
{

	for i in $(seq 0 25 100) ; do sleep 1; echo $i | ${target_tool} --backtitle "Testing gauge" --gauge "Please wait for this test gauge" 10 70 0; done
	echo "(allows to display percentage values)"

}


test_all()
{

	test_msgbox
	test_yesno
	test_infobox
	test_inputbox
	test_pause
	test_textbox
	test_menu
	test_radiolist
	test_treeview
	test_checklist
	test_buildlist
	test_fselect
	test_dselect
	test_calendar
	test_timebox
	test_gauge
}


#test_msgbox
#test_yesno
#test_infobox
#test_inputbox
#test_pause
#test_passwordbox
#test_textbox
#test_menu
#test_radiolist
#test_treeview
#test_checklist
#test_buildlist
#test_fselect
#test_dselect
#test_calendar
#test_timebox
#test_gauge

test_all


echo "  End of '${target_tool_name}' test."
