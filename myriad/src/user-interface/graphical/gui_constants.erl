% Copyright (C) 2023-2024 Olivier Boudeville
%
% This file is part of the Ceylan-Oceanic library.
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
% Creation date: Monday, March 6, 2023.


% @doc Module defining most of the <b>MyriadGUI constants</b>, notably so that
% higher level MyriadGUI atom-designated values and backend-specific (wx) ones
% can be translated, generally both ways.
%
% Called by gui:generate_support_modules/0.
%
-module(gui_constants).



% When adding a spec function here, do not forget to list its name as well in
% list_topic_spec_functions/0 just below:
%
-export([ get_object_type_topic_spec/0, get_window_style_topic_spec/0,
		  get_frame_style_topic_spec/0, get_button_style_topic_spec/0,
		  get_sizer_flag_topic_spec/0, get_menu_item_id_topic_spec/0,
		  get_button_id_topic_spec/0,
		  get_bitmap_id_topic_spec/0, get_icon_name_id_topic_spec/0,
		  get_menu_item_kind_topic_spec/0, get_menu_style_topic_spec/0,
		  get_status_bar_style_topic_spec/0,
		  get_toolbar_style_topic_spec/0,
		  get_static_text_display_style_topic_spec/0,

		  get_dialog_return_topic_spec/0,
		  get_message_dialog_style_topic_spec/0,
		  get_single_choice_dialog_style_topic_spec/0,
		  get_multi_choice_dialog_style_topic_spec/0,
		  get_text_entry_dialog_style_topic_spec/0,
		  get_file_selection_dialog_style_topic_spec/0,
		  get_directory_selection_dialog_style_topic_spec/0,
		  %get_colour_selection_dialog_style_topic_spec/0,
		  %get_font_selection_dialog_style_topic_spec/0,

		  get_event_type_topic_spec/0,
		  get_direction_topic_spec/0, get_orientation_topic_spec/0 ] ).


-export([ list_topic_spec_functions/0 ]).


% @doc Lists all the functions of this module that define a topic specification.
-spec list_topic_spec_functions() -> [ basic_utils:function_name() ].
list_topic_spec_functions() ->

	% Directly adapted from the first export define:
	[ get_object_type_topic_spec, get_window_style_topic_spec,
	  get_frame_style_topic_spec, get_button_style_topic_spec,
	  get_sizer_flag_topic_spec,
	  get_menu_item_id_topic_spec, get_menu_style_topic_spec,
	  get_button_id_topic_spec, get_bitmap_id_topic_spec,
	  get_icon_name_id_topic_spec, get_menu_item_kind_topic_spec,
	  get_status_bar_style_topic_spec, get_toolbar_style_topic_spec,
	  get_static_text_display_style_topic_spec,

	  get_dialog_return_topic_spec,
	  get_message_dialog_style_topic_spec,
	  get_single_choice_dialog_style_topic_spec,
	  get_multi_choice_dialog_style_topic_spec,
	  get_text_entry_dialog_style_topic_spec,
	  get_file_selection_dialog_style_topic_spec,
	  get_directory_selection_dialog_style_topic_spec,
	  %get_colour_selection_dialog_style_topic_spec,
	  %get_font_selection_dialog_style_topic_spec,

	  get_event_type_topic_spec,
	  get_direction_topic_spec, get_orientation_topic_spec ].



% For the wx defines:
-include("gui_internal_defines.hrl").


% Implementation notes:
%
% These constants correspond to many of the ones that were defined through
% (one-way) functions in gui_wx_backend.erl. Thanks to the
% const_bijective_topics module, they are compiled in the gui_generated module,
% used by many MyriadGUI gui_* modules.
%
% All topics could be configured maybe-ones (instead of strict-ones) to resist
% to the lookèup of unknown elements, yet for a GUI we prefer crashing.
%
% At least generally the first elements are MyriadGUI ones, and the second ones
% are backend ones. Entries shall preferably be listed according to the backend
% native order (thus based on the second elements), to check more easily whether
% a current conversion covers all backend-defined values (that may be enriched
% with newer versions).
%
% Quite often both conversion directions cannot be enabled, as different wx
% defines correspond actually to the same value (then only the first_to_second
% conversion direction is requested, from MyriadGUI to the backend).
%
% As much as possible, when such wx synonyms exist (e.g. wxICON_HAND being an
% alias for wxICON_ERROR), here one is kept and the other(s) prohibited.
%
% Many tables are "mostly one-way"; for example a MyriadGUI widget style is
% often converted into a backend one - not the other way round. If maintaining
% systematically a reverse table was deemed too expensive, performing more
% CPU-demanding yet punctual reverse lookups based on a unique
% MyriadGUI-to-backend table could be considered.
%
% Of course a given MyriadGUI symbol (e.g. 'text', 'right') must be unique only
% within a given bijective table (so different bijective tables may share some
% of such symbols, and associate to possibly different values).
%
% For a topic T, we generate here gui_generated:get_{first,second}_for_T/1 (if
% both directions are enabled) and possibly
% gui_generated:get_maybe_{first,second}_for_T/1.
%
% Some wx symbols, like ?wxFD_MULTIPLE, are resolved at compilation time,
% through a persistent term that is not available when building the current
% module. We determined (see wx_test:determine_constants/0) their actual value
% on our platform (GNU/Linux) and hardcoded it here - although it must be for
% some reason a platform-specific value and/or one that may change over time. An
% improvement at this level will probably be needed.

% See const_bijective_topics:topic_spec() for defaults (i.e. the 'strict'
% element look-up and the 'both' conversion direction).


% Shorthands:

-type topic_spec( F, S ) :: const_bijective_topics:topic_spec( F, S ).

-type bit_mask() :: basic_utils:bit_mask().

-type myriad_object_type() :: gui:myriad_object_type().
-type wx_object_type() :: gui:wx_object_type().
-type direction() :: gui:direction().
-type orientation() :: gui:orientation().

-type event_type() :: gui_event:event_type().
-type wx_event_type() :: gui_event:wx_event_type().

-type window_style_opt() :: gui_window:window_style_opt().
-type frame_style_opt() :: gui_window:frame_style_opt().
-type icon_name_id() :: gui_window:icon_name_id().

-type bitmap_id_opt() :: gui_bitmap:bitmap_id_opt().

-type sizer_flag_opt() :: gui_sizer:sizer_flag_opt().

-type button_id() :: gui_button:button_id().
-type button_style_opt() :: gui_button:button_style_opt().

-type menu_item_id() :: gui_menu:menu_item_id().
-type menu_item_kind() :: gui_menu:menu_item_kind().
-type menu_style() :: gui_menu:menu_style().

-type status_bar_style() :: gui_statusbar:status_bar_style().

-type toolbar_style() :: gui_toolbar:toolbar_style().

-type dialog_return_code() :: gui_dialog:dialog_return_code().
-type message_dialog_style() :: gui_dialog:message_dialog_style().
-type single_choice_dialog_style() :: gui_dialog:single_choice_dialog_style().
-type multi_choice_dialog_style() :: gui_dialog:multi_choice_dialog_style().
-type text_entry_dialog_style() :: gui_dialog:text_entry_dialog_style().
-type file_selection_dialog_style() :: gui_dialog:file_selection_dialog_style().
-type directory_selection_dialog_style() ::
		gui_dialog:directory_selection_dialog_style().
%-type colour_selection_dialog_style() ::
%    gui_dialog:colour_selection_dialog_style().

%-type font_selection_dialog_style() ::
%    gui_dialog:font_selection_dialog_style().


-type wx_art_id() :: gui_wx_backend:wx_art_id().
-type wx_enum() :: gui_wx_backend:wx_enum().
-type wx_orientation() :: gui_wx_backend:wx_orientation().
-type wx_direction() :: gui_wx_backend:wx_direction().

-type wx_id() :: gui_id:wx_id().



% @doc Returns the two-way conversion specification for the 'object_type' topic.
%
% First elements are myriad_object_type(), second ones are wx_object_type().
%
-spec get_object_type_topic_spec() ->
		topic_spec( myriad_object_type(), wx_object_type() ).
get_object_type_topic_spec() ->

	% We use our recommended order (first set for internal, second one for
	% third-party).

	Entries = [
		{ object,                wxObject         },
		{ event_handler,         wxEvtHandler     },
		{ window,                wxWindow         },
		{ control,               wxControl        },
		{ button,                wxButton         },
		{ toggle_button,         wxToggleButton   },
		{ bitmap_button,         wxBitmapButton   },
		{ panel,                 wxPanel          },
		{ gl_canvas,             wxGLCanvas       },
		{ status_bar,            wxStatusBar      },
		{ top_level_window,      wxTopLevelWindow },
		{ dialog,                wxDialog         },
		{ frame,                 wxFrame          },
		{ sizer,                 wxSizer          },
		{ bitmap,                wxBitmap         },
		{ menu,                  wxMenu           },
		{ toolbar,               wxToolBar        },
		{ memory_device_context, wxMemoryDC       } ],

	% Thus strict look-up:
	{ object_type, Entries }.



% @doc Returns the two-way conversion specification for the 'window_style'
% topic.
%
-spec get_window_style_topic_spec() ->
						topic_spec( window_style_opt(), bit_mask() ).
get_window_style_topic_spec() ->

	Entries = [
		{ default_border,            ?wxBORDER_SIMPLE      },
		{ simple_border,             ?wxBORDER_SIMPLE      },
		{ sunken_border,             ?wxBORDER_SUNKEN      },
		{ raised_border,             ?wxBORDER_RAISED      },
		{ static_border,             ?wxSTATIC_BORDER      },
		{ theme_border,              ?wxBORDER_THEME       },
		{ no_border,                 ?wxBORDER_NONE        },
		{ double_border,             ?wxBORDER_DOUBLE      },
		{ transparent,               ?wxTRANSPARENT_WINDOW },
		{ tab_traversable,           ?wxTAB_TRAVERSAL      },
		{ grab_all_keys,             ?wxWANTS_CHARS        },
		{ with_vertical_scrollbar,   ?wxVSCROLL            },
		{ with_horizontal_scrollbar, ?wxHSCROLL            },
		{ never_hide_scrollbars,     ?wxALWAYS_SHOW_SB     },
		{ clip_children,             ?wxCLIP_CHILDREN      },

		% Forces a complete redraw of the window whenever it is resized instead
		% of redrawing just the part of the window affected by resizing:
		%
		% (see https://docs.wxwidgets.org/stable/classwx_window.html)
		%
		{ full_repaint_on_resize, ?wxFULL_REPAINT_ON_RESIZE } ],

	% Cannot be bijective, as a second_to_first function cannot be defined: some
	% wx defines collide, at least on some configurations (e.g. platforms; for
	% example ?wxBORDER_THEME may be equal to ?wxBORDER_DOUBLE):
	%
	{ window_style, Entries, _ElemLookup=strict, _Direction=first_to_second }.



% @doc Returns the two-way conversion specification for the 'frame_style'
% topic.
%
-spec get_frame_style_topic_spec() ->
						topic_spec( frame_style_opt(), bit_mask() ).
get_frame_style_topic_spec() ->

	{ window_style, WindowEntries, ElemLookup, Direction } =
		get_window_style_topic_spec(),

	% As a frame is a special case of window:
	Entries = WindowEntries ++ [

		{ default, ?wxDEFAULT_FRAME_STYLE },
		{ caption, ?wxCAPTION },

		% Useless 'minimize' (Windows-only);
		%{ minimize, ?wxMINIMIZE },

		{ minimize_icon, ?wxMINIMIZE_BOX },

		% Useless 'maximize' (Windows-only);
		%{ maximize, ?wxMAXIMIZE_BOX},

		{ close_icon,      ?wxCLOSE_BOX },
		{ stay_on_top,     ?wxSTAY_ON_TOP },
		{ system_menu,     ?wxSYSTEM_MENU },
		{ resize_border,   ?wxRESIZE_BORDER },
		{ tool_window,     ?wxFRAME_TOOL_WINDOW },
		{ no_taskbar,      ?wxFRAME_NO_TASKBAR },
		{ float_on_parent, ?wxFRAME_FLOAT_ON_PARENT },
		{ shaped,          ?wxFRAME_SHAPED } ],

	{ frame_style, Entries, ElemLookup, Direction }.




% @doc Returns the two-way conversion specification for the 'button_style'
% topic.
%
-spec get_button_style_topic_spec() ->
						topic_spec( button_style_opt(), bit_mask() ).
get_button_style_topic_spec() ->

	Entries = [
		% Meaningless: { default,          0              },
		{ left_justified,   ?wxBU_LEFT     },
		{ right_justified,  ?wxBU_RIGHT    },
		{ top_justified,    ?wxBU_TOP      },
		{ bottom_justified, ?wxBU_BOTTOM   },
		{ exact_fit,        ?wxBU_EXACTFIT }

		% Not implemented:
		%{ flat,          },

			  ],

	{ button_style, Entries }.



% @doc Returns the two-way conversion specification for the 'sizer_flag' topic.
-spec get_sizer_flag_topic_spec() ->
						topic_spec( sizer_flag_opt(), bit_mask() ).
get_sizer_flag_topic_spec() ->

	Entries = [
		{ default,                 0                               },
		{ top_border,              ?wxTOP                          },
		{ bottom_border,           ?wxBOTTOM                       },
		{ left_border,             ?wxLEFT                         },
		{ right_border,            ?wxRIGHT                        },
		{ all_borders,             ?wxALL                          },
		{ expand_fully,            ?wxEXPAND                       },
		{ expand_shaped,           ?wxSHAPED                       },
		{ fixed_size,              ?wxFIXED_MINSIZE                },
		{ counted_even_if_hidden,  ?wxRESERVE_SPACE_EVEN_IF_HIDDEN },
		{ align_center,            ?wxALIGN_CENTER                 },
		{ align_left,              ?wxALIGN_LEFT                   },
		{ align_right,             ?wxALIGN_RIGHT                  },
		{ align_top,               ?wxALIGN_TOP                    },
		{ align_bottom,            ?wxALIGN_BOTTOM                 },
		{ align_center_vertical,   ?wxALIGN_CENTER_VERTICAL        },
		{ align_center_horizontal, ?wxALIGN_CENTER_HORIZONTAL      } ],

	% Not a bijection, the second element '0' is present thrice:
	{ sizer_flag, Entries, _ElemLookup=strict, _Direction=first_to_second }.



% A possible future change is to replace separate tables for button and menu
% item identifiers by a single 'stock_id' one (currently we rely on the event
% type as a context used to select a name identifier either belonging to buttons
% or menu items):
%
% at-doc Returns the two-way maybe-conversion specification for the 'stock_id'
% topic, that is identifiers of standard elements, for which stock buttons and
% menu items are created.
%
% Previously, separate bijective tables existed for menu items and buttons,
% however standard (stock) ones had the same backend identifiers. So for example
% ?wxID_OPEN could correspond both to open_menu_item and open_button (which
% actually relate to the same bitmap). Even by defining a priority order
% (e.g. buttons before menu items), such an approach could not be satisfactory
% as incoming event would include a name identifier that would or would not
% match the one expected by the application (typically open_menu_item would be
% expected, but open_button would be received).
%
% We could consider that identifiers of menu items and buttons could be
% discriminated based on the event at hand (e.g. translates backend identifier
% as menu item for the onItemSelected event type), however we can imagine that
% some event types may apply to menu items and buttons (yet we suppose it is not
% the case), or even that other widgets rely on such identifiers (this does not
% seem to be the case, though).





% Converts wx standard menu item identifiers.
%
% Note that the same numerical identifiers also apply to buttons (button_id/0);
% a single bijective table cannot therefore be considered, as for a given
% backend identifier (e.g. ?wxID_NEW) two name identifiers will correspond
% (e.g. new_menu_item and new_button). Therefore resolving a backend identifier
% into a name one must be done by possibly looking it up in both tables
% (although at least currently buttons fully supersede menu items).
%
% Refer to https://docs.wxwidgets.org/stable/page_stockitems.html for their
% list.
%
-spec get_menu_item_id_topic_spec() -> topic_spec( menu_item_id(), wx_id() ).
get_menu_item_id_topic_spec() ->

	% Some may be lacking (at least on some platforms, like GTK3), in which case
	% neither their icons/images nor their standard label would be displayed
	% (thus resulting in fully-blank entries); they are commented as "ML"
	% (Maybe-Lacking); those that have been known to be lacking but are not
	% anymore, at least on some platforms, are between parentheses.
	%
	% Refer also to wx-x.y/include/wx.hrl for their wx support.

	Entries = [
		{ new_menu_item,             ?wxID_NEW               },
		{ open_menu_item,            ?wxID_OPEN              },
		{ close_menu_item,           ?wxID_CLOSE             },
		{ save_menu_item,            ?wxID_SAVE              },
		{ save_as_menu_item,         ?wxID_SAVEAS            },
		{ revert_to_saved_menu_item, ?wxID_REVERT_TO_SAVED   }, % (ML)
		{ undelete_menu_item,        ?wxID_UNDELETE          },
		{ print_menu_item,           ?wxID_PRINT             },

		% Print preview:
		{ preview_menu_item,         ?wxID_PREVIEW           },

		{ revert_menu_item,          ?wxID_REVERT            }, % ML
		{ edit_menu_item,            ?wxID_EDIT              },
		{ file_menu_item,            ?wxID_FILE              },
		{ properties_menu_item,      ?wxID_PROPERTIES        },
		{ cut_menu_item,             ?wxID_CUT               },
		{ copy_menu_item,            ?wxID_COPY              },
		{ paste_menu_item,           ?wxID_PASTE             },
		{ delete_menu_item,          ?wxID_DELETE            },
		{ find_menu_item,            ?wxID_FIND              },
		{ select_all_menu_item,      ?wxID_SELECTALL         },

		% Find and replace:
		{ replace_menu_item,         ?wxID_REPLACE           },

		{ replace_all_menu_item,     ?wxID_REPLACE_ALL       }, % ML
		{ clear_menu_item,           ?wxID_CLEAR             },
		{ ok_menu_item,              ?wxID_OK                },
		{ cancel_menu_item,          ?wxID_CANCEL            },
		{ apply_menu_item,           ?wxID_APPLY             },
		{ yes_menu_item,             ?wxID_YES               },
		{ no_menu_item,              ?wxID_NO                },
		{ add_menu_item,             ?wxID_ADD               },
		{ convert_menu_item,         ?wxID_CONVERT           },
		{ execute_menu_item,         ?wxID_EXECUTE           },
		{ remove_menu_item,          ?wxID_REMOVE            },
		{ home_menu_item,            ?wxID_HOME              },
		{ refresh_menu_item,         ?wxID_REFRESH           },
		{ stop_menu_item,            ?wxID_STOP              },
		{ index_menu_item,           ?wxID_INDEX             },
		{ select_color_menu_item,    ?wxID_SELECT_COLOR      },
		{ select_font_menu_item,     ?wxID_SELECT_FONT       },
		{ forward_menu_item,         ?wxID_FORWARD           },
		{ backward_menu_item,        ?wxID_BACKWARD          },
		{ up_menu_item,              ?wxID_UP                },
		{ down_menu_item,            ?wxID_DOWN              },
		{ top_menu_item,             ?wxID_TOP               },
		{ bottom_menu_item,          ?wxID_BOTTOM            },
		{ first_menu_item,           ?wxID_FIRST             },
		{ last_menu_item,            ?wxID_LAST              },
		{ jump_to_menu_item,         ?wxID_JUMP_TO           },
		{ info_menu_item,            ?wxID_INFO              },

		{ zoom_factor_one_menu_item, ?wxID_ZOOM_100          }, % (ML)
		{ zoom_factor_fit_menu_item, ?wxID_ZOOM_FIT          }, % (ML)
		{ zoom_factor_in_menu_item,  ?wxID_ZOOM_IN           }, % (ML)
		{ zoom_factor_out_menu_item, ?wxID_ZOOM_OUT          }, % (ML)

		{ undo_menu_item,            ?wxID_UNDO              },
		{ redo_menu_item,            ?wxID_REDO              },
		{ help_menu_item,            ?wxID_HELP              },
		{ preferences_menu_item,     ?wxID_PREFERENCES       },
		{ about_menu_item,           ?wxID_ABOUT             },
		{ floppy_menu_item,          ?wxID_FLOPPY            },
		{ hard_disk_menu_item,       ?wxID_HARDDISK          },
		{ network_menu_item,         ?wxID_NETWORK           },

		{ bold_menu_item,            ?wxID_BOLD              }, % ML
		{ cdrom_menu_item,           ?wxID_CDROM             }, % ML
		{ indent_menu_item,          ?wxID_INDENT            }, % ML
		{ italic_menu_item,          ?wxID_ITALIC            }, % ML

		{ justify_center_menu_item,  ?wxID_JUSTIFY_CENTER    }, % ML
		{ justify_fill_menu_item,    ?wxID_JUSTIFY_FILL      }, % ML
		{ justify_left_menu_item,    ?wxID_JUSTIFY_LEFT      }, % ML
		{ justify_right_menu_item,   ?wxID_JUSTIFY_RIGHT     }, % ML

		{ sort_ascending_menu_item,  ?wxID_SORT_ASCENDING    }, % ML
		{ sort_descending_menu_item, ?wxID_SORT_DESCENDING   }, % ML

		{ spell_check_menu_item,     ?wxID_SPELL_CHECK       }, % ML

		{ strikethrough_menu_item,   ?wxID_STRIKETHROUGH     }, % ML

		{ underline_menu_item,       ?wxID_UNDERLINE         }, % ML
		{ unindent_menu_item,        ?wxID_UNINDENT          }, % ML

		% Quit:
		{ exit_menu_item,            ?wxID_EXIT              },

		% (blank)
		{ undefined,                 ?wxID_ANY               } ],

	{ menu_item_id, Entries, _ElemLookup=maybe }.



% Converts wx standard menu styles.
%
% Refer to https://docs.wxwidgets.org/stable/classwx_menu.html.
%
-spec get_menu_style_topic_spec() -> topic_spec( menu_style(), wx_enum() ).
get_menu_style_topic_spec() ->

	Entries = [
		{ detachable, ?wxMENU_TEAROFF } ],

	{ menu_style, Entries }.



% @doc Returns the two-way maybe-conversion specification for the 'button_id'
% topic.
%
% Converts wx standard menu item identifiers.
%
% Note that the same numerical identifiers also apply to menu items
% (menu_item_id/0).
%
-spec get_button_id_topic_spec() -> topic_spec( button_id(), wx_id() ).
get_button_id_topic_spec() ->

	{ menu_item_id, MenuEntries, ElemLookup } = get_menu_item_id_topic_spec(),

	ButtonEntries = [
		case MenuId of

			% Special case for the 'undefined' identifier:
			undefined ->
				{ MenuId, WxId };

			% For example so that sort_ascending_menu_item becomes
			% sort_ascending_button:
			%
			_ ->
				LabelStr = text_utils:atom_to_string( MenuId ),

				case text_utils:split_before_suffix( _Suffix="_menu_item",
													 LabelStr ) of

					no_suffix ->
						throw( { invalid_menu_id, MenuId, WxId } );

					LeadingStr ->
						ButtonId = text_utils:atom_format( "~ts_button",
														   [ LeadingStr ] ),
						{ ButtonId, WxId }

				end

		end || { MenuId, WxId } <- MenuEntries ],

	{ button_id, ButtonEntries, ElemLookup }.




% @doc Returns the two-way maybe-conversion specification for the 'bitmap_id'
% topic.
%
% Converts wx standard bitmap identifiers.
%
-spec get_bitmap_id_topic_spec() ->
						topic_spec( bitmap_id_opt(), wx_art_id() ).
get_bitmap_id_topic_spec() ->

	Entries = [
		{ error_bitmap,            "wxART_ERROR"            },
		{ question_bitmap,         "wxART_QUESTION"         },
		{ warning_bitmap,          "wxART_WARNING"          },
		{ information_bitmap,      "wxART_INFORMATION"      },
		{ add_bookmark_bitmap,     "wxART_ADD_BOOKMARK"     },
		{ delete_bookmark_bitmap,  "wxART_DEL_BOOKMARK"     },
		{ help_side_panel_bitmap,  "wxART_HELP_SIDE_PANEL"  },
		{ help_settings_bitmap,    "wxART_HELP_SETTINGS"    },
		{ help_book_bitmap,        "wxART_HELP_BOOK"        },
		{ help_folder_bitmap,      "wxART_HELP_FOLDER"      },
		{ help_page_bitmap,        "wxART_HELP_PAGE"        },
		{ go_back_bitmap,          "wxART_GO_BACK"          },
		{ go_forward_bitmap,       "wxART_GO_FORWARD"       },
		{ go_up_bitmap,            "wxART_GO_UP"            },
		{ go_down_bitmap,          "wxART_GO_DOWN"          },
		{ go_to_parent_bitmap,     "wxART_GO_TO_PARENT"     },
		{ go_home_bitmap,          "wxART_GO_HOME"          },
		{ goto_first_bitmap,       "wxART_GOTO_FIRST"       },
		{ goto_last_bitmap,        "wxART_GOTO_LAST"        },
		{ print_bitmap,            "wxART_PRINT"            },
		{ help_bitmap,             "wxART_HELP"             },
		{ tip_bitmap,              "wxART_TIP"              },
		{ report_view_bitmap,      "wxART_REPORT_VIEW"      },
		{ list_view_bitmap,        "wxART_LIST_VIEW"        },
		{ new_folder_bitmap,       "wxART_NEW_DIR"          },
		{ folder_bitmap,           "wxART_FOLDER"           },
		{ open_folder_bitmap,      "wxART_FOLDER_OPEN"      },
		{ go_folder_up_bitmap,     "wxART_GO_DIR_UP"        },
		{ executable_file_bitmap,  "wxART_EXECUTABLE_FILE"  },
		{ normal_file_bitmap,      "wxART_NORMAL_FILE"      },
		{ tick_mark_bitmap,        "wxART_TICK_MARK"        },
		{ cross_mark_bitmap,       "wxART_CROSS_MARK"       },
		{ missing_image_bitmap,    "wxART_MISSING_IMAGE"    },
		{ new_bitmap,              "wxART_NEW"              },
		{ file_open_bitmap,        "wxART_FILE_OPEN"        },
		{ file_save_bitmap,        "wxART_FILE_SAVE"        },
		{ file_save_as_bitmap,     "wxART_FILE_SAVE_AS"     },
		{ file_delete_bitmap,      "wxART_DELETE"           },
		{ copy_bitmap,             "wxART_COPY"             },
		{ cut_bitmap,              "wxART_CUT"              },
		{ paste_bitmap,            "wxART_PASTE"            },
		{ undo_bitmap,             "wxART_UNDO"             },
		{ redo_bitmap,             "wxART_REDO"             },
		{ plus_bitmap,             "wxART_PLUS"             },
		{ minus_bitmap,            "wxART_MINUS"            },
		{ close_bitmap,            "wxART_CLOSE"            },
		{ quit_bitmap,             "wxART_QUIT"             },
		{ find_bitmap,             "wxART_FIND"             },
		{ find_and_replace_bitmap, "wxART_FIND_AND_REPLACE" },
		{ full_screen_bitmap,      "wxART_FULL_SCREEN"      },
		{ edit_bitmap,             "wxART_EDIT"             },
		{ hard_disk_bitmap,        "wxART_HARDDISK"         },
		{ floppy_bitmap,           "wxART_FLOPPY"           },
		{ cdrom_bitmap,            "wxART_CDROM"            },
		{ removable_bitmap,        "wxART_REMOVABLE"        },
		{ backend_logo_bitmap,     "wxART_WX_LOGO"          } ],

	{ bitmap_id, Entries, _ElemLookup=maybe }.



% @doc Returns the two-way maybe-conversion specification for the 'icon_name_id'
% topic.
%
-spec get_icon_name_id_topic_spec() ->
						topic_spec( icon_name_id(), wx_art_id() ).
get_icon_name_id_topic_spec() ->

	% Based on the wx standard bitmap identifiers:
	Entries = [
		{ asterisk_icon,    ?wxICON_ASTERISK    },
		{ stop_icon,        ?wxICON_STOP        },
		{ information_icon, ?wxICON_INFORMATION },
		{ question_icon,    ?wxICON_QUESTION    },
		{ error_icon,       ?wxICON_ERROR       },
		{ warning_icon,     ?wxICON_WARNING     },
		{ hand_icon,        ?wxICON_HAND        },
		{ exclamation_icon, ?wxICON_EXCLAMATION } ],

	% Not a bijection, the element '512' is present thrice, and the element
	% '256' and '2048' are present twice:
	%
	{ icon_name_id, Entries, _ElemLookup=maybe, _Direction=first_to_second }.



% @doc Returns the two-way conversion specification for the 'menu_item_kind'
% topic.
%
-spec get_menu_item_kind_topic_spec() ->
						topic_spec( menu_item_kind(), wx_enum() ).
get_menu_item_kind_topic_spec() ->

	Entries = [
		{ normal,    ?wxITEM_NORMAL    },
		{ toggle,    ?wxITEM_CHECK     },
		{ radio,     ?wxITEM_RADIO     },
		{ separator, ?wxITEM_SEPARATOR },
		{ dropdown,  ?wxITEM_DROPDOWN  } ],

	% No ?wxITEM_MAX

	{ menu_item_kind, Entries }.



% @doc Returns the two-way conversion specification for the 'status_bar_style'
% topic.
%
-spec get_status_bar_style_topic_spec() ->
						topic_spec( status_bar_style(), wx_enum() ).
get_status_bar_style_topic_spec() ->

	Entries = [
		{ normal, ?wxSB_NORMAL },
		{ flat,   ?wxSB_FLAT   },
		{ raised, ?wxSB_RAISED },
		{ sunken, ?wxSB_SUNKEN } ],

	{ status_bar_style, Entries }.



% @doc Returns the two-way conversion specification for the 'toolbar_style'
% topic.
%
-spec get_toolbar_style_topic_spec() ->
						topic_spec( toolbar_style(), wx_enum() ).
get_toolbar_style_topic_spec() ->

	Entries = [
		{ top,               ?wxTB_TOP           },
		{ bottom,            ?wxTB_BOTTOM        },
		{ left,              ?wxTB_VERTICAL      },
		{ right,             ?wxTB_RIGHT         },
		{ flat,              ?wxTB_FLAT          },
		{ dockable,          ?wxTB_DOCKABLE      },
		{ no_icons,          ?wxTB_NOICONS       },
		{ text,              ?wxTB_TEXT          },
		{ no_divider,        ?wxTB_NODIVIDER     },
		{ no_align,          ?wxTB_NOALIGN       },
		{ horizontal_layout, ?wxTB_HORZ_LAYOUT   },
		{ no_tooltips,       ?wxTB_NO_TOOLTIPS   },

		% Warning: ?wxTB_DEFAULT_STYLE is not a constant, it is actually a call:
		% 'wxe_util:get_const(wxTB_DEFAULT_STYLE)', which must moreover be
		% evaluated after wx is started (otherwise it fails in
		% 'persistent_term:get(wx_consts)'). We replaced it as the constant it
		% resolves to (at least in our setting):
		%
		%{ default,           ?wxTB_DEFAULT_STYLE } ],
		{ default,           4 } ],

	% Not a bijection, the element '4' is present twice:
	{ toolbar_style, Entries, _ElemLookup=strict, _Direction=first_to_second }.



% @doc Returns the two-way conversion specification for the
% 'static_text_display_style' topic.
%
-spec get_static_text_display_style_topic_spec() ->
				topic_spec( gui_text:static_display_style(), wx_enum() ).
get_static_text_display_style_topic_spec() ->

	% See https://docs.wxwidgets.org/stable/classwx_static_text.html:

	Entries = [
		{ align_left,       ?wxALIGN_LEFT              },
		{ align_right,      ?wxALIGN_RIGHT             },
		{ center,           ?wxALIGN_CENTRE_HORIZONTAL },
		{ fixed_size,       ?wxST_NO_AUTORESIZE        },
		{ ellipsize_end,    ?wxST_ELLIPSIZE_END        },
		{ ellipsize_middle, ?wxST_ELLIPSIZE_MIDDLE     },
		{ ellipsize_begin,  ?wxST_ELLIPSIZE_START      } ],

	{ static_text_display_style, Entries }.



% @doc Returns the two-way conversion specification for the
% 'dialog_return' topic.
%
-spec get_dialog_return_topic_spec() ->
			topic_spec( dialog_return_code(), wx_enum() ).
get_dialog_return_topic_spec() ->

	% We tried to determine exactly the values returned by actual dialogs:
	Entries = [
		{ ok_returned,     ?wxID_OK     },
		{ cancel_returned, ?wxID_CANCEL },
		{ yes_returned,    ?wxID_YES    },
		{ no_returned,     ?wxID_NO     } ],

	{ dialog_return, Entries }.



% @doc Returns the two-way conversion specification for the
% 'message_dialog_style' topic.
%
-spec get_message_dialog_style_topic_spec() ->
						topic_spec( message_dialog_style(), wx_enum() ).
get_message_dialog_style_topic_spec() ->

	% Refer to https://docs.wxwidgets.org/stable/classwx_message_dialog.html.

	% Perhaps some constants collide on some platforms and not on others
	% (e.g. at least on this GNU/Linux ?wxYES_DEFAULT =:= ?wxOK_DEFAULT); then
	% the lookup should be set to first_to_second.

	Entries = [
		{ ok_button,         ?wxOK               },
		{ cancel_button,     ?wxCANCEL           },
		{ yes_no_buttons,    ?wxYES_NO           },
		{ help_button,       ?wxHELP             },
		{ default_is_no,     ?wxNO_DEFAULT       },
		{ default_is_cancel, ?wxCANCEL_DEFAULT   },
		%{ default_is_yes,    ?wxYES_DEFAULT      }, % alias of wxOK_DEFAULT
		{ default_is_ok,     ?wxOK_DEFAULT       },
		{ no_icon,           ?wxICON_NONE        },
		{ error_icon,        ?wxICON_ERROR       }, % alias of wxICON_HAND

		% Alias of wxICON_EXCLAMATION:
		{ warning_icon,      ?wxICON_WARNING     },

		{ question_icon,     ?wxICON_QUESTION    },
		{ information_icon,  ?wxICON_INFORMATION },
		{ security_icon,     ?wxICON_AUTH_NEEDED },
		{ stay_on_top,       ?wxSTAY_ON_TOP      },
		{ center,            ?wxCENTRE           } ],

	{ message_dialog_style, Entries }.



% @doc Returns the two-way conversion specification for the
% 'single_choice_dialog_style' topic.
%
-spec get_single_choice_dialog_style_topic_spec() ->
						topic_spec( single_choice_dialog_style(), wx_enum() ).
get_single_choice_dialog_style_topic_spec() ->

	% Refer to
	% https://docs.wxwidgets.org/stable/classwx_single_choice_dialog.html.

	% See get_message_dialog_style_topic_spec/0 for more comments.

	Entries = [
		{ ok_button,     ?wxOK     },
		{ cancel_button, ?wxCANCEL },
		{ center,        ?wxCENTRE } ],

	{ single_choice_dialog_style, Entries }.



% @doc Returns the two-way conversion specification for the
% 'multi_choice_dialog_style' topic.
%
-spec get_multi_choice_dialog_style_topic_spec() ->
						topic_spec( multi_choice_dialog_style(), wx_enum() ).
get_multi_choice_dialog_style_topic_spec() ->

	% See https://docs.wxwidgets.org/stable/classwx_multi_choice_dialog.html for
	% more comments.

	Entries = [
		{ ok_button,     ?wxOK     },
		{ cancel_button, ?wxCANCEL },
		{ center,        ?wxCENTRE } ],

	{ multi_choice_dialog_style, Entries }.



% @doc Returns the two-way conversion specification for the
% 'text_entry_dialog_style' topic.
%
-spec get_text_entry_dialog_style_topic_spec() ->
						topic_spec( text_entry_dialog_style(), wx_enum() ).
get_text_entry_dialog_style_topic_spec() ->

	% See https://docs.wxwidgets.org/stable/classwx_text_entry_dialog.html for
	% more comments.

	Entries = [
		{ ok_button,     ?wxOK     },
		{ cancel_button, ?wxCANCEL },
		{ center,        ?wxCENTRE } ],

	{ text_entry_dialog_style, Entries }.



% @doc Returns the two-way conversion specification for the
% 'file_selection_dialog_style' topic.
%
-spec get_file_selection_dialog_style_topic_spec() ->
						topic_spec( file_selection_dialog_style(), wx_enum() ).
get_file_selection_dialog_style_topic_spec() ->

	% See https://docs.wxwidgets.org/stable/classwx_file_dialog.html for more
	% comments.

	Entries = [
		{ open_file,          ?wxFD_OPEN                     },
		{ save_file,          ?wxFD_SAVE                     },
		{ confirm_overwrite,  ?wxFD_OVERWRITE_PROMPT         },
		{ follow_no_link,     ?wxFD_NO_FOLLOW                },
		{ only_existing_file, ?wxFD_FILE_MUST_EXIST          },

		% See implementation notes:
		%{ multiple_files,     ?wxFD_MULTIPLE                },
		{ multiple_files,     512                            },

		{ change_working_dir, ?wxFD_CHANGE_DIR               },
		{ preview_selected,   ?wxFD_PREVIEW                  },
		{ show_hidden_files,  ?wxFD_SHOW_HIDDEN              } ],

	{ file_selection_dialog_style, Entries }.



% @doc Returns the two-way conversion specification for the
% 'directory_selection_dialog_style' topic.
%
-spec get_directory_selection_dialog_style_topic_spec() ->
					topic_spec( directory_selection_dialog_style(), wx_enum() ).
get_directory_selection_dialog_style_topic_spec() ->

	% See https://docs.wxwidgets.org/stable/classwx_dir_dialog.html for more
	% comments.

	Entries = [
		{ only_existing_directory,   ?wxDD_DIR_MUST_EXIST },
		{ change_working_dir,        ?wxDD_CHANGE_DIR },
		{ multiple_directories,      ?wxDD_MULTIPLE },
		{ show_hidden_directories,   ?wxDD_SHOW_HIDDEN },

		% However wxDD_NEW_DIR_BUTTON not in wxwidgets docs:
		{ enable_directory_creation, ?wxDD_NEW_DIR_BUTTON } ],

	{ directory_selection_dialog_style, Entries }.


% No colour_selection_dialog_style topuc.

% No font_selection_dialog_style topic.



% @doc Returns the two-way conversion specification for the 'event_type' topic.
-spec get_event_type_topic_spec() ->
						topic_spec( event_type(), wx_event_type() ).
get_event_type_topic_spec() ->

	Entries = [

		% Mouse section:

		{ onMouseMoved, motion },

		{ onMouseLeftButtonPressed,       left_down   },
		{ onMouseLeftButtonReleased,      left_up     },
		{ onMouseLeftButtonDoubleClicked, left_dclick },

		{ onMouseMiddleButtonPressed,       middle_down   },
		{ onMouseMiddleButtonReleased,      middle_up     },
		{ onMouseMiddleButtonDoubleClicked, middle_dclick },

		{ onMouseRightButtonPressed,       right_down   },
		{ onMouseRightButtonReleased,      right_up     },
		{ onMouseRightButtonDoubleClicked, right_dclick },


		{ onMouseFourthButtonPressed,       aux1_down   },
		{ onMouseFourthButtonReleased,      aux1_up     },
		{ onMouseFourthButtonDoubleClicked, aux1_dclick },


		{ onMouseFifthButtonPressed,       aux2_down   },
		{ onMouseFifthButtonReleased,      aux2_up     },
		{ onMouseFifthButtonDoubleClicked, aux2_dclick },

		{ onMouseWheelScrolled, mousewheel },

		{ onMouseEnteredWindow, enter_window },
		{ onMouseLeftWindow,    leave_window },


		% Keyboard section:

		{ onCharEntered,     char      },
		{ onCharEnteredHook, char_hook },
		{ onKeyPressed,      key_down  },
		{ onKeyReleased,     key_up    },


		% Menu section/tool(bar) section:

		{ onItemSelected,     command_menu_selected },
		{ onToolbarEntered,   command_tool_enter    },
		{ onToolRightClicked, command_tool_rclicked },


		% Button-related section:
		{ onButtonClicked, command_button_clicked },
		{ onButtonToggled, command_togglebutton_clicked },


		% Window section:

		{ onShown,         show },
		{ onResized,       size },
		{ onRepaintNeeded, paint },
		{ onWindowClosed,  close_window } ],

	{ event_type, Entries }.



% @doc Returns the two-way conversion specification for the 'direction' topic.
-spec get_direction_topic_spec() ->
						topic_spec( direction(), wx_direction() ).
get_direction_topic_spec() ->
	{ direction, [ { vertical,   ?wxVERTICAL   },
				   { horizontal, ?wxHORIZONTAL } ] }.


% @doc Returns the two-way conversion specification for the 'orientation' topic.
-spec get_orientation_topic_spec() ->
						topic_spec( orientation(), wx_orientation() ).
get_orientation_topic_spec() ->
	DirEntries = pair:second( get_direction_topic_spec() ),
	{ orientation, [ { both, ?wxBOTH } | DirEntries ] }.
