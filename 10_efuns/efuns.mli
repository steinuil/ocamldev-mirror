open Interface
  
exception UnboundKey
type map = {
  char_map : binding array;
  mutable complex_bindings : (key * binding) list;
  mutable interactives : (string * (action * prefix option)) list;
} 
and key = mod_ident * Xtypes.keySym 
and action = frame -> unit 
and generic_action =
    BufferAction of (buffer -> unit)
  | FrameAction of (frame -> unit)
and mod_ident = NormalMap | ControlMap | MetaMap | ControlMetaMap
and prefix = key list 
and binding = Function of action | Prefix of map | Unbound
and buffer = {
  mutable buf_text : Text.t;
  mutable buf_modified : int;
  mutable buf_name : string;
  mutable buf_filename : string option;
  mutable buf_last_saved : int;
  mutable buf_history : (int * Text.action) list;
  mutable buf_charreprs : string array;
  mutable buf_syntax_table : bool array;
  mutable buf_map_partial : bool;
  buf_map : map;
  mutable buf_sync : bool;
  mutable buf_mark : Text.point option;
  mutable buf_point : Text.point;
  mutable buf_start : Text.point;
  mutable buf_shared : int;
  mutable buf_minor_modes : minor_mode list;
  mutable buf_major_mode : major_mode;
  mutable buf_finalizers : (unit -> unit) list;
  mutable buf_vars : Local.vars;
  buf_location : location;
} 
and major_mode = {
  maj_name : string;
  maj_map : map;
  mutable maj_hooks : (buffer -> unit) list;
  mutable maj_vars : Local.vars;
} 
and minor_mode = {
  min_name : string;
  min_map : map;
  mutable min_hooks : (buffer -> unit) list;
  mutable min_vars : Local.vars;
} 
and frame = {
  mutable frm_buffer : buffer;
  mutable frm_location : location;
  mutable frm_window : window;
  mutable frm_last_text_updated : int;
  mutable frm_last_buf_updated : int;
  mutable frm_prefix : key list;
  mutable frm_repeat_action : int;
  mutable frm_last_action : action;
  mutable frm_start : Text.point;
  mutable frm_end : Text.point;
  mutable frm_y_offset : int;
  mutable frm_point : Text.point;
  mutable frm_cursor_x : int;
  mutable frm_cursor_y : int;
  mutable frm_cursor : string;
  mutable frm_cursor_attr : Text.attribute;
  mutable frm_force_point : bool;
  mutable frm_force_start : bool;
  mutable frm_force_cursor : bool;
  mutable frm_x_offset : int;
  mutable frm_cutline : int;
  mutable frm_has_scrollbar : int;
  mutable frm_has_status_line : int;
  mutable frm_status : status;
  mutable frm_xpos : int;
  mutable frm_ypos : int;
  mutable frm_width : int;
  mutable frm_height : int;
  mutable frm_table : line_repr array;
  mutable frm_killed : bool;
  mutable frm_mini_buffer : string option;
  mutable frm_redraw : bool;
} 
and status_info =
    StatModified
  | StatName
  | StatLine
  | StatCol
  | StatFile
  | StatMode
and status = {
  mutable status_string : string;
  mutable status_modified : bool;
  mutable status_format : (status_info * (int * int)) list;
  mutable stat_col : int;
  mutable stat_name : string;
  mutable stat_file : string;
  mutable stat_line : int;
  mutable stat_modified : bool;
  mutable stat_modes : minor_mode list;
  mutable stat_mode : major_mode;
} 
and line_repr = {
  mutable repr_line : Text.line;
  mutable repr_y : int;
  mutable repr_x : int;
  mutable repr_prev_offset : int;
  mutable repr_prev_reprs : Text.repr list;
  mutable repr_offset : int;
  mutable repr_reprs : Text.repr list;
} 
and top_window = {
  mutable top_location : location;
  mutable top_display : WX_xterm.xterm_display option;
  mutable top_xterm : WX_xterm.xterm_window option;
  mutable top_term : WX_xterm.t;
  top_attrs : WX_xterm.xterm_gc option array;
  mutable top_windows : window;
  mutable top_mini_buffers : frame list;
  mutable top_width : int;
  mutable top_height : int;
  mutable top_name : string;
  mutable top_active_frame : frame;
  mutable top_second_cursor : frame option;
  mutable top_root : WX_root.t;
  mutable top_appli : WX_appli.t;
  mutable top_scrollbar : WX_adjust.t;
} 
and window = {
  mutable win_xpos : int;
  mutable win_ypos : int;
  mutable win_width : int;
  mutable win_height : int;
  mutable win_down : window_down;
  mutable win_up : window_up;
  mutable win_mini : bool;
} 
and window_up = Window of window | TopWindow of top_window
and window_down =
    HComb of window * window
  | VComb of window * window
  | NoFrame of unit
  | WFrame of frame
and location = {
  loc_map : map;
  mutable loc_windows : top_window list;
  mutable loc_buffers : (string, buffer) Hashtbl.t;
  mutable loc_files : (string, buffer) Hashtbl.t;
  mutable loc_dirname : string;
  mutable loc_width : int;
  mutable loc_height : int;
  mutable loc_fg : string;
  mutable loc_bg : string;
  mutable loc_font : string;
  loc_vars : Local.vars;
  mutable loc_counter : int;
  loc_fonts : (string, int) Hashtbl.t;
  loc_fonts_names : string array;
  mutable loc_fonts_n : int;
  loc_colors : (string, int) Hashtbl.t;
  loc_colors_names : string array;
  mutable loc_colors_n : int;
  loc_mutex : Concur.Mutex.t;
} 
and sens = Backward | Forward
and to_regexp = Regexp | RegexpString
val start_hooks : (location -> unit) list ref
val add_start_hook : (location -> unit) -> unit
val init : location -> unit
val set_global : location -> 'a Local.var -> 'a -> unit
val set_local : buffer -> 'a Local.var -> 'a -> unit
val get_var : buffer -> 'a Local.var -> 'a
val get_global : location -> 'a Local.var -> 'a
val get_local : buffer -> 'a Local.var -> 'a
val set_minor_var : minor_mode -> 'a Local.var -> 'a -> unit
val set_major_var : major_mode -> 'a Local.var -> 'a -> unit
val exec_hooks : ('a -> unit) list -> 'a -> unit
val add_hook : location -> 'a list Local.var -> 'a -> unit
val load_path : string list Options.option_record
(*val path : string list ref *)
(*val efuns_path : string list *)
val init_files : string list ref
val init_frames : string list ref
val displayname : string ref
val no_init : bool ref
val xdefaults : string
val resname : string list
val x_res : string Xrm.t
val t : string Xrm.t
val width_opt : int option ref
val height_opt : int option ref
val font_opt : string option ref
val fg_opt : string option ref
val bg_opt : string option ref
val check : bool ref
val width : int Options.option_record
val height : int Options.option_record
val font : string Options.option_record
val foreground : string Options.option_record
val background : string Options.option_record
(** Max *)
val server : bool Options.option_record
(** Fin Max *)
val actions : (string, generic_action) Hashtbl.t
val define_action : string -> (frame -> unit) -> unit
val define_buffer_action : string -> (buffer -> unit) -> unit
val no_action : generic_action
val get_action : string -> generic_action
val execute_action : string -> frame -> unit
val execute_buffer_action : string -> buffer -> unit
val string_to_regex : string -> string * Str.regexp
val regexp_option : (string * Str.regexp) Options.option_class
