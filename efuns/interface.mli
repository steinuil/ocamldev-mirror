(*
  
module Xtypes : sig
    type keySym = int
    type pixel
    type modifiers = int      
    type button = int
    type display
      
    val mod1Mask : int
    val anyKey : int
    val anyModifier : int
    val controlMask : int
    val shiftMask : int
    val mod2Mask : int
    val mod3Mask : int
    val mod4Mask : int
    val mod5Mask : int
  end

module WX_types : sig
    
    type font
    type container
    type contained
    val modifiers_event : int ref
    val key_string : string ref
    val key_sym : int ref
    val button_event : int ref
      
    type event_desc =
    | EnterWindow
    | LeaveWindow
    | ButtonPress
    | KeyPress
    | ButtonReleased
    | ButtonMotion
    | Key of Xtypes.keySym * Xtypes.modifiers
    | Button of Xtypes.button * Xtypes.modifiers
    | GrabbedKey of Xtypes.keySym * Xtypes.modifiers
    | GrabbedButton of Xtypes.button * Xtypes.modifiers
    | FocusIn
    | FocusOut
      
type handler = unit -> unit      
    type base_attributes =
    | MinWidth of int
    | MinHeight of int
    | MaxWidth of int
    | MaxHeight of int
    | ExpandX of bool
    | ExpandY of bool
    | FillX of bool
    | FillY of bool
    | IncX of int
    | IncY of int
    | PadX of int
    | PadY of int
    | IpadX of int
    | IpadY of int
    | RetractX of bool
    | RetractY of bool
    | Position of int * int
    | Background of string
    | Foreground of string
    | BorderColor of string
    | BorderWidth of int
    | Bindings of (event_desc * handler) list

  
  val loop : unit -> unit

  end

module WX_text : sig
    type item
    type item_attr
    module type Text =
      sig
        type line
        and text
        val representation : text -> line -> string * int
        val items : text -> line -> item array
      end
    module Make :
      functor (Text : Text) ->
      sig
        type text = Text.text
        and line = Text.line
        and 'a tree_desc = {
            mutable tree_nlines : int;
            mutable tree_width : int;
            mutable tree_height : int;
            mutable tree_parts : 'a array;
            mutable tree_up : tree tree_desc;
            mutable tree_pos : int;
            mutable tree_modified : bool;
            mutable line_height : int;
            mutable line_width : int;
            mutable tree_text : text;
          } 
        and tree = Parts of tree tree_desc | Lines of line tree_desc
        and redraw = NoRedraw | Redraw of int * int * int * int | TotalRedraw
        val get_font : WX_types.font -> item_attr list -> WX_types.font
        val get_attrs :
          Xtypes.pixel * Xtypes.pixel * WX_types.font ->
          item_attr list -> Xtypes.pixel * Xtypes.pixel * WX_types.font
        val modify : 'a tree_desc -> unit
    val make_text : text -> line array -> tree tree_desc
  end
end
  module WX_display : sig
    class t : string -> object
  end
  end

module WX_root : sig
    class t : WX_display.t -> int -> object
        method display : Xtypes.display
  end
end

module X : sig
      
  end

module WX_xterm : sig
    type xterm_display = {
        root_oo: WX_root.t        
      }
    type xterm_window
    type xterm_gc
(*
and xterm_display =
  { dpy: Xtypes.display;
    root: Xtypes.window;
    visual: Xtypes.visual;
    colormap: Xtypes.colormap;
    depth: int;
    mutable f_width: int;
    mutable f_ascent: int;
    mutable f_height: int;
    windows: (Xtypes.window, unit -> unit) Hashtbl.t;
    dpy_pixels_names: string array;
    dpy_pixels: Xtypes.pixel array;
    mutable dpy_pixels_n: int;
    dpy_fonts_names: string array;
    dpy_fonts: Xtypes.font array;
    mutable dpy_fonts_n: int;
    mutable dpy_highlighted: int;
root_oo: WX_root.t
  }
and xterm_window =
  { display: xterm_display;
    win: Xtypes.window;
    gc: Xtypes.gc;
    mutable gc_state: int;
    mutable ncols: int;
    mutable nlines: int;
    mutable table: string array;
    mutable gc_table: int array array;
    mutable modified: bool array;
    mutable region: string option;
    mutable handler: Xtypes.xevent -> unit }
*)    
    type xterm_event =
    | XTKeyPress of Xtypes.modifiers * string * Xtypes.keySym
    | XTResize of int * int
    | XTButtonPress of Xtypes.modifiers * int * int * int
    | XTDoublePress of Xtypes.modifiers * int * int * int
    | XTMouseMotion of Xtypes.modifiers * int * int * int
    
    class t : WX_types.container -> xterm_display -> int -> int ->
      object
        method contained : WX_types.contained
        method xterm : xterm_window
        method check_abort : bool

  end

val destroy_window : xterm_window -> unit
val install_handler : xterm_display -> xterm_window -> 
  (xterm_event -> unit) -> unit
val clear_eol : xterm_window -> int -> int -> int -> unit
  val draw_string : xterm_window -> int -> int -> string -> int -> int -> int -> unit
    val update_displays : unit -> unit
val root_oo : WX_root.t
val set_cutbuffer : xterm_window -> string -> unit
val set_selection : t -> (unit -> string) -> unit
val get_cutbuffer : xterm_window -> string
val update_displays : unit -> unit
val change_font : xterm_window -> string -> unit
val setHighlight : xterm_display -> int -> unit
val create_display : WX_root.t -> string array -> string array ->
  xterm_display
  
val set_property : xterm_window -> string -> string -> unit
  end

module WX_bar : sig
    class h : 
      WX_types.container -> int list -> object
        method contained : WX_types.contained
        method container : WX_types.container
        method container_add_s : WX_types.contained list -> unit
  end
  end

module WX_button : sig
    class t : object
        method height : int
        method root_coordinates : int * int
  end
  end  
module WX_appli : sig
    class t : WX_root.t -> int list ->
      object
        method configure : WX_types.base_attributes list -> unit
        method setWM_NAME : string -> unit
        method setWM_CLASS : string -> string -> unit
        method container :   WX_types.container
        method contained :   WX_types.contained
        method container_add : WX_types.contained -> unit
        method add_button : string -> (WX_button.t -> unit -> unit) -> unit
        method add_menu : string -> (string * (unit -> unit)) array -> unit
        method add_separator : unit 
        method show : unit
        method destroy : unit
  end
end

module WX_adjust : sig
    class t : unit -> object
        method set_params : int -> int -> int -> unit
        method add_subject : (unit -> unit) -> unit
        method get_pos : int -> int
  end
end

module WX_scrollbar : sig
    class v : WX_types.container ->
      WX_adjust.t -> int list -> object
        method contained : WX_types.contained
        method container : WX_types.container
        
  end
  end

module Xrm : sig
    type 'a t
    val create : unit -> 'a t
    val safe_load : 'a t -> string -> unit
  end
  
module Utils : sig
    val count_char_sub : string -> int -> int -> char -> (int * int)
    val count_char : string -> char -> int * int
    val read_string : in_channel -> string
    val normal_name : string -> string -> string
    val hashtbl_mem : ('a,'b) Hashtbl.t -> 'a -> bool
    val list_remove : 'a list -> 'a -> 'a list
    val list_of_hash : ('a,'b) Hashtbl.t -> ('a * 'b) list
    val list_nth : int -> 'a list -> 'a
    val common_suffix : string list -> string -> string * int
    val completion : string list -> string -> string list
    val file_list : string -> string list
    val list_removeq : 'a list -> 'a -> 'a list
    val hash_add_assoc : ('a,'b) Hashtbl.t -> ('a * 'b) list -> unit
    val load_directory : string -> string 
    val is_directory : string -> bool
    val do_and_format : ('a -> 'b) -> 'a -> string * 'b
    val format_to_string : ('a -> 'b) -> 'a -> string
    val set_signal: int -> Sys.signal_behavior -> unit
  end
  
module Str2 : sig
    val replace_matched : string -> string -> string
    val regexp_from_list : string list -> Str.regexp
  end
  
module XK : sig
    val keysym_to_name : (int * string) list
    val name_to_keysym : (string * int) list
    val xk_Menu : int
    val xk_Hyper_R : int
    val xk_Shift_L : int
    val xk_Pointer_Drag_Dflt : int
    val xk_Pointer_Button_Dflt : int
    val xk_Pointer_DblClick_Dflt : int
    val xk_Pointer_Button5 : int
    val xk_Pointer_Button1 : int
    val xk_Pointer_Button2 : int
    val xk_Pointer_Button3 : int
    val xk_Pointer_Button4 : int
    val xk_Pointer_Drag1 : int
    val xk_eacute : int
    val xk_egrave : int
    val xk_ccedilla : int
    val xk_agrave : int
    val xk_ugrave : int
    val xk_mu: int
    val xk_sterling : int
    val xk_section: int
    val xk_degree : int
    val xk_Return : int
    val xk_Up : int
    val xk_Down : int
    val xk_BackSpace : int
    val xk_Tab : int
    val xk_Prior : int
    val xk_Next : int
    val xk_Left : int
    val xk_Right : int
    val xk_grave : int
    val xk_igrave : int
    val xk_ograve : int
    val xk_Atilde : int
    val xk_Adiaeresis : int
    val xk_Acircumflex : int
    val xk_Aacute : int
    val xk_Agrave : int
    val xk_Ediaeresis : int
    val xk_Ecircumflex : int
    val xk_Eacute : int
    val xk_Egrave : int
    val xk_Idiaeresis : int
    val xk_Icircumflex : int
    val xk_Iacute : int
    val xk_Igrave : int
    val xk_Otilde : int
    val xk_Ocircumflex : int
    val xk_Odiaeresis : int
    val xk_Ograve : int
    val xk_Oacute : int
    val xk_Ugrave : int
    val xk_adiaeresis : int
    val xk_apostrophe : int
    val xk_aacute : int
    val xk_iacute : int
    val xk_oacute : int
    val xk_uacute : int
    val xk_Udiaeresis : int
    val xk_Ucircumflex : int
    val xk_Uacute : int
    val xk_ugrave : int
    val xk_semicolon : int
    val xk_asciitilde : int
    val xk_asciicircum : int
    val xk_ecircumflex : int
    val xk_icircumflex : int
    val xk_ucircumflex : int
    val xk_ocircumflex : int
    val xk_acircumflex : int
    val xk_quotedbl : int
    val xk_ediaeresis : int
    val xk_adiaeresis : int
    val xk_idiaeresis : int
    val xk_udiaeresis : int
    val xk_odiaeresis : int
    val xk_ntilde : int
    val xk_atilde : int
    val xk_otilde : int
    val xk_Atilde : int
    val xk_Otilde : int
    val xk_Ntilde : int
    val xk_bracketleft : int
    val xk_bracketright : int
    val xk_q:int
    val xk_Insert: int
    val xk_Delete : int
    val xk_dead_belowdot : int
    val xk_degree : int
    val xk_dead_semivoiced_sound : int
    val	xk_dead_grave : int
    val xk_dead_circumflex : int 
  end

module WX_filesel : sig
    type info =
      { filter: string;
        current_selection: string;
        predicat: info -> bool;
        mutable action: string -> unit;
        mutable cancel: unit -> unit }
    class t :
      WX_root.t ->
      info ->
      WX_types.base_attributes list ->
      object
        method setWM_TRANSIENT_FOR : WX_types.container -> unit
        method destroy : unit
        method show : unit
  end
end

module WX_popup : sig
    class t : WX_root.t ->  (string * (unit -> unit)) array -> object
        method popup_once: int -> int -> int option -> unit
  end
end

*)