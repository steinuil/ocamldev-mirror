val ( -- ) : int ref -> unit
val ( -= ) : int ref -> int -> unit
val ( += ) : int ref -> int -> unit
val ( |= ) : int ref -> int -> unit
val ( lsr ) : int -> int -> int
val ( lsl ) : int -> int -> int
val allMods : int
exception EnableToGetKeysyms
exception EnableToGetModifierMapping
val uKeysymToModifiers : Xtypes.display -> Xtypes.keySym -> int
val computeMaskFromKeyTrans : Xtypes.display -> Xtypes.keyTrans -> unit
val recComputeMaskFromKeyTrans :
  Xtypes.display -> Xtypes.keyTrans list -> unit
val convertCase : int -> int * int
val uKeyCodeToKeySym : Xtypes.display -> int -> int -> Xtypes.keycode
val resetModMap : Xtypes.display -> unit
val initModMap : Xtypes.display -> unit
val uKeyInitialize : Xtypes.display -> unit
val keycodeToKeysym : Xtypes.display -> int -> int -> Xtypes.keycode
exception Found of int * int
val keysymToKeycode : Xtypes.display -> Xtypes.keycode -> int * int
val lookupKeysym : Xtypes.display -> Xtypes.event -> int -> Xtypes.keycode
val refreshKeyboardMapping : Xtypes.display -> Xtypes.event -> unit
val uTranslateKeySym : Xtypes.display -> Xtypes.keySym -> int -> string
val rebindKeysym :
  Xtypes.display -> Xtypes.keySym -> Xtypes.keySym array -> string -> unit
val isKeypadKey : int -> bool
val isPrivateKeypadKey : int -> bool
val isCursorKey : int -> bool
val isPFKey : int -> bool
val isFunctionKey : int -> bool
val isMiscFunctionKey : int -> bool
val isModifierKey : int -> bool
val uTranslateKey : Xtypes.display -> int -> int -> int * Xtypes.keycode
val lookupString :
  Xtypes.display -> Xtypes.event -> string * Xtypes.keycode * int
