signature FS_UTILS =
  sig
    val cmp     : string * string -> bool
    val mv      : string * string -> unit
    val rm      : string -> unit
    val tmpName : unit -> string
  end