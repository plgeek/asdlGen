signature SOURCE_ID =
  sig
    type id 
    type path = {base:string,qualifier:string list}

    val namespace    : SourceIds.namespace      

    val fromPath     : path -> id 
    val toPath       : id ->  path
    val uniqueId     : path -> id

    val subst        : (path -> path option) -> id  -> id
    val prefixBase   : string -> id -> id
    val suffixBase   : string -> id -> id

    val getQualifier : id -> string list
    val getBase      : id -> string
      
    val compare      : (id * id) -> order
    val eq           : (id * id) -> bool

    val fromString   : string -> id
    val tempId       : string -> id

    val toString     : id -> string
    val toString'    : string -> id -> string

    val toSid        : id -> SourceIds.sid
    val fromSid      : SourceIds.sid -> id 
  end





