signature PP_AST =
  sig
    structure TypeId    : SOURCE_ID
    structure VarId     : SOURCE_ID
    structure ModuleId  : SOURCE_ID
    type pp = PPUtil.pp

    val fix_mid : ModuleId.id -> ModuleId.id
    val fix_tid : TypeId.id -> TypeId.id
    val fix_vid : VarId.id -> VarId.id

    val tid  : TypeId.id   -> pp
    val vid  : VarId.id    -> pp
    val mid  : ModuleId.id -> pp

    val base_tid  : TypeId.id -> pp
    val base_vid  : VarId.id  -> pp

    val local_tid : ModuleId.id -> TypeId.id -> pp
    val local_vid : ModuleId.id -> VarId.id  -> pp
(*
    val decl_tid : TypeId.id   -> pp -> pp
    val decl_vid : VarId.id    -> pp -> pp
    val decl_mid : ModuleId.id -> pp -> pp
*)      
    val num  : int -> pp
    val str  : string -> pp
    val kw   : string -> pp
    val cat  : pp list -> pp

    val empty : pp
    val ws    : pp
    val nl    : pp    
    val vsep  : string -> pp
    val hsep  : string -> pp

    val vb   : int -> pp -> pp -> pp -> pp
    val hb   : int -> pp -> pp -> pp -> pp

    val seq  : pp -> ('a -> pp) -> 'a list -> pp
    val seq' : pp -> ('a -> pp) -> 'a list -> pp

    val opt : pp -> ('a -> pp) -> 'a option -> pp
    val lst : pp -> ('a list -> pp) -> 'a list -> pp
  end