(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure GenericTypeId =
  mkSourceId(val namespace = SourceIds.mkNameSpace "generic type"
	     val sep = ".")
structure GenericVarId =
  mkSourceId(val namespace = SourceIds.mkNameSpace "generic type"
	     val sep = ".")
structure GenericModId =
  mkSourceId(val namespace = SourceIds.mkNameSpace "generic type"
	     val sep = ".")
functor mkLangAst(type decls) =
  struct
    structure ModuleId :> SOURCE_ID = GenericModId
    type mod_id = ModuleId.id
    type  decls = decls
    datatype module = Module of {name:mod_id,
				 imports:mod_id list,
				 decls:decls}
  end
structure LangIds :> LANG_IDS =
    struct
	structure   TypeId :> SOURCE_ID = GenericTypeId
	structure    VarId :> SOURCE_ID = GenericVarId
	type  ty_id = TypeId.id
	type     id = VarId.id
    end