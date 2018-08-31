(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature LANG_IDS =
    sig
      structure   TypeId : SOURCE_ID
      structure    VarId : SOURCE_ID
      type  ty_id = TypeId.id
      type     id = VarId.id
    end
  
signature LANG_AST =
  sig
    structure ModuleId : SOURCE_ID
    type mod_id = ModuleId.id
    type decls 
    datatype module = Module of {name:mod_id,imports:mod_id list,decls:decls}
  end


