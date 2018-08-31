(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
functor mkIdCvt(structure Ast :
		  sig
		    structure TypeId   : SOURCE_ID
		    structure VarId    : SOURCE_ID
		    structure ModuleId : SOURCE_ID
		  end
		structure IdMap : ID_MAP) : ID_CVT =
  struct
    (* move this out *)
    structure Ht =
      HashTableFn(struct
	type hash_key = string
	val hashVal = HashString.hashString
	val sameKey = (op =): (hash_key * hash_key) -> bool
      end)

    structure TypeId = Ast.TypeId
    structure VarId = Ast.VarId
    structure ModuleId = Ast.ModuleId

    type path = {base:string,qualifier:string list}
    type ('a,'b) cvt = (('a -> path) * (path -> 'b))
    fun mk_tbl n l =
      let val tbl =
	Ht.mkTable(128,Error.error ["Lookup in table ",n])
      in List.app (Ht.insert tbl) l; tbl
      end
    (* more complex than need be *)
    fun fix_path tbl (arg as {base,qualifier}) =
      let
	fun get x =
	  (case (Ht.find tbl x) of
	     NONE => (x,false)
	   | (SOME x) => (x,true))
	     
	val (base,b_sub) = get base
	val (qualifier,q_sub) = List.foldr
	  (fn (x,(xs,ts)) =>
	   let val (x,t) = get x
	   in (x::xs,t orelse ts)
	   end) ([],b_sub) qualifier
      in
	if q_sub then {base=base,qualifier=qualifier}
	else arg
      end

    val id_tbl = mk_tbl IdMap.name IdMap.id_map
    val ty_tbl = mk_tbl IdMap.name IdMap.ty_map

    val id_fix = fix_path id_tbl
    val ty_fix = fix_path ty_tbl
      
    val t2t = (Semant.Type.Id.toPath,Ast.TypeId.fromPath o ty_fix)
    val c2t = (Semant.Con.Id.toPath,Ast.TypeId.fromPath o ty_fix)
    val f2t = (Semant.Field.Id.toPath,Ast.TypeId.fromPath o ty_fix)
    val m2t = (Semant.Module.Id.toPath,Ast.TypeId.fromPath o ty_fix)

    val t2v = (Semant.Type.Id.toPath,Ast.VarId.fromPath o id_fix)
    val c2v = (Semant.Con.Id.toPath,Ast.VarId.fromPath o id_fix)
    val f2v = (Semant.Field.Id.toPath,Ast.VarId.fromPath o id_fix)
    val m2v = (Semant.Module.Id.toPath,Ast.VarId.fromPath o id_fix)

    val t2m = (Semant.Type.Id.toPath,Ast.ModuleId.fromPath o id_fix)
    val c2m = (Semant.Con.Id.toPath,Ast.ModuleId.fromPath o id_fix )
    val f2m = (Semant.Field.Id.toPath,Ast.ModuleId.fromPath o id_fix)
    val m2m = (Semant.Module.Id.toPath,Ast.ModuleId.fromPath o id_fix)

    fun trans (from,to) = to o from
    fun trans_base (from,to) x =
      let val {base,qualifier} = from x
      in to {base=base,qualifier=[]}
      end
  end











