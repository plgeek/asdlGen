(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature ID_CVT =
    sig
	structure TypeId    : SOURCE_ID
	structure VarId     : SOURCE_ID
	structure ModuleId  : SOURCE_ID
	type ('a,'b) cvt
	  
	val t2t: (Semant.Type.Id.id, TypeId.id) cvt
	val c2t: (Semant.Con.Id.id, TypeId.id) cvt
	val f2t: (Semant.Field.Id.id, TypeId.id) cvt
	val m2t: (Semant.Module.Id.id, TypeId.id) cvt

	val t2v: (Semant.Type.Id.id, VarId.id) cvt
	val c2v: (Semant.Con.Id.id, VarId.id) cvt
	val f2v: (Semant.Field.Id.id, VarId.id) cvt
	val m2v: (Semant.Module.Id.id, VarId.id) cvt

	val t2m: (Semant.Type.Id.id, ModuleId.id) cvt
	val c2m: (Semant.Con.Id.id, ModuleId.id) cvt
	val f2m: (Semant.Field.Id.id, ModuleId.id) cvt
	val m2m: (Semant.Module.Id.id, ModuleId.id) cvt

	val trans      : ('a,'b) cvt -> 'a -> 'b
	val trans_base : ('a,'b) cvt -> 'a -> 'b
    end

