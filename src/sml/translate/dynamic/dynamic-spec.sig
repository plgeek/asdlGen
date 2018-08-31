(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature DYNAMIC_PP =
  sig
    structure Ast : DYNAMIC_AST
      include CODE_PP
        where type code = (Ast.module * Semant.Module.P.props)
  end

signature DYNAMIC_TYPE_DECL =
  sig
    structure Ast : DYNAMIC_AST
      include TYPE_DECL
      where type tag =  {c:Ast.ty_id,v:int}
	and type exp = Ast.exp
	and type ty_exp = Ast.ty_exp
        and type VarId.id = Ast.VarId.id
        and type TypeId.id = Ast.TypeId.id
  end

signature DYNAMIC_SPEC =
  sig
    structure Ty    : DYNAMIC_TYPE_DECL
    val inits : Semant.MEnv.P.init list
    val prims : Semant.type_info list -> Ty.ty_decl list

    val get_reps : Semant.MEnv.P.props ->
                   Semant.kind -> {mkrep:Ty.ty_exp -> Ty.ty_exp,
				   mktid:Ty.ty_id -> Ty.ty_id,
				     con:Ty.ty_con}

    val get_info: Semant.Type.P.props -> Ty.ty_info

    val get_wrappers  : Ty.ty_exp -> Semant.Type.P.props ->
      {natural_ty: Ty.ty_exp,
             wrap: Ty.exp -> Ty.exp,
           unwrap: Ty.exp -> Ty.exp}

    val get_aux_decls : Semant.MEnv.P.props -> Ty.env
                 -> Ty.ty_decl list -> Ty.Ast.decl list
  end

