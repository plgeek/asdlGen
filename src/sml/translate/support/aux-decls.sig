(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature AUX_DECLS =
  sig
    structure Ty : TYPE_DECL 
    type decl 
    val trans : Ty.env -> Ty.ty_decl list -> decl list
  end
