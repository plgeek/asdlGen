(* 
 *
 * COPYRIGHT (c) 1997, 1998, 1999 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature SEXP_PICKLER_ARG =
  sig
    include PICKLER_ARG

    val rd_name : rd_name
    val wr_name : wr_name

    val write_prod  : Ty.ty_exp -> Ty.ty_id -> Ty.exp list -> Ty.exp
    val read_prod   : Ty.ty_exp -> Ty.ty_id -> Ty.exp -> Ty.exp

    val write_sum   : Ty.ty_exp -> (Ty.tag * Ty.exp list) -> Ty.exp
    val read_sum    : Ty.ty_exp -> (Ty.tag * Ty.exp) list -> Ty.exp

  end
