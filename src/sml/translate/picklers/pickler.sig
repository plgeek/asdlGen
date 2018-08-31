(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
[[signature PICKLER_ARG]] describes the argument taken by
the [[functor <X>Pickler]] to build a pickler generator. The
interface describe by the signature are the primitive functions
needed in order to generate code for pickling. The interface is
designed to hide the details of the namming convetions and the
underlying target language from the pickler generator.
**)
signature PICKLER_ARG =
  sig
    structure Ty : TYPE_DECL
    type decl
    type rd_name
    type wr_name

    val call_read       : rd_name -> Ty.ty_id -> Ty.exp
    val call_write      : wr_name -> Ty.ty_id -> Ty.exp -> Ty.exp

    val decl_write      : wr_name ->
                     {name:Ty.ty_id,
		       arg:Ty.ty_exp,
		      body:Ty.exp -> Ty.exp} -> decl

    val decl_read       : rd_name ->
                     {name:Ty.ty_id,
		       ret:Ty.ty_exp,
		      body:Ty.exp} -> decl
  end
(**)
