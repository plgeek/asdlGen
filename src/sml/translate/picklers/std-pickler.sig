(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
[[signature STD_PICKLER_ARG]] describes the argument taken by
the [[functor StdPickler]] to build a pickler generator. The
interface describe by the signature are the primitive functions
needed in order to generate code for pickling. The interface is
designed to hide the details of the namming convetions and the
underlying target language from the pickler generator.
**)
(**:[[signature STD_PICKLER_ARG]]:
The [[structure Ty]] represents the expression in the language for
which the pickler in being constructed. The [[type decl]] is an
abstract type the describes a pickler function declaration.
**)
signature STD_PICKLER_ARG =
  sig
    structure Ty : TYPE_DECL
    type decl
(**:[[signature STD_PICKLER_ARG]]:
The functions [[read_tag]] and [[write_tag]] construct expressions
that reads or write a taged values from or to a pickle. The
[[read_tag]] function takes a list of tags and expressions
and returns an expression that evaluates the expression which matches
the appropiate tag value. [[read_tag]] behaves like a specialized
"case" or pattern match.
**)      
    val write_tag  : Ty.tag -> Ty.exp
    val read_tag   : (Ty.tag * Ty.exp) list -> Ty.exp
(**:[[signature STD_PICKLER_ARG]]:
The function [[read]] constructs an expression which reads a value
from a pickle of a given type. The [[write]] function constructs an
expression which writes the out the value of an expression or a known
type to a pickle.
**)
    val read       : Ty.ty_id -> Ty.exp
    val write      : Ty.ty_id -> Ty.exp -> Ty.exp
(**:[[signature STD_PICKLER_ARG]]:
These functions produce actual pickle function declarations.
**)
    val write_decl : {name:Ty.ty_id,
		       arg:Ty.ty_exp,
		      body:Ty.exp -> Ty.exp} -> decl

    val read_decl  : {name:Ty.ty_id,
		       ret:Ty.ty_exp,
		      body:Ty.exp} -> decl
(**:[[signature STD_PICKLER_ARG]]:
The [[expSeq]] function takes a sequence of expresions and converts
it into a single expression with the same semantics.
**)
    val expSeq     : Ty.exp list -> Ty.exp
  end
(**)




