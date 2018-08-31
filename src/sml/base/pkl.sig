(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature PKL =
    sig
	type T
	val write : T -> BinIO.outstream  -> unit
	val read  : BinIO.instream -> T
    end




