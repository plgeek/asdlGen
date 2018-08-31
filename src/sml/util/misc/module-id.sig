(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature MODULE_ID =
    sig
	type mid
	type path = {base:string,qualifier:string list}

	val compare: (mid * mid) -> order

	val fromPath     : path -> mid 
	val toPath       : mid ->  path

	val subst        : (path -> path option) -> mid  -> mid
	val prefixBase   : string -> mid -> mid
	val suffixBase   : string -> mid -> mid

	val getQualifier : mid -> string list
	val getBase      : mid -> string

	val eq           : (mid * mid) -> bool
	val eqQualifier  : (mid * mid) -> bool
	val fromString   : string -> mid
	val toString     : mid -> string
	val toString'    : string -> mid -> string
    end



