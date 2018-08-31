(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature IDENTIFIER =
    sig
	type identifier
	val fromString: string -> identifier
	val toString: identifier -> string
	val compare: (identifier * identifier) -> order
	val eq: (identifier * identifier) -> bool
    end
