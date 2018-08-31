(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure Identifier : IDENTIFIER =
    struct
	type identifier = Atom.atom
	val fromString = Atom.atom
	val toString = Atom.toString
	fun eq (x,y) =
	    case (Atom.compare(x,y)) of
		EQUAL => true
	      | _ => false
	val compare = Atom.compare
    end

