(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature PP_UTIL =
    sig
	type pp 
	val empty: pp	     (* empty      *)
	val nl   : pp        (* hard break *)
	val ws   : pp        (* soft break *)

	val s  : string -> pp
	val d  : int -> pp

	val grp: pp list -> pp (* equiv to box 0 e *)
	val box: int -> pp list -> pp
	val vbox: int -> pp list -> pp

	val cat: pp list -> pp
	    
	val wrap: ('a -> string) -> 'a -> pp
	val opt: {some:'a -> pp,none:pp} -> 'a option -> pp
	val seq: {fmt:'a -> pp,sep:pp} -> 'a list -> pp
	val seq': {fmt:'a -> pp,sep:pp,empty:pp} -> 'a list -> pp
	val seq_term: {fmt:'a -> pp,sep:pp} -> 'a list -> pp

	val pp_to_outstream: TextIO.outstream -> int -> pp -> unit
	val pp_to_string: int -> pp -> string

	(* deprecated *)
	val hblock : int -> pp list -> pp
	val vblock : int -> pp list -> pp
    end

structure PPUtil :> PP_UTIL = 
    struct
	type pp = Wpp.doc

	val empty = Wpp.empty
	val nl = Wpp.nl
	val ws = Wpp.line
	fun wrap f  = Wpp.text o f
	val fromInt = wrap Int.toString 
	val s = Wpp.text
	val d = fromInt
	val cat = List.foldr Wpp.^^ Wpp.empty
	fun box i ds  = Wpp.group (Wpp.nest i (cat ds))
	fun vbox i ds  = (Wpp.nest i (cat ds))

	val hblock = box
	val vblock = vbox
	fun grp ds = Wpp.group (cat ds)

	fun opt {some,none} (SOME v) = (some v)
	  | opt {some,none} NONE = none

	fun seq {sep,fmt} xs =
	  let fun pp (x,pp) =  Wpp.^^(Wpp.^^(sep,fmt x),pp)
	  in case xs of [] => Wpp.empty | (x::xs) =>
	    Wpp.^^(fmt x,List.foldr pp Wpp.empty xs)
	  end
	fun seq' {sep,fmt,empty} [] = empty
	  | seq' {sep,fmt,empty} x = seq {fmt=fmt,sep=sep} x

	fun seq_term {fmt,sep} [] = Wpp.empty
	  | seq_term {fmt,sep} x = Wpp.^^(seq {fmt=fmt,sep=sep} x,sep)

	fun pp_to_outstream outs i pp = Wpp.pretty i pp outs
	fun pp_to_string i pp = Wpp.prettyStr i pp 
    end





