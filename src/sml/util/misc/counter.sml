(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature COUNTER =
    sig
	type 'a counter

	val mkcounter: (('a * 'a) -> bool ) -> 'a counter
	val add: ('a counter * 'a ) -> ('a counter * int)
	val num: ('a counter * 'a ) -> int
    end

structure Counter:COUNTER =
    struct
	datatype 'a counter = T of {l:('a * int) list,eq:('a * 'a) -> bool}

	fun mkcounter eq = T{l=nil,eq=eq}
	fun add (T{l,eq},x) =
	    let
		fun step((y,c),(l,i)) =
		    if eq(y,x) then
			((y,c+1)::l,c+1)
		    else
			((y,c)::l,i)
		val (l,i) = List.foldl step ([],0) l 
	    in
		if i = 0 then (T{l=(x,1)::l,eq=eq},1)
		else (T{l=l,eq=eq},i)
	    end
	fun num (c,x) =
	    let
		val (_,i) = add (c,x)
	    in
		i-1
	    end
	    
			   
    end
    
    