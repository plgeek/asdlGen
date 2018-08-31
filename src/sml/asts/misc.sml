(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure AstMisc =
    struct
	fun cannon_enumers es =
	    let
		fun cannon ({name,value=NONE},(i,xs)) =
		    (i+1,{name=name,value=SOME i}::xs)
		  | cannon ({name,value=SOME i},(_,xs)) =
		    (i+1,{name=name,value=SOME i}::xs)
	    in
		List.rev (#2(List.foldl cannon (0,[]) es))
	    end
		     
    end
