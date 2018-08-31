(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


local
    structure IdOrdKey =
	struct
	    type ord_key = SourceId.sid
	    val compare = SourceId.compare
	end
in
    structure S   = SplaySetFn(IdOrdKey)
    structure Env = SplayMapFn(IdOrdKey)
end




