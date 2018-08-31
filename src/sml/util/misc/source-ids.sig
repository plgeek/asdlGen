(**::
 Source identifiers consist of an identity and external representation. 
**)
(* 
 *
 * COPYRIGHT (c) 1997, 1998, 1999 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature SOURCE_IDS =
  sig
    type namespace
    type sid

    type ord_key = sid
    type path = {ns:namespace,base:string,qualifier:string list}
      
    val mkNameSpace : string -> namespace
        
    (* make a fresh identifier or reuse a non-unqiue id with the same path *)
    val fromPath    : path -> sid
    val toPath      : sid -> path
    val uniqueId    : path -> sid 
    val compare     : (sid * sid) -> order

  end








