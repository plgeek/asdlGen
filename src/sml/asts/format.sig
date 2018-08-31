(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature FORMAT_DOC =
    sig
	include LANG_IDS
	(* LaTeX intersect HTML *)
	datatype format =
	    STR   of string 
	  | BR
	  | HR
	  | NBS
	  | EM    of format list
	  | BF    of format list
	  | IT    of format list
	  | TT    of format list
	  | RM    of format list
	  | P     of format list
	  | UL    of format list
	  | OL    of format list
	  | DL    of ditem  list

	  | SECT  of int * format list
	  | LABEL of id * format list
	  | REF   of id * format list

	withtype ditem = {tag:format,fmt:format}
	type format_doc = {title:string,body:format list}
      
      include LANG_AST where type decls = format_doc 
    end
