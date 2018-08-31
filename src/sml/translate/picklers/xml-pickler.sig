(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature XML_PICKLER_ARG =
  sig
    structure Ty : TYPE_DECL
    type decl

    type attrb_cvt = {toString:Ty.exp -> Ty.exp,fromString:Ty.exp -> Ty.exp}
    type attrib = {name:string,cvt:attrb_cvt}

    val xml_con_name   : Ty.tag -> string
    val xml_write_elem : {name:string,
		       attribs:(attrib * Ty.exp) list,
		       content:Ty.exp list} -> Ty.exp

    val xml_read_elem : {name:string,
		      attribs:attrib list,
		      content:Ty.exp list -> Ty.exp} -> Ty.exp


    val xml_read_tagged_elems : {tag:Ty.tag,
			  attribs:attrib list,
		          content:Ty.exp list -> Ty.exp} list -> Ty.exp

    val read       : Ty.ty_id -> Ty.exp
    val write      : Ty.ty_id -> Ty.exp -> Ty.exp

    val write_decl : {name:Ty.ty_id,
		       arg:Ty.ty_exp,
		      body:Ty.exp -> Ty.exp} -> decl

    val read_decl  : {name:Ty.ty_id,
	 	       ret:Ty.ty_exp,
		      body:Ty.exp} -> decl
  end
