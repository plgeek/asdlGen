(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
structure XMLDTD :> XML_DTD =
  struct 
    open LangIds
    datatype content_spec =
        EMPTY
      | ANY
      | Mixed of ty_id list 
      | Children of children
    and children =
        Choice of children * children list
      | Seq of children * children list
      | ZeroOrMore of children
      | ZeroOrOne of children
      | OneOrMore of children
      | Child of ty_id
    and att_type =
        CDATA 
      | NOTATION of  id * id list
      | Enumeration of id * id list
      | OneToken of token
      | Tokens of token
    and token = ID 
      | IDREF 
      | ENTITY
      | NMTOKEN
    and att_default =
        REQUIRED
      | IMPLIED
      | FIXED of att_value * att_value list
    and att_value =
        Str of string
      | EntRef of id
      | CharRef of int
    withtype att_def       = {name:id,att_type:att_type,default:att_default}
         and element_decl  = {element:ty_id,
			      content:content_spec,att_defs:att_def list}
    structure T = mkLangAst(type decls = element_decl list)
      open T
  end




