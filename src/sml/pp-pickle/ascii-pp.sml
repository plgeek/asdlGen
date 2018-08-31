(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure ASCIIPP =
    struct
	
	structure V = AsdlValue
	open PPUtil

	    
	fun pp_id  {base,qualifier} = seq{fmt=wrap Identifier.toString,
					    sep=s "_"} (qualifier@[base])
	fun pp_asdl_value
	    (V.SumValue{typename,con,attrbs,vs}) =
	    hblock 2 [pp_id con,ws,seq{fmt=pp_asdl_value,sep=ws}(attrbs@vs)]
	  | pp_asdl_value (V.ProductValue{typename,v,vs}) =
	    hblock 2 [seq{fmt=pp_asdl_value,sep=ws} (v::vs)]
	  | pp_asdl_value (V.SequenceValue {typename,vs}) =
	    vblock 2 [s "*[",seq{fmt=pp_asdl_value,sep=ws} vs,s "]"]
	  | pp_asdl_value (V.NoneValue{typename}) =
	    cat [s "?[]"]
	  | pp_asdl_value (V.SomeValue{typename,v}) =
	    cat [s "?[",pp_asdl_value v,s "]"]
	  | pp_asdl_value (V.PrimValue{typename,v}) =
	    pp_prim_value v
	and pp_prim_value (V.IntValue x) =  s ((Int.toString x)^"_")
	  | pp_prim_value (V.StringValue x) =
	    cat [s"\"",s(String.toCString x),s "\""]
	  | pp_prim_value (V.IdentifierValue x) =
	    cat [s"'",s (String.toCString (Identifier.toString x)),s "'"]
	    
	fun pp_value f outs t x =
	    pp_to_outstream outs 256
	    (cat [s "# ",s t,nl,pp_asdl_value x,nl])
    end
