(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure TextPP =
    struct
	
	structure V = AsdlValue
	open PPUtil

	val comma_sep = cat [s ",",ws]
	    
	fun pp_qid false  {base,qualifier} = s (Identifier.toString base)
	  | pp_qid true   {base,qualifier} =
	    seq{fmt=wrap Identifier.toString,
		sep=s "."} (qualifier@[base])
	    
	    
	val pp_typ = ref false
	val pp_qids = ref true
	fun pp_id x = pp_qid (!pp_qids) x
	fun pp_type x =
	    if !pp_typ then	cat [s " : ",pp_id x] else empty
	fun pp_typel x =
	    if !pp_typ then	cat [s " : ",pp_id x, s" list"] else empty
	fun pp_typeo x = 
	    if !pp_typ then	cat [s " : ",pp_id x, s " option"] else empty

	fun pp_asdl_value
	    (V.SumValue{typename,con,attrbs,vs}) =
	    cat[pp_id  con,
		hblock 2 [s "(",seq{fmt=pp_asdl_value,sep=comma_sep}
			  (attrbs@vs),s ")"],pp_type typename]
	  | pp_asdl_value (V.ProductValue{typename,v,vs}) =
	    cat[hblock 2 [s "(",seq{fmt=pp_asdl_value,sep=comma_sep}
			  (v::vs),s ")"],pp_type typename]
	  | pp_asdl_value (V.SequenceValue {typename,vs}) =
	    cat [vblock 1 [s "[",seq{fmt=pp_asdl_value,sep=comma_sep} vs,s
			   "]"],pp_typel typename]
	  | pp_asdl_value (V.NoneValue{typename}) =
	    cat [s "NONE",pp_typeo typename]
	  | pp_asdl_value (V.SomeValue{typename,v}) =
	    cat [s "(SOME ",pp_asdl_value v,s ")",pp_typeo typename]
	  | pp_asdl_value (V.PrimValue{typename,v}) =
	    pp_prim_value v
	and pp_prim_value (V.IntValue x) = PPUtil.wrap Int.toString x
	  | pp_prim_value (V.StringValue x) =
	    cat [s"\"",s(String.toCString x),s "\""]
	  | pp_prim_value (V.IdentifierValue x) =
	    wrap Identifier.toString x
	    
	fun pp_value f outs t x =
	    pp_to_outstream outs 76
	    (cat [s "--",s t,nl,pp_asdl_value x])
    end
