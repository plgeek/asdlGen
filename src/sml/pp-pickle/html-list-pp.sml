(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)



structure HTMLListPP =
    struct
	
	structure V = AsdlValue
	open PPUtil

	val comma_sep = cat [s ",",ws]
	val li_sep = cat [nl,s "<li>"]
	val dt_sep = cat [nl,s "<dd>"]
	    
	fun pp_qid false  {base,qualifier} = s (Identifier.toString base)
	  | pp_qid true   {base,qualifier} =
	    seq{fmt=wrap Identifier.toString,
		sep=s "."} (qualifier@[base])
	    
	    
	val pp_qids = ref true
	fun pp_id x = pp_qid (!pp_qids) x
	fun pp_type x =	cat [s "<em>",pp_id x,s "</em>"] 
	fun pp_typel x = cat [s "<em>",pp_id x, s " list</em>"] 
	fun pp_typeo x = cat [s "<em>",pp_id x, s " option</em>"] 

	fun pp_field f (x,id) =
	    cat [s "<li>",s (Identifier.toString id),
		 s " = ",pp_asdl_value f x,nl]
	and pp_fields f (x,vs) =
	    vblock 2 [s "<ul compact>",nl,
		      cat ((List.map (pp_field f)) ((f x) vs)),
		      s "</ul>"]
	    
	and pp_asdl_value f (V.SumValue{typename,con,attrbs,vs}) =
	    cat[s "<b>",
		pp_id  con,
		s "</b> : ",
		pp_type typename,nl,
		pp_fields f ((SOME typename,SOME con),attrbs@vs)]
	  | pp_asdl_value f (V.ProductValue{typename,v,vs}) =
	    cat[pp_type typename,nl,
		pp_fields f ((SOME typename,NONE),v::vs)]
	  | pp_asdl_value f (V.SequenceValue {typename,vs=[]}) =
	    cat [s "<em>empty </em>",pp_typel typename]
	  | pp_asdl_value f (V.SequenceValue {typename,vs}) =
	    cat [pp_typel typename,nl,
		 vblock 1 [s "<ol compact><li>",
			   seq{fmt=(pp_asdl_value f),sep=li_sep} vs,s
			   "</ol>"]]
	  | pp_asdl_value f (V.NoneValue{typename}) =
	    cat [s "NONE",pp_typeo typename]
	  | pp_asdl_value f (V.SomeValue{typename,v}) =
	    cat [pp_typeo typename,s "(SOME ",pp_asdl_value f v,s ")"]
	  | pp_asdl_value f (V.PrimValue{typename,v}) =
	    pp_prim_value v
	and pp_prim_value (V.IntValue x) = PPUtil.wrap Int.toString x
	  | pp_prim_value (V.StringValue x) =
	    cat [s"<tt>\"",s(String.toCString x),s "\"</tt>"]
	  | pp_prim_value (V.IdentifierValue x) =
	    wrap Identifier.toString x
	    
	fun pp_value f outs t x =
	    pp_to_outstream outs 76
	    (cat [s "<h1>",s t,s "</h1>",nl,
		  (pp_asdl_value f x)])
    end
