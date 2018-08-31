(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure XMLPP =
    struct
      structure V =
	struct 
	  open AsdlValue AsdlValueUtil
	end

	open PPUtil

	val toHex = Int.fmt StringCvt.HEX
	fun fix_char #"&" = "&amp;"
	  | fix_char #"'" = "&apos;"
	  | fix_char #"\"" = "&quot;"
	  | fix_char #">" = "&gt;"
	  | fix_char #"<" = "&lt;"
	  | fix_char x =
	  if (Char.isPrint x) then Char.toString x
	  else "&#x"^(toHex (Char.ord x))^";"

	fun pp_id {base,qualifier} =  seq{fmt=wrap Identifier.toString,
					  sep=s "."} (qualifier@[base])

	fun pp_label_opt NONE = empty
	  | pp_label_opt (SOME l) =
	  cat [s " lb=\"",s (Identifier.toString l),s "\""]

	fun pp_element (i,l) (SOME c) =
	  cat[s "<",pp_id i,pp_label_opt l,s ">",ws,c,ws,s "</",pp_id i,s ">"]
	  | pp_element (i,l) NONE =  cat[s "<",pp_id i,pp_label_opt l,s "/>"]

	fun pp_seq (i,x) l c =
	  vblock 0 [s "<",pp_id i,s "-seq",pp_label_opt x,
		    s " sz=\"",d l,s "\">",ws,c,
	      s "</",pp_id i,s "-seq>"]
	fun pp_opt (i,x) NONE  =
	  cat[s "<",pp_id i,s "-opt",pp_label_opt x,s " sz=\"0\"></",
	      pp_id i,s "-opt>"]
	  | pp_opt (i,x) (SOME c)  =
	       cat[s "<",pp_id i,s "-opt",pp_label_opt x,s "sz=\"1\">",
		   c,s "</",pp_id i,s "-opt>"]

	fun pp_value f outs t x =
	  let
	    fun pp_asdl_value l (V.SumValue{typename,con,attrbs,vs}) =
	      let
		val lvs = f (SOME typename,SOME con) (attrbs@vs)
	      in
		pp_element (typename,l)
		(SOME (vblock 2 [pp_element (con,NONE)
				 (SOME (seq{fmt=pp_lv,sep=ws} lvs))]))
	      end
	      | pp_asdl_value l (V.ProductValue{typename,v,vs}) =
	      let
		val lvs = f (SOME typename,NONE) (v::vs)
	      in
		pp_element (typename,l)
		(SOME (hblock 2 [seq{fmt=pp_lv,sep=ws} lvs]))
	      end
	      | pp_asdl_value l (V.SequenceValue {typename,vs}) =
	      pp_seq (typename,l) (List.length vs)
	      (seq{fmt=pp_asdl_value NONE,sep=ws} vs)
	      
	      | pp_asdl_value l (V.NoneValue{typename}) =
	      pp_opt (typename,l) NONE
	      
	      | pp_asdl_value l (V.SomeValue{typename,v}) =
	      pp_opt (typename,l) (SOME(pp_asdl_value NONE v))
	      
	      | pp_asdl_value l (V.PrimValue{typename,v}) =  pp_prim_value l v
	    and pp_prim_value l (V.IntValue x) =
	      cat [s "<int",pp_label_opt l,
		   s " v=\"",s (Int.toString x),s "\"/>"]
	      | pp_prim_value l (V.StringValue x) =
	      cat [s "<string",pp_label_opt l,
		   s " v=\"",s (String.translate fix_char x),s "\"/>"]
	      | pp_prim_value l (V.IdentifierValue x) =
	      cat [s "<identifier",pp_label_opt l,
		   s " v=\"",s (Identifier.toString x),s "\"/>"]
	    and pp_lv (v,i) = pp_asdl_value (SOME i) v
	  in
	    pp_to_outstream outs 132
	       (cat [s "<!--",s t,s "-->",
		     nl,pp_asdl_value NONE x])
	  end
    end

