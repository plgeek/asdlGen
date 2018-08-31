(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)



structure HTMLTablePP =
    struct
	
	structure V = AsdlValue
	open PPUtil

	datatype cell =
	    P   of {ty:TypePickle.qid option,v:V.prim_value}
	  | Ref of {ty:TypePickle.qid,id:int}
	  | Tbl of table
	  | Opt of {ty:TypePickle.qid,cell_opt:cell option}
	  | Seq of {ty:TypePickle.qid,cells:cell list}

	withtype trec =
	    {ty:TypePickle.qid,
	    con:TypePickle.qid option,
	  cells:cell list} 
	and table =
	    {id:int,
	    fields:Identifier.identifier list,
	      rows:trec list}

	fun next f (x,(xs,st)) =
	    let
		val (x,st) = f (x,st)
	    in
		(x::xs,st)
	    end

	val pp_qids = ref true
	val nest_depth = ref 2
	fun max_depth x = (x > (!nest_depth))
	
	fun value2cell d (V.PrimValue {typename,v},st) = (P {ty=NONE,v=v},st)
	  | value2cell d (V.NoneValue {typename},st) =
	    (Opt {ty=typename,cell_opt=NONE},st)
	  | value2cell d (V.SomeValue{typename,v},st) =
	    let
		val (v,st) = value2cell (d+1) (v,st)
	    in
		(Opt {ty=typename,cell_opt=SOME v},st)
	    end
	  | value2cell d (V.SequenceValue {typename,vs},st) =
	    let
		val (cells,st) =
		    List.foldr (next (value2cell (d+1))) ([],st) vs
	    in
		(Seq {ty=typename,cells=cells},st)
	    end

	  | value2cell d (V.ProductValue
			{typename,v=V.PrimValue {typename=_,v},vs=[]},st) =
	    (P {v=v,ty=SOME typename},st)

	  | value2cell d (V.ProductValue{typename,v,vs},st) =
	    let
		val too_deep = (max_depth d)
		val new_d = if too_deep then 0 else (d+1)
		val (cells,st) =
		    List.foldr (next (value2cell new_d)) ([],st) (v::vs)
		val (tables,id,f) = st
		val fields = f (SOME typename,NONE) (v::vs)
		val trec = {ty=typename,con=NONE,cells=cells}
		val table = {fields=fields,id=id,rows=[trec]}
	    in
		if (too_deep) then
		    (Ref{ty=typename,id=id},(table::tables,id+1,f))
		else
		    (Tbl table,(tables,id+1,f))
	    end
	  | value2cell d (V.SumValue{typename,con,attrbs,vs},st) =
	    let
		val too_deep = (max_depth d)
		val new_d = if too_deep then 0 else (d+1)
		val con_id = Identifier.fromString "con"
		val (cells,st) =
		    List.foldr (next (value2cell new_d)) ([],st) (attrbs@vs)
		val (tables,id,f) = st
		val fields = (con_id::(f (SOME typename,SOME con) (attrbs@vs)))
		val trec = {ty=typename,con=SOME con,cells=cells}
		val table = {fields=fields,id=id,rows=[trec]}
		    
	    in
		if (too_deep) then
		    (Ref{ty=typename,id=id},(table::tables,id+1,f))
		else
		    (Tbl table,(tables,id+1,f))
	    end

	fun pp_qid false  {base,qualifier} = s (Identifier.toString base)
	  | pp_qid true   {base,qualifier} =
	    seq{fmt=wrap Identifier.toString,
		sep=s "."} (qualifier@[base])


	fun pp_id x = pp_qid (!pp_qids) x
	fun pp_type x =	cat [s "<em>",pp_id x,s "</em>"] 
	fun pp_typel x = cat [s "<em>",pp_id x, s " list</em>"] 
	fun pp_typeo x = cat [s "<em>",pp_id x, s " option</em>"] 

	fun mk_sep x = cat [s x,ws]
	fun pp_cell (P{ty=NONE,v}) = pp_prim_value v
	  | pp_cell (P{ty=SOME ty,v}) =
	    cat [pp_prim_value v,s " : " ,pp_type ty]
	  | pp_cell (Ref{ty,id}) =
	    cat [s "<a href=\"#a",d id,s "\">",pp_type ty,s "</a>"]
	  | pp_cell (Seq{ty,cells=[]}) = s "nil list"

	  | pp_cell (Seq{ty,cells}) =
	    let
		exception TmpExn
		fun merge_tbls (Tbl {rows,...},xs) = rows@xs
		  | merge_tbls _ = raise TmpExn

	    in
		(case (cells) of
		    [] => s "empty list"
		  | (Tbl{fields,...}::_) =>
		    let
			val rows = (List.foldr merge_tbls [] cells)
		    in
			vblock 1 [s "<table border=1>",
				  nl,
				  pp_header fields,
				  nl,
				  seq_term {fmt=pp_rec,sep=nl} rows,
				  s "</table>"]
		    end
		  | _ => raise TmpExn)
		     handle TmpExn =>  (seq {fmt=pp_cell,sep=mk_sep "<br>"} cells)
	    end

	  | pp_cell (Opt{ty,cell_opt=NONE}) =
	    cat [pp_typeo ty, s " NONE"]
	  | pp_cell (Opt{ty,cell_opt=(SOME cell_opt)}) =
		 cat [pp_typeo ty, s " SOME", pp_cell cell_opt]
	  | pp_cell (Tbl tbl) = pp_table tbl
	and pp_row NONE cells =
	    hblock 1 [s "<tr><td valign=top>",
		      seq {fmt=pp_cell,
			   sep=mk_sep "</td><td valign=top>"} cells,
		      s "</td></tr>"]
	  | pp_row (SOME c) cells =
	    hblock 1 [s "<tr><th>",  pp_id c,
		      s "</th><td valign=top>",
		      seq {fmt=pp_cell,
			   sep=mk_sep "</td><td valign=top>"} cells ,
		      s "</td></tr>"]
	and pp_header fields =
	    hblock 1 [s "<tr><th valign=top>",
		      seq {fmt=wrap Identifier.toString,
			   sep=mk_sep "</th><th>"} fields,
		      s "</th></tr>"]
	and pp_rec {ty,con,cells} = pp_row con cells
	and pp_table {fields,rows,id} =
	    vblock 1 [s "<a name=\"a",d id,s "\">",
		      s "</a><table border=1>",
		      nl,
		      pp_header fields,
		      nl,
		      seq_term {fmt=pp_rec,sep=nl} rows,
		      s "</table>"]

	and pp_prim_value (V.IntValue x) = PPUtil.wrap Int.toString x
	  | pp_prim_value (V.StringValue x) =
	    cat [s"<tt>\"",s(String.toCString x),s "\"</tt>"]
	  | pp_prim_value (V.IdentifierValue x) =
	    wrap Identifier.toString x
	val br_sep = cat [nl,s "<br>"]

	fun do_root (Tbl tbl,rest) = tbl::rest
	  | do_root (_,rest) = rest

	fun pp_value f outs t x =
	    let
		fun newf x l =
		    (List.map (fn (_,x) => x ))  (f x l)
		val (root,(recs,_,_)) = value2cell 0 (x,([],0,newf))
		val pp = seq {fmt=pp_table,sep=br_sep}
		    (do_root(root,recs))
	    in
		pp_to_outstream outs 76
		(vblock 0 [s "<h1>",s t,s "</h1>",nl, pp])
	    end


    end
