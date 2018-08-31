(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(*just a hack for now *)
structure CPlusPlusPP : OO_PP =
  struct
    
    structure Ast = OOAst
    structure PP =
      mkPPAst(structure Ast = Ast
	      structure IdMap = IdMaps.CPlusPlus
	      val cap_mod = false
	      val cap_typ = false
	      val sep = "_")

    type code = (Ast.module * Semant.Module.P.props)
    type output = (string list * PPUtil.pp) list
      
    val opts = CommandOptions.empty
    open OOAst
    open PP
      
      fun mkComment l =
	vb 0 (str "////") (seq nl (fn x => str ("// "^x)) l) (str "////") 
      val mkDeps = PPDepends.makefile
      val pp_id = PP.vid
      val pp_tid = PP.tid
	
      fun group_scopes_pp init pp f l =
	let
	  fun do_it (x,(public,private,protected)) =
	    case (f x) of
	      Public =>	((pp x)::public,private,protected)
	    | Private => (public,(pp x)::private,protected)
	    | Protected => (public,private,(pp x)::protected)
	in (List.foldr do_it init l)
	end
      
      fun pp_scopes (public,private,protected)  =
	let
	  fun id x = x
	  fun do_pp [] = empty
	    | do_pp ((s,x)::xs) =
	    let val res = (do_pp xs)
	    in lst res (fn x => vb 4 (str s) (seq nl id (List.rev x)) res) x
	    end
	in do_pp [("public:",public),
		  ("protected:",protected),
		  ("private:",private)]
	end
      fun modifiers_scope ({scope,static,final}:modifiers) = scope
      fun mfield_scope ({mods,field}:mfield) = modifiers_scope mods
      fun cnstr_scope ({scope,inline,args,body}:cnstr) = scope
      fun mth_scope (MthAbstract{mods,...}) = modifiers_scope mods
	| mth_scope (Mth{mods,...}) = modifiers_scope mods
	
      fun pp_str_if s b = if b then (str s) else empty

      fun pp_ty_exp (TyId tid) = pp_tid tid
	| pp_ty_exp TyVoid= (str "void")
	| pp_ty_exp (TyArray(te,iopt)) =
	cat [pp_ty_exp te, str "[", opt empty num iopt, str "]"]
	| pp_ty_exp (TyReference te) =	cat [pp_ty_exp te,str "*"]
	| pp_ty_exp (TyOption te) = cat [str "Opt<",pp_ty_exp te,str ">"] 
	| pp_ty_exp (TySequence te) = cat [str "Seq<",pp_ty_exp te,str ">"] 
	
      and pp_ty_decl (DeclAbstractClass
		      {name,idecls,scope,inherits,fields,mths}) =
	let val scopes = ([pp_ty_idecls idecls],
			  []:PPUtil.pp list,[]:PPUtil.pp list)
	  val scopes = group_scopes_pp scopes pp_mfield mfield_scope fields
	  val scopes = group_scopes_pp scopes pp_mth mth_scope mths
	in vb 0 (cat  [str "class ", pp_tid name,
		       opt empty
		       (fn x => cat [str " : public ",pp_tid x]) inherits,
		       str " {"]) (pp_scopes scopes) (str "}")
	end
	| pp_ty_decl (DeclClass
		      {name,idecls,scope,final,
		       inherits,fields,mths,cnstrs}) =
	let
	  val scopes =  ([pp_ty_idecls idecls],
			 []:PPUtil.pp list,[]:PPUtil.pp list)
	  val scopes = group_scopes_pp scopes pp_mfield mfield_scope fields
	  val scopes = group_scopes_pp scopes (pp_cnstr name)
	    cnstr_scope cnstrs
	  val scopes = group_scopes_pp scopes pp_mth mth_scope mths
	in vb 0 (cat  [str "class ", pp_tid name,
		       opt empty
		       (fn x => cat [str " : public ",pp_tid x]) inherits,
		       str " {"]) (pp_scopes scopes) (str "}")
	end
	| pp_ty_decl (DeclConst{field,public,value}) = 
	cat [str (if public then "extern " else "static"), pp_field field]
	| pp_ty_decl (DeclFun{name,inline=true, public=true,args,ret,body}) = 
	cat [str "inline ",
	     pp_ty_exp ret,str " ",
	     pp_id name,
	     hb 2 (str "(") (seq (hsep ",") pp_field args) (str ") "),
	     pp_block body]
	| pp_ty_decl (DeclFun{name,inline,public,args,ret,body}) = 
	cat [str (if public then "extern " else "static"),
	     pp_ty_exp ret,str " ",
	     pp_id name,
	     hb 2 (str "(") (seq (hsep ",") pp_field args) (str ") "),
	     pp_block body]
      and pp_ty_idecl (IDeclEnum{name,enums}) =
	hb 2 (cat [str "enum ",pp_tid name,str " {"])
	(seq (hsep ",") pp_enumer enums) (str "}")
	
      and pp_mth (MthAbstract{name,mods,args,ret}) =
	cat [str "virtual ",
	     pp_modifiers mods,
	     pp_ty_exp ret,str " ",
	     pp_id name,
	     hb 2 (str "(") (seq (hsep ",") pp_field args) (str ") = 0;")]
	| pp_mth (Mth{name,inline,mods,args,ret,body}) =
	cat [pp_str_if "inline " inline,
	     pp_modifiers mods,
	     pp_ty_exp ret,str " ",
	     pp_id name,
	     hb 2 (str "(") (seq (hsep ",") pp_field args) (str ") "),
	     pp_block body]
	
      and pp_const (IntConst i) = num i
	| pp_const (EnumConst (tid,id)) = cat[pp_tid tid,str "::",pp_id id]
	| pp_const (VarConst (id)) = pp_id id
      and pp_exp (NilPtr) = str "NULL"
	| pp_exp (MthCall(e,es)) =
	hb 2 (cat [pp_exp e,str "("]) (seq (hsep ",") pp_exp es) (str ")")
	| pp_exp (SMthCall(tid,id,es)) =
	hb 2 (cat [pp_tid tid,str "::",pp_id id,str "("])
	(seq (hsep ",") pp_exp es) (str ")")
	| pp_exp (FunCall(id,es)) =
	hb 2 (cat [pp_id id,str "("]) (seq (hsep ",") pp_exp es) (str ")")
	| pp_exp (Id id) = pp_id id
	| pp_exp (ThisId (id)) = cat [str "this->",pp_id id]
	| pp_exp (This) = cat [str "this"]
	| pp_exp (Const c) = pp_const c
	| pp_exp (FieldSub (DeRef e,id)) = cat [pp_exp e, str "->",pp_id id]
	| pp_exp (FieldSub (e,id)) = cat [pp_exp e, str ".",pp_id id]
	| pp_exp (DeRef e) = cat [str "*",pp_exp e]
	| pp_exp (NotNil e) = cat [pp_exp e, str " != NULL"]
	| pp_exp (NotZero e) = 	cat [pp_exp e, str " != 0"]
	| pp_exp (Less(l,r)) = 	cat [pp_exp l, str " <", pp_exp r]
	| pp_exp (NotEqConst (e,c)) = cat [pp_exp e, str " != ",pp_const c]
	| pp_exp (Cast(t,e)) =	cat [str "(",pp_ty_exp t,str ")",pp_exp e]
	| pp_exp (New(t,es)) =
	hb 2 (cat [str "new ",pp_tid t,str "("])
	(seq (hsep ",") pp_exp es) (str ")")
	| pp_exp (PlusOne e) =
	cat [pp_exp e, str " + 1"]
	| pp_exp (MinusOne e) =
	cat [pp_exp e, str " - 1"]
	| pp_exp (ArraySub (e,idx)) =
	cat [pp_exp e, str "[" ,pp_exp idx,str "]"]
	| pp_exp (SeqNew {elm_ty,len}) =
	cat [str "Seq<" ,pp_ty_exp elm_ty,str ">(",pp_exp len,str ")"]
	| pp_exp (SeqLen {elm_ty,seq}) =
	cat [str "((",pp_exp seq, str ").len())"]
	| pp_exp (SeqGet {elm_ty,seq,idx}) =
	cat [str "((",pp_exp seq, str ").get(",pp_exp idx, str "))"]
	| pp_exp (SeqSet {elm_ty,seq,idx,v}) =
	cat [str "((",pp_exp seq, str ").set(",
	     pp_exp idx,str ", ",pp_exp v,str "))"]
	| pp_exp (OptNone(elm_ty)) =
	cat [str "Opt<" ,pp_ty_exp elm_ty, str ">()"]
	| pp_exp (OptSome(elm_ty,exp)) =
	cat [str "Opt<" ,pp_ty_exp elm_ty,
	     str ">(",pp_exp exp,str ")"]
	| pp_exp (OptIsSome (elm_ty,e)) = 
	cat [str "((",pp_exp e, str ").is_some())"]
	| pp_exp (OptGetVal (elm_ty,e)) = 
	cat [str "((",pp_exp e, str ").get_val())"]
      and pp_stmt (Assign(dst,src)) =
	cat [pp_exp dst, str " = " ,pp_exp src,str ";"]
	| pp_stmt (Die s) = str "throw Error(\"fatal\");"
	| pp_stmt (Return e) =
	cat [str "return ", pp_exp e,str ";"]
	| pp_stmt Nop = str ";"
	| pp_stmt (Expr e) =
	cat [pp_exp e,str ";"]
	| pp_stmt (Case {test,clauses,default=Nop}) =
	vb 2 (cat[str "switch(",pp_exp test,str ") {"])
	(seq nl pp_clause clauses) (str "}")
	      | pp_stmt (Case {test,clauses,default}) =
	vb 2 (cat[str "switch(",pp_exp test,str ") {"])
	(cat [seq' nl pp_clause clauses, str "default: ",pp_stmt default])
	(str "}")
	| pp_stmt (If{test,then_stmt,else_stmt=Nop}) =
	vb 2 (cat [str "if(",pp_exp test,str ") "]) (pp_stmt then_stmt) empty
	| pp_stmt (If{test,then_stmt, else_stmt}) =
	vb 2 (cat [str "if(",pp_exp test,str ") "]) (pp_stmt then_stmt) 
	(cat [str " else ",pp_stmt else_stmt])
	| pp_stmt (Block {vars=[],body=[]}) = PP.empty
	| pp_stmt (Block {vars=[],body=[x]}) = pp_stmt x
	| pp_stmt (Block b) = pp_block b
	| pp_stmt (While {test,body=Block b}) =
	cat [str "while(",pp_exp test, str ") ",pp_block b]
	| pp_stmt (While {test,body}) =
	vb 2 (cat [str "while(",pp_exp test, str ") "]) (pp_stmt body) empty
      and pp_clause {tag,body} =
	cat [str "case ",pp_const tag,str ": ",
	     pp_stmt body,PP.nl,str "break;"]
      and pp_block {vars=[],body=[]} = str ";"
	| pp_block {vars,body} =
	vb 2 (str "{")
	(cat [seq' (cat [str ";",nl]) pp_field vars,
	      seq nl pp_stmt body]) (str "}")
      and pp_enumer  {name,value=(SOME i)} =
	cat [pp_id name,str " = ",num i]
	| pp_enumer  {name,value=NONE} = pp_id name
      and pp_field {name,ty} =
	cat [pp_ty_exp ty,str " ",pp_id name]
      and pp_modifiers {scope,static,final} =
	cat [pp_str_if "static " static]
      and pp_mfield {mods,field} =
	cat [pp_modifiers mods,pp_field field,str ";"]
      and pp_cnstr tid {scope,inline,args,body} =
	cat [pp_str_if "inline " inline,
	     pp_tid tid,
	     hb 2 (str "(") (seq (hsep ",") pp_field args) (str ") "),
	     pp_block body]
      and pp_ty_decls decls = cat [seq' (cat [str ";",nl]) pp_ty_decl decls]
      and  pp_ty_idecls idecls =
	cat [seq' (cat [str ";",nl]) pp_ty_idecl idecls]

      fun split_decls decls =
	let
	  (* choose "large" methods *)
	  fun is_big {vars=[],body=[]} = false
	    | is_big {vars=[],body=[Case _]} = true
	    | is_big {vars=[],body=[Case _,_]} = true
	    | is_big {vars=[],body=[Block x]} = is_big x
		      | is_big {vars=[],body=[Block _,Block _]} = true
	    | is_big {vars=[],body=[_,_]} = false
	    | is_big {vars=[],body=[_]} = false 
	    | is_big _ = true
	    
	  fun fix_mth (m as(Mth(arg as
				{name,mods,args,ret,body,inline=inline})), 
		       (xs,ys))=
	    if (is_big body) orelse (not inline) then
	      ((Mth{name=name,mods=mods,inline=inline,
		    args=args,ret=ret,body={vars=[],body=[]}
		    })::xs,arg::ys)
	    else (m::xs,ys)
	    | fix_mth (m,(xs,ys)) = (m::xs,ys)
	      
	  fun fix_cnstr 
	    (c as ({scope,args,body,inline}), (xs,ys))=
	    if (is_big body) then
	      ({scope=scope,args=args,inline=inline,
		body={vars=[],body=[]}}::xs,c::ys)
	    else
	      (c::xs,ys)
	      
	  fun fix_class (DeclAbstractClass
			 {name,idecls,scope,inherits,
			  fields,mths},(ds,acc))  =
	    let
	      val (mths,ms) = (List.foldr fix_mth ([],[]) mths)
	    in
	      ((DeclAbstractClass
		{name=name,idecls=idecls,
		 scope=scope,inherits=inherits,fields=fields,
		 mths=mths})::ds,(name,[],ms)::acc)
	    end
	    | fix_class  (DeclClass
			  {name,cnstrs,idecls,scope,final,
			   inherits,fields,mths},(ds,acc))  =
	    let
	      val (mths,ms) = (List.foldr fix_mth  ([],[]) mths)
	      val (cnstrs,cs) =	(List.foldr fix_cnstr ([],[]) cnstrs)
	    in ((DeclClass
		 {name=name,cnstrs=cnstrs,
		  idecls=idecls,final=final,
		  scope=scope,inherits=inherits,fields=fields,
		  mths=mths})::ds,(name,cs,ms)::acc)
	    end
	    | fix_class (x,(ds,acc)) = (x::ds,acc)
	    
	  val (decls,acc) = List.foldr fix_class ([],[]) decls
	in (decls,acc)
	end
      (* hack to fix foward declarations *)				
      fun fix_decs_pp decls =
	let
	  fun do_it (x as (DeclAbstractClass{name,...}),(xs,ys)) =
	    ((str "class ")::(pp_tid name)::
	     (str ";")::(PP.nl)::xs,x::ys)
	    | do_it (x as (DeclClass{name,inherits=NONE,...}),
		     (xs,ys)) =
	    ((str "class ")::(pp_tid name)::
	     (str ";")::(PP.nl)::xs,x::ys)
	    | do_it (x as (DeclConst{field,public=true,value}),
		     (xs,ys)) =
	    ((pp_ty_decl x)::(str ";")::(PP.nl)::xs,ys)
	    | do_it (x as (DeclFun{public=true,...}),
		     (xs,ys)) =
	    ((pp_ty_decl x)::(PP.nl)::xs,ys)
	    | do_it (x,(xs,ys)) = (xs,x::ys)
	    
	  fun pp_const (DeclConst {field,public,value}) =
	    SOME (cat [pp_str_if "static " (not public),
		       pp_field field ,str " = ",pp_exp value,
		       str ";",PP.nl])
	    | pp_const _ = NONE
	    
	  val consts = List.mapPartial pp_const decls
	    
	  val (decls,acc) = split_decls decls
	  val (fdecs,decls) = List.foldr do_it ([],[]) decls
	    
	  fun get f g (tid,cs,ms) =
	    (tid,List.filter f cs,List.filter g ms)
	    
	  val iv = List.map (get #inline #inline) acc
	  val ov = List.map (get (not o #inline)
			     (not o #inline)) acc
	    
	  fun pp_mdec  tid {name,mods,args,ret,body,inline} =
	    cat [pp_str_if "inline " inline,
		 pp_ty_exp ret,str " ",
		 pp_tid tid,str "::", pp_id name,
		 hb 2 (str "(") (seq (hsep ",") pp_field args) (str ") "),
		 pp_block body]
	  and pp_cdec tid {scope,inline,args,body} =
	    cat [pp_str_if "inline " inline,
		 pp_tid tid,str "::", pp_tid tid,
		 hb 2 (str "(") (seq (hsep ",") pp_field args) (str ")"),
		 pp_block body]
	  fun pp_acc (tid,cs,ms) =
	    cat  [seq' nl (pp_cdec tid) cs, seq' nl (pp_mdec tid) ms]
	    
	  val header = cat [cat fdecs, pp_ty_decls  decls,
			    cat (List.map pp_acc iv)]
	  val body = cat ((List.map pp_acc ov)@consts)
	in (header,body)
	end
      
      val header_prologue = PPUtil.wrap Semant.Module.P.interface_prologue 
      val header_epilogue = PPUtil.wrap Semant.Module.P.interface_epilogue
      val body_prologue = PPUtil.wrap Semant.Module.P.implementation_prologue 
      val body_epilogue = PPUtil.wrap Semant.Module.P.implementation_epilogue

      fun pp_code p (Module{name,imports,decls},props) =
	let
	  val mn = ModuleId.toString (fix_mid name)
	  fun mk_file b x =
	    let val x = ModuleId.toString (fix_mid x)
	    in OS.Path.joinBaseExt {base=x,ext=SOME b}
	    end
	  fun pp_inc s =  cat [str "#include \"",mid s,str ".hxx\""]
	  val pp_incs = seq' nl pp_inc
	  fun pp_impl body =
	    cat [pp_inc name,body_prologue props,nl,
		 body,nl,body_epilogue props,nl]

	  fun pp_interface header incs =
	    cat [str ("#ifndef _"),mid name,str "_", nl,
		 str ("#define _"),mid name,str "_", nl,
		 pp_incs incs,
		 header_prologue props,nl,
		 header, nl,
		 header_epilogue props,nl,
		 str ("#endif /* _"),mid name, str "_ */", nl]
	  val (header,body) = fix_decs_pp decls
	in 
	  [FileSet.mkFile{name=mk_file "hxx" name,
			 depends=List.map (mk_file "hxx") imports,
			  body=pp_interface header imports},
	   FileSet.mkFile{name=mk_file "cxx" name,
			   depends=[mk_file "hxx" name],
			   body=pp_impl body}]
	end
  end
