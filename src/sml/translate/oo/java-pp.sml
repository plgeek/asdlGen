(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


(*just a hack for now *)
structure JavaPP :  sig
    include OO_PP
    val package_prefix : string 
  end =
    struct
      structure PP = PPUtil
      structure Ast = OOAst
      type code = (Ast.module * Semant.Module.P.props)
      fun dbg f x = ((PP.pp_to_outstream TextIO.stdOut 80 (f x));x)
      val package_prefix = "asts"
      structure PP =
	mkPPAst(structure Ast = Ast
		structure IdMap = IdMaps.Java
		val cap_mod = false
		val cap_typ = false
		val sep = ".")
      val opts = CommandOptions.empty
      open PP
      open Ast
      fun mkComment s = vb 1 (str "/*") (seq nl str s) (str " */")
      val mkDeps = PPDepends.makefile
	val const_class =  "g"
	(* java requires lots of special magic *)
	fun add_package_prefix {base,qualifier} =
	  (case qualifier of
	     [] => NONE
	   | [""] => SOME {base=base,qualifier=[package_prefix]}
	   | _ =>  SOME {base=base,qualifier=[package_prefix]@qualifier})
	fun fix_ty_path {qualifier,base} =
	  case base of
	    "java_boolean" => SOME {qualifier=[],base="boolean"}
	  | "java_byte" => SOME {qualifier=[],base="byte"}
	  | "java_char" => SOME {qualifier=[],base="char"}
	  | "java_double" => SOME {qualifier=[],base="double"}
	  | "java_int" => SOME {qualifier=[],base="int"}
	  | "java_float" => SOME {qualifier=[],base="float"}
	  | "java_long" => SOME {qualifier=[],base="long"}
	  | "java_short" => SOME {qualifier=[],base="short"}
	  | "java_lang_String" => SOME {qualifier=[],base="String"}
	  | "java_math_BigInteger" =>
	      SOME {qualifier=[],base="java.math.BigInteger"}
	  | x => add_package_prefix {qualifier=qualifier,base=x}

	val pp_tid  = str o TypeId.toString o (TypeId.subst fix_ty_path) o PP.fix_tid
	val pp_id = str o VarId.toString o (VarId.subst add_package_prefix) o PP.fix_vid
	val pp_idb = str o VarId.getBase o PP.fix_vid
	val pp_tidb = str o TypeId.getBase o (TypeId.subst fix_ty_path) o PP.fix_tid

	val semi_sep = cat [str ";",PP.nl]
	val comma_sep = cat [str ",",PP.ws]
	  
	fun pp_str_if s b = (if b then (str s) else empty)
	     
	fun if_unboxed_int (TyId tid) x y =
	  (case TypeId.toPath tid of
	     ({qualifier=_,base="java_int"}) => x 
	   | ({base,...}) => y)
	  | if_unboxed_int _ _ y =  y
	     
	fun pp_ty_exp (TyId tid) = pp_tid tid
	  | pp_ty_exp TyVoid = str "void"
	  | pp_ty_exp (TyArray(te,iopt)) =
	  cat [pp_ty_exp te, str "[",opt empty num iopt, str "]"]
	  | pp_ty_exp (TyReference te) = pp_ty_exp te
	  | pp_ty_exp (TyOption te) =
	  let val ps = if_unboxed_int te (str "java.lang.Integer") (pp_ty_exp te)
	  in cat [ps,str "/* opt */"]
	  end
	  | pp_ty_exp (TySequence te) = cat [str "java.util.Vector"] 
	  
	and pp_ty_decl (DeclAbstractClass
			{name,idecls,scope,inherits,fields,mths}) =
	  vb 4 (cat [pp_scope scope,str " abstract class ", pp_tidb name,
		     opt empty
		     (fn x => cat [str " extends ",pp_tid x])  inherits,
		     str " {"])
	  (cat [pp_ty_idecls idecls,
		seq' (cat [str ";",nl]) pp_mfield fields,
		seq nl pp_mth mths])
	  (str "}")
	  | pp_ty_decl (DeclClass
			{name,idecls,scope,final,
			 inherits,fields,mths,cnstrs}) =
	  vb 4 (cat [pp_scope scope, pp_str_if " final" final, str " class ",
		     pp_tidb name,
		     opt empty
		     (fn x => cat [str " extends ",pp_tid x])  inherits,
		     str " {"])
	  (cat [pp_ty_idecls idecls,
		seq' (cat [str ";",nl]) pp_mfield fields,
		seq' nl (pp_cnstr name) cnstrs,
		seq nl pp_mth mths])
	  (str "}")
	      | pp_ty_decl _ = raise Error.internal
	  
	and pp_ty_idecl (IDeclEnum{name,enums}) =
	  seq' (cat [str ";",nl]) pp_enumer  (AstMisc.cannon_enumers enums)
	  
	and pp_mth (MthAbstract{name,mods,args,ret}) =
	  cat  [str "abstract ",  pp_modifiers mods,  pp_ty_exp ret,str " ",
		pp_idb name,
		hb 2 (str "(") 	(seq (hsep ",") pp_field args) (str ");")]
	  | pp_mth (Mth{name,mods,args,ret,body,inline}) =
	  cat [pp_modifiers mods,  pp_ty_exp ret,str " ",
	       pp_idb name,
	       hb 2 (str "(") 	(seq (hsep ",") pp_field args) (str ") "),
	       pp_block body]
	and pp_const (IntConst i) = num i
	  | pp_const (EnumConst (tid,id)) = cat [pp_tid tid,str ".",pp_idb id]
	  | pp_const (VarConst id) =
	  cat [str  const_class, str ".",pp_idb id]
	and pp_exp (NilPtr) = str "null"
	  | pp_exp (MthCall(e,es)) =
	  hb 2 (cat [pp_exp e,str "("])
	  (seq (hsep ",") pp_exp es)
	  (str ")")
	  | pp_exp (SMthCall(tid,id,es)) =
	  hb 2 (cat [pp_tid tid,str ".",pp_id id,str "("])
	  (seq (hsep ",") pp_exp es)
	  (str ")")
	  | pp_exp (FunCall(id,es)) =
	  hb 2 (cat [pp_id (VarId.prefixBase "g." id),str "("])
	  (seq (hsep ",") pp_exp es)
	  (str ")")
	  | pp_exp (Id id) = pp_id id
	  | pp_exp (ThisId (id)) = cat [str "this.",pp_id id]
	  | pp_exp (This) = str "this"
	  | pp_exp (Const c) = pp_const c
	  | pp_exp (FieldSub (e,id)) = cat [pp_exp e, str ".",pp_id id]
	  | pp_exp (DeRef e) = pp_exp e
	  | pp_exp (NotNil e) = cat [pp_exp e, str " != null"]
	  | pp_exp (Less(el,er)) = cat [pp_exp el, str " < ", pp_exp er]
	  | pp_exp (NotZero e) = cat [pp_exp e, str " != 0"]
	  | pp_exp (NotEqConst (e,c)) = 
	  cat [pp_exp e, str " != ",pp_const c]
	  | pp_exp (Cast(t,e)) = cat [str "(",pp_ty_exp t,str ")",pp_exp e]
	  | pp_exp (New(t,es)) =
	  hb 2 (cat [str "new ",pp_tid t, str "("])
	  (seq (hsep ",") pp_exp es)
	  (str ")")
	  | pp_exp (PlusOne e) =cat [pp_exp e, str " + 1"]
	  | pp_exp (MinusOne e) =cat [pp_exp e, str " - 1"]
	  | pp_exp (ArraySub (e,idx)) =
	  cat [pp_exp e, str "[" ,pp_exp idx,str "]"]
	  | pp_exp (SeqNew{elm_ty,len}) =
	  cat [str "new java.util.Vector(",pp_exp len,str ")"]
	  | pp_exp (SeqLen{elm_ty,seq}) =
	  cat [str "((",pp_exp seq,str ").size())"]
	  | pp_exp (SeqGet{elm_ty,seq,idx}) =
	  let
	    fun do_int () =
	      cat [str "(((java.lang.Integer)((",
		   pp_exp seq,str ").elementAt(",
		   pp_exp idx,
		   str "))).intValue())"]
	      
	    fun do_rest () =
	      cat [str "((",pp_ty_exp elm_ty,
		   str ")((",pp_exp seq,str ").elementAt(",
		   pp_exp idx,
		   str ")))"]
	  in (if_unboxed_int elm_ty do_int do_rest)()
	  end
	  | pp_exp (SeqSet{elm_ty,seq,idx,v}) =
	  cat [str "(",pp_exp seq,str ").setElementAt(",
	       pp_exp
	       (if_unboxed_int elm_ty
		(New(TypeId.fromString "java.lang.Integer",[v]))
		v),str ", ",pp_exp idx,
	       str ")"]
	  | pp_exp (OptNone _) = pp_exp NilPtr
	  | pp_exp (OptSome (te,e)) =
	  pp_exp (if_unboxed_int te
		  (New(TypeId.fromString "java.lang.Integer",[e]))
		  e)
	  | pp_exp (OptIsSome (te,e)) = pp_exp (NotNil e)
	  | pp_exp (OptGetVal (te,e)) =
	  pp_exp (if_unboxed_int te
		  (MthCall(FieldSub(e,VarId.fromString "intValue"),[]))
		  e)
	and pp_stmt (Assign(dst,src)) =
	  cat [pp_exp dst, str " = " ,pp_exp src,str ";"]
	  | pp_stmt (Die s) = str "throw new Error(\"fatal\");"
	  | pp_stmt (Return e) =
	  cat [str "return ", pp_exp e,str ";"]
	  | pp_stmt Nop = str ";"
	  | pp_stmt (Expr e) =
	  cat [pp_exp e,str ";"]
	  | pp_stmt (Case {test,clauses,default=Nop}) =
	  vb 2 (cat [str "switch(",pp_exp test,str ") {"])
	  (seq nl pp_clause clauses)
	  (str "}")
	  | pp_stmt (Case {test,clauses,default}) =
	  vb 2 (cat [str "switch(",pp_exp test,str ") {"])
	  (cat [seq' nl pp_clause clauses,
		str "default: ",pp_stmt default])
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
	  
	and need_break (Return _) = false
	  | need_break (Block{vars,body=[Return _]}) = false
	  | need_break (Block{vars,body=(x::xs)}) =
	  need_break (Block{vars=[],body=xs}) 
	  | need_break (Block{vars,body=[]}) = true
	  | need_break (If{then_stmt,else_stmt,...}) =
	  (need_break then_stmt) orelse (need_break else_stmt)
	  | need_break x = true
	  
	and pp_clause {tag,body} =
	  vb 2 (cat [str "case ",pp_const tag,str ":"])
	  (pp_stmt body)
	  (pp_str_if "break;" (need_break body))
	and pp_block {vars=[],body=[]} = str "{ }"
	  | pp_block {vars,body} =
	  vb 2 (str "{")
	  (cat [seq' (cat [str ";",nl]) pp_field vars,
		seq nl pp_stmt body])
	  (str "}")
	  
	and pp_scope Public = str "public"
	  | pp_scope Private = str "private"
	  | pp_scope Protected = str "protected"
	  
	and pp_enumer  {name,value=(SOME i)} =
	  cat [str "public final static int ",
	       pp_idb name,str " = ",num i]
	  | pp_enumer  {name,value=NONE} = raise Error.internal
	  
	and pp_field {name,ty} =  cat [pp_ty_exp ty,str " ",pp_idb name]
	and pp_modifiers {scope,static,final} =  
	  cat [pp_scope scope,str " ",
	       pp_str_if "final " final,
	       pp_str_if "static " static]
	and pp_mfield {mods,field} =
	  cat [pp_modifiers mods,pp_field field]
	and pp_cnstr tid {scope,args,body,inline} = 
	  cat [pp_scope scope,str " ",
	       pp_tidb tid,
	       hb 2 (str "(") (seq (hsep ",") pp_field args) (str ") "),
	       pp_block body]

	and pp_ty_decls decls = seq' nl pp_ty_decl decls
	and pp_ty_idecls idecls = seq' nl pp_ty_idecl idecls	  
	fun pp_cls "" x =
	  let
	    fun get_name (DeclClass x) = PP.fix_tid(#name(x))
	      | get_name (DeclAbstractClass x) = PP.fix_tid(#name(x))
	      | get_name _ = raise Error.internal
	    val pp =
	      cat [str ("package "^package_prefix^";"), nl,
		   pp_ty_decl x]
	    val fname =
	      OS.Path.concat(package_prefix,OS.Path.joinBaseExt
	      {base=TypeId.getBase (get_name x),ext=SOME "java"})
	  in  FileSet.mkFile{name=fname,
			     depends=[OS.Path.concat(package_prefix,
						     "g.java")],body=pp}
	  end
	  | pp_cls mn x =
	  let
	    fun get_name (DeclClass x) = PP.fix_tid(#name(x))
	      | get_name (DeclAbstractClass x) = PP.fix_tid(#name(x))
	      | get_name _ = raise Error.internal
	    val pp =
	      cat [str ("package "^package_prefix^"."^mn^";"), nl,
		   pp_ty_decl x]
	    val fname =
	      OS.Path.concat(package_prefix,
	      OS.Path.concat(mn,
	      OS.Path.joinBaseExt
			     {base=TypeId.getBase (get_name x),
			      ext=SOME "java"}))
	  in  FileSet.mkFile{name=fname,
			  depends=[OS.Path.concat(package_prefix,
				      OS.Path.concat(mn,"g.java"))],
			     body=pp}
	  end
	fun do_const (DeclConst {field,public,value},(cs,ds)) =
	  ((cat [pp_str_if  " public" public,
		 str " final static ",
		 pp_field field,str " = ",pp_exp value,
		 str ";",PP.nl])::cs,ds)
	  | do_const (x,(cs,ds)) = (cs,x::ds)
	  
	val body_prologue =
	  PPUtil.wrap Semant.Module.P.implementation_prologue 
	val body_epilogue =
	  PPUtil.wrap Semant.Module.P.implementation_epilogue
	  
	fun mk_dep name =
	  let val mn = ModuleId.toString (PP.fix_mid name)
	  in
	    OS.Path.concat(package_prefix,
 	    OS.Path.concat(mn,
	    OS.Path.joinBaseExt
			   {base=const_class,
			    ext=SOME "java"}))
	  end
	fun  pp_consts "" x props imports =
	  let
	    val pp =
	      cat
	      [str ("package "^package_prefix^";"), nl,
	       body_prologue props, nl,
	       vb 2
	       (cat [str "final public class ",str const_class, str " {"])
	       (cat x)
	       (cat [body_epilogue props,nl, str "}"])]
	    val fname = 
	      OS.Path.concat(package_prefix,
	      OS.Path.joinBaseExt {base=const_class,ext=SOME "java"})
	  in FileSet.mkFile{name=fname,
			    depends=List.map mk_dep imports,
			    body=pp}
	  end
	  | pp_consts mn x props imports =
	  let
	    val pp =
	      cat
	      [str ("package "^package_prefix^"."^mn^";"), nl,
	       body_prologue props, nl,
	       vb 2
	       (cat [str "final public class ",str const_class, str " {"])
	       (cat x)
	       (cat [body_epilogue props,nl, str "}"])]
	    val fname = 
	      OS.Path.concat(package_prefix,
	      OS.Path.concat(mn,
	      OS.Path.joinBaseExt
			     {base=const_class,
			      ext=SOME "java"}))
	  in FileSet.mkFile{name=fname,
			    depends=List.map mk_dep imports,
			    body=pp}
	  end
	fun pp_code p  (Module{name,imports,decls},props) =
	  let val (cs,ds) = List.foldr do_const ([],[]) decls
	      val mn = ModuleId.toString (PP.fix_mid name)
	  in (pp_consts mn cs props imports)::(List.map (pp_cls mn) ds)
	  end
    end 
