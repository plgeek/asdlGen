(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature PP_ANSI_C =
    sig
	structure T : ANSI_C
	type pp = PPUtil.pp

	val pp_ty_prim       : T.ty_prim -> pp
	val pp_ty_exp        : T.ty_exp -> pp
	val pp_ty_dec        : T.ty_dec -> pp
	val pp_qualifier     : T.qualifier -> pp
	val pp_aggregate     : T.aggregate -> pp
	val pp_storage_class : T.storage_class -> pp
	val pp_var_dec       : T.var_dec -> pp
	val pp_fun_dec       : T.fun_dec -> pp
	val pp_decl          : T.decl -> pp
	val pp_const_exp     : T.const_exp -> pp
	val pp_unary_op      : T.unary_op -> pp
	val pp_binary_op     : T.binary_op -> pp
	val pp_exp           : T.exp -> pp
	val pp_stmt          : T.stmt -> pp
	val pp_switch_arm    : T.switch_arm -> pp
	val pp_enumer        : T.enumer -> pp
	val pp_block         : T.block -> pp
	val pp_decls         : T.decl list -> pp
    end
(* not perefect need to fix handling of precedence *)
functor mkPPAnsiC(structure T: ANSI_C) : PP_ANSI_C =
    struct
	structure T = T
	structure PP = mkPPAst(structure Ast = T
			       structure IdMap = IdMaps.AnsiC
			       val cap_mod = false
			       val cap_typ = false
			       val sep = "_")
	type pp = PPUtil.pp
	open PP
	fun toString_ty_prim T.VOID  = "void"
	  | toString_ty_prim T.INT   = "int"
	  | toString_ty_prim T.CHAR  = "char"

	val pp_ty_prim = PP.str o toString_ty_prim
	val semi_sep = cat [str ";",nl]
	fun pp_ty_id id = cat [PP.tid id,str "_ty"] 
	val pp_id = PP.vid

	fun pp_ty_exp (T.TyPrim x) = pp_ty_prim x
	  | pp_ty_exp (T.TyId x) = pp_ty_id  x
	  | pp_ty_exp (T.TyPointer te) = cat [pp_ty_exp te,str "*"]
	  | pp_ty_exp (T.TyArray (te,iopt)) =
	    cat [pp_ty_exp te,str "[",opt empty num iopt,str "]"]
	  | pp_ty_exp (T.TyEnum (i,er)) =
	    hb 2 (str "enum {") (seq (hsep ",") pp_enumer er) (str "}")
	  | pp_ty_exp (T.TyFunctionPtr(fl,te)) =
	    hb 2 (cat [pp_ty_exp te,str "(*)("])
	    (seq (hsep ",") pp_field fl) (str ")")
	  | pp_ty_exp (T.TyAggregate(aggre,iopt,[])) =
	    cat [pp_aggregate aggre, str " ", opt empty pp_id iopt]
	  | pp_ty_exp (T.TyAggregate(aggre,iopt,fl)) =
	    vb 2 (cat [pp_aggregate aggre, str " ",
		       opt empty pp_id iopt, str " {"])
	    (cat [seq semi_sep pp_field fl, str ";"])
	    (str "}")
	  | pp_ty_exp (T.TyQualified (q,T.TyPointer te))  =
	    cat [pp_ty_exp te, str " * ",pp_qualifier q]
	  | pp_ty_exp (T.TyQualified (q,te)) =
	    cat [pp_qualifier q,(str " "),pp_ty_exp te]
	  | pp_ty_exp (T.TyAnnotate (s,te)) =
	    hb 2 (str "/* ") (str s) (cat [str " */ ",pp_ty_exp te])
	  | pp_ty_exp (T.TyGroup te) =
	    hb 2 (str "(") (pp_ty_exp te) (str ")")

	and pp_ty_dec (T.TyDec (tid,te)) =
	   cat [str "typedef ",pp_ty_exp te,(str " "),pp_ty_id tid]
	  | pp_ty_dec (T.TyAggregateDec(aggre,id,fl)) =
	    vb 2 (cat [pp_aggregate aggre,(str " "), pp_id id, str " {"])
	    (cat [seq semi_sep pp_field fl,str ";"])
	    (str "}")

	  | pp_ty_dec (T.TyEnumDec (tid,er)) =
	    vb 2 (cat  [str "enum ",pp_ty_id tid,str " {"])
	    (seq (hsep ",") pp_enumer er)
	    (str "}")

	and pp_qualifier T.Const = str "const"
	  | pp_qualifier T.Voliatile = str "voliatile"

	and pp_aggregate T.Union = str "union"
	  | pp_aggregate T.Struct = str "struct"

	and pp_storage_class T.Auto = str "auto "
	  | pp_storage_class T.Registier = str "register "
	  | pp_storage_class T.Static = str "static "
	  | pp_storage_class T.Extern = str "extern "

	and pp_var_dec (T.VarDecs (sc,te,id,ids)) =
	    cat [opt empty pp_storage_class sc,
		 pp_ty_exp te, (str " "),
		 seq (hsep ",") pp_id (id::ids)]
	  | pp_var_dec (T.VarDecInit(sc,te,id,exp)) =
	    cat [opt empty pp_storage_class sc,
		 pp_ty_exp te, (str " "), pp_id id, str " = ",pp_exp exp]

	and pp_fun_dec (T.FunPrototypeDec(id,[],te)) =
	  cat [pp_ty_exp te, (str " "), pp_id id, str "(void);"]
	  | pp_fun_dec (T.FunPrototypeDec(id,fl,te)) =
	    hb 2 (cat [pp_ty_exp te,str " ", pp_id id, str "("])
	    (seq (hsep ",") pp_field fl) (str ");")

	  | pp_fun_dec (T.FunDec(id,fl,te,b)) =
	    cat [hb 2 (cat [pp_ty_exp te,(str " "),pp_id id,str "("])
		 (lst (str "void") (fn x => seq (hsep ",") pp_field x) fl)
		 (str ") "), pp_block b]
	  | pp_fun_dec (T.FunStaticDec x) =
	    cat [str "static ", pp_fun_dec (T.FunDec x)]

	and pp_decl (T.Ty td) = cat [pp_ty_dec td,str ";"]
	  | pp_decl (T.Fun fd) = pp_fun_dec fd
	  | pp_decl (T.Var vd) =  cat [pp_var_dec vd,str ";"]
	  | pp_decl (T.Com s) =  vb 2 (str "/*") (str s) (str "*/")
	  | pp_decl (T.TagTable x) =
	    let fun pp_pair (s,v) =
	      cat [str "{",pp_const_exp (T.S s),str ", ",pp_const_exp (T.I v),
		   str "}"]
	    in
	      hb 2 (str "struct xml_tag_map_entry_s xml_tag_map[] = {")
	      (seq' (hsep ",") pp_pair x)
	      (str "{NULL , -1}};")
	    end

	and pp_const_exp (T.I i) = num i
	  | pp_const_exp (T.E i) = pp_id i
	  | pp_const_exp (T.A i) = cat [str "&",pp_id i]
	  | pp_const_exp (T.C c) = str ("'"^(Char.toString c )^"'" )
	  | pp_const_exp (T.S s) = str ("\""^(String.toCString s)^"\"" )
	  | pp_const_exp (T.Void) = str "((void)0)"
	  | pp_const_exp (T.NULL) = str "NULL"

	and pp_unary_op T.NEG = str "-"
	  | pp_unary_op T.NOT = str "!"
	  | pp_unary_op T.DEREF = str "*"
	  | pp_unary_op T.ADDR = str "&"

	and pp_binary_op T.BLSHIFT = str "<<"
	  | pp_binary_op T.BRSHIFT = str ">>"
	  | pp_binary_op T.BAND    = str "&"
	  | pp_binary_op T.BOR     = str "|"
	  | pp_binary_op T.BXOR    = str "^"
	  | pp_binary_op T.BNOT    = str "~"
	  | pp_binary_op T.PLUS    = str "+"
	  | pp_binary_op T.SUB     = str "-"
	  | pp_binary_op T.MUL     = str "*"
	  | pp_binary_op T.DIV     = str "/"
	  | pp_binary_op T.MOD     = str "%"
	  | pp_binary_op T.EQ      = str "=="
	  | pp_binary_op T.GT      = str ">"
	  | pp_binary_op T.LT      = str "<"
	  | pp_binary_op T.NEQ     = str "!="
	  | pp_binary_op T.GEQ     = str ">="
	  | pp_binary_op T.LEQ     = str "<="
	  | pp_binary_op T.LAND    = str "&&"
	  | pp_binary_op T.LOR     = str "||"

	and pp_exp (T.Constant cst) = pp_const_exp cst
	  | pp_exp (T.Variable id) = pp_id id
	  | pp_exp (T.Call (e,el)) =
	  cat [pp_exp e, hb 2 (str "(") (seq (hsep ",") pp_exp el) (str ")")]
	  | pp_exp (T.Assign(src as (T.Variable x),
			     dst as (T.Binop(bop,T.Variable y,exp)))) =
	    if T.VarId.eq(x,y) then
		case (bop,exp) of
		    (T.PLUS,T.Constant(T.I 1)) =>
			cat [pp_id x, str "++"]
		  | (T.SUB,T.Constant(T.I 1)) =>
			cat [pp_id x, str "--"]
		  | _ => cat [pp_id x,(str " "),
			      pp_binary_op bop, str "= ",pp_exp exp]
	    else cat [pp_exp dst, str " = ",pp_exp src]
	  | pp_exp (T.Assign(dst,src)) =
	    cat [pp_exp dst, str " = ",pp_exp src]
	  | pp_exp (T.Unop (uop,e)) = cat [pp_unary_op uop,pp_exp e]
	  | pp_exp (T.Binop (bop,lhs,rhs)) =
	    cat [pp_exp lhs,(str " "), pp_binary_op bop,(str " "),pp_exp rhs]
	  | pp_exp (T.Cast (te,e)) =
	    cat [str "(",pp_ty_exp te,str ") ",pp_exp e]
	  | pp_exp (T.AggarSub(T.Unop(T.DEREF,exp),id)) =
	    cat [pp_exp exp,str "->",pp_id id]
	  | pp_exp (T.AggarSub(exp,id)) =
	    cat [pp_exp exp,str ".",pp_id id]
	  | pp_exp (T.ArraySub(exp,idx)) =
	    cat [pp_exp exp,str "[",pp_exp idx,str "]"]
	  | pp_exp (T.Comment s) =
	    cat [str "/* ",str s,str " */"]
	  | pp_exp (T.Sizeof e) =
	    cat [str "sizeof(",pp_exp e,str ")"]
	  | pp_exp (T.SizeofT te) =
	    cat [str "sizeof(",pp_ty_exp te,str ")"]
	  | pp_exp (T.ExpSeq el) =
	    hb 2 (str "(") (seq (hsep ",") pp_exp el) (str ")")
	  | pp_exp (T.IfExp {test,then_exp,else_exp}) =
	    cat[str "(",pp_exp test,str " ? ",
		   pp_exp then_exp, str " : ",
		   pp_exp else_exp, str ")"]
	  | pp_exp (T.ExpGroup exp) =
	    cat[str "(",pp_exp exp,str ")"]

	and pp_stmt (T.Nop) = str ";"
	  | pp_stmt T.Break = str "break;"
	  | pp_stmt T.Continue = str "continue;"
	  | pp_stmt (T.Exp e) = cat[pp_exp e,str ";"]
	  | pp_stmt (T.If{test,then_stmt,else_stmt=T.Nop}) =
	  vb 2 (cat [str "if(",pp_exp test,str ")"])
	  (pp_stmt then_stmt) empty
	  | pp_stmt (T.If{test,then_stmt,else_stmt}) =
	    vb 2 (cat [str "if(",pp_exp test,str ")"])
	    (pp_stmt then_stmt) 
	    (vb 2 (str "else") (pp_stmt else_stmt) empty)
	  | pp_stmt (T.For {init,test,step,body}) =
	    cat [hb 2 (str "for(")
		 (seq (hsep ";") pp_exp [init,test,step]) (str ")"),
		 pp_stmt body]
	  | pp_stmt (T.While {test,body}) =
	    cat [str "while(",pp_exp test, str ")", pp_stmt body]
	  | pp_stmt (T.Label _) = str "/* label */"
	  | pp_stmt (T.Goto _) = str "/* label */"
	  | pp_stmt (T.Return e) = cat [str "return ", pp_exp e,str ";"]
	  | pp_stmt (T.Block b) = pp_block b
	  | pp_stmt (T.Switch {test,body,default=T.Nop}) =
	    vb 2 (cat [str "switch(",pp_exp test,str ") {"])
	    (seq nl pp_switch_arm body) (str "}")
	  | pp_stmt (T.Switch {test,body,default}) =
	    vb 2 (cat [str "switch(",pp_exp test,str ") {"])
	    (cat [seq' nl pp_switch_arm body,
		  str "default: ", pp_stmt default])
	     (str "}")
	and pp_switch_arm (T.CaseInt(i,sl)) =
	    vb 2 (cat [str "case ",num i,str ":"])
	    (seq nl pp_stmt sl) (str "/**/")
	  | pp_switch_arm (T.CaseEnum(id,sl)) =
	    vb 2 (cat [str "case ",pp_id id,str ":"])
	    (seq nl pp_stmt sl) (str "/**/")
	and pp_field {name,ty} = cat [pp_ty_exp ty,(str " "),pp_id name]
	and pp_enumer {name,value} =
	  cat [pp_id name, opt empty (fn x => cat[str "=",num x]) value]
	and pp_block {ty_decs,var_decs,stmts} =
	    let
		fun flatten (T.Block{ty_decs=[],var_decs=[],stmts},xs)  =
		    List.foldr flatten xs stmts
		  | flatten (T.Block{ty_decs,var_decs,stmts},xs)  =
		    (T.Block{ty_decs=ty_decs,
			     var_decs=var_decs,
			     stmts=(List.foldr flatten [] stmts)})::xs
		  | flatten (T.Switch{test,body,default},xs) =
		    let
			fun flatten_arm (T.CaseInt(x,sl)) =
			    T.CaseInt(x,List.foldr flatten [] sl)
			  | flatten_arm (T.CaseEnum(x,sl)) =
			    T.CaseEnum(x,List.foldr flatten [] sl)
			val body = List.map flatten_arm body
		    in
			(T.Switch{test=test,body=body,default=default})::xs
		    end
		  | flatten (x,xs) = (x::xs)
		val stmts = List.foldr flatten [] stmts 
	    in vb 2 (str "{")
	      (cat [seq' semi_sep pp_ty_dec ty_decs,
		    seq' semi_sep pp_var_dec var_decs,
		    seq nl pp_stmt stmts])
	      (str "}")
	    end
	and pp_decls dl = seq' nl pp_decl dl
    end

structure AnsiCPP : ALGOL_PP =
    struct
	structure Trans = TranslateAnsiC
	structure Ast = Trans.T
	structure PP = mkPPAnsiC(structure T = AnsiC)
	type code = (Ast.module * Semant.Module.P.props)

	val opts = CommandOptions.empty
	val mkDeps = PPDepends.makefile
	local
	    open AnsiC
	in
	(* hack need to generailze in future  *)
	fun fix_decls decls =
	    let
		fun fix_ty_dec 
		    (TyDec(tid,
			   TyPointer
			   (TyAggregate(aggre,SOME id,fl as (x::xs))))) =
		    ([TyDec(tid,TyPointer(TyAggregate(aggre,SOME id,[])))],
		     [TyAggregateDec(aggre,id,fl)])
		  | fix_ty_dec x = ([x],[])
		
		fun get_ty (Ty td) = SOME td
		  | get_ty _ = NONE

		fun mkProto (Fun(FunDec(id,fl,ty,_))) =
		    SOME (Fun(FunPrototypeDec(id,fl,ty)))
		  | mkProto (Var(VarDecInit(NONE,te,id,_))) =
		    SOME (Var(VarDecs(SOME Extern,te,id,[])))
		  | mkProto _ = NONE
		    
		val ty_decs = List.mapPartial
		    ((Option.map fix_ty_dec) o get_ty) decls
		val not_ty_decs =
		    List.filter (not o Option.isSome o get_ty) decls
		val (fdecs,decs) =
		    List.foldr (fn ((x,y),(xs,ys)) => (x@xs,y@ys))
		    ([],[]) ty_decs
		val fdecs = List.map Ty fdecs
		val protos = List.mapPartial mkProto not_ty_decs
		val decs = List.map Ty decs

		val header =
		    [Com "Typedefs"]@fdecs@
		    [Com "Prototypes"]@protos@
		    [Com "Type Representation"]@decs

	    in
		(PP.pp_decls header,PP.pp_decls not_ty_decs)
	    end
	end
	fun mkComment s =
	    PPUtil.vblock 4 [PPUtil.s "/*",
			 PPUtil.seq_term {fmt=PPUtil.s,sep=PPUtil.nl} s,
			 PPUtil.s "*/"]
	val header_prologue =
	    PPUtil.wrap Semant.Module.P.interface_prologue 
	val header_epilogue =
	    PPUtil.wrap Semant.Module.P.interface_epilogue
	val body_prologue =
	    PPUtil.wrap Semant.Module.P.implementation_prologue 
	val body_epilogue =
	    PPUtil.wrap Semant.Module.P.implementation_epilogue
	    
	fun pp_code p  (arg as (Ast.Module{name,decls,imports}),props)  =
	  let
	    val mn = Ast.ModuleId.toString name
	    val mn = case mn of "" => "Ast" | x => x
	    fun mk_file b x =
	      let val x = AnsiC.ModuleId.toString x
		val x = case x of "" => "Ast" | x => x
	      in OS.Path.joinBaseExt {base=x,ext=SOME b}
	      end
	    fun pp_inc s =  PPUtil.s ("#include \""^s^"\"")
	    val pp_incs =
	      PPUtil.seq_term {fmt=pp_inc,sep=PPUtil.nl}
	      
	    fun pp_impl name body =
	      PPUtil.cat [pp_inc name,
			  body_prologue props,PPUtil.nl,
			  body,PPUtil.nl,
			  body_epilogue props,PPUtil.nl]
		    
	    fun pp_interface name header incs =
	      PPUtil.cat
	      [PPUtil.s ("#ifndef _"^name^"_"), PPUtil.nl,
	       PPUtil.s ("#define _"^name^"_"), PPUtil.nl,
	       pp_incs incs,
	       header_prologue props,PPUtil.nl,
	       header,
	       PPUtil.nl,
	       header_epilogue props,PPUtil.nl,
	       PPUtil.s ("#endif /* _"^name^"_ */"), PPUtil.nl]
	    val (Trans.Ast.Module{decls,name,imports}) =
	      Trans.translate p arg
	    val (header,body) = fix_decls decls
	    val includes = (List.map (mk_file "h") imports)
	  in [FileSet.mkFile{name=mk_file "h" name,
			     depends=includes,
			     body=pp_interface mn header includes},
	      FileSet.mkFile{name=mk_file "c" name,
			     depends=[mk_file "h" name],
			     body=pp_impl (mk_file "h" name) body}]
	  end
    end







