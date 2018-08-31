(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure SMLPP : ALGEBRAIC_PP =
  struct 
    structure Ast = AlgebraicAst
    structure PP = mkPPAst(structure Ast = Ast
			   structure IdMap = IdMaps.SML
			   val cap_mod = false
			   val cap_typ = false
			   val sep = ".")
    open PP
    open Ast

    type code =  (Ast.module * Semant.Module.P.props)
    val opts = CommandOptions.empty
    fun mkComment s = vb 1 (str "(*") (seq nl str s) (str " *)")
    val mkDeps = PPDepends.cmfile
    fun isSum (DeclSum _) = true
      | isSum _ = false
      
    fun isTy (DeclTy _ ) = true
      | isTy _ = false
      
    fun isSigFun (DeclFun _ ) = true
      | isSigFun _ = false

    fun isStrFun (DeclFun _ ) = true
      | isStrFun _ = false

    fun pp_mlstr s = str ("\""^(String.toString s)^"\"")
      fun pp_code p (Module{name,imports,decls},props) =
	let
	  val pp_id = local_vid name
	  val pp_ty_id = local_tid name
	    
	  fun pp_rec_seq eq fmt x y =
	    let
	      fun zip_fields ([],[]) = []
		| zip_fields  (x::xs,{name=y,ty}::ys) =
		if (eq(x,y)) then (NONE,y)::(zip_fields (xs,ys))
		else (SOME x,y)::(zip_fields  (xs,ys))
		| zip_fields _ = raise Error.internal
	      fun pp_one (NONE,y) = base_vid y
		| pp_one (SOME x,y) = PP.cat [base_vid y,str "=", fmt x]
	      val s = zip_fields (x,y)
	    in hb 4 (str "{") (seq (hsep ",") pp_one s) (str "}")
	    end
	  val pp_opt_ty = opt empty (fn x => cat [str " : ",pp_ty_id x])
	  fun pp_ty_exp (TyId tid) = pp_ty_id tid
	    | pp_ty_exp (TyList te) = cat [pp_ty_exp te,str " list"]
	    | pp_ty_exp (TyOption te) = cat [pp_ty_exp te,str " option"]
	    | pp_ty_exp (TySequence te) = cat [pp_ty_exp te,str " Seq.seq"]
	    | pp_ty_exp (TyCon (tid,[te])) =
	    cat [pp_ty_exp te,str " ",pp_ty_id tid]
	    | pp_ty_exp (TyCon (tid,tes)) =
	    hb 4 (str "(") (seq (hsep ",") pp_ty_exp tes) 
	    (cat [str ") ",pp_ty_id tid])
	    | pp_ty_exp (TyVector te) = cat [pp_ty_exp te,str " vector"]
	    | pp_ty_exp (TyTuple []) = str "unit"
	    | pp_ty_exp (TyTuple tes) =
	    hb 4 (str "(") (seq (hsep " *") pp_ty_exp tes) (str")")
	    
	    | pp_ty_exp (TyRecord []) = str "unit"
	    | pp_ty_exp (TyRecord fes) =
	    hb 4 (str "{") (seq (hsep ", ") pp_field fes) (str"}")
	    | pp_ty_exp (TyFunction (args,res)) =
	    cat [lst (str "unit") (seq (hsep " ->") pp_ty_exp) args,
		 (hsep " ->"), pp_ty_exp res]
	  and pp_exp (Id id) = pp_id id
	    | pp_exp (Int i) = num i
	    | pp_exp (Str s) = pp_mlstr s
	    | pp_exp (Call (e,el)) =
	    hb 4 (str "(") (seq ws pp_exp (e::el)) (str ")")
	    | pp_exp (Cnstr(id,Tuple([],_))) = pp_id id
	    | pp_exp (Cnstr(id,Record([],[],_))) = pp_id id
	    | pp_exp (Cnstr(id,e)) = cat [pp_id id,pp_exp e]
	    | pp_exp (Tuple (el,opt_ty)) =
	    hb 4 (str "(") (seq (hsep ",") pp_exp el) (str ")")
	    | pp_exp (Record (el,fl,opt_ty)) =
	    let fun eq _ = false
	    in (pp_rec_seq eq pp_exp el fl)
	    end
	    | pp_exp (Match(v,cl)) =
	    vb 2 (cat [str "(case (",pp_id v,str ") of "])
	    (cat [str "  ",seq (vsep "| ") pp_match_clause cl,str ")"])
	     (str "(* end case *)")
	    | pp_exp (Let x) =  pp_exp (LetBind x)
	    | pp_exp (LetBind([],e)) =  pp_exp e
	    | pp_exp (LetBind(cl,e)) =
		 cat [vb 2 (str "let")(seq nl pp_let_clause cl) (str "in "),
		      pp_exp e, PP.nl,
		      str "end"]
	    | pp_exp (Seq [e]) = pp_exp e
	    | pp_exp (Seq els) =
	    let fun flatten (Seq x,xs) =  List.foldr flatten xs x
		  | flatten (x,xs) = (x::xs)
		val el = List.foldr flatten [] els
	    in hb 1 nl
	      (cat [str "(",seq (cat [str";",nl]) pp_exp el,str ")"])
	      empty
	    end
	  | pp_exp (Error (id,s)) =
	    (cat [str "raise (",pp_id id,str " ",pp_mlstr s,str ")"])
	  and pp_match (MatchRecord(ml,fl,opt_ty)) = 
	    let fun eq (MatchId (x,_),y)  = VarId.eq (x,y)
		  | eq _ = false
	    in cat [pp_rec_seq eq pp_match ml fl,pp_opt_ty opt_ty]
	    end
	    | pp_match (MatchTuple(ml,_,opt_ty)) =
	    hb 4 (str "(") (seq (hsep ",") pp_match ml)
	    (cat [str ")", pp_opt_ty opt_ty])
	    | pp_match (MatchId(id,_)) = pp_id id
	    | pp_match (MatchCnstr(MatchTuple([],_,_),{name,...})) = pp_id name
	    | pp_match (MatchCnstr(MatchRecord([],_,_),{name,...}))= pp_id name
	    | pp_match (MatchCnstr(m,{name,...})) =
	    cat [str "(",pp_id name,pp_match m,str ")"]
	    | pp_match (MatchInt i) = num i
	    | pp_match (MatchStr s) = pp_mlstr s
	    | pp_match (MatchAny) = str "_"
	      
	  and pp_field {name,ty} =
	    cat [pp_id name,str ":",pp_ty_exp ty]
	  and pp_match_clause (match,exp) =
	    hb 2 (cat [pp_match match,str " => "]) (pp_exp exp) empty
	  and pp_let_clause (match,exp) =
	    hb 2 (cat [str "val ",pp_match match,str " = "]) (pp_exp exp) empty
	      	      
	  val sig_prologue =
	    PPUtil.wrap Semant.Module.P.interface_prologue 
	  val sig_epilogue =
	    PPUtil.wrap Semant.Module.P.interface_epilogue
	  val struct_prologue =
	    PPUtil.wrap Semant.Module.P.implementation_prologue 
	  val struct_epilogue =
	    PPUtil.wrap Semant.Module.P.implementation_epilogue
	      
	  val ast = decls
	  val mn = ModuleId.toString (fix_mid name)
	      
	  val sdecs = List.filter isSum ast
	  val decs = List.filter isTy ast
	  val sigfdecs = List.filter isSigFun ast
	  val strfdecs = List.filter isStrFun ast
	      
	  fun pp_sdec (DeclSum (i,cnstrs)) =
	    vb 4 (cat [pp_ty_id i,str " ="])
		 (cat [str "  ", seq (vsep "| ") pp_cnstr cnstrs]) empty
	    | pp_sdec _ = raise Error.impossible
	      
	  and pp_cnstr{name,ty_arg=TyTuple([])} = pp_id name
	    | pp_cnstr {name,ty_arg} =
		  cat [pp_id name,str " of ",pp_ty_exp ty_arg]
	      
	  fun pp_dec (DeclTy(i,te)) =
		  cat [pp_ty_id i,str " = ",pp_ty_exp te]
	    | pp_dec _ = raise Error.impossible
	      
	  fun pp_fun_str (DeclFun (id,args,body,ret)) =
	    vb 4 (cat [pp_id id,str " ",
		       seq (str " ") (base_vid o #name) args, str " = "])
	    (pp_exp body) empty
	    | pp_fun_str _ = raise Error.impossible
	      
	  fun pp_fun_sig (DeclFun (id,args,body,ret)) =
	    hb 4 (cat [str "val ",pp_id id,str " : "])
	    (cat [seq (hsep " ->") (pp_ty_exp o #ty) args,
		  (hsep " ->"), pp_ty_exp ret]) empty
	    | pp_fun_sig _ = raise Error.impossible
	      
	  val pp_ty_decs =
	    case (sdecs,decs) of
	      ([],[]) => empty
	    | (sdecs,[]) =>
		lst empty
		(fn x =>
		 cat [str "datatype ", seq (vsep "and ") pp_sdec x]) sdecs
	      | ([],decs) => lst empty
		(fn x =>
		 cat [str "type ", seq (vsep "and ") pp_dec x]) decs
	      | (sdecs,decs) => 
		cat [str "datatype ",
		     seq (vsep "and ") pp_sdec sdecs,nl,
		     str "withtype ",
		     seq (vsep "and ") pp_dec decs]
	  val pp_fdecs =
	    lst empty
	    (fn x =>  cat [nl,str "fun ",
			   seq (vsep "and ") pp_fun_str x]) strfdecs
	  val pp_fsigs =
	    cat [nl, seq nl pp_fun_sig sigfdecs]

	  fun pp_str name body =
	    vb 2 (cat [str ("structure "^name), str (" : "^name^"_SIG =")])
	    (vb 2 (str "struct") body (str "end (* struct *)"))
	    empty

	  fun pp_sig name body =
	    vb 2 (cat [str ("signature "^name^"_SIG = ")])
	    (vb 2 (str "sig") body (str "end (* sig *)"))
	    empty

	  val sig_tys = [sig_prologue props, pp_ty_decs]
	  val str_tys = [struct_prologue props, pp_ty_decs]

	  val sig_fdecs = [pp_fsigs, sig_epilogue props]
	  val str_fdecs = [pp_fdecs, PP.nl, struct_epilogue props]

	  val (dsig_body,dstr_body) = (sig_tys@sig_fdecs,str_tys@str_fdecs)

	  val dsig = pp_sig mn (cat dsig_body) 
	  val dstr = pp_str mn (cat dstr_body) 
	  fun mk_file b x =
	    let val x = ModuleId.toString (fix_mid x)
	    in OS.Path.joinBaseExt {base=x,ext=SOME b}
	    end

	in [FileSet.mkFile{name=mk_file "sig" name,
			 depends=List.map (mk_file "sml") imports,
			    body=dsig},
	    FileSet.mkFile{name=mk_file "sml" name,
			   depends=[mk_file "sig" name],
			   body=dstr}]
       end
  end
