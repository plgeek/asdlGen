(* 
 *
 * COPYRIGHT (c) 1997, 1998, 1999 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure OCamlPP : ALGEBRAIC_PP =
  struct 
    structure Ast = AlgebraicAst
    structure PP = mkPPAst(structure Ast = Ast
			   structure IdMap = IdMaps.OCaml
			   val cap_mod = true
			   val cap_typ = false
			   val sep = ".")
    open PP
    open Ast
    type code =  (Ast.module * Semant.Module.P.props)
    val opts = CommandOptions.empty
    fun mkComment s = vb 1 (str "(*") (seq nl str s) (str " *)")
    val mkDeps = PPDepends.makefile
    fun isTyDec (DeclSum _) = true
      | isTyDec (DeclTy _) = true
      | isTyDec _ = false
      
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
	  in hb 2 (str "{") (seq (hsep ",") pp_one s) (str "}")
	  end
	fun pp_opt_ty tyopt pp =
	  opt pp (fn x => cat [str "(",pp,str " : ",pp_ty_id x,str ")"]) tyopt
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
	  hb 4 (str "{") (seq (hsep "; ") pp_field fes) (str"}")
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
	    vb 2 (cat [str "(match (",pp_id v,str ") with "])
	    (cat [str "  ",seq (vsep "| ") pp_match_clause cl,str ")"])
	    (str "(* end match *)")
	  | pp_exp (Let x) =  pp_exp (LetBind x)
	  | pp_exp (LetBind([],e)) =  pp_exp e
	  | pp_exp (LetBind(cl,e)) =
	    cat [seq nl pp_let_clause cl,nl,pp_exp e]
	  | pp_exp (Seq [e]) = pp_exp e
	  | pp_exp (Seq els) =
	    let fun flatten (Seq x,xs) =  List.foldr flatten xs x
		   | flatten (x,xs) = (x::xs)
		val el = List.foldr flatten [] els
	    in vb 2 (str "begin") (seq (cat [str";",nl]) pp_exp el) (str "end")
	    end
	  | pp_exp (Error (id,s)) =
	    (cat [str "raise (",pp_id id,str " ",pp_mlstr s,str ")"])
	and pp_match (MatchRecord(ml,fl,opt_ty)) = 
	  let fun eq (MatchId (x,_),y)  = VarId.eq (x,y)
		| eq _ = false
	  in pp_opt_ty opt_ty (cat [pp_rec_seq eq pp_match ml fl])
	  end
	  | pp_match (MatchTuple(ml,_,opt_ty)) =
	  pp_opt_ty opt_ty
	  (hb 4 (str "(") (seq (hsep ",") pp_match ml) (str ")"))
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
	  hb 2 (cat [pp_match match,str " -> "]) (pp_exp exp) empty
	and pp_let_clause (match,exp) =
	  hb 2 (cat [str "let ",pp_match match,str " = "])
	  (pp_exp exp) (str " in")

	val sig_prologue =
	  PPUtil.wrap Semant.Module.P.interface_prologue 
	val sig_epilogue =
	  PPUtil.wrap Semant.Module.P.interface_epilogue
	val struct_prologue =
	  PPUtil.wrap Semant.Module.P.implementation_prologue 
	val struct_epilogue =
	  PPUtil.wrap Semant.Module.P.implementation_epilogue
	  
	val ast = decls
	  
	val ty_decs  = List.filter isTyDec ast
	val sigfdecs = List.filter isSigFun ast
	val strfdecs = List.filter isStrFun ast
	  
	fun pp_ty_dec (DeclSum (i,cnstrs)) =
	  vb 4 (cat [pp_ty_id i,str " ="])
	  (cat [str "  ", seq (vsep "| ") pp_cnstr cnstrs]) empty
	  | pp_ty_dec (DeclTy(i,te)) =
	  cat [pp_ty_id i,str " = ",pp_ty_exp te]
	  | pp_ty_dec _ = raise Error.impossible	      
	  
	and pp_cnstr{name,ty_arg=TyTuple([])} = pp_id name
	  | pp_cnstr {name,ty_arg} =
	  cat [pp_id name,str " of ",pp_ty_exp ty_arg]
	  
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
	  lst empty
	  (fn x =>
	   cat [str "type ", seq (vsep "and ") pp_ty_dec x]) ty_decs
	  
	val pp_fdecs =
	  lst empty
	  (fn x =>  cat [nl,str "let rec ",
			 seq (vsep "and ") pp_fun_str x]) strfdecs
	val pp_fsigs =
	  cat [nl, seq nl pp_fun_sig sigfdecs]
	  
	fun pp_str name body =
	  vb 2 (str ("(* module "^name^"*)")) body empty
	fun pp_sig name body = 
	  vb 2 (str ("(* module type "^name^"*)")) body empty
	  
	val sig_tys = [sig_prologue props, pp_ty_decs]
	val str_tys = [struct_prologue props, pp_ty_decs]
	  
	val sig_fdecs = [pp_fsigs, sig_epilogue props]
	val str_fdecs = [pp_fdecs, PP.nl, struct_epilogue props]
	  
	val (dsig_body,dstr_body) = (sig_tys@sig_fdecs,str_tys@str_fdecs)
	val mn = ModuleId.toString (fix_mid name)
	val dsig = pp_sig mn (cat dsig_body) 
	val dstr = pp_str mn (cat dstr_body) 
	val uncap = CvtCase.cvt_string CvtCase.uncapitalize
	fun mk_file b x =
	  let val x = ModuleId.toString (fix_mid x)
	  in OS.Path.joinBaseExt {base=uncap x,ext=SOME b}
	  end
      in [FileSet.mkFile{name=mk_file "mli" name,
			 depends=List.map (mk_file "mli") imports,
			 body=dsig},
	  FileSet.mkFile{name=mk_file "ml" name,
			 depends=[mk_file "mli" name],
			 body=dstr}]
      end
  end






