(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 * Originally ml-pp.sml 
 * Modified for Haskell output by
 * Fermin Reig Galilea
 * University of Glasgow
 * http://www.dcs.gla.ac.uk/~reig/
 *)

structure HaskellPP : ALGEBRAIC_PP =
  struct 
    structure Ast = AlgebraicAst
    structure PP = mkPPAst(structure Ast = Ast
			   structure IdMap = IdMaps.Haskell
			   val cap_mod = true
			   val cap_typ = true
			   val sep = ".")
    open PP
    open Ast
    val semi = cat [str ";",nl]
    type code =  (Ast.module * Semant.Module.P.props)
    structure O = CommandOptions
    val opts = CommandOptions.empty
    val (opts,base_imp) = O.stringParam opts
      {name="haskell-import",
       flags="",
       arg_dflt=NONE,
       dflt="qualified StdPkl",
       advice="string",
       doc="global import for Haskell code"}
    val (opts,data_derives) = O.stringParam opts
      {name="haskell-deriving",
       flags="",
       arg_dflt=NONE,
       dflt="Prelude.Eq, Prelude.Ord, Prelude.Show",
       advice="string",
       doc="haskell deriving clause"}

    fun mkComment s = vb 1 (str "{-") (seq nl str s) (str "-}")
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
	val pp_id = (local_vid name)
	val pp_ty_id = (local_tid name)
	  
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
	  opt pp (fn x => cat [str "(",pp,str " :: ",pp_ty_id x,str ")"]) tyopt

	fun pp_ty_exp (TyId tid) = pp_ty_id tid
	  | pp_ty_exp (TyList te) = cat [str "[",pp_ty_exp te,str "]"]
	  | pp_ty_exp (TyOption te) = cat [str "(Maybe ",pp_ty_exp te,str ")"]
	  | pp_ty_exp (TySequence te) = cat [str "[",pp_ty_exp te,str "]"]
	  | pp_ty_exp (TyCon (tid,tes)) =
	  hb 4  (cat [str "(",pp_ty_id tid,ws])
	      (seq ws pp_ty_exp tes) (str ")") 
	  | pp_ty_exp (TyVector te) = cat [str "[",pp_ty_exp te,str "]"]
	  | pp_ty_exp (TyTuple []) = str "()"
	  | pp_ty_exp (TyTuple tes) =
	  hb 4 (str "(") (seq (hsep ",") pp_ty_exp tes) (str ")")
	  | pp_ty_exp (TyRecord []) = str "()"
	  | pp_ty_exp (TyRecord fes) =
	  hb 4 (str "{") (seq (hsep ",") pp_field fes) (str"}")
	  | pp_ty_exp (TyFunction (args,res)) =
	  cat [lst (str "()") (seq (hsep " ->") pp_ty_exp) args,
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
	    vb 2 (cat [str "case (",pp_id v,str ") of {"])
	    (seq semi pp_match_clause cl)
	    (str "}")
	  | pp_exp (Let([],e)) =  pp_exp e
	  | pp_exp (Let(cl,e)) = 
	    cat [vb 2 (str "let ")(seq semi pp_let_clause cl) (str "in "),
		 pp_exp e, PP.nl]
	  (* hacks... need to think more carefully about do notation*)
	  | pp_exp (LetBind([],e)) = cat [str "return(",pp_exp e,str ")"]
	  | pp_exp (LetBind(cl,e as Match(_,_))) =
	    vb 2 (str "do {")
	    (cat [seq semi pp_do_clause cl,semi,pp_exp e])
		 (str "}")
	  | pp_exp (LetBind(cl,Seq es)) =
	    let fun flatten (Seq x,xs) =  List.foldr flatten xs x
		  | flatten (x,xs) = (x::xs)
		val es = List.foldr flatten [] es
		val len = List.length es
		val front = List.take (es,len - 1)
		val e = List.hd (List.drop (es,len - 1))
		val cl = cl@(List.map (fn e => (MatchAny,e)) front)
	    in pp_exp (LetBind(cl,e))
	    end
	  | pp_exp (LetBind(cl,e)) =
	    vb 2 (str "do {")
	    (cat [seq semi pp_do_clause cl,semi,
		  str "return (",pp_exp e,str ")"])
		 (str "}")
	  | pp_exp (Seq [e]) = pp_exp e
	  | pp_exp (Seq els) =
	    let fun flatten (Seq x,xs) =  List.foldr flatten xs x
		   | flatten (x,xs) = (x::xs)
		val el = List.foldr flatten [] els
	    in vb 2 (str "do {") (seq semi pp_exp el) (str "}")
	    end
	  | pp_exp (Error (_,s)) =
	    (cat [str "Prelude.ioError (Prelude.userError ",
		  pp_mlstr s,str ")"])
	(* end hacks *)
	and pp_match (MatchRecord(ml,fl,opt_ty)) = 
	  let fun eq (MatchId (x,_),y)  = VarId.eq (x,y)
		| eq _ = false
	  in (cat [pp_rec_seq eq pp_match ml fl])
	  end
	  | pp_match (MatchTuple(ml,_,opt_ty)) =
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
	  cat [pp_id name,str "::",pp_ty_exp ty]
	and pp_match_clause (match,exp) =
	  hb 2 (cat [pp_match match,str " -> "]) (pp_exp exp) empty
	and pp_let_clause (match,exp) =
	  cat [pp_match match,str " = ",pp_exp exp]
	and pp_do_clause (MatchAny,exp) =
	  cat [pp_exp exp]
	  |  pp_do_clause (match,exp) =
	  cat [pp_match match,str " <- ",pp_exp exp]
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
	val derives =  data_derives p
	fun pp_ty_dec (DeclSum (i,cnstrs)) =
	  vb 2 (cat [str "data ",pp_ty_id i,str " ="])
	  (cat [str "  ", seq (vsep "| ") pp_cnstr cnstrs])
	  (str ("  deriving ("^derives^")"))
	  | pp_ty_dec (DeclTy(i,te)) =
	  cat [str "type ",pp_ty_id i,str " = ",pp_ty_exp te]
	  | pp_ty_dec _ = raise Error.impossible	      
	  
	and pp_cnstr{name,ty_arg=TyTuple([])} = pp_id name
	  | pp_cnstr {name,ty_arg} =
	  cat [pp_id name,str " ",pp_ty_exp ty_arg]
	  
	fun pp_fun_str (DeclFun (id,args,body,ret)) =
	  vb 2 (cat [pp_id id,str " ",
		     seq (str " ") (base_vid o #name) args, str " = "])
	  (pp_exp body) empty
	  | pp_fun_str _ = raise Error.impossible
	  
	fun pp_fun_sig (DeclFun (id,args,body,ret)) =
	  hb 4 (cat [pp_id id,str " :: "])
	  (cat [seq (hsep " ->") (pp_ty_exp o #ty) args,
		(hsep " ->"), pp_ty_exp ret]) empty
	  | pp_fun_sig _ = raise Error.impossible

	fun pp_export (DeclTy(name,_)) = pp_ty_id name
	  | pp_export (DeclSum(name,_)) = cat [pp_ty_id name,str "(..)"]
	  | pp_export (DeclVar(name,_,_)) = pp_id name
	  | pp_export (DeclFun(name,_,_,_)) = pp_id name
	  
	val pp_ty_decs = 
	  lst empty (fn x =>  cat [nl,seq nl pp_ty_dec x]) ty_decs
	val pp_exports = seq (hsep ",") pp_export ast

	val pp_fdecs =
	  lst empty (fn x =>  cat [nl,seq nl pp_fun_str x]) strfdecs
	val pp_fsigs =
	  lst empty (fn x =>  cat [nl,seq nl pp_fun_sig x]) sigfdecs

	val pp_imports =
	  seq nl (fn x => cat [str "import qualified ", mid x]) imports

	fun pp_module name exps imps body =
	  vb 0 (cat [str "module ",mid name,
		     hb 4 (str "(") exps (str ") where")])
	  (cat [imps,nl,body])
	  empty
	  
	val exports = cat 
	  [sig_prologue props,
	   pp_exports,
	   sig_epilogue props]

	val body = cat
	  [struct_prologue props,
	   pp_ty_decs,
	   pp_fsigs,
	   pp_fdecs,nl,
	   struct_epilogue props]
	val base_import = base_imp p
	val pp_imports =
	  cat [str "import Prelude (Maybe(..),return)",nl,
	       str "import qualified Prelude",nl,
	       str "import qualified SexpPkl",nl,
	       str ("import "^base_import),nl,
	       pp_imports]

	val module = pp_module name exports pp_imports body
	val mn = ModuleId.toString (fix_mid name)
	fun file_name m =
	  OS.Path.joinBaseExt{base=(ModuleId.toString (fix_mid m)),
				    ext=SOME "hs"}
      in [FileSet.mkFile{name=file_name name,
			 depends=List.map file_name imports,
			 body=module}]
      end
  end

