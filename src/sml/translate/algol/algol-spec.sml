(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure AlgolTy : ALGOL_TYPE_DECL =
  struct
    structure Ast = AlgolAst
    structure T =
      mkTypeDecl(structure TypeId = Ast.TypeId
		 structure VarId = Ast.VarId
		 type ty_exp = Ast.ty_exp
		 type exp = (Ast.ty_exp,
			     Ast.id,Ast.exp,Ast.stmt) StmtExp.stmt_exp
		 type tag = {c:Ast.id,v:int}) 
    open T
  end

functor mkAlgolSpec(structure Ty    : ALGOL_TYPE_DECL) : ALGOL_SPEC =
  struct
(* todo get rid of all the opens *)
    structure Arg =
      struct
	open Ty.Ast
	open StmtExp

	type decl = Ty.Ast.decl
	type attrb_cvt =
	  {toString:Ty.exp -> Ty.exp,fromString:Ty.exp -> Ty.exp}
	type attrib = {name:string,cvt:attrb_cvt}
	structure Ty = Ty

	val inits = [Semant.MEnv.P.init_mono_types false]
	  
	fun mk_name s  =
	  (VarId.prefixBase (s^"_")) o VarId.fromPath o TypeId.toPath
	  
	val rd_name = mk_name "read"
	val wr_name = mk_name "write"

	fun pkl_kind me {xml,std} =
	  case (Semant.MEnv.P.pickler_kind me) of
	    ("xml") => xml
	  | _ => std

	fun die _ =  ProcCall((VarId.fromString "die"),[])
	  
	(* should be determined by functor parmeters *)
	val grd_name = mk_name "read_generic"
	val gwr_name = mk_name "write_generic"
	  
	val arg_id     = VarId.fromString "x_"
	val ret_id     = VarId.fromString "r_"
	val stream_id  = VarId.fromString "s_"

	fun std_pkl s = VarId.fromPath{qualifier=["StdPkl"],base=s}  
	val wr_tag_name = std_pkl "write_tag"
	val rd_tag_name = std_pkl "read_tag"
	val outstream_ty = TyId (TypeId.fromString "outstream")
	val instream_ty = TyId (TypeId.fromString "instream")
	  
	val next_id = ref 0
	fun tmpId () =
	  (next_id := (!next_id) + 1;
	   VarId.fromString ("t"^(Int.toString (!next_id))^"_"))
	(* conservative check *)
	fun isPure (Const _) = true
	  | isPure (NilPtr) = true
	  | isPure (Id _) = true
	  | isPure (VarRecSub(e,_,_)) = isPure e
	  | isPure (RecSub(e,_)) = isPure e
	  | isPure (DeRef e) = isPure e
	  | isPure _ = false
	  
	fun expId (Id id) = SOME id
	  | expId _ = NONE
	  
	fun mk_block (vars,body) =
	  ({vars=List.map (fn (id,ty) => {name=id,ty=ty}) vars,body=body})
	val finfo = {tmpId=tmpId,
		     isPure=isPure,
		     expId=expId,
		     getId=Id,
		     setId=(fn (i,e) => Assign(Id i,e)),
		     stmtScope=Block o mk_block}
	  
	fun get_block res = mk_block o (StmtExp.flatten finfo res)
	fun get_stmt res = Block o (get_block res)
	fun get_proc_body e =
	  (next_id := 0;get_block NONE e)
	  
	fun get_fun_body (e,ty)  =
	  (next_id := 0;
	   get_block NONE (EVAL (e,ty,(fn v => STMT (Return v)))))

	  
	fun write_tag {c,v} =
	  STMT (ProcCall(wr_tag_name,[Const(IntConst v),Id stream_id]))
	  
	fun read_tag cs =
	  let
	    fun mk_clause ret ({c,v},exp)  =
	      {tag=IntConst v,body=get_stmt ret exp}
	    fun exp ret =
	      Case{test=FnCall(rd_tag_name,[Id stream_id]),
		   clauses=List.map (mk_clause ret) cs,
		   default=die "bad tag"}
	  in
	    EXPR exp
	  end
	
	fun read tid = RET (FnCall(rd_name tid,[Id stream_id]))
	fun write tid e =   
	  EVAL(e,TyId tid,
	       (fn e => STMT(ProcCall(wr_name tid,[e,Id stream_id]))))
	  
	fun write_decl {name,arg,body} =
	  DeclProc(wr_name name,
		   [{name=arg_id,ty=arg},{name=stream_id,ty=outstream_ty}],
		   get_proc_body (body (RET(Id arg_id))))
	  
	fun read_decl {name,ret,body} =
	  DeclFun(rd_name name,[{name=stream_id,ty=instream_ty}],
		  get_fun_body (body,ret),
		  ret)
	  
	fun expSeq exps = BIND {vars=[],exps=[],body=(fn _ => exps)}
	fun xml_con_name {c,v} = VarId.toString c
	  
	fun xml_write_elem {name,attribs,content} =
	  let
	    val beg_e =
	      STMT (ProcCall(VarId.fromString "xml_write_element_begin",
			  [Const(StrConst name),Id stream_id]))
	    val end_e =
	      STMT (ProcCall(VarId.fromString "xml_write_element_end",
			     [Const(StrConst name),Id stream_id]))
	  in
	    (expSeq ([beg_e]@content@[end_e]))
	  end
	fun xml_read_elem {name,attribs,content} =
	  let
	    val beg_e =
	      [ProcCall(VarId.fromString "xml_read_element_begin",
			[Const(StrConst name),Id stream_id])]
	    val end_e =
	      [ProcCall(VarId.fromString "xml_read_element_end",
			[Const(StrConst name),Id stream_id])]
	  in
	    EXPR (fn res =>
		  let
		val {vars,body} = get_block res (content[])
		  in
		    Block{vars=vars,body=(beg_e@body@end_e)}
		  end)
	  end
	
	fun xml_read_tagged_elems elems =
	  let
	    val test =
	      (FnCall(VarId.fromString "xml_read_tagged_element",
		      [Id stream_id]))
	    fun read_elem res ({tag=tag as {c,v},attribs,content}) =
	      let
		val name = xml_con_name tag
		val end_e =
		  [ProcCall(VarId.fromString "xml_read_element_end",
			    [Const(StrConst name),Id stream_id])]
		val {vars,body} = get_block res (content []) 
	      in
	    {tag=IntConst v,body=Block{vars=vars,body=body@end_e}}
	      end


	  in
	    EXPR (fn res =>
		  Case{clauses=List.map (read_elem res) elems,
		       test=test,
		       default=die "bad tag"})
	  end
	val prims = []
      end
    structure XMLPklGen = XMLPickler(structure Arg = Arg
				     val tag = "xml")
    structure StdPklGen = StdPickler(structure Arg = Arg
				     val tag = "std")
    open Arg
    fun mk_info x = Ty.addRdWr "std" x Ty.noInfo
    fun get_aux_decls me =
      pkl_kind me {xml=XMLPklGen.trans, std=StdPklGen.trans}
    fun get_tag_decls tags =
      let
    fun topair {c,v} = (VarId.toString c,v)
      in [DeclTagTable(List.map topair tags)]
      end
    
    fun get_reps m k =
      let
	val seq_rep = TySequence
	val opt_rep = TyOption
	fun s2p ty e =
	  EVAL(e,TyShare ty,
	       (fn e => RET(FnCall(VarId.fromString "share2ptr",[e]))))
	fun p2s e =
	  EVAL(e,TyRefAny,
	       (fn e => RET(FnCall(VarId.fromString "ptr2share",[e]))))
	fun id ty e = e
	val (share_rep,p2s,s2p) =
	  if (Semant.MEnv.P.explicit_sharing m) then  (TyShare,(fn x => x),id)
	  else ((fn t => TyAnnotate("shared",t)),p2s,s2p)
	  
	fun ty_exp  (Ty.Prim {ty,...}) = ty
	  | ty_exp  (Ty.Prod {ty,...}) = ty
	  | ty_exp  (Ty.Sum {ty,...}) = ty
	  | ty_exp  (Ty.Alias(tid)) = TyId tid
	  | ty_exp  (_) = raise Error.unimplemented

	fun mk_std_rd f tid = RET (FnCall(f,[Id (grd_name tid),Id stream_id]))
	fun mk_std_wr f (tid,ty) e =
	  EVAL(e,ty,(fn e =>
		     STMT (ProcCall(f,[Id (gwr_name tid),e,Id stream_id]))))

	fun mk_xml_rd f tid =
	  RET (FnCall(f,[Const(StrConst (TypeId.toString tid)),
			 Id (grd_name tid),Id stream_id]))
	fun mk_xml_wr f (tid,ty) e =
	  EVAL(e,ty,(fn e =>
		     STMT (ProcCall(f,
				    [Const(StrConst (TypeId.toString tid)),
				       Id (gwr_name tid),e,Id stream_id]))))

	val (mk_rd,mk_wr,prefix) =
	  pkl_kind m {xml=(mk_xml_rd,mk_xml_wr,"XMLPkl_"),
		      std=(mk_std_rd,mk_std_wr,"StdPkl_")}

	val rd_list_name = VarId.fromString (prefix^"read_list")
	val wr_list_name = VarId.fromString (prefix^"write_list")
	  
	val rd_option_name = VarId.fromString (prefix^"read_option")
	val wr_option_name = VarId.fromString (prefix^"write_option")

	val rd_share_name = VarId.fromString (prefix^"read_share")
	val wr_share_name = VarId.fromString (prefix^"write_share")

	val seq_con =
	  let
	    fun ty_con (tid,t) =
	      let val ty = seq_rep (ty_exp t)
		  val rd = mk_rd rd_list_name tid
		  val wr = mk_wr wr_list_name (tid,ty)
	      in (ty,mk_info {wr=SOME wr,rd=SOME rd})
	      end
	  in  ty_con:Ty.ty_con
	  end
	val opt_con =
	  let
	    fun ty_con (tid,t) =
	      let val ty = opt_rep (ty_exp t)
		  val rd = mk_rd rd_option_name tid
		  val wr = mk_wr wr_option_name (tid,ty)
	      in (ty,mk_info {wr=SOME wr,rd=SOME rd})
	      end
	  in ty_con:Ty.ty_con
	  end
	val share_con =
	  let
	    fun ty_con (tid,t) =
	      let val ty = share_rep (ty_exp t)
		  val share_ty = (TyShare (ty_exp t))
		  val rd = (s2p share_ty) (mk_rd rd_share_name tid)
		  val wr = (mk_wr wr_share_name (tid,share_ty)) o p2s
	      in (ty,mk_info {wr=SOME wr,rd=SOME rd})
	      end
	  in ty_con:Ty.ty_con
	  end
	
	val seq_tid = TypeId.suffixBase "_list" 
	val share_tid = TypeId.suffixBase "_share" 
	val opt_tid = TypeId.suffixBase "_option" 
      in
	case k of
	  Semant.Sequence => {mktid=seq_tid,mkrep=seq_rep,con=seq_con}
	| Semant.Option =>  {mktid=opt_tid,mkrep=opt_rep,con=opt_con}
	| Semant.Shared =>  {mktid=share_tid,mkrep=share_rep,con=share_con}
      end

    fun get_prims me tinfo =
      let
	val {con=seq_con,mktid=seq_tid,...} = get_reps me Semant.Sequence
	val {con=opt_con,mktid=opt_tid,...} = get_reps me Semant.Option
	val {con=share_con,mktid=share_tid,...} = get_reps me Semant.Shared
 	fun read tid = RET (FnCall(mk_name "read" tid,[Id stream_id]))
 	fun write tid e =   
 	  EVAL(e,TyId tid,
 	       (fn e => STMT(ProcCall(mk_name ("write") tid,
 				      [e,Id stream_id]))))
	fun addPrim (tinfo,ps) =
	  let
	    val tname = Semant.Type.src_name tinfo
	    val tid = (TypeId.fromPath o Semant.Type.Id.toPath) tname
	    val info = mk_info {rd=SOME(read tid), wr=SOME(write tid)}
	  in
	    (tid,Ty.Prim {ty=TyId tid,info=info,
			  name=Semant.Type.Id.getBase tname})::
	    (seq_tid tid,Ty.App(seq_con,tid))::
	    (share_tid tid,Ty.App(share_con,tid))::
	    (opt_tid tid,Ty.App(opt_con,tid))::ps
	  end
      in List.foldl addPrim [] tinfo 
      end

    fun get_tag_decls me tags =
      pkl_kind me
      {xml=
       let fun topair {c,v} = (VarId.toString c,v)
       in [DeclTagTable(List.map topair tags)] end,
       std=[]}

    fun call_fn path args = RET (FnCall(VarId.fromPath path,args))
      fun get_info ty p =
	let
	  val rd =
	    case (Semant.Type.P.reader p) of
	      (SOME x) => SOME (call_fn x [Id stream_id])
	    | NONE => NONE
	  val wr =
	    case (Semant.Type.P.writer p) of
	      (SOME x) => SOME
		(fn e =>
		 EVAL(e,ty,
		      (fn v =>
		       STMT(ProcCall(VarId.fromPath x,[v,Id stream_id])))))
	    | NONE => NONE
	in mk_info {wr=wr,rd=rd}
	end
      fun get_wrappers ty p =
	let
	  val natural_ty =
	    case (Semant.Type.P.natural_type p) of
	      SOME t => TyId (TypeId.fromPath t)
	    | NONE => ty

	  val unwrap =
	    case (Semant.Type.P.unwrapper p) of
	      SOME x =>	(fn e => EVAL(e,natural_ty,(fn v => call_fn x [v])))
	    | NONE => (fn x => x)

	  val wrap =
	    case (Semant.Type.P.wrapper p) of
	      SOME x => (fn e => EVAL(e,ty,(fn v => call_fn x [v])))
	    | NONE => (fn x => x)
	  val init =
	    case (Semant.Type.P.user_init p) of
	      NONE => (fn x => x)
	    | SOME x => (fn e => EVAL(e,ty,(fn v => call_fn x [v])))
	in
	  {natural_ty=natural_ty,unwrap=unwrap,wrap=wrap,init=init}
	end
      fun generic_fns ty_id =
	let
	  val rd_decl =
	    DeclFun(grd_name ty_id,[{name=stream_id,ty=instream_ty}],
		     get_fun_body (read ty_id,TyRefAny),
		     TyRefAny)
	  val wr_decl =
	    DeclProc(gwr_name ty_id,
		     [{name=arg_id,ty=TyRefAny},
		      {name=stream_id,ty=outstream_ty}],
		     get_proc_body (write ty_id (RET (Id arg_id))))
	in
	  [rd_decl,wr_decl]
	end
      val user_field_name = VarId.fromString "client_data"

      fun get_user_fields p =
	case (Semant.Type.P.user_attribute p) of
	  NONE => []
	| SOME x =>
	    [{name=user_field_name, ty=TyId (TypeId.fromPath x)}]


  end
  




