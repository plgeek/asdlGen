(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure OOTy : OO_TYPE_DECL =
  struct
    structure Ast = OOAst
    structure T =
      mkTypeDecl(structure TypeId = Ast.TypeId
		 structure VarId = Ast.VarId
		 type ty_exp = Ast.ty_exp
		 type exp = (Ast.ty_exp,
			     Ast.id,Ast.exp,Ast.stmt) StmtExp.stmt_exp
		 type tag = {c:Ast.id,v:int}) 
    open T
  end

functor mkOOSpec(structure Ty    : OO_TYPE_DECL
		 val streams_ty  : {outs:string,ins:string} option
		 val int_ty      : string 
		 val int_kind    : bool) : OO_SPEC =
  struct
    open Ty.Ast
    open StmtExp
    val inits = []
    val int_kind = int_kind

    structure Arg =
      struct
	structure Ty = Ty
	type decl = (Ty.ty_id * Ty.Ast.mth)
	fun mk_name s  =
	  (VarId.prefixBase (s^"_")) o VarId.fromPath o TypeId.toPath
	val streams_ty = Option.getOpt
	  (streams_ty,{ins="instream",outs="outstream"})
	val int_tid = TypeId.fromString int_ty
	val int_ty = TyId int_tid
	val rd_name = VarId.fromString "read"
	val wr_name = VarId.fromString "write"

	val die = Die

	(* should be determined by functor parmeters *)
	val mrd_name = mk_name "read"
	val mwr_name = mk_name "write"

	val arg_id     = VarId.fromString "x_"
	val ret_id     = VarId.fromString "r_"
	val stream_id  = VarId.fromString "s_"
	  
	fun std_pkl s = VarId.fromPath{qualifier=["StdPkl"],base=s}
	val wr_tag_name = std_pkl "write_tag"
	val rd_tag_name = std_pkl "read_tag"
	val outstream_ty = TyId (TypeId.fromString (#outs streams_ty))
	val instream_ty = TyId (TypeId.fromString (#ins streams_ty))

	val next_id = ref 0
	fun tmpId () =
	  (next_id := (!next_id) + 1;
	   VarId.fromString ("t"^Int.toString (!next_id)))
	(* conservative check *)
	fun isPure (Const _) = true
	  | isPure (NilPtr) = true
	  | isPure (Id _) = true
	  | isPure (FieldSub(e,_)) = isPure e
	  | isPure (ArraySub(e1,e2)) = (isPure e1) andalso (isPure e2)
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

	(*   fun get_fun_body (e,ty)  =
	 (next_id := 0;
	 let
	   val {vars,body} =
	     get_block NONE (BIND{vars=[(ret_id,ty)],exps=[e],
	     body=(fn [v] => [STMT (Return (Id v))]
	 | _ => raise Error.impossible)})
       in
	 {vars=vars,body=(Assign (Id ret_id,NilPtr))::body}
       end)*)

	fun write_tag {c,v} =
	  STMT (Expr(FunCall(wr_tag_name,[Const(IntConst v),Id stream_id])))
	  
	fun read_tag cs =
	  let
	    fun mk_clause ret ({c,v},exp)  =
	      {tag=IntConst v,body=get_stmt ret exp}
	    fun exp ret =
	      Case{test=FunCall(rd_tag_name,[Id stream_id]),
		   clauses=List.map (mk_clause ret) cs,
		   default=die "bad tag"}
	  in
	    EXPR exp
	  end
	
	fun read tid = RET (SMthCall(tid,rd_name,[Id stream_id]))
	fun write tid e =   
	  EVAL(e,TyReference(TyId tid),
	       (fn v =>
		STMT(Expr(SMthCall(tid,wr_name,[v,Id stream_id])))))
	  
	val void_ty  = TyVoid
	fun write_decl {name,arg,body} =
	  (name,Mth{name=wr_name,
		    inline=false,
		    mods={scope=Public,static=true,final=true},
		    args=[{name=arg_id,ty=arg},
			  {name=stream_id,ty=outstream_ty}],
		    ret=void_ty,
		    body=get_proc_body (body (RET(Id arg_id)))})
	  
	fun read_decl {name,ret,body} =
	  (name,Mth{name=rd_name,
		    inline=false,
		    mods={scope=Public,static=true,final=true},
		    args=[{name=stream_id,ty=instream_ty}],
		    ret=ret,
		    body=get_fun_body (body,ret)})
	  
	  
	fun expSeq exps = BIND {vars=[],exps=[],body=(fn _ => exps)}
      end
    structure StdPklGen = StdPickler(structure Arg = Arg
				     val tag = "std")
    open Arg
    fun get_aux_decls me env tids =
      let val pkls = StdPklGen.trans env tids
      in pkls
      end
    fun mk_info x = Ty.addRdWr "std" x Ty.noInfo
    val seq_tid =  TypeId.suffixBase "_list" 
    val opt_tid =  TypeId.suffixBase "_option" 
    val seq_rep = TySequence
    val opt_rep = TyOption
      
    fun ty_exp  (Ty.Prim {ty,...}) = ty
      | ty_exp  (Ty.Prod {ty,...}) = ty
      | ty_exp  (Ty.Sum {ty,...}) = ty
      | ty_exp  (_) = raise Error.unimplemented

    fun elt_ops (tid,t) =
      (case t of Ty.Prim{ty,...} =>
	 let
	   val rd_elt =  (RET (FunCall(mrd_name tid,[Id stream_id])))
	   fun wr_elt e =
	     EVAL(e,ty,(fn v =>
			STMT (Expr(FunCall(mwr_name tid,[v,Id stream_id])))))
	 in (rd_elt,wr_elt)
	 end
    | _ => (read tid,write tid))
      
    val seq_con =
      let
	fun (*ty_con (tid,Ty.Prim{ty,...}) =
	  let
	    val tid = (seq_tid tid)
	    val ty = (TyId tid)
	    val rd = RET (FunCall(mrd_name tid,[Id stream_id]))
	    fun wr e =
	      EVAL(e,ty,(fn v =>
			 STMT (Expr(FunCall(mwr_name tid,[v,Id stream_id])))))
	  in (ty,{wr=SOME wr,rd=SOME rd})
	  end 
      | *) ty_con (tid,t) =
	  let
	    val (rd_elt,wr_elt) = elt_ops (tid,t)
	    val elm_ty = (ty_exp t)
	    val ty = seq_rep elm_ty
	    val len_var = (VarId.fromString "len",int_ty)
	    val idx_var = (VarId.fromString "idx",int_ty)
	    val seq_var = (VarId.fromString "seq",ty)

	    fun rd_body [len,idx,seq] =
	      [EVAL(rd_elt,elm_ty,
		    (fn v =>
		     STMT (While{test=Less(Id idx,Id len),
			   body=Block
			   {vars=[],body=
			    [Expr (SeqSet{elm_ty=elm_ty,seq=Id seq,idx=Id idx,
					  v=v}),
			     Assign(Id idx,PlusOne (Id idx))]}}))),
	       EXPR (fn (SOME (id,_)) => Assign(Id id,Id seq)
	                   |     NONE => Nop)]
	      | rd_body _ = raise Error.impossible
	    val rd =
	      BIND{vars=[len_var,idx_var,seq_var],
		   exps=[RET (FunCall(rd_tag_name,[Id stream_id])),
			 RET (Const(IntConst 0)),
			 RET (SeqNew{elm_ty=elm_ty,len=Id (#1 len_var)})],
		   body=rd_body}

	    fun wr_body seq [len,idx] =
	      let
		val {vars,body} = 
		  get_block NONE
		  (wr_elt (RET (SeqGet{elm_ty=elm_ty,seq=seq,idx=Id idx})))
	      in
		[STMT (Expr(FunCall(wr_tag_name,[Id len,Id stream_id]))),
		 STMT (While{test=Less(Id idx,Id len),
			     body=Block{vars=vars,body=body@
					[Assign(Id idx,PlusOne (Id idx))]}})]
	      end
	      | wr_body _ _ = raise Error.impossible
	    fun wr e =
	      EVAL(e,ty,
		   (fn seq =>
		    BIND{vars=[len_var,idx_var],
			 exps=[RET (SeqLen{elm_ty=elm_ty,seq=seq}),
			       RET (Const(IntConst 0))],
			 body=wr_body seq}))
	  in (ty,mk_info {wr=SOME wr,rd=SOME rd})
	  end
      in ty_con:Ty.ty_con
      end

    val opt_con =
      let
	val some_tag = {v=1,c=VarId.fromString "SOME"}
	val none_tag = {v=0,c=VarId.fromString "NONE"}

	fun (*ty_con (tid,Ty.Prim{ty,...}) =
	  let
	    val tid = (opt_tid tid)
	    val ty = (TyId tid)
	    val rd = RET (FunCall((mrd_name tid),[Id stream_id]))
	    fun wr e =
	      EVAL(e,ty,(fn v =>
			 STMT(Expr(FunCall((mwr_name tid),[v,Id stream_id])))))
	  in (ty,{wr=SOME wr,rd=SOME rd})
	  end 
	  | *)ty_con (tid,t) =
	  let
	    val elm_ty = (ty_exp t)
	    val ty = opt_rep (ty_exp t)
	    val (rd_elt,wr_elt) = elt_ops (tid,t)
	    val rd =
	      EXPR (fn arg =>
		    If{test=NotZero(FunCall(rd_tag_name,[Id stream_id])),
		       then_stmt=get_stmt arg
		       (EVAL(rd_elt,elm_ty,
			(fn x => RET(OptSome (elm_ty, x))))),
		       else_stmt=
		       (case arg of
			 NONE => Nop
		       | (SOME (ret,_)) =>
			   Assign(Id ret, OptNone elm_ty))})
	    fun wr e =
	      EVAL(e,ty,
		   (fn v =>
		    STMT(If{test=OptIsSome(elm_ty,v),
			    then_stmt=get_stmt NONE 
			    (expSeq [write_tag some_tag,
				     wr_elt (RET (OptGetVal(elm_ty,v)))]),
			    else_stmt=get_stmt NONE (write_tag none_tag)})))
	  in (ty,mk_info{wr=SOME wr,rd=SOME rd})
	  end

      in ty_con:Ty.ty_con
      end

      fun addPrim (tinfo,ps) =
	let
	  val tname = Semant.Type.src_name tinfo
	  val tid = (TypeId.fromPath o Semant.Type.Id.toPath) tname
	  val rd = RET (FunCall(mk_name "read" tid,[Id stream_id]))
	  fun wr e = 
	    EVAL(e,TyId tid,(fn e =>
			     STMT(Expr(FunCall(mk_name "write" tid,
					       [e,Id stream_id])))))
	  val info = mk_info {rd=SOME rd,wr=SOME wr}
	in (tid,Ty.Prim {ty=TyId tid,info=info,
			 name=Semant.Type.Id.getBase tname})::
	   (seq_tid tid,Ty.App(seq_con,tid))::
	   (opt_tid tid,Ty.App(opt_con,tid))::ps
	end
      fun prims tinfos = List.foldl addPrim [] tinfos
    

      fun call_fn path args = RET (FunCall(VarId.fromPath path,args))
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
		       STMT(Expr(FunCall(VarId.fromPath x,
					 [v,Id stream_id]))))))
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
	    | SOME x => (fn e =>
			 EVAL(e,ty,(fn v => RET(MthCall(Id (VarId.fromPath x),
							[v])))))
	in
	  {natural_ty=natural_ty,unwrap=unwrap,wrap=wrap,init=init}
	end
    val user_field_name = VarId.fromString "client_data"

      fun get_user_fields p =
	case (Semant.Type.P.user_attribute p) of
	  NONE => []
	| SOME x =>
	    [{name=user_field_name, ty=TyId (TypeId.fromPath x)}]


  end


