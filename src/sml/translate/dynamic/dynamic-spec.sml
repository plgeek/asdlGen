(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
structure DynamicTy : DYNAMIC_TYPE_DECL =
  struct
    structure Ast = DynamicAst
    structure T =
      mkTypeDecl(structure TypeId = Ast.TypeId
		 structure VarId = Ast.VarId
		 type ty_exp = Ast.ty_exp
		 type exp = Ast.exp
		 type tag = {c:Ast.ty_id,v:int}) 
    open T
  end

functor mkDynamicSpec(structure Ty : DYNAMIC_TYPE_DECL) : DYNAMIC_SPEC =
  struct
    structure Arg =
      struct 
	open Ty.Ast
	type decl = Ty.Ast.decl
	type get_ty = (Ty.ty_id -> Ty.ty_exp option)
	structure Ty = Ty
	val inits = []
	val streams_ty ={ins="instream",outs="outstream"}
	fun std_pkl s = VarId.fromPath{qualifier=["StdPkl"],base=s}

	fun die s = Error s
	
	fun mk_name s id =
	  let
	    val {qualifier,base} = TypeId.toPath id
	    val base = s^"_"^base
	  in VarId.fromPath{base=base,qualifier=qualifier}
	  end
	  
	val rd_name = mk_name "read"
	val wr_name = mk_name "write"
	val arg_id     = VarId.fromString "x_"
	val stream_id  = VarId.fromString "s_"
	val wr_tag_name = std_pkl "write_tag"
	val rd_tag_name = std_pkl "read_tag"

	val unit_ty = (TyId (TypeId.fromString "void"))
	val outstream_ty = TyId 
	  (TypeId.fromPath {qualifier=["StdPkl"],
			    base=(#outs streams_ty)})
	val instream_ty = TyId 
	  (TypeId.fromPath {qualifier=["StdPkl"],
			    base=(#ins streams_ty)})

	fun write_tag {c,v} = Call(Id(wr_tag_name),
				   [(Const (Int v)),Id stream_id])
	fun read_tag cs =
	  let fun mk_clause ({c,v},exp) = {const=Int v,body=exp}
	  in Case{test=Call(Id rd_tag_name,[Id stream_id]),
		  clauses=(List.map mk_clause cs),
		  default=(die "bad tag")}
	  end
	
	fun read tid = Call(Id (rd_name tid),[Id stream_id])
	fun write tid e = Call(Id (wr_name tid),[e,Id stream_id])
	fun write_decl {name,arg,body} =
	  DeclFun(wr_name name,
		  [{name=arg_id,ty=arg},
		   {name=stream_id,ty=outstream_ty}],
		  body (Id arg_id),unit_ty)
	fun read_decl {name,ret,body} =
	  DeclFun(rd_name name,
	      [{name=stream_id,ty=instream_ty}],body,ret)
	val expSeq = Seq
      end

    open Arg
    structure StdPklGen = StdPickler(structure Arg = Arg
				     val tag = "std" )

    fun mk_info x = Ty.addRdWr "std" x Ty.noInfo
    fun get_aux_decls me env tids = StdPklGen.trans env tids
    fun seq_rep te = TyCon(TypeId.fromString "list",[te])
    fun opt_rep te = TyCon(TypeId.fromString "opt",[te])
    fun share_rep te = TyCon(TypeId.fromString "share",[te])

    fun ty_exp  (Ty.Prim {ty,...}) = ty
      | ty_exp  (Ty.Prod {ty,...}) = ty
      | ty_exp  (Ty.Sum {ty,...}) = ty
      | ty_exp  (_) = raise Error.unimplemented

    val seq_con =
      let
	val rd_list_name = std_pkl "read_list"
	val wr_list_name = std_pkl "write_list"
	fun ty_con (tid,t) =
	  let
	    val ty = seq_rep (ty_exp t)
	    val rd = Call(Id rd_list_name,
			  [Id (rd_name tid),Id stream_id])
	    fun wr e =
	      Call(Id wr_list_name,
		   [Id (wr_name tid),e,Id stream_id])
	  in (ty,mk_info {wr=SOME wr,rd=SOME rd})
	  end
      in ty_con
      end
    
    val opt_con =
      let
	val rd_option_name = std_pkl "read_option"
	val wr_option_name = std_pkl "write_option"
	fun ty_con (tid,t) =
	  let
	    val ty = opt_rep (ty_exp t)
	    val rd = Call(Id rd_option_name,
			  [Id (rd_name tid),Id stream_id])
	    fun wr e = Call(Id wr_option_name,
			    [Id (wr_name tid),e,Id stream_id])
	  in
	    (ty,mk_info{wr=SOME wr,rd=SOME rd})
	  end
      in ty_con
      end
    
    val share_con =
      let
	val rd_share_name = std_pkl "read_share"
	val wr_share_name = std_pkl "write_share"
	fun ty_con (tid,t) =
	  let
	    val ty = share_rep (ty_exp t)
	    val rd = Call(Id rd_share_name,
			  [Id (rd_name tid),Id stream_id])
	    fun wr e =
	      Call(Id wr_share_name,
		   [Id (wr_name tid),e,Id stream_id])
	  in (ty,mk_info{wr=SOME wr,rd=SOME rd})
	  end
      in ty_con
      end

    val seq_tid =  TypeId.suffixBase "_list" 
    val opt_tid =  TypeId.suffixBase "_option" 
    val share_tid =  TypeId.suffixBase "_share" 


    fun addPrim (tinfo,ps) =
      let
	val tname = Semant.Type.src_name tinfo
	val tid = (TypeId.fromPath o Semant.Type.Id.toPath) tname
	val info = mk_info {rd=SOME(read tid),
			    wr=SOME(write tid)}
      in(tid,Ty.Prim {ty=TyId tid,info=info,
		      name=Semant.Type.Id.getBase tname})::
	(seq_tid tid,Ty.App(seq_con,tid))::
	(opt_tid tid,Ty.App(opt_con,tid))::ps
      end
    fun prims tinfos = List.foldl addPrim [] tinfos
      
    fun get_reps me Semant.Sequence =
      {mktid=seq_tid,mkrep=seq_rep,con=seq_con}
      | get_reps me Semant.Option =
      {mktid=opt_tid,mkrep=opt_rep,con=opt_con}
      | get_reps me Semant.Shared = 
      {mktid=share_tid,mkrep=share_rep,con=share_con}
      
    fun get_info p =
      let
	val rd =
	  case (Semant.Type.P.reader p) of
	    (SOME x) => SOME (Call(Id(VarId.fromPath x),[Id stream_id]))
	  | NONE => NONE
	val wr =
	  case (Semant.Type.P.writer p) of
	    (SOME x) =>
	      SOME (fn e => Call(Id(VarId.fromPath x),[e,Id stream_id]))
	  | NONE => NONE
      in mk_info {wr=wr,rd=rd}
      end
    
    structure S = Semant
    fun get_wrappers ty p =
      let
	val ty =
	  case (S.Type.P.natural_type p,S.Type.P.natural_type_con p) of
	    (SOME t,_) =>  TyId (TypeId.fromPath t)
	  | (NONE,SOME t) => (TyCon (TypeId.fromPath t,[ty]))
	  | _ => ty
	val unwrap =
	  case (S.Type.P.unwrapper p) of
	    (SOME x) =>
	      (fn e =>
	       Call(Id(VarId.fromPath x),[e]))
	  | NONE => (fn x => x)
	val wrap =
	  case (S.Type.P.wrapper p) of
	    (SOME y) =>
	      (fn x => Call(Id(VarId.fromPath y),[x]))
	  | NONE => (fn x => x)
      in {natural_ty=ty,unwrap=unwrap,wrap=wrap}
      end
  end


