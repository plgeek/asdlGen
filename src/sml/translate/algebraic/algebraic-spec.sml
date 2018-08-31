(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**:[[structure AlgebraicTy]]:**)
structure AlgebraicTy : ALGEBRAIC_TYPE_DECL =
  struct
    structure Ast = AlgebraicAst
    structure T =
      mkTypeDecl(structure TypeId = Ast.TypeId
		 structure VarId = Ast.VarId
		 type ty_exp = Ast.ty_exp
		 type exp = Ast.exp
		 type tag = {c:Ast.cnstr,v:int}) 
    open T
  end
(**)
(**:[[functor mkAlgebraicSpec]]:**)
functor mkAlgebraicSpec(structure Ty    : ALGEBRAIC_TYPE_DECL
			val get_attribs : bool
			val prim_ty     : {intT:string,stringT:string}
			val streams_ty  : {outs:string,ins:string} option
			val monad_cons  : {inm:string,outm:string} option
			val ignore_labels : bool) : ALGEBRAIC_SPEC =
  struct
    val aux_suffix = "Util"
    val ignore_labels = ignore_labels
    
    val seq_rep = Ty.Ast.TyList
    val opt_rep = Ty.Ast.TyOption

    val share_id = Ty.Ast.TypeId.fromPath{qualifier=["Share"],base="share"}
    fun share_rep te = Ty.Ast.TyCon (share_id,[te])

    fun ty_exp  (Ty.Prim {ty,...}) = ty
      | ty_exp  (Ty.Prod {ty,...}) = ty
      | ty_exp  (Ty.Sum {ty,...}) = ty
      | ty_exp  (_) = raise Error.unimplemented
    val seq_tid =  Ty.Ast.TypeId.suffixBase "_list" 
    val opt_tid =  Ty.Ast.TypeId.suffixBase "_option" 
    val share_tid =  Ty.Ast.TypeId.suffixBase "_share" 
    val stream_id  = Ty.Ast.VarId.fromString "s_"

    functor mkCommonPkl(val prefix : string)  =
      struct
	open Ty.Ast
	type decl = Ty.Ast.decl
	type rd_name = Ty.Ast.ty_id -> Ty.Ast.id
	type wr_name = Ty.Ast.ty_id -> Ty.Ast.id
	structure Ty = Ty
	val streams_ty = Option.getOpt
	  (streams_ty,{ins="instream",outs="outstream"})
	  
	val outstream_ty = TyId 
	      (TypeId.fromPath {qualifier=[prefix],
				base=(#outs streams_ty)})
	val instream_ty = TyId 
	  (TypeId.fromPath {qualifier=[prefix],
			    base=(#ins streams_ty)})

	val string_ty = TyId 
	  (TypeId.fromPath {qualifier=[],
			    base=(#stringT prim_ty)})

	val int_ty = TyId 
	  (TypeId.fromPath {qualifier=[],
			    base=(#intT prim_ty)})
	  
	val monad =
	  Option.map (fn {inm,outm} =>
		      {inm=TypeId.fromPath {qualifier=[prefix],base=inm},
		       outm=TypeId.fromPath {qualifier=[prefix],base=outm}})
		       monad_cons
	  
	val arg_id     = VarId.fromString "x_"
	val stream_id  = VarId.fromString "s_"
	val tmp_id  = VarId.fromString "tmp_"
	val unit_ty = (TyTuple [])
	  
	fun mk_name s id =
	  let
	    val {qualifier,base} = TypeId.toPath id
	    val base = s^"_"^base
	    val qualifier =
	      case qualifier of
		["<toplevel>"] => [aux_suffix]
	      |	[q] => [q^aux_suffix]
	      | _ => qualifier
	  in VarId.fromPath{base=base,qualifier=qualifier}
	  end
	
	fun bind g te (Id x) f = (f x)
	  | bind g te e f =
	  let val x = VarId.tempId "t"
	  in g([(MatchId (x,te),e)],f x)
	  end

	val wrapin = case monad of
	  NONE => (fn x => x)
	| (SOME {inm,...}) => (fn x => TyCon(inm,[x]))
	val wrapout = case monad of
	  NONE => (fn x => x)
	| (SOME {outm,...}) => (fn x => TyCon(outm,[x]))

	fun pkl_lib s = VarId.fromPath{qualifier=[prefix],base=s}
	fun call id args = Call(Id id,args)
	fun call_read f tid = call (f tid) [Id stream_id]
	fun call_write f tid e = call (f tid) [e,Id stream_id]
	fun decl_write f {name,arg,body} =
	  DeclFun(f name,[{name=arg_id,ty=arg},
			  {name=stream_id,ty=outstream_ty}],
		  body (Id arg_id),wrapout unit_ty)
	fun decl_read f {name,ret,body} =
	  DeclFun(f name,[{name=stream_id,ty=instream_ty}],body,wrapin ret)
	fun die s = Error(pkl_lib "IOError",s)
      end
(**:[[functor mkAlgebraicSpec]] [[structure Arg]]:**)
    structure StdPklArg =
      struct
	structure Common = mkCommonPkl(val prefix = "StdPkl")
	open Common
	type get_ty = (Ty.ty_id -> Ty.ty_exp option)
	fun mk_info x = Ty.addRdWr "std" x Ty.noInfo
	val rd_name = mk_name "read"
	val wr_name = mk_name "write"
	  
	val wr_tag_name = pkl_lib "write_tag"
	val rd_tag_name = pkl_lib "read_tag"

	val rd_share_name =  pkl_lib "read_share"
	val wr_share_name =  pkl_lib "write_share"

	val rd_list_name = pkl_lib "read_list"
	val wr_list_name = pkl_lib "write_list"

	val rd_option_name =  pkl_lib "read_option"
	val wr_option_name =  pkl_lib "write_option"
	  
	fun write_tag {c,v} = call wr_tag_name [Int v,Id stream_id]
	fun read_tag cs =
	  let fun mk_clause ({c,v},exp) =  (MatchInt v,exp)
	  in bind LetBind int_ty
	    (call rd_tag_name [Id stream_id])
	    (fn x =>
	     Match(x,(List.map mk_clause cs)@[(MatchAny,die "bad tag")]))
	  end
	val read = call_read rd_name
	val write = call_write wr_name
	val write_decl  = decl_write wr_name 
	val read_decl = decl_read rd_name
	val expSeq = Seq
	fun getter_decl {name,arg,ret,body} =
	  DeclFun(mk_name "attrbs" name,[{name=arg_id,ty=arg}],
		  body (Id arg_id),ret)
	fun mk_fields get_ty xs  =
	  List.map (fn {label,label',tid} =>
		    {name=label',
		     ty=(case (get_ty tid) of
			   SOME ty => ty
			 | NONE => TyId tid)}) xs
	fun mk_record_exp get_ty xs =
	  let val (fds,exps) = ListPair.unzip xs
	  in Record (exps,mk_fields get_ty fds,NONE)
	  end
	fun mk_record_typ get_ty fds = TyRecord (mk_fields get_ty fds)
	fun seq_info (tid,t) =
	  let val rd = Call(Id rd_list_name,
			    [Id (rd_name tid),Id stream_id])
	    fun wr e =
	      Call(Id wr_list_name,
		   [Id (wr_name tid),e,Id stream_id])
	  in mk_info{wr=SOME wr,rd=SOME rd}
	  end
	fun opt_info (tid,t) =
	  let
	    val rd = Call(Id rd_option_name,
			  [Id (rd_name tid),Id stream_id])
	    fun wr e = Call(Id wr_option_name,
			    [Id (wr_name tid),e,Id stream_id])
	  in mk_info{wr=SOME wr,rd=SOME rd}
	  end
        fun share_info (tid,t) =
	  let
	    val rd = Call(Id rd_share_name,
			  [Id (rd_name tid),Id stream_id])
	    fun wr e =
	      Call(Id wr_share_name,
		   [Id (wr_name tid),e,Id stream_id])
	  in mk_info{wr=SOME wr,rd=SOME rd}
	  end
      end
    structure SexpPklArg =
      struct
	structure Common = mkCommonPkl(val prefix = "SexpPkl")
	open Common
	fun mk_info x = Ty.addRdWr "sexp" x Ty.noInfo
	val rd_name = mk_name "sexp_rd"
	val wr_name = mk_name "sexp_wr"

	val rd_share_name =  pkl_lib "rd_share"
	val wr_share_name =  pkl_lib "wr_share"

	val rd_list_name = pkl_lib "rd_list"
	val wr_list_name = pkl_lib "wr_list"

	val rd_option_name =  pkl_lib "rd_option"
	val wr_option_name =  pkl_lib "wr_option"

	val wr_grp_name =  pkl_lib "wr_grp"
	val rd_grp_name =  pkl_lib "rd_grp"
	fun name {c={name,ty_arg},v} = VarId.toString' "_" name
	fun tname id = TypeId.toString' "_" id
	fun write_prod ty tid es =
	  Seq [call (pkl_lib "wr_lp") [Id stream_id],
	       call (pkl_lib "wr_sym") [Str (tname tid),Id stream_id],
	       Seq es,
	       call (pkl_lib "wr_rp") [Id stream_id]]
	fun read_prod ty tid e =
	  Seq [call (pkl_lib "rd_lp") [Id stream_id],
	       call (pkl_lib "rd_sym") [(Str (tname tid)),Id stream_id],
	       LetBind([(MatchId (tmp_id,ty),e)],
		     (Seq [call (pkl_lib "rd_rp") [Id stream_id],
			     Id tmp_id]))]
	fun write_sum ty (tag,es) =
	  Seq [call (pkl_lib  "wr_lp") [Id stream_id],
	       call (pkl_lib  "wr_sym")  [Str (name tag),Id stream_id],
	       Seq es,
	       call (pkl_lib  "wr_rp") [Id stream_id]]
	fun read_sum ty cs =
	  let
	    fun mk_clause (s,exp) = (MatchStr (name s),exp)
	    val e =
	      bind LetBind string_ty (call (pkl_lib "get_sym") [Id stream_id])
	      (fn x =>
	       Match(x,(List.map mk_clause cs)@[(MatchAny,die "bad tag")]))
	  in  
	    Seq [call (pkl_lib "rd_lp") [Id stream_id],
		 LetBind([(MatchId (tmp_id,ty),e)],
			 (Seq [call (pkl_lib "rd_rp") [Id stream_id],
			       Id tmp_id]))]
	  end
	fun seq_info (tid,t) =
	  let
	    val rd = call rd_list_name  [Id (rd_name tid),Id stream_id]
	    fun wr e = call wr_list_name [Id (wr_name tid),e,Id stream_id]
	  in mk_info {wr=SOME wr,rd=SOME rd}
	  end

	fun opt_info (tid,t) =
	  let
	    val rd = call rd_option_name [Id (rd_name tid),Id stream_id]
	    fun wr e = call wr_option_name [Id (wr_name tid),e,Id stream_id]
	  in mk_info {wr=SOME wr,rd=SOME rd}
	  end

        fun share_info (tid,t) =
	  let
	    val rd = call rd_share_name [Id (rd_name tid),Id stream_id]
	    fun wr e = call wr_share_name [Id (wr_name tid),e,Id stream_id]
	  in mk_info {wr=SOME wr,rd=SOME rd}
	  end
      end
    (**)
(**:[[functor mkAlgebraicSpec]] glue code to build utility code:
The [[structure Arg]] describes an interface from which one can
automatically construct both pickler generators and attribute accessors
functions.	
**)
    open Ty.Ast
    fun mk_con rep f1 f2 (tid,t) =
      (rep (ty_exp t),Ty.merge(f1 (tid,t),f2 (tid,t)))
    val seq_con:Ty.ty_con =
      mk_con seq_rep SexpPklArg.seq_info StdPklArg.seq_info
    val opt_con:Ty.ty_con =
      mk_con opt_rep SexpPklArg.opt_info StdPklArg.opt_info
    val share_con:Ty.ty_con =
      mk_con share_rep SexpPklArg.share_info StdPklArg.share_info

    structure SexpPklGen =
      SexpPickler(structure Arg = SexpPklArg
		  val tag = "sexp")
    structure StdPklGen =
      StdPickler(structure Arg = StdPklArg
		 val tag = "std")
    structure AttribGetGen = AttribGetter(structure Arg = StdPklArg)
    structure Ty = Ty
    val inits = []

    fun pkl_choose me sexp std =
        case (Semant.MEnv.P.pickler_kind me) of ("sexp") => sexp
	  | _ => std

    fun get_aux_decls me env tids =
      let
	val pkls =
	  case (Semant.MEnv.P.pickler_kind me) of
	    ("sexp") => SexpPklGen.trans env tids
	  | ("std") =>  StdPklGen.trans env tids
	  | ("std,sexp") =>
	      (StdPklGen.trans env tids) @ (SexpPklGen.trans env tids) 
	  | ("sexp,std") =>
	      (StdPklGen.trans env tids) @ (SexpPklGen.trans env tids) 
	  | ("empty") =>  []
	  | _ =>  (Error.warn ["Unknown pickler generating nothing"];[])
	val attrbs =
	  if get_attribs then  AttribGetGen.trans env tids
	  else []
      in attrbs@pkls
      end
(**)
(**:[[functor mkAlgebraicSpec]] definition of type constructor:**)

(**)
(**:[[functor mkAlgebraicSpec]] definition of primitive types:**)
    fun addPrim me (tinfo,ps) =
      let
	val tname = Semant.Type.src_name tinfo
	val tid = (TypeId.fromPath o Semant.Type.Id.toPath) tname
	val info =
	  List.foldr Ty.merge Ty.noInfo
	  [SexpPklArg.mk_info
	   {rd=SOME(SexpPklArg.call_read SexpPklArg.rd_name tid),
	    wr=SOME(SexpPklArg.call_write SexpPklArg.wr_name tid)},
	   StdPklArg.mk_info
	   {rd=SOME(StdPklArg.read tid),
	    wr=SOME(StdPklArg.write tid)}] 

      in (tid,Ty.Prim {ty=TyId tid,info=info,
		       name=Semant.Type.Id.getBase tname})::
	(seq_tid tid,Ty.App(seq_con,tid))::
	(opt_tid tid,Ty.App(opt_con,tid))::ps
      end
    fun prims me tinfos = List.foldl (addPrim me) [] tinfos
      
    fun get_reps me Semant.Sequence =
      {mktid=seq_tid,mkrep=seq_rep,con=seq_con}
      | get_reps me Semant.Option =
      {mktid=opt_tid,mkrep=opt_rep,con=opt_con}
      | get_reps me Semant.Shared = 
      {mktid=share_tid,mkrep=share_rep,con=share_con}

(**)
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
      in StdPklArg.mk_info {wr=wr,rd=rd}
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
(**)

