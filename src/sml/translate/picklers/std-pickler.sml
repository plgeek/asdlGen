(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
 The [[StdPickler]] functor builds a pickler generator, which is
 a function that maps a type decription into declarations that
 read and write each type. The functor is parameterized so that it is
 independent of any particular language.
**)
functor StdPickler (structure Arg : STD_PICKLER_ARG
		    val tag : string) : AUX_DECLS =
  struct
(**:[[functor StdPickler]]:
The only exported takes a type envrionment and a list of type 
identifiers bound in that environment and returns a list of
declarations that are the readers and writers for those bound types.
**)
    structure Ty = Arg.Ty
    type decl = Arg.decl

    fun trans env tids =
(**:[[functor StdPickler]] [[fun trans]]:useful functions
**)
      let
	fun get_ty tid =
	  (case Ty.lookup(env,tid) of
	    SOME ty => (tid,ty)
	  | NONE => raise Error.internal)
	fun get_rd info =
	  (case (Ty.getRd tag info) of
	     NONE => raise (Error.error ["Can't find primitve reader"])
	   | SOME rd => rd)
	fun defaultOrElse (i:Ty.ty_info) f x =
	  Option.getOpt ((f i),x)
(**:[[functor StdPickler]] [[fun trans]]:Generate readers
**)
	fun rd_decl (ty_id,Ty.Prim {ty,info,...}) =
	  Arg.read_decl{name=ty_id,ret=ty,body=get_rd info}
	  | rd_decl (ty_id,Ty.Prod{ty,fields,cnstr,info,match}) =
	    let val body = defaultOrElse info (Ty.getRd tag)
	      (cnstr (List.map rd_field fields)) 
	    in Arg.read_decl{name=ty_id,ret=ty,body=body}
	    end
	  | rd_decl (ty_id,Ty.Sum{ty,cnstrs,match,info,...}) =
	    let val body = defaultOrElse info (Ty.getRd tag)
	      (Arg.read_tag (List.map rd_con cnstrs))
	    in Arg.read_decl{name=ty_id,ret=ty,body=body}
	    end
	  | rd_decl (ty_id,Ty.App(f,arg)) =
	    let
	      val (ty,info) = f (get_ty arg)
	      val rd = Option.valOf (Ty.getRd tag info)
	    in Arg.read_decl{name=ty_id,ret=ty,body=rd}
	    end
	  | rd_decl (ty_id,Ty.Alias(ty_id')) =
	    let val (_,ty) =  get_ty ty_id'
	    in rd_decl(ty_id,ty)
	    end
	and rd_con {tag,fields,cnstr} =
	  (tag,cnstr (List.map rd_field fields))

	and rd_field {tid,...} =
	  case Ty.lookup(env,tid) of
	    SOME (Ty.App(f,ty)) =>
	      (Option.valOf o (Ty.getRd tag) o #2 o f o get_ty) ty
	  | SOME (Ty.Prim{info,...}) =>
	      (Option.valOf (Ty.getRd tag info))
	  |  _ => Arg.read tid
(**:[[functor StdPickler]] [[fun trans]]:Generate writers
**)		 
	fun wr_decl (ty_id,Ty.Prim {ty,info,...}) =
 	  Arg.write_decl{name=ty_id,arg=ty,
			 body=Option.valOf (Ty.getWr tag info)}
	  | wr_decl (ty_id,Ty.Prod{ty,fields,cnstr,match,info}) =
	    let val body = defaultOrElse info (Ty.getWr tag)
	      (match (Arg.expSeq o (List.map wr_match)))
	    in Arg.write_decl{name=ty_id,arg=ty,body=body}
	    end
	  | wr_decl (ty_id,Ty.Sum{ty,cnstrs,match,info,...}) =
	    let val body = defaultOrElse info (Ty.getWr tag) (match wr_con)
	    in  Arg.write_decl{name=ty_id,arg=ty,body=body}
	    end
	  | wr_decl (ty_id,Ty.App(f,arg)) =
	    let
	      val (ty,info) = f (get_ty arg)
	      val wr = Option.valOf (Ty.getWr tag info)
	    in Arg.write_decl{name=ty_id,arg=ty,body=wr}
	    end
	  | wr_decl (ty_id,Ty.Alias(ty_id')) =
	    let val (_,ty) =  get_ty ty_id'
	    in wr_decl(ty_id,ty)
	    end
	and wr_match ({tid,...},exp) =
	  case Ty.lookup(env,tid) of
	    SOME (Ty.App(f,ty)) =>
	      ((Option.valOf o (Ty.getWr tag) o #2 o f o get_ty) ty) exp
	  | SOME (Ty.Prim{info,...}) =>
	      (Option.valOf (Ty.getWr tag info)) exp
	  |  _ => Arg.write tid exp
	and wr_con (tag,matches) =
	  Arg.expSeq ((Arg.write_tag tag)::(List.map wr_match matches))
(**:[[functor StdPickler]] [[fun trans]]:
Apply the readers and writers to bound types.
**)
	val rds = List.map rd_decl tids
	val wrs = List.map wr_decl tids
      in
	rds @ wrs
      end
(**)    
(**)    
  end
