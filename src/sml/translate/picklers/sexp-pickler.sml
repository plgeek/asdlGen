(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
functor SexpPickler (structure Arg : SEXP_PICKLER_ARG
		     val tag : string) : AUX_DECLS =
  struct

    structure Ty = Arg.Ty
    type decl = Arg.decl
    val decl_read = Arg.decl_read Arg.rd_name 
    val decl_write = Arg.decl_write Arg.wr_name 

    val call_read = Arg.call_read Arg.rd_name 
    val call_write = Arg.call_write Arg.wr_name 
    fun trans env tids =
      let
	fun get_ty tid =
	  (case Ty.lookup(env,tid) of
	    SOME ty => (tid,ty)
	  | NONE => raise Error.internal)

	fun defaultOrElse (i:Ty.ty_info) f x =
	  Option.getOpt ((f i),x)
	fun rd_decl (ty_id,Ty.Prim {ty,info,...}) =
 	  decl_read{name=ty_id,ret=ty,body=Option.valOf (Ty.getRd tag info)}
	  | rd_decl (ty_id,Ty.Prod{ty,fields,cnstr,info,match}) =
	    let val body = defaultOrElse info (Ty.getRd tag)
	      (Arg.read_prod ty ty_id (cnstr (List.map rd_field fields)))
	    in decl_read{name=ty_id,ret=ty,body=body}
	    end
	  | rd_decl (ty_id,Ty.Sum{ty,cnstrs,match,info,...}) =
	    let val body = defaultOrElse info (Ty.getRd tag)
	      (Arg.read_sum ty (List.map rd_con cnstrs))
	    in decl_read{name=ty_id,ret=ty,body=body}
	    end
	  | rd_decl (ty_id,Ty.App(f,arg)) =
	    let
	      val (ty,info) = f (get_ty arg)
	      val rd = Option.valOf (Ty.getRd tag info)
	    in decl_read{name=ty_id,ret=ty,body=rd}
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
	  | SOME (Ty.Prim{info,...}) => Option.valOf (Ty.getRd tag info)
	  |  _ => call_read tid

	fun wr_decl (ty_id,Ty.Prim {ty,info,...}) =
 	  decl_write{name=ty_id,arg=ty,body=Option.valOf (Ty.getWr tag info)}
	  | wr_decl (ty_id,Ty.Prod{ty,fields,cnstr,match,info}) =
	    let val body = defaultOrElse info (Ty.getWr tag)
	     (match (Arg.write_prod ty ty_id o (List.map wr_match)))
	    in decl_write{name=ty_id,arg=ty,body=body}
	    end
	  | wr_decl (ty_id,Ty.Sum{ty,cnstrs,match,info,...}) =
	    let val body = defaultOrElse info (Ty.getWr tag)
	      (match (wr_con ty))
	    in  decl_write{name=ty_id,arg=ty,body=body}
	    end
	  | wr_decl (ty_id,Ty.App(f,arg)) =
	    let
	      val (ty,info) = f (get_ty arg)
	      val wr = Option.valOf (Ty.getWr tag info)
	    in decl_write{name=ty_id,arg=ty,body=wr}
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
	      (Option.valOf ((Ty.getWr tag info))) exp
	  |  _ => call_write tid exp
	and wr_con ty (tag,matches) =
	  Arg.write_sum ty
	  (tag,(List.map wr_match matches))

	val rds = List.map rd_decl tids
	val wrs = List.map wr_decl tids
      in
	rds @ wrs
      end
  end

