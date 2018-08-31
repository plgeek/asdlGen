(* 
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
functor mkDynamicSemantTranslator
  (structure Spec   : DYNAMIC_SPEC): SEMANT_TRANSLATOR  =
     struct

      structure S = Semant
      structure Ty = Spec.Ty
      structure Ast = Spec.Ty.Ast
      structure T = Ast
      structure IdCvt =
	mkIdCvt(structure Ast = Ast
		structure IdMap = IdMaps.Empty)
      open IdCvt
	
      val set_dir = true
      val ignore_supress = false
      val fix_fields = false
      val inits = Spec.inits
      type defined_value  = {ty_decl:Ty.ty_decl,decls:T.decl list}
      type con_value      = {con:Ty.con,
		       mk_choice:Ty.exp -> Ty.choice,
			    decl:T.decl,
			      ty:T.ty_id}
      type field_value    = {fd:T.field,ty_fd:Ty.field}
	
      type type_con_value = Ty.ty_decl list
      type module_value   = Ty.ty_decl list * (T.module * S.Module.P.props)
      type output         = (T.module * S.Module.P.props) list

      fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	let
	  val tid = trans t2t tname
	  val ty = (T.TyId tid)
	  val {natural_ty,...} = Spec.get_wrappers ty props 
	  val (ty,tid) =
	    case kind of
	      NONE => (natural_ty,tid)
	    | SOME k =>
		let val {mkrep,mktid,...} = Spec.get_reps p k
		in (mkrep natural_ty,mktid tid)
		end
	  val name = trans f2v name
	  val fd = {name=name,ty=ty}
	in {fd=fd,ty_fd={label=SOME name,label'=name,tid=tid}}
	end

      fun trans_fields tid (fields:field_value list) =
	let
	  val fds = List.map #fd (fields:field_value list)
	  fun f2m x ({fd,ty_fd}) = (ty_fd,T.GetField(x,fd))
	  val bvars = List.map #name fds
	  fun mk_binds es =
	    List.map (fn (x,e) => {name=x,v=e}) (ListPair.zip (bvars,es))
	  fun mk_match e = (List.map (f2m e) fields)
	  fun cnstr x = 
	    (T.Bind{binds=mk_binds x,
		  body=T.MakeStruct(tid,List.map T.Id bvars,fds)})
	in {cnstr=cnstr,mk_match=mk_match}
	end

      fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} =
	let
	  val tname = trans t2t (S.Type.name tinfo)
	  val name = trans c2t name
	  val tag_v = S.Con.tag cinfo
	  val fields = (attrbs@fields)
	  val {mk_match,cnstr} = trans_fields name fields
	  val tag = {c=name,v=tag_v}
	  val con = {tag=tag,fields=List.map #ty_fd fields,
		   cnstr=cnstr}
	in {con=con,mk_choice=(fn e => (tag,mk_match e)),ty=name,
	    decl=T.DeclStruct(name,List.map #fd fields,T.TyId tname)}
	end

      fun trans_defined p {tinfo,name,fields,cons=[],props} =
	let
	  val name = trans t2t name
	  val {cnstr,mk_match}= trans_fields name fields
	  val {natural_ty,unwrap,wrap,...} =
	    Spec.get_wrappers (T.TyId name) props 
	  val info = Spec.get_info props
	  fun match f e =
	    let val temp_var = T.VarId.tempId "t"
	    in T.Bind{binds=[{name=temp_var,v=unwrap e}],
		      body=f (mk_match (T.Id temp_var))}
	    end
	  val product =
	    {ty=natural_ty,
	     fields=List.map #ty_fd fields,
	     info=info,
	     match=match,
	     cnstr=(fn x => (wrap (cnstr x)))}
	in {decls=[T.DeclStruct(name,List.map #fd fields,T.TyId name)],
	    ty_decl=(name,Ty.Prod product)}
	end
	| trans_defined p {tinfo,name,fields,cons,props} =
	let
	  val name = trans t2t name
	  val ty = (T.TyId name)
	  fun mk_clause e f {con,mk_choice,ty,decl} =
	    {const=T.TypeName ty,body=f (mk_choice e)}
	  val {natural_ty,unwrap,wrap,...} = Spec.get_wrappers ty props 
	  val info = Spec.get_info props
	    
	  fun match f e =
	    let val temp_var = T.VarId.tempId "t"
	    in T.Bind{binds=[{name=temp_var,v=unwrap e}],
		      body=
		      T.Case{test=T.GetStructType(T.Id temp_var),
			     clauses=List.map
			     (mk_clause (T.Id temp_var) f) cons,
			     default=(T.Error "wrong type")}}
	    end
	  fun mk_cnstr {tag,fields,cnstr} =
	    {tag=tag,fields=fields,cnstr=wrap o cnstr}
	  val cnstrs =  List.map (mk_cnstr o #con) cons
	  val decls = List.map #decl cons
	in
	{decls=decls,
	 ty_decl=(name,Ty.Sum{ty=natural_ty,info=info,
			      num_attrbs=List.length fields,
			      cnstrs=cnstrs,match=match})}
	end
      fun trans_type_con p {tinfo,name,props,kinds}  =
	let
	  val tid = trans t2t name
	  fun do_kind k = 
	    let val {mktid,con,...} = Spec.get_reps p k
	    in (mktid tid,Ty.App(con,tid))
	    end
	  val ty_decls = List.map do_kind kinds
	in ty_decls
	end
      fun trans_module p {module,defines,type_cons,imports,props} =
	let
	  fun merge ({ty_decl,decls},(ty_decls,decls')) =
	    (ty_decl::ty_decls,decls@decls')
	  val ty_cons = List.foldr (op @) [] type_cons 
	  val (ty_decls,decls) =
	    List.foldr merge (ty_cons,[]) defines
	  val toMid = (trans m2m) o Semant.Module.src_name
	in
	  (ty_decls,(T.Module{name=toMid module,
			     imports=List.map toMid imports,
			     decls=decls},props))
	end
      fun trans p {modules,prim_types,prim_modules} =
	let
	  val ms = modules
	  val toMid = Ast.ModuleId.fromPath o
	    S.Module.Id.toPath o S.Module.src_name
	  val prim_mods = List.map toMid prim_modules
	  val ty_decls = List.foldl (fn ((x,_),xs) => x@xs)
	    (Spec.prims prim_types) (ms:module_value list)
(* Call the Spec.aux_decls to generate pickler code as well as other useful
   functions *)
	  val new_decls = (Spec.get_aux_decls p (Ty.mk_env ty_decls))
	  fun mk_mods (ty_decls,(T.Module{name,imports,decls},mp)) =
	    (T.Module{name=name,
		     imports=prim_mods@imports,
		     decls=decls@(new_decls ty_decls)},mp)
	  val out = (List.map mk_mods ms)
	in List.filter (not o S.Module.P.suppress o #2) out
	end
    end


