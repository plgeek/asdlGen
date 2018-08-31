
functor mkOOBuildAux(structure T : OO_AST
		     val mk_tid : (T.mod_id * string) -> T.ty_id
		     val visit_id : T.ty_id -> T.id) =
    struct
	open T
	datatype aux_info =
	    Class of {name:ty_id,inherits:ty_id option,
		      vars:{name:id,tid:ty_id} list,abstract:bool}
	  | Const of {name:ty_id,inherits:ty_id option}

	    
	type aux_fn =
	    (aux_info * ((ty_id * mth) list)) ->  (ty_id * mth) list

	fun pub_fields x =
	    let
		fun get_pub ({mods={scope=Public,...},field}:mfield) =
		    (case field of
			 {name,ty=(TyReference(TyId tid))} =>
			     SOME {name=name,tid=tid}
		       | {name,ty=TyId tid} =>
			     SOME {name=name,tid=tid} 
		       | _ => NONE)
		  | get_pub _ = NONE
	    in
		List.mapPartial get_pub x
	    end

	fun compose_fn (f:aux_fn) (g:aux_fn) (info,rest) =
		f(info,g(info,rest))

	structure Env =
	    SplayMapFn(struct
			   type ord_key = T.TypeId.id
			   val compare = T.TypeId.compare
		       end)

	val totid = (T.TypeId.fromPath o T.VarId.toPath)
	fun do_decls f (DeclAbstractClass{name,scope=Public,
					    inherits,fields,...},rest) =
	    f (Class{name=name,
		     inherits=inherits,
		     vars=pub_fields fields,abstract=true},rest)
	  | do_decls f (DeclClass{name,scope=Public,
				inherits,fields,...},rest) =
	    f (Class{name=name,
		inherits=inherits,
		vars=pub_fields fields,abstract=false},rest)

	  | do_decls f (DeclConst{public=true,
				  field=
				  {name,
				   ty=T.TyReference (T.TyId x)},...}, rest) =
	    f (Const{name=totid name,inherits=SOME x},rest)
	  | do_decls f (DeclConst{public=true,
				  field={name,ty},...}, rest) =
	    f (Const{name=totid name,inherits=NONE},rest)
	  | do_decls _ (_,rest) = rest
	fun fold_decls f b d = List.foldl (do_decls f) b d

	fun fold_decls_inherit f b d =
	    let
		fun mk_field_map (Class{name,vars,inherits,...},env) =
		    Env.insert(env,name,(inherits,vars))
		  | mk_field_map (_,env) = env
		val field_map = fold_decls mk_field_map Env.empty d
		fun map_info (Class{name,inherits,vars,abstract},r) =
		    let
			fun lookup c =
			    case (Env.find(field_map,c)) of
				NONE => []
			      | SOME(NONE,vars) => vars
			      | SOME(SOME c',vars) => ((lookup c')@vars)
		    in
			(Class{name=name,inherits=inherits,abstract=abstract,
			       vars=lookup name},r)
		    end
		  | map_info x = x
	    in
		fold_decls (f o map_info) b d
	    end

	(* visitor code *)
	val void_ty  = T.TyVoid
	val visit_arg  =  (T.VarId.fromString "x")
	val visit_def_mth  =  (T.VarId.fromString "visit")

	val accept_visitor  =  (T.VarId.fromString "v")
	val accept_id =  T.VarId.fromString "accept"

	fun guard_null id stmt =
	    If{test=T.NotNil (Id id),then_stmt=stmt,else_stmt=Nop}

	fun mk_vmth name tid body =
	    T.Mth {name=name,
		   inline=false,
		   body={vars=[],body=body},
		   mods={scope=T.Public,static=false, final=false},
		   args=[{name=visit_arg,
			  ty=T.TyReference(T.TyId tid)}],ret=void_ty}

	fun mk_apply tid vid =
	    [guard_null vid (T.Expr (T.MthCall(T.Id (visit_id tid),
					       [T.Id vid])))]
	    

	fun mk_visit name tid NONE = mk_vmth name tid
	    [T.Expr (T.MthCall(T.Id visit_def_mth, []))]
	  | mk_visit name tid (SOME super) = mk_vmth name tid
	    (mk_apply (super) visit_arg)

	
	fun visit_aux visit_tid  (Class{name,inherits,vars,
					abstract=true,...},rest) =
	    (visit_tid,mk_visit (visit_id name) name NONE)::rest
	  | visit_aux visit_tid (Class{name,inherits,vars,...},rest) =
	    (visit_tid,mk_visit (visit_id name) name inherits)::rest
	  | visit_aux _ (_,rest) = rest

	val v_default_mth =
	    T.Mth {name=visit_def_mth,
		   inline=false,
		   body={vars=[],body=[T.Nop]},
		   mods={scope=T.Public,static=false, final=false},
		   args=[],ret=void_ty}
	fun mk_visit_class visit_tid =
	    T.DeclAbstractClass
	    {name=visit_tid,idecls=[],
	     scope=T.Public,inherits=NONE,
	     fields=[],
	     mths=[v_default_mth]}

	(* walker  code *)
	val walker_visitor_pre   =  (T.VarId.fromString "pre")
	val walker_visitor_post  =  (T.VarId.fromString "post")

	fun same_qualifier (x,y) =
	    (TypeId.getQualifier x) =
	    (TypeId.getQualifier y)

	fun call_v id =
	    guard_null id
	    (T.Expr
	    (T.MthCall(T.FieldSub(T.DeRef(T.Id visit_arg),
				  accept_id),[T.Id id])))
	fun mk_walk_fields id fs =
	    let
		fun get_f f =
		    T.FieldSub
		    (T.DeRef
		     (T.FieldSub(T.DeRef(T.Id visit_arg),f)),accept_id)
		    
		fun apply ({name,tid},rest) =
		    if (same_qualifier (id,tid)) then 
			(T.Expr
			  (T.MthCall(get_f name,[T.This])))::rest
		    else rest
	    in
		List.foldr apply [] fs
	    end
	    
	fun wrap_body body =
	    [guard_null visit_arg
	    (Block{vars=[],
		   body=[call_v walker_visitor_pre]@body@
		    [call_v walker_visitor_post]})]

	fun mk_walk name tid NONE fs =
	    mk_vmth name tid (wrap_body (mk_walk_fields tid fs))
	  | mk_walk name tid (SOME super) [] =
		    mk_vmth name tid (mk_apply super visit_arg)
	  | mk_walk name tid (SOME super) fs =
		    mk_vmth name tid
		    ((mk_apply super visit_arg)@
		    (wrap_body (mk_walk_fields tid fs)))


	fun walk_aux walk_tid (Class{name,inherits,vars,...},rest) =
	    (walk_tid,mk_walk (visit_id name) name NONE vars)::rest
	  | walk_aux walk_tid (Const{name,inherits=SOME tid},rest) =
	    (walk_tid,mk_walk (visit_id name) tid (SOME tid) [])::rest
	  | walk_aux _ (_,rest) = rest

	fun mk_walker_class walk_tid visit_tid =
	    let
		val visit_ty = T.TyReference(T.TyId visit_tid)
		val cnstrs =
		    [{inline=false,
		      scope=T.Public,
		      args=[{name=walker_visitor_pre,  ty=visit_ty},
			    {name=walker_visitor_post, ty=visit_ty}],
		      body={vars=[],
			    body=
			    [T.Assign(T.ThisId walker_visitor_pre,
				      T.Id walker_visitor_pre),
			     T.Assign(T.ThisId walker_visitor_post,
				      T.Id walker_visitor_post)
			     ]}}]
		val fields =
		    [{mods={scope=T.Public,static=false,final=false},
		      field={name=walker_visitor_pre,ty=visit_ty}},
		     {mods={scope=T.Public,static=false,final=false},
		      field={name=walker_visitor_post,ty=visit_ty}}]
	    in
		T.DeclClass
		{name=walk_tid,
		 idecls=[],cnstrs=cnstrs,
		 final=false,scope=T.Public,
		 inherits=SOME visit_tid,fields=fields,
		 mths=[]}
	    end

	(* copy methods *)
	val shallow_copy_id = T.VarId.fromString "shallow_copy"
	val deep_copy_id = T.VarId.fromString "deep_copy"
	val copy_temp_id = T.VarId.fromString "t"

	fun mk_copy (Class{name,inherits,vars,abstract=false}) =
	    let
		val id = name
		val ret_ty =
		    case inherits of
			NONE => (T.TyReference (T.TyId name))
		      | (SOME i) => (T.TyReference (T.TyId i))
		val my_ty = (T.TyReference (T.TyId name));

		val args = []
		fun call_mth id x =
		    T.MthCall(T.FieldSub(T.DeRef x, id),[])		    
		    
		fun mk_shallow_copy_vars {name,tid} =
		    T.FieldSub(T.DeRef(T.This),name)
		fun mk_deep_copy_vars {name,tid} =
		    let
			val field =
			    T.FieldSub(T.DeRef(T.Id copy_temp_id),name)
		    in
			if (same_qualifier (id,tid)) then
			    SOME(T.If{test=T.NotNil(field),
				      then_stmt=
				      T.Assign(field,
					       call_mth deep_copy_id field),
				      else_stmt=T.Nop})
			else NONE		  
		    end
		val shallow_copy_vars = List.map mk_shallow_copy_vars vars
		val deep_copy_vars = (List.mapPartial (mk_deep_copy_vars) vars)
		val shallow_copy_exp =
		    if List.null vars then T.This
		    else
			(T.New(name,shallow_copy_vars))

		val deep_copy_body =
		    [T.Assign(T.Id copy_temp_id,shallow_copy_exp)]@
		    (deep_copy_vars)@
		    [T.Return (T.Id copy_temp_id)]
		val shallow_copy_body = [T.Return shallow_copy_exp]
		val shallow_copy_mth =
		    T.Mth {name=shallow_copy_id,
			   inline=false,
			   body={vars=[],
				 body=shallow_copy_body},
			   mods={scope=T.Public,static=false, final=false},
			   args=args,ret=ret_ty}
		val deep_copy_mth =
		    T.Mth {name=deep_copy_id,
			   inline=false,
			   body={vars=[{name=copy_temp_id,ty=my_ty}],
				 body=deep_copy_body},
			   mods={scope=T.Public,static=false, final=false},
			   args=args,ret=ret_ty}
    	    in
		[(name,deep_copy_mth), (name,shallow_copy_mth)]
	    end
	  | mk_copy (Class{name,inherits,vars,abstract=true}) =
	    let
		val ret_ty = T.TyReference (T.TyId name)
	    in
		[(name,
		  T.MthAbstract
		  {name=shallow_copy_id,
		   mods={scope=T.Public,static=false, final=false},
		   args=[],ret=ret_ty}),
		 (name,
		   T.MthAbstract
		   {name=deep_copy_id,
		    mods={scope=T.Public,static=false, final=false},
		    args=[],ret=ret_ty})]
	    end
	  | mk_copy (_) = []
	    
	fun copy_aux (x,rest) = (mk_copy x)@rest

	fun build_aux mname {walker_code,copy_code} decls =
	    let
		val visit_tid = mk_tid (mname,"Visitor")

		val visitor = mk_visit_class visit_tid

		val walk_tid = mk_tid (mname,"Walker")
		val walker = mk_walker_class walk_tid visit_tid
		val aux_classes =  [visitor] 
		val aux_fn = (visit_aux visit_tid)
		val (aux_fn,aux_classes) =
		    if walker_code then
			(compose_fn aux_fn (walk_aux walk_tid),
			 walker::aux_classes)
		    else (aux_fn,aux_classes)
			
		val aux_fn =
		    if copy_code then (compose_fn aux_fn copy_aux)
		    else aux_fn

		val new_mths = fold_decls_inherit aux_fn [] decls
	    in
	      T.add_methods new_mths (decls@aux_classes)
	    end
	    
    end
