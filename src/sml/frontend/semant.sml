(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
This needs to be cleaned up. It's evolved over time and needs a redesign.
**)
(**:[[structure Semant]]:**)
structure Semant :> SEMANT =
    struct

	structure T = Asdl
	structure TU = AsdlUtil

	structure TId =
	  mkSourceId(val namespace = SourceIds.mkNameSpace "ASDL Type"
		     val sep = ".")
	structure CId =
	  mkSourceId(val namespace = SourceIds.mkNameSpace "ASDL Constructor"
		     val sep = ".")
	structure MId =
	  mkSourceId(val namespace = SourceIds.mkNameSpace "ASDL Module"
		     val sep = ".")
	structure FId =
	  mkSourceId(val namespace = SourceIds.mkNameSpace "ASDL Field"
		     val sep = ".")

	structure S =
	  SplaySetFn(struct
	    type ord_key = TId.id
	    val compare = TId.compare
	  end)
	structure Env = SplayMapFn(SourceIds)

(**:[[structure Semant]] [[structure QSet]]:
Sets of qualifers.
**)
	datatype kind = datatype T.tycon
	structure QSet =
	  ListSetFn(struct 
	    type ord_key = kind
	    fun compare (T.Option,T.Sequence) = GREATER
	      | compare (T.Option,T.Shared) = GREATER
	      | compare (T.Sequence,T.Shared) = GREATER
	      | compare (x,y) = if x = y then EQUAL
				else LESS
	  end)
(**)
(**:[[structure Semant]] type declarations:**)
        type field_info =
	  {kind:kind option,
	   props:FieldProps.props,
	    tref:TId.id,
	    name:FId.id option}     

	type con_info =
	     {tag:int,
	     name:CId.id,
	   fields:field_info list,
	    props:ConProps.props,
	     tref:TId.id}

	type type_info =
	    {tag:int,
	    name:TId.id,
	    uses:S.set,
	  fields:field_info list, 
	    cons:con_info list,
	 is_enum:bool, (* should move them into props *)
	 is_prim:bool,
	   props:TypProps.props}

	datatype info = T of type_info | C of con_info 

	datatype import = Import of module_info
	               | ImportAlias of MId.id
	               | ImportAll of module_info
	  
	and module_info =
	  M of {name:MId.id,
		defs:S.set,
	     is_prim:bool,
	        penv:type_info Env.map,
	         env:info Env.map,
	       props:ModProps.props,
	     imports:import Env.map}

       and menv_info =
	   ME of {menv:module_info Env.map,
		  penv:type_info Env.map,
		  uenv:QSet.set Env.map,
		  errs:string list,
		 props:ModEnvProps.props,
		 count:int}
(**)
(**:[[structure Semant]] misc. functions:**)	  
	fun get_t (x:type_info) = x
	fun get_c (x:con_info) =  x
	fun get_f (x:field_info) =  x
	fun get_m (M x) = x

	fun mk_src_name f (name,NONE) = name
	  | mk_src_name f (name,SOME x) = f x
(**)
(**:[[structure Semant]] [[structure Type]]:**)
	structure Type =
	  struct
	    structure P = TypProps	 
	    structure Id = TId
	    val props = #props o get_t
	    val tag = #tag o get_t 
	    val name = #name o get_t
	    fun src_name x =
	      mk_src_name Id.fromPath (name x,P.source_name (props x))
	    val cons = #cons o get_t
	    val fields = #fields o get_t
	    val is_enum = #is_enum o get_t
	    val is_prim  = #is_prim o get_t
	    val uses = S.listItems o #uses o get_t
	  end
(**)
(**:[[structure Semant]] [[structure Con]]:**)
	structure Con =
	  struct
	    structure P = ConProps
	    structure Id = CId
	    val props = #props o get_c
	    val tag = #tag o get_c
	    val name = #name o get_c
	    fun src_name x =
	      mk_src_name Id.fromPath (name x,P.source_name (props x))
	    val fields = #fields o get_c
	  end
(**)
(**:[[structure Semant]] [[structure Field]]:**)
	structure Field =
	  struct
	    structure P = FieldProps
	    structure Id = FId
	    val kind = #kind o get_f
	    val name = #name o get_f
	    val props = #props o get_f
	    fun src_name x =
	      case (name x,P.source_name (props x)) of
		 (SOME x,NONE) => x
		| (_,SOME x) => (Id.fromPath x)
		|  _ => raise (Error.internal)
	  end
(**)
(**:[[structure Semant]] [[structure Module]]:
This is a hairy mess.
**)
	structure Module =
	  struct
(**:[[structure Semant]] [[structure Module]]:
Some basic functions
**)
	    structure P = ModProps	    
	    structure Id = MId
	    val props = #props o get_m
	    val name  = #name o get_m
	    val prim_module  = #is_prim o get_m
	    val file  = P.file o props
	    fun src_name x =
	      mk_src_name Id.fromPath
	      (#name (get_m x),P.source_name (props x))

	    fun imports m =
	      let
		fun get_i (Import x) = SOME x
		  | get_i (ImportAlias _) = NONE
		  | get_i (ImportAll x) = NONE
		val imps =
		  (List.mapPartial get_i) o Env.listItems o #imports o get_m
	      in (imps m)
	      end
	    val get_mod_name = (MId.toSid o MId.fromString o
				List.hd o #qualifier o SourceIds.toPath)
(**:[[structure Semant]] [[structure Module]]:
Lookup functions 
**)
	    fun find_info (M{name,penv,env,imports,...},sid) =
	      let
		fun try_local (SOME x) = (SOME x)
		  | try_local NONE = try_prim (Env.find(penv,sid))
		  
		and try_prim (SOME t) = SOME (T t)
		  | try_prim NONE =
		  try_imports (Env.find(imports,get_mod_name sid))
		  
		and try_imports NONE = NONE
		  | try_imports (SOME (Import m)) = find_info (m,sid)
		  | try_imports (SOME (ImportAlias(mid))) =
		  try_imports (Env.find(imports,MId.toSid mid))
		  | try_imports (SOME (ImportAll(m))) =
 		  let
		    val M{name,...} = m
		    val {base,qualifier,...} = SourceIds.toPath sid
		    val sid' = MId.toSid (MId.fromPath
		      {base=base,qualifier=[MId.getBase name]})
		  in find_info (m,sid')
		  end
	      in try_local (Env.find(env,sid))
	      end
	    fun type_info' m tid =
	      case find_info(m,TId.toSid tid) of
		NONE => NONE
	      | (SOME (T t)) => (SOME t)
	      | _ => NONE
		  
	    fun type_info m tid =
	      case (type_info' m tid) of
		NONE =>
		  raise (Error.error ["Can't find type ", TId.toString tid])
	      | (SOME x) => x

	    fun con_info' m cid =
	      case find_info(m,(CId.toSid cid)) of
		NONE => NONE
	      | (SOME (C c)) => (SOME c)
	      | _ => NONE
		  
	    fun con_info m cid =
	      case (con_info' m cid) of
		NONE => raise
		  (Error.error ["Can't find constructor ", CId.toString cid])
	     | (SOME x) => x
	    fun field_type m = ((type_info m) o #tref o get_f)
	    fun con_type m = ((type_info m) o #tref o get_c)
	    fun is_defined m t =
	      let val env =  (#env o get_m) m
	      in  case (Env.find(env,TId.toSid (Type.name t))) of
		NONE => false
	      | _ => true
	      end
(**:[[structure Semant]] [[structure Module]]:
Dependency graph nodes 
**)
	    structure Node =
	      struct
		datatype ord_key = Root 
		| Ty of (TId.id * type_info)
		| UnDef of TId.id
		  
		fun compare (Ty(x,_),Ty(y,_)) = TId.compare(x,y)
		  | compare (UnDef x,UnDef y) = TId.compare(x,y)
		  | compare (Root,Root) = EQUAL
		  | compare (Root,_) = GREATER
		  | compare (Ty _,_) = GREATER
		  | compare _ = LESS
		   
		fun is_product (x as Ty(id,info)) = 
		  if (List.null (Type.cons info)) then SOME x
		  else NONE
		  | is_product x = (SOME x)

		fun mkNode m id =
		  case (type_info' m id) of
		    (SOME ti) => Ty (id,ti)
		  | NONE => UnDef id
		      
		fun luses (roots,ti) =
		  S.listItems
		  (S.intersection(roots,#uses(get_t ti)))
		  
		fun follow  roots m Root =
		  List.map (mkNode m) (S.listItems roots)
		  | follow  roots m (Ty (_,ti)) =
		  List.map (mkNode m) (luses (roots,ti))
		  | follow  roots m (UnDef _) = []
		  
	       fun follow'  roots m Root =
		 List.mapPartial (is_product o (mkNode m))
		   (S.listItems roots)
		 | follow'  roots m (Ty (_,ti)) =
		   List.mapPartial (is_product o (mkNode m)) (luses (roots,ti))
		 | follow'  roots m (UnDef _) = []
	      end
	    structure Scc =  SCCUtilFun(structure Node = Node)
(**:[[structure Semant]] [[structure Module]]:
Return types in topological order.
**)      
	    fun types (m as M{defs,...} ) =
	      let
		val scc =
		  Scc.topOrder{root=Node.Root,
			       follow=Node.follow defs m}
		fun get_id (Node.Ty(x,_),xs) = x::xs
		  | get_id  (_,xs) = xs
		  
		fun get_ids(Scc.SIMPLE x,xs) =
		  List.foldl get_id xs [x]
		  | get_ids (Scc.RECURSIVE x,xs) =
		  List.foldl get_id xs x
	      in
		List.foldl get_ids [] scc
	      end
(**:[[structure Semant]] [[structure Module]]:
Validate an indvidual module
**)
	    fun validate me (m as (M{defs,...}),errs) =
	      let
		val scc =
		  Scc.topOrder{root=Node.Root,follow=Node.follow' defs m}
		fun error x = ("Error: product type "^(TId.toString x)^
			       " recursively defined")
		fun check_node Node.Root  = NONE
		  | check_node (Node.UnDef x) =
		  SOME ("Error: "^(TId.toString x)^" undefined")
		  | check_node (Node.Ty (id,info)) =
		  if (List.null (Type.cons info)) then
		    SOME (error id)
		  else NONE
		    
		fun check (Scc.SIMPLE (Node.UnDef x),acc) =
		  ("Error: "^(TId.toString x)^" undefined")::acc
		  | check (Scc.SIMPLE _,acc) = acc
		  | check (Scc.RECURSIVE n,acc) = 
		  (List.mapPartial check_node n)@acc
		  
		fun check_extern (x,xs) =
		  case (type_info' m x) of
		    NONE => ("Error: "^(TId.toString x)^" undefined")::xs
		  | SOME _ =>  xs
	      (* val errs =
	       S.foldr check_extern errs (S.difference(uses,defs)) *)
	      in
		List.foldr check errs scc
	      end
       	  end
(**:[[structure Semant]] [[structure Module]]:
Add a module to an existing module environment.
This really needs to be reworked. 
**)	
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
**)
	val toMid = MId.fromString o Identifier.toString 

       fun declare_module view ({file,decl=T.Module def},
				ME{menv,penv,uenv,count,errs,props}) =
	   let
	     val {name,imports,decls} = def
	     val name_id = toMid name
	     val mname = Identifier.toString name
	     fun mk_import (T.Imports{module,alias},env) =
	       let
		 val minfo = (Option.valOf (Env.find(menv,MId.toSid (toMid module))))
		 val env = Env.insert(env,MId.toSid(toMid module),Import minfo)
	       in case alias of
		 NONE => env
	       | SOME alias =>
		   Env.insert(env,MId.toSid (toMid alias),ImportAlias(toMid module))
	       end
	     val imports = List.foldl mk_import Env.empty imports
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Rewrite identifiers to include a fully qualified path.
**)
	       fun fix_typ {qualifier=NONE,base} =
		 let
		   val base = Identifier.toString base
		   val qualifier = [mname]
		   val pid = TId.fromString base
		 in case (Env.find(penv,(TId.toSid pid))) of
		   NONE => TId.fromPath {base=base,qualifier=qualifier}
		 | (SOME {name,...}) => pid
		 end
		 | fix_typ {qualifier=SOME q,base} =
		 (case (Env.find(imports,MId.toSid (toMid q))) of
		    SOME (ImportAlias(alias)) =>
		      TId.fromPath {base=Identifier.toString base,
				   qualifier=[MId.toString alias]}
		  | _ =>
		      TId.fromPath {base=Identifier.toString base,
				   qualifier=[Identifier.toString q]})
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Walk over the AST and figure out the set of types and qualifiers used
in the definition of the type.
**)
	       fun types_used x =
		   let
		     fun get_fields (T.SumType{attribs=fs,c,cs,...}) =
		       List.foldl (fn ({fs,...},xs) => fs@xs) fs (c::cs)
		     | get_fields (T.ProductType{f,fs,...}) =  f::fs
		     val fields = get_fields x
		     fun add_set ({typ,tycon_opt,...}:T.field,env) =
		       let
			 val typ = fix_typ typ
			 val qset = (case (Env.find(env,(TId.toSid typ))) of
				      NONE => QSet.empty
				    | SOME qs => qs)
			 val qset = (case tycon_opt of
				       NONE => qset
				     | SOME q => QSet.add(qset,q))
		       in Env.insert(env,(TId.toSid typ),qset)
		       end
		   in List.foldl add_set Env.empty fields
		   end
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Build a [[field_info]] record from a list of fields
**)
	       fun mk_field_info c q fl =
		   let
		     fun do_field({typ,tycon_opt,label_opt,...}:T.field,
				  (i,fi)) =
		       let
			 val label_opt =
			   Option.map
			   (FId.fromString o Identifier.toString) label_opt
			 val fdname =
			   Option.getOpt(label_opt,
					 FId.fromString ("f"^(Int.toString i)))
			 val tref = fix_typ typ
			 fun mk_sid id =
			   FId.toSid (FId.fromPath{base=FId.getBase id,
						   qualifier=q})
			 val init = FieldProps.init_source_name
			   (SOME (FId.toPath fdname))
			 val inits = FieldProps.parse (view (mk_sid fdname))
			 val props = FieldProps.new (init::inits)
		       in
			 (i+1,{kind=tycon_opt,props=props,
			       name=label_opt,tref=tref}::fi)
		       end
		     val (_,fls) = List.foldl do_field (c,[]) fl
		   in List.rev fls
		   end
		 
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Build a list of [[con_info]] records and return a boolean as to
whether the constructors can be represented as a simple enumeration.
**)
	       fun mk_con_info c tref cons =
		   let
		     fun do_con ({name,fs},(tag,box,cs)) =
		       let
			 val tag = tag + 1
			 val basename = Identifier.toString name
			 val name =
			   CId.fromPath{qualifier=[mname],
					base=basename}
			 val box = box orelse not (List.null fs)
			 val inits = ConProps.parse (view (CId.toSid name))
			 val props = ConProps.new inits
		       in
			 (tag,box,{tag=tag,name=name,
				   fields=mk_field_info c [mname,basename] fs,
				   props=props,
				   tref=tref}::cs)
		       end
		     val (_,box,cons) = List.foldl do_con (0,false,[]) cons
		   in (box,List.rev cons)
		   end
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Declare a type and update all the necessary book keeping.
**)
	       fun declare_type (t,{uenv,defs,count,env,errs}) =
		   let
		       val count = count + 1
		       val (tid,fields,cons) =
			 (case t of
			    T.SumType{name,attribs,c,cs} =>
			      (name,attribs,c::cs)
			  | T.ProductType{name,f,fs} => (name,f::fs,[]))
		       val basename = Identifier.toString tid
		       val tname =
			 TId.fromPath{base=basename,qualifier=[mname]}
		       val fields = mk_field_info 0 [mname,basename] fields
		       val c = List.length fields
		       val (box_cons,cons) = mk_con_info c tname cons
		       val is_enum = box_cons orelse (not (List.null fields))
		       val ty_uses = types_used t
		       val inits = TypProps.parse (view (TId.toSid tname))
		       val props = TypProps.new (inits)
		       val tinfo = T {tag=count,
				      is_prim=false,
				      name=tname,
				      fields=fields,
				      cons=cons,
				      props=props,
				      is_enum=is_enum,
				      uses=
				      Env.foldli (fn (k,_,s) => S.add(s,TId.fromSid k))
				      S.empty ty_uses}

		       val uenv = Env.unionWith QSet.union (ty_uses,uenv)
		       val defs = S.add(defs,tname)

 		       fun add_def tostr f (k,v,(env,errs)) =
			   case Env.find(env,f k) of
			       NONE => (Env.insert(env,f k,v),errs)
			     | (SOME _) =>
				   (env,
				    (String.concat
				    ["Redefinition of ", tostr k])::errs)
		       val rest = add_def TId.toString TId.toSid (tname,tinfo,(env,errs))
		       val (env,errs) =
			 List.foldl
			 (fn (x,rest) => add_def CId.toString CId.toSid (#name x,C x,rest)) rest cons
		   in {uenv=uenv,defs=defs,env=env,errs=errs,count=count}
		   end
(**:[[structure Semant]] [[structure Module]] [[fun declare_module]]:
Build a new module info and add it to the current module environment.
**)
	       val {uenv,defs,count,env,errs} =
		 List.foldl declare_type  {uenv=uenv,
					   defs=S.empty,
					   count=count,
					   errs=errs,
					   env=Env.empty} decls
	       val inits = ModProps.parse (view (MId.toSid name_id))
	       val mprops = ModProps.new ((ModProps.mk_file file)::(inits))
	       val m =
		 M{name=name_id,props=mprops,is_prim=false,
		   defs=defs,penv=penv,env=env,imports=imports}
	       val menv = Env.insert(menv,MId.toSid name_id,m)
	   in
	     ME{uenv=uenv,penv=penv,menv=menv,
		count=count,errs=errs,props=props}
	   end
	 | declare_module view ({file,
				 decl=T.PrimitiveModule{name,exports}},me) =
	   let
	     val mqualifier = [Identifier.toString name]
	     fun declare_prim (pname,ME{uenv,menv,penv,count,errs,props}) =
	       let
		 val base = Identifier.toString pname
		 val full_name =
		   TId.fromPath{qualifier=mqualifier,base=base}
		 val short_name =
		   TId.fromPath{qualifier=[],base=base}
		 val count = count + 1
		 val inits = TypProps.parse (view (TId.toSid full_name))
		 val tinfo =
		   {tag=count,name=full_name,
		    uses=S.empty,fields=[],cons=[],
		    props=TypProps.new inits,is_prim=true,is_enum=false}
	       in ME{menv=menv,errs=errs,props=props,uenv=uenv,
		     penv=Env.insert(penv,TId.toSid short_name,tinfo),count=count}
	       end
	     val name_id = toMid name
	     val inits = ModProps.parse (view (MId.toSid name_id))
	     val mprops = ModProps.new ((ModProps.mk_file file)::(inits))
	     val minfo = M{name=name_id,defs=S.empty,
			   is_prim=true,penv=Env.empty,
			   env=Env.empty,props=mprops,imports=Env.empty}
	     val ME{uenv,menv,penv,count,errs,props} = me
	     val menv = Env.insert(menv,MId.toSid (toMid name),minfo)
	     val me = ME{menv=menv,errs=errs,props=props,uenv=uenv,
			 penv=penv,count=count}
	   in List.foldl declare_prim me exports
	   end
	 | declare_module _ (_,me) = me
(**)
(**)
(**:[[structure Semant]] [[structure MEnv]]:**)
       structure MEnv =
	 struct
	   structure P = ModEnvProps
	    fun props (ME{props,...}) = props
	    fun modules (ME{menv,...}) = Env.listItems menv
	    fun prim_types (ME{penv,...}) = Env.listItems penv

	    structure IEnv =
	      SplayMapFn(struct
		type ord_key = Identifier.identifier
		val compare = Identifier.compare
	      end)
	    structure Scc =
	      SCCUtilFun(structure Node =
			   struct
			     type ord_key = Identifier.identifier option
			     fun compare (SOME x,SOME y) =
			       Identifier.compare(x,y)
			       | compare (NONE,NONE) = EQUAL
			       | compare (NONE,_) = LESS
			       | compare (_,NONE) = GREATER
			   end)
	    fun mk_view_env ({file,decl=T.View{name,decls}},venv) =
	      let
		val id = MId.toSid (MId.fromString (Identifier.toString name))
		fun mk_view (entries,env) =
		  let
		    fun toId x =
		      let val len = List.length x
			val (qualifier,base) =
			  (List.take (x,len-1),List.drop (x,len - 1))
		      in case (qualifier,List.hd base) of
			([],base) => (MId.toSid (MId.fromString
						(Identifier.toString base)))
		      | ([q],base) =>
			  (TId.toSid (TId.fromPath
				      {base=Identifier.toString base,
				       qualifier=[Identifier.toString q]}))
		      | ([q1,q2],base) =>
			  (FId.toSid (FId.fromPath
				      {base=Identifier.toString base,
				       qualifier=
				       List.map Identifier.toString
				       [q1,q2]}))
		      | _ => raise (Error.error ["bad view entity"])
		      end
		    fun insert ({entity,prop,value},e) =
		      let val entity = toId entity
			val v = case (Env.find(e,entity)) of
			  NONE => [(prop,value)]
			| SOME rest => ((prop,value)::rest)
		      in Env.insert(e,entity,v)
		      end
		  in List.foldl insert env entries
	       end val env = (case (Env.find(venv,id)) of
				NONE => Env.empty | SOME e => e)
		val env = mk_view (decls,env)
	      in Env.insert(venv,id,env)
	      end
	      | mk_view_env (_,venv) = venv
	    (* reorder declarations *)
	    fun build_scc inps =
	      let
		fun is_view (T.View _) = true 
		  | is_view _ = false
		fun mk_env (x as {file,decl} ,env) =
		  if (is_view decl) then env
		  else IEnv.insert(env,#name(TU.attrbs_decl decl),x)
		val env = List.foldl mk_env IEnv.empty inps
		  
		fun mkNode id =
		  case (IEnv.find(env,id)) of
		 (SOME {file,decl=T.Module{imports,...}}) =>
		   List.map (fn (T.Imports{module,...}) => SOME module) imports
	       | (SOME {file,decl=T.PrimitiveModule _}) => []
	       | _ => raise
		   (Error.error ["Can't find module: ",Identifier.toString id])
		fun follow (SOME id) = mkNode id
		  | follow NONE = List.map (SOME o #1) (IEnv.listItemsi env)
		val torder = Scc.topOrder {root=NONE,follow=follow}
		fun check (Scc.SIMPLE (SOME n),acc) =
		  (Option.valOf(IEnv.find (env,n)))::acc
		  | check (Scc.SIMPLE NONE,acc) = acc
		  | check (Scc.RECURSIVE n,acc) =
		  raise Error.error ["Circular module dependency"]
		val view_env = List.foldl mk_view_env Env.empty inps
		fun get_view id =
		  case Env.find(view_env,id) of
		SOME e => (fn id => Option.getOpt(Env.find(e,id),[]))
	      | NONE => (fn _ => [])
	      in (List.foldl check [] torder,get_view)
	      end

	    fun prim_env inits =
	      ME{menv=Env.empty,penv=Env.empty,uenv=Env.empty,
		 count=0,props=ModEnvProps.new inits,
		 errs=[]}

	    fun is_prim {file,decl=T.PrimitiveModule _} = true
	      | is_prim _ = false
				     
	    fun declare {view,inits} ds =
	      let
		val prim_ms =
		  DefaultDecls.std_prims@(List.filter is_prim ds)
		val ds =
		  DefaultDecls.std_views@(List.filter (not o is_prim) ds)
		val (ds,gv) = build_scc ds
		val v = gv (MId.toSid (MId.fromString view))
		val penv = prim_env inits
		val penv = List.foldl (declare_module v) penv prim_ms
	      in List.foldl (declare_module v) penv ds
	      end
	    fun qualified (ME{uenv,...}) (M{defs,...}) =
	      let
		fun find (i,xs) =
		  case (Env.find(uenv,(TId.toSid i))) of
		    NONE => xs
		  | SOME q =>
		      if QSet.isEmpty q then xs
		      else (i,QSet.listItems q)::xs
	      in S.foldl find [] defs
	      end
	    fun validate (me as ME{menv,errs,...}) =
	      Env.foldl (Module.validate me) errs menv
	 end
(**)
    end
(**)
