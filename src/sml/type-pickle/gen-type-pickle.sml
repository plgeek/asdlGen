(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure GenPickleTranslator:TRANSLATE =
    struct
	structure S = Semant
	structure T = TypePickle
	structure TU = TypePickleUtil
	structure Output = BinIOFileOutput
	fun mapi f l = (List.foldl
			(fn (x,(i,xs)) => (i+1,f(i,x)::xs)) (0,[]) l) 
	    
	fun insert_item (x,map) =
	    case (Env.find(map,x)) of
		NONE => Env.insert(map,x,Env.numItems map)
	      | SOME _ => map

	fun make_keys menv (m,(type_map,cnstr_map)) =
	    let
		val defs = (S.Module.types m)
		val get_cons = S.Type.cons o (S.Module.type_info m)
		fun insert_ci (x,xs) =
		    List.foldl insert_item xs
		    (List.map S.Con.name (get_cons x))

		val type_map = List.foldl insert_item type_map defs
		val cnstr_map = List.foldl insert_ci cnstr_map defs

	    in
		(type_map,cnstr_map)
	    end

	val prim_map = Env.empty
(*	    List.foldl (fn ((x,y),env) => Env.insert(env,x,y)) Env.empty
	    [(S.Prim.int,T.Int),
	     (S.Prim.string,T.String),
	     (S.Prim.identifier,T.Identifier)]
*)
	    
	val cfg = Output.cfg
	val (cfg,output_file)  =
	    Params.declareString cfg
	    {name="output-file",flag=SOME #"o",default="env.typ"}
	val (cfg,output_directory)  =
	    Params.declareString cfg
	    {name="output-dir",flag=SOME #"d",default=OS.Path.currentArc}

	type input = S.menv_info
	type output = Output.output
	    
	fun translate p menv =
	    let
		val modules = S.MEnv.modules menv
		fun file_name p (m::ms) =
		    let
			val input = S.Module.file m
			val {dir,file} = OS.Path.splitDirFile input
			val dir =
			    if dir = "" then OS.Path.currentArc
			    else dir
			val params = Params.fromList cfg  [] 
		    in
			Params.mergeParams(p,params)
		    end
		  | file_name p _ = p
		val p = file_name p modules 
		val prim_info = S.MEnv.prim_types menv
		val type_map =  List.foldl
		    (fn (x,y) => insert_item (S.Type.name  x,y))
		    Env.empty prim_info
		    
		val (type_map,cnstr_map) =
		    List.foldl (make_keys menv)
		    (type_map,Env.empty) modules
		    
		fun mk_prim x =
		    (Option.valOf (Env.find(prim_map,x)))
		fun mk_qid id =
		    let
			val {base,qualifier} = Id.toPath id
			val base = Identifier.fromString base
			val qualifier =
			    List.map Identifier.fromString qualifier
		    in
			{base=base,qualifier=qualifier}:T.qid
		    end
		
		fun type_key id =
		    case (Env.find(type_map,id)) of
			NONE =>
			    raise Error.error ["Lookup ",Id.toString id]
		      | (SOME i) => i
				
		fun cnstr_key id =
		    case (Env.find(cnstr_map,id)) of
			NONE => raise Error.error ["Lookup ",Id.toString id]
		      | (SOME i) => i
			    
		fun field_type_key m  =
			    (type_key o S.Type.name o (S.Module.field_type m))
			    
		fun cnstr_type_key m  =
		    (type_key o S.Type.name o (S.Module.con_type m))
		    
		fun mk_field m fi =
		    let
			val kind =
			    case (S.Field.kind fi) of
				NONE => T.Id
			      | SOME S.Sequence => T.Sequence
			      | SOME S.Option => T.Option
			      | _ => raise Error.unimplemented
			val type_map_key = field_type_key m fi
			val label = S.Field.src_name fi
		    in
			kind {type_map_key=type_map_key,label=label}
		    end
		
		and mk_type m ti =
		    let
			val key = type_key (S.Type.name ti)
			val fields =
			    List.map (mk_field m)  (S.Type.fields ti)
			val name = mk_qid (S.Type.name ti)
			val cnstr_map_keys =
			    List.map (cnstr_key o S.Con.name)
			    (S.Type.cons ti)
			val pkl_tag = S.Type.tag ti
			val is_prim = S.Type.is_prim ti
			val v =
			    if is_prim then
				T.Prim{pkl_tag=pkl_tag,
				       p=mk_prim (S.Type.name ti)}
			    else
				T.Defined
				{pkl_tag=pkl_tag,
				 name=name,
				 fields=fields,
				 cnstr_map_keys=cnstr_map_keys}
		    in
			{key=key,v=v}:T.type_map_entry
		    end
		
		and mk_cnstr m ci =
		    let
			val key = cnstr_key (S.Con.name ci)
			val name = mk_qid (S.Con.name ci)
			val type_map_key = cnstr_type_key m ci
			val fields =
			    List.map (mk_field m)  (S.Con.fields ci)
			val pkl_tag = S.Con.tag ci
			val v =
			    {pkl_tag=pkl_tag,
			     type_map_key=type_map_key,
			     name=name,fields=fields}
		    in
			{key=key,v=v}:T.cnstr_map_entry
		    end
		
		and mk_module (key,m) =
		    let
			val name = mk_qid (S.Module.name  m)
			val file = S.Module.file m
			val v =	{name=name,file=file}
		    in
			{key=key,v=v}:T.module_map_entry
		    end
		
		fun make_maps menv (m,(types,cnstrs)) =
		    let
			val defs = (S.Module.types m)
			fun do_type (id,(tis,cis)) =
			    let
				val ti = (S.Module.type_info m) id
				val get_cons = S.Type.cons ti
				val cons = S.Type.cons ti
				val tis = (mk_type m ti)::tis
				val cis =
				    (List.map (mk_cnstr m) cons)@cis
			    in
				(tis,cis)
			    end
		    in
			List.foldl do_type (types,cnstrs) defs
		    end
		
		
		fun max_key ({key,v},i) = Int.max(key,i)
		    
		fun make_prim ti =
		    let
			val pkl_tag = S.Type.tag ti
			val name = (S.Type.name ti)
			val key = type_key name
			val v = T.Prim{p=mk_prim name,pkl_tag=pkl_tag}
		    in
			{key=key,v=v}
		    end
		
		val tentries = List.map make_prim prim_info
		    
		val (tentries,centries) =
		    List.foldl (make_maps menv) (tentries,[]) modules
		    
		val tmax_key = List.foldl max_key 0 tentries
		val tmap = {max_key=tmax_key,entries=tentries}
		    
		val cmax_key = List.foldl max_key 0 centries
		val cmap = {max_key=cmax_key,entries=centries}
		    
		val (mmax_key,mentries) = mapi mk_module modules
		val mmap = {max_key=mmax_key-1,entries=mentries}
		val tenv =
		    {magic=1,version=1,mmap=mmap,tmap=tmap,cmap=cmap}
		val fname = (output_file p)
	    in
		Output.translate p [([fname],TU.write_type_env tenv)]
	    end
    end










