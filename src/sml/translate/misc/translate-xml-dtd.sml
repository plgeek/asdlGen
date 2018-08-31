(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
structure XMLDTDTranslator : SEMANT_TRANSLATOR =
    struct
	structure S = Semant
	structure Ast = XMLDTD
	structure T = Ast

	type input_value    = S.module_info
	type defined_value  = T.element_decl  * T.element_decl list
	type con_value      = (T.children * T.element_decl)
	type field_value    = T.children
	type module_value   = T.module * T.children list
	type output         = T.module list
	type type_con_value = T.element_decl list
	val set_dir = true
	val ignore_supress = true
	val fix_fields = false
	val inits = []
	    
	structure IdCvt =
	mkIdCvt(structure Ast = Ast
		structure IdMap = IdMaps.Empty)
	open IdCvt

	val mangle_seq = T.TypeId.suffixBase "-seq" 
	val mangle_opt = T.TypeId.suffixBase "-opt" 
	fun tag_att i = {name=T.VarId.fromString "tag-hint",
			 att_type=T.CDATA,
			 default=T.FIXED(T.Str (Int.toString i),[])}
	fun common_attrbs p =
	    [{name=T.VarId.fromString "lb",
	      att_type=T.OneToken T.NMTOKEN,
	      default=T.IMPLIED}]

	val sz_attrib = {name=T.VarId.fromString "sz",
			 att_type=T.CDATA,default=T.REQUIRED}

	fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	    let
	      val tname = trans t2t tname
	      val tname =
		case (kind) of
		NONE => tname
	      | SOME S.Option => mangle_opt tname
	      | SOME S.Sequence => mangle_seq tname
	      | _ => raise Error.unimplemented
	    in T.Child tname
	    end

	fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} =
	  let
	    val name = T.TypeId.fromPath(Semant.Con.Id.toPath name) 
	    val tag_v = S.Con.tag cinfo
	    val content =
	      case (attrbs,fields) of
		([],[]) => T.EMPTY
	      | ([],y::ys) => T.Children (T.Seq (y,ys))
	      | (x::xs,ys) => T.Children (T.Seq (x,xs@ys))
		  
	    val con = {element=name,
		       content=content,
		       att_defs=[tag_att tag_v]}
	  in (T.Child name,con)
	  end

	fun trans_defined p {tinfo,name,cons=[],fields,props} =
	    let
	      val name = trans t2t name
	      val att_defs = (common_attrbs p)
	    in ({element=name,
	       content=T.Children (T.Seq (hd fields,tl fields)),
	       att_defs=att_defs},[])
	    end

	 | trans_defined p {tinfo,name,cons,fields,props} =
	    let
	      val con_children = List.map #1 (cons:con_value list)
	      val con_tags = List.map #2 cons
	      val name = trans t2t name
	      val att_defs = (common_attrbs p)
	    in
	      ({element=name,content=T.Children
	       (T.Choice (hd con_children,tl con_children)),
	       att_defs=att_defs},con_tags)
	    end

	fun trans_type_con p {name,tinfo,props,kinds} =
	  let
	    val name = trans t2t name
	    val child = T.Child name
	    val opt_name = mangle_opt name
	    val seq_name = mangle_seq name
	    val att_defs = sz_attrib::(common_attrbs p)
	    fun do_kind S.Sequence =
	      {element=seq_name,
	       content=T.Children (T.ZeroOrMore child),
	       att_defs=att_defs}
	      | do_kind S.Option =
	      {element=opt_name,content=T.Children (T.ZeroOrOne child),
	       att_defs=att_defs}
	  in List.map do_kind kinds
	  end

	fun trans_module p {module,imports,defines,type_cons,props} =
	  let 
	    val ty_cons = List.foldr (op @) [] type_cons 	    
	    val tags = List.foldr (fn ((_,xs),acc) => xs@acc)
	      ty_cons (defines:defined_value list) 
	    val roots = List.map #1 defines

	    fun mk_spec ({element,...}:T.element_decl) = T.Child element
	    val mpath = Semant.Module.Id.toPath (S.Module.name module)
	    val toMid = (trans m2m) o S.Module.name
	  in (T.Module{name=toMid module,
		       imports=List.map toMid imports,
		       decls=(roots@tags)},List.map mk_spec roots)
	  end
	fun trans p {modules=mv,prim_types,prim_modules} =
	  let
	    val root_id = T.TypeId.fromString "root"
	    fun do_mod ((m as (T.Module{name,...}),roots),(ms,imps,xs)) =
	      (m::ms,name::imps,roots@xs)
	    val (mods,imports,roots) =  List.foldr do_mod ([],[],[]) mv
	    val root =
	      T.Module{name=T.ModuleId.fromString "root",
		       imports=imports,
		       decls=[{element=root_id,
			       content=T.Children 
			       (T.Choice (List.hd roots,List.tl roots)),
			       att_defs=[]}]}
	  in root::mods
	  end
    end



