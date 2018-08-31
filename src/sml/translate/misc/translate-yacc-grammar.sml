(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure YaccGrammarTranslator : SEMANT_TRANSLATOR =
    struct
	structure S = Semant

	structure Ast = YaccGrammar
	structure T = Ast

	type input_value    = S.module_info
	type defined_value  = T.production
	type type_con_value = T.production list
	type con_value      = T.rule
	type field_value    = T.rule_atom
	type module_value   = T.production list
	type output         = T.grammar list
	type type_con_value = T.production list
	val set_dir = true
	val fix_fields = false
	val inits = []

	structure IdCvt =
	mkIdCvt(structure Ast = Ast
		structure IdMap = IdMaps.Empty)
	open IdCvt
	  
	val mangle_seq = T.TypeId.suffixBase "_seq" 
	val mangle_opt = T.TypeId.suffixBase "_opt" 

	val mangle_tmp = T.TypeId.suffixBase "_internal" 

	val beg_seq = T.Term (T.VarId.fromString "SPECIAL_BEG_SEQ")
	val beg_opt = T.Term (T.VarId.fromString "SPECIAL_BEG_OPT")
	val end_seq = T.Term (T.VarId.fromString "SPECIAL_END_SEQ")
	val end_opt = T.Term (T.VarId.fromString "SPECIAL_END_OPT")
	fun trans_field p {finfo,kind,name,tname,tinfo,is_local,props} =
	    let
	      val tname = trans t2t tname
	      val tname =
		case (kind) of
		NONE => tname
	      | SOME S.Option => mangle_opt tname
	      | SOME S.Sequence => mangle_seq tname
	      | _ => raise Error.unimplemented
	    in T.NonTerm tname
	    end
	fun trans_con p {cinfo,tinfo,name,fields,attrbs,tprops,cprops} =
	  let val ra = T.Term(trans c2v name)
	  in (ra::(attrbs@fields),NONE)
	  end

	fun trans_defined p {tinfo,name,cons=[],fields,props} =
	  (trans t2t name,[(fields,NONE)])
	  | trans_defined p {tinfo,name,cons,fields,props} =
	  (trans t2t name,cons)

	fun trans_type_con p {tinfo,name,kinds,props} =
	  let
	    val ty_name = trans t2t name
	    val seq_name = mangle_seq ty_name
	    val seq_tmp = mangle_tmp seq_name
	    val opt_name = mangle_opt ty_name
	    val opt_tmp = mangle_tmp opt_name
	    fun do_con (S.Sequence,xs) =
	      [(seq_tmp,[([T.NonTerm ty_name,T.NonTerm seq_tmp],NONE),
			 ([],NONE)]),
	       (seq_name,[([beg_seq,T.NonTerm seq_tmp,end_seq],NONE)])]@xs
	      | do_con (S.Option,xs) =
	      [(opt_tmp,[([T.NonTerm ty_name,T.NonTerm opt_tmp],NONE),
			 ([],NONE)]),
	       (opt_name,[([beg_opt,T.NonTerm opt_tmp,end_opt],NONE)])]
	  in List.foldl do_con [] kinds
	  end

	fun trans_module p {module,imports,defines,type_cons,props} =
	  let val aux = List.foldr (op @) [] type_cons
	  in List.foldr (op ::) aux defines 
	  end

	fun add_prim (s,prims) =
	  let
	    val ty = T.TypeId.fromString s
	    val seq_name = mangle_seq ty
	    val seq_tmp = mangle_tmp seq_name
	  in [(ty,[([T.Term (T.VarId.fromString ("SPECIAL_"^s))],NONE)]),
	      (seq_tmp,[([T.NonTerm ty,T.NonTerm seq_tmp],NONE),
			([],NONE)]),
	      (seq_name,[([beg_seq,T.NonTerm seq_tmp,end_seq],NONE)])]@prims
	  end

	val prims = List.foldl add_prim ([]:YaccGrammar.grammar)
	  ["int","string","identifier"]
	  
	fun trans p {modules,prim_types,prim_modules} =
	  [List.foldr (op @) prims modules]

    end
