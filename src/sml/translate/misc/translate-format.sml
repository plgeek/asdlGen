(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure FormatTranslator :>
  sig
    val do_it : Semant.menv_info -> FormatDoc.module list
  end =
struct
  structure S = Semant
  structure Ast = FormatDoc
  structure T = FormatDoc
  structure Id = Ast.VarId
    
  fun getdoc f x =
    (case (f x) of
       NONE => [T.EM [T.STR "No Documentation"]]
       | (SOME x) => [T.STR x])
  open AsdlSemant
  fun fmt_list i sep f fmt [] = T.RM []
    | fmt_list i sep f fmt xs =
    let
      fun do_sep [] = []
	| do_sep [x] = (fmt x)::[f]
	| do_sep (x::xs) = (fmt x)::sep::(do_sep xs)
    in T.RM (i::(do_sep xs))
    end
  fun tag {tag,fmt} = tag

  fun do_field (Fd{kind,name,tname,props,...}) =
    let
      val sep =
	case kind of
	  NONE => ""
	| SOME S.Option =>  "?"
	| SOME S.Sequence =>  "*"
	| SOME S.Shared =>  "!"
      val tag = T.RM[T.EM [T.STR (S.Type.Id.toString tname)],T.STR sep,
			   T.NBS,T.STR (S.Field.Id.toString name)]
      val fmt = T.P (getdoc S.Field.P.doc_string props)
    in {fmt=fmt,tag=tag}
    end

  fun do_con (Con{name,fields,cprops,...}) =
    let
      val tag = T.RM [T.BF [T.STR (S.Con.Id.toString name)],
		      fmt_list (T.STR "(") (T.RM[T.STR ",",T.NBS])
		               (T.STR ")") tag fields]
      val fmt = T.RM [T.P (getdoc S.Con.P.doc_string cprops),
		      T.DL fields]
    in {fmt=fmt,tag=tag}
    end

  fun do_typ (Sum{name,props,cons,fields,...}) =
    let
      val attribs =
	case fields of
	  [] => T.RM []
	|  x => T.RM [T.BF [T.STR "attributes"], T.DL fields]
      val tag = T.EM [T.STR (S.Type.Id.toString name)]
      val fmt = T.RM [T.P (getdoc S.Type.P.doc_string props),
		      attribs,T.DL cons]
    in {fmt=fmt,tag=tag}
    end
    | do_typ (Product{name,props,fields,...}) =
    let
      val tag = T.RM [T.EM [T.STR (S.Type.Id.toString name)],
		      fmt_list (T.STR "= (") (T.RM[T.STR ",",T.NBS])
		      (T.STR ")") tag fields]
      val fmt = T.RM [T.P (getdoc S.Type.P.doc_string props),
		      T.DL fields]
    in {fmt=fmt,tag=tag}
    end

  fun do_tycon _ = ()

  fun do_module (Module{module,imports,props,typs,type_cons,...}) =
    let
      val mname = S.Module.Id.toString (S.Module.name module)
      val docs = getdoc S.Module.P.doc_string props
      val toMid = Ast.ModuleId.fromPath o S.Module.Id.toPath o S.Module.name
      val decl =
	{title="Description for Module "^mname,
	 body=[T.SECT(1,[T.STR ("Description of Module "^mname)]),
	       T.P docs,		
	       T.SECT(2,[T.STR ("Locally defined types")]),
	       T.DL typs]}
    in  T.Module{name=toMid module,
		 imports=List.map toMid imports,
		 decls=decl}
    end
  fun do_menv (MEnv{modules,prim_modules,prim_types}) = modules
  fun do_it x = fold {menv=do_menv,
		      module=do_module,
		      typ=do_typ,
		      con=do_con,
		      field=do_field,
		      tycon=do_tycon} x
end



