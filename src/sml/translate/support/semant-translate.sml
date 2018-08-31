(**::
As well as implementing a generic recusive walk, this code also tries
to mangle field names properly so language like Haskell don't
complain about non-unique field names. Currently the mangling isn't
completely effective. The real fix would be to move the mangling code
into the [[structure Semant]].

The functor takes two structures one is the translation functions
described by the [[SEMANT_TRANSLATOR]] signature, the other is
another phase of translation to pass the result of the constructed
translation. This extra structure really needs to go away, and is
here only because the code evolved this way.
**)
(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(* needs some major clean up *)
functor mkTranslateFromTranslator
  (structure G : TRANSLATE 
   structure T : SEMANT_TRANSLATOR where type output = G.input) : TRANSLATE =
    struct
      structure S = Semant
      structure Ast = T.Ast
      type output = G.output 
      type input = S.menv_info
      val opts = G.opts
      open AsdlSemant
      fun translate p menv =
	let
	  fun do_it p =
	    let
	      fun do_menv (MEnv x) = T.trans p x
	      fun do_module (Module {module,imports,props,typs,type_cons}) =
		T.trans_module p {module=module,imports=imports,props=props,
				  defines=typs,type_cons=type_cons}
	      fun do_field (Fd{finfo,kind,name,tname,
			       is_local,tinfo,props,tprops}) =
		T.trans_field p
		{finfo=finfo,kind=kind,name=name,tname=tname,
		 is_local=is_local,tinfo=tinfo,props=tprops}
	 
	      fun do_con (Con x) = T.trans_con p x
	      fun do_tycon (TyCon x) = T.trans_type_con p x
	      fun do_typ (Sum x) = T.trans_defined p x
		| do_typ (Product {tinfo,props,name,fields}) =
		T.trans_defined p {tinfo=tinfo,props=props,name=name,
				   cons=[],
				   fields=fields}
	    in fold {menv=do_menv,
		     module=do_module,
		     field=do_field,
		     con=do_con,
		     typ=do_typ,
		    tycon=do_tycon} menv
	    end
	  val props = S.MEnv.props menv
	in (G.translate p) (do_it props)
	end
    end

functor mkTranslateFromFn
  (structure G : TRANSLATE 
   val opts : CommandOptions.args_spec
   val set_dir : bool
   val do_it : Semant.menv_info -> G.input ) : TRANSLATE =
     struct
       type input = Semant.menv_info
       type output = G.output
       val opts = opts
     fun translate p menv =
       let
       in (G.translate p) (do_it menv)
       end
   end
