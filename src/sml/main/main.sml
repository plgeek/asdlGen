(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

functor mkMain (structure      S : SEMANT
		structure Parser : ASDL_PARSER
		structure    Gen : TRANSLATE
		  where type input = S.menv_info
			and  type output = string list
		val dflt_view    : string) =
  struct
    structure S = S
    val opts = Gen.opts
    structure O = CommandOptions
    val (opts,view_name)  =  O.stringParam opts
      {name="view",
       flags="V",
       arg_dflt=NONE,
       dflt=dflt_view,
       advice="view",
       doc="name of view"}

    val (opts,pickler)  = O.stringParam opts
      {name="pickler",
       flags="p",
       arg_dflt=NONE,
       dflt="std",
       advice="{std,sexp,xml,empty}",
       doc="kind of pickler"}
(*
    val (opts,aux_suffix)  = O.stringParam opts
      {name="util-suffix",
       flags="",
       arg_dflt=NONE,
       dflt="",
       advice="string",
       doc="suffix for auxilary modules"}
  *)    
    fun do_it args = let
      val files = O.getRest args
(*      val init =
	case aux_suffix args of
	  "" => S.MEnv.P.init_aux_mod_suffix NONE
	| s => S.MEnv.P.init_aux_mod_suffix (SOME s)
	    *)
      val inits = [S.MEnv.P.init_pickler_kind (pickler args)]
      val decls = Parser.parse files
      val menv = S.MEnv.declare {view=view_name args,inits=inits} decls
      val msgs = S.MEnv.validate menv
    in (if (List.null msgs) then Gen.translate args menv
	else (List.app (fn x => Error.say (x^"\n")) msgs;
	      raise Error.fatal))
    end
  end





