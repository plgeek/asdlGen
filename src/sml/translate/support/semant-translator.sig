(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
The [[signature SEMANT]] provides a simple procedural interface to the
ASDL semantic entities. Very often, code just needs to make a one pass
recursive walk over the entitites and translate each into an new
value. Rather than repeating the same basic recursive walk several
times. We introduce the [[signature SEMANT_TRANSLATOR]] that describes
how to translate each entity into a new abstract value.  Modules that
implement the [[SEMANT_TRANSLATOR]] signature can be passed to
[[mkTranslateFromTranslator]] functor which implements the recursive
walk.
**)
(* This interface needs to be clean up *)
(**:[[signature SEMANT_TRANSLATOR]]:
**)
signature SEMANT_TRANSLATOR =
    sig
	structure Ast : LANG_AST
(**)
(**:[[signature SEMANT_TRANSLATOR]]:
\begin{description}
\item [{[[type defined_value]]}] The type of abstract values produced
 by translating a defined ASDL sum or product definition.
\item [{[[type con_value]]}] The type of abstract values produced
 by translating an ASDL constructor definition.
\item [{[[type field_value]]}] The type of abstract values produced
 by translating an ASDL field definition.
\item [{[[type type_con_value]]}] The type of abstract values produced
 by translating type constructor (sequences, options, shared, ...)
\item [{[[type module_value]]}] The type of abstract values produced
 by translating a complete ASDL module definition.
\item [{[[type output]]}] The type of abstract values produced
 by translating a complete closed module environment.
\end{description}
**)
	type defined_value
	type con_value
	type type_con_value
	type field_value
	type module_value
	type output
(**:[[signature SEMANT_TRANSLATOR]]:
Cruft, I wan't to make disappear. Mainly to control cosmetic issues
of the translation.
**)
	val inits: Semant.MEnv.P.init list
	val set_dir : bool
	val fix_fields : bool
(**:[[signature SEMANT_TRANSLATOR]]:
For each of the previously defined abstract types there are functions to 
create each type that take arguments which reflect the structure of the
[[signature SEMANT]] interface plus extra arguments that provide
extra information to make the translation a bit easier.
**) 
	val trans_defined: Semant.MEnv.P.props ->
	    {tinfo:Semant.type_info,
	     props:Semant.Type.P.props,
 	      name:Semant.Type.Id.id,
	      cons:con_value list,
	    fields:field_value list} -> defined_value

	val trans_type_con : Semant.MEnv.P.props ->
	    {tinfo:Semant.type_info,	     
	      name:Semant.Type.Id.id,
	     kinds:Semant.kind list,
	     props:Semant.Type.P.props} -> type_con_value 
	  
	val trans_con: Semant.MEnv.P.props ->
	    {cinfo:Semant.con_info,
	    cprops:Semant.Con.P.props,
	    tprops:Semant.Type.P.props,
	     tinfo:Semant.type_info,
	      name:Semant.Con.Id.id,
	    attrbs:field_value list,
	    fields:field_value list} -> con_value
       
	val trans_field: Semant.MEnv.P.props ->
	    {finfo:Semant.field_info,
	      kind:Semant.kind option,
	      name:Semant.Field.Id.id,
	     tname:Semant.Type.Id.id,
	  is_local:bool, 
	     tinfo:Semant.type_info,
	     props:Semant.Type.P.props} -> field_value

	val trans_module: Semant.MEnv.P.props ->
	    {module: Semant.module_info,
	    imports: Semant.module_info list,
    	      props: Semant.Module.P.props,
	    defines: defined_value list,
	  type_cons: type_con_value list} -> module_value

	val trans : Semant.MEnv.P.props ->
	  {modules:module_value list,
      prim_modules:Semant.module_info list,
	prim_types:Semant.type_info list} -> output
(**)
    end
(**)