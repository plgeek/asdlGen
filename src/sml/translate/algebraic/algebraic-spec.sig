(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
The [[mkAlgebraicSemantTranslator]] functor builds
a translator from ASDL semantic values to the semantic values
of an algebraic language. It is parameterized over several structures
that act as a specification for a particular language. Many of
parameter structures implement refinements of more abstract
signatures by specifying concrete implementations for previously
abstract types.
**)
(**:[[signature ALGEBRAIC_PP]]:
Specialized version of the [[CODE_PP]] signature constrains the pretty
printer to a toplevel module definition of implementations of the
[[ALGEBRAIC_AST]] signature.
**) 
signature ALGEBRAIC_PP =
  sig
    structure Ast : ALGEBRAIC_AST
      include CODE_PP
        where type code = (Ast.module * Semant.Module.P.props)
  end
(**)
(**:[[signature ALGEBRAIC_TYPE_DECL]]:
Specialized version of the [[TYPE_DECL]] signature that expose
concrete representation for the abstract types found in the
[[TYPE_DECL]] signature.
**)
signature ALGEBRAIC_TYPE_DECL =
  sig
    structure Ast : ALGEBRAIC_AST
      include TYPE_DECL
      where type tag =  {c:Ast.cnstr,v:int}
	and type exp = Ast.exp
	and type ty_exp = Ast.ty_exp
        and type VarId.id = Ast.VarId.id
        and type TypeId.id = Ast.TypeId.id
  end
(**)
(**:[[signature ALGEBRAIC_SPEC]]:
The signature [[ALGEBRAIC_SPEC]] specifies a number of functions that 
control the precise details of how code is generated for a specific target 
language. 
**)
signature ALGEBRAIC_SPEC =
  sig
    structure Ty    : ALGEBRAIC_TYPE_DECL
(**:[[signature ALGEBRAIC_SPEC]]:
The [[inits]] values is a list of defaults properties for this
transalation. The [[prims]] is a list of the primitive types for this language.
**)
    val inits : Semant.MEnv.P.init list
    val prims : Semant.MEnv.P.props ->
      Semant.type_info list -> Ty.ty_decl list
(**:[[signature ALGEBRAIC_SPEC]]:
We split the generated code into two different modules. One that just
contains the types and the other the pickler and other auxiliary
code. The name of the auxiliary module is derived by appending a
suffix to the name of the original module.
**)
    val aux_suffix : string
    val ignore_labels : bool
(**:[[signature ALGEBRAIC_SPEC]]:
Given a property list that describes how to translate some of the
qualified types in ASDL and the kind of a qualified type return a function
to construct the qualified type from the base type, a name mangler
for the base type identifiers and a type constructor.
**)
    val get_reps : Semant.MEnv.P.props ->
                   Semant.kind -> {mkrep:Ty.ty_exp -> Ty.ty_exp,
				   mktid:Ty.ty_id -> Ty.ty_id,
				     con:Ty.ty_con}
(**:[[signature ALGEBRAIC_SPEC]]:
Lookup any user defined readers or writers for this particular type.
**)
    val get_info: Semant.Type.P.props -> Ty.ty_info
(**:[[signature ALGEBRAIC_SPEC]]:
Lookup any view information for this type.
**)
    val get_wrappers  : Ty.ty_exp -> Semant.Type.P.props ->
      {natural_ty: Ty.ty_exp,
             wrap: Ty.exp -> Ty.exp,
           unwrap: Ty.exp -> Ty.exp}
(**:[[signature ALGEBRAIC_SPEC]]:
Build any auxiliary functions from a type environemnt.
**)
    val get_aux_decls : Semant.MEnv.P.props -> Ty.env
                 -> Ty.ty_decl list -> Ty.Ast.decl list
  end
(**)
