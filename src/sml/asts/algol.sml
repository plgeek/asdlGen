(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

structure AlgolAst :  ALGOL_AST =
    struct
	open LangIds
	datatype ty_exp =
	    TyRefAny
	  | TyId         of ty_id
	  | TyArray      of (ty_exp * int option)
	  | TyRecord     of {fixed:field list,
			     variant:variant option}
	  | TyReference  of ty_exp
	  | TyEnum       of enumer list
	  | TyFunction   of field list * ty_exp
	  | TyOption     of ty_exp
	  | TySequence   of ty_exp
	  | TyShare      of ty_exp
	  | TyAnnotate   of (string * ty_exp)

	and const =
	    IntConst of (int)
	  | EnumConst of (id)
	  | AddrConst of (id)
	  | StrConst of (string)
	and exp =
	    Const of const
	  | NilPtr
	  | FnCall of (id * exp list)
	  | Id  of (id)
	  | RecSub of (exp * id)
	  | VarRecSub of (exp * id * id)
	  | ArraySub of (exp * exp)
	  | Addr of (exp)
	  | DeRef of (exp)
	  | PlusOne of (exp)
	  | MinusOne of (exp)
	  | NotNil of (exp)
	  | NotZero of (exp)
	  | NotEqConst of (exp * const)

	and stmt =
	   Nop
	  | Assign      of (exp * exp)
	  | AllocateRec of {dst:id,ty:ty_id,field_inits: field_init list,
			    variant_init: variant_init option}
	  | If          of {test:exp,then_stmt:stmt,else_stmt:stmt}
	  | While       of {test:exp,body:stmt}
	  | Case        of {test:exp,clauses:clause list,default:stmt}
	  | Block       of (block)
	  | ProcCall    of (id * exp list)
	  | Return      of (exp)

	
	and decl =
	    DeclTy         of (ty_id * ty_exp)
	  | DeclFun        of (id * field list * block * ty_exp)
	  | DeclProc       of (id * field list * block)
	  | DeclConst      of (id * const * ty_exp)
	  | DeclLocalConst of (id * const * ty_exp)
	  | DeclTagTable   of (tag_info) list
	withtype field        = {name:id,ty:ty_exp}
	     and choice       = {name:id,fields:field list}
             and enumer       = {name:id,value:int option}
	     and variant      = {tag:id,tag_ty:enumer list,choices:choice list}
             and field_init   = {name:id,init:exp}
             and variant_init = {tag:id,name:id,fields:field_init list}
             and clause       = {tag:const,body:stmt}
             and block        = {vars:field list,body:stmt list}
	     and tag_info     = (string * int)

	structure T = mkLangAst(type decls = decl list)
	open T
    end
