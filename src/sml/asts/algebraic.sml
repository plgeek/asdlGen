(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure AlgebraicAst :  ALGEBRAIC_AST =
    struct
	open LangIds
	    
	datatype ty_exp =
	    TyId        of ty_id
	  | TyList      of ty_exp
	  | TyVector    of ty_exp
	  | TyTuple     of ty_exp list
	  | TyRecord    of field list
	  | TyFunction  of ty_exp list * ty_exp
	  | TyOption    of ty_exp
	  | TySequence  of ty_exp
	  | TyCon       of ty_id * ty_exp list

	and exp =
	    Id      of (id)
	  | Int     of (int)
	  | Str     of (string)
	  | Call    of (exp * exp list)
	  | Cnstr   of (id * exp)
	  | Tuple   of (exp list * ty_id option)
	  | Record  of (exp list * field list * ty_id option)
	  | Match   of (id * clause list)
	  | LetBind of (clause list * exp) 
	  | Let     of (clause list * exp)
	  | Seq     of (exp list)
	  | Error   of (id * string)
	and match =
	    MatchRecord of match list * field list  * ty_id option
	  | MatchTuple  of match list * ty_exp list * ty_id option
	  | MatchCnstr  of match      * cnstr	    
	  | MatchId     of id         * ty_exp 
	  | MatchStr    of string
	  | MatchInt    of int
	  | MatchAny 

	and decl =
	    DeclTy  of (ty_id * ty_exp)
	  | DeclSum of (ty_id * cnstr list)
	  | DeclVar of (id * exp * ty_exp)
	  | DeclFun of (id * field list * exp * ty_exp )

	withtype cnstr = {name:id,ty_arg:ty_exp}
	    and field  = {name:id,ty:ty_exp}
	    and clause = (match * exp)
	structure T = mkLangAst(type decls = decl list)
	open T
    end




