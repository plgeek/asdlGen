(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature ANSI_C =
    sig
	include LANG_IDS
	
	datatype ty_prim   =   VOID | INT | CHAR

	and ty_exp         =
	    TyPrim         of (ty_prim)
	  | TyId           of (ty_id)
	  | TyPointer      of (ty_exp)
	  | TyArray        of (ty_exp * int option)
	  | TyEnum         of (id option * enumer list)
	  | TyFunctionPtr  of (field list * ty_exp)
	  | TyAggregate    of (aggregate * id option * field list)
	  | TyQualified    of (qualifier * ty_exp)
	  | TyGroup        of (ty_exp)
	  | TyAnnotate     of (string * ty_exp)
	    
	and ty_dec =
	    TyDec          of (ty_id * ty_exp)
	  | TyAggregateDec of (aggregate * id * field list)
	  | TyEnumDec      of (ty_id * enumer list)

	and qualifier     = Const | Voliatile

	and aggregate     = Union | Struct
	    
	and storage_class = Auto | Registier | Static | Extern 

	and var_dec       =
	    VarDecs       of (storage_class option * ty_exp * id * id list)
	  | VarDecInit    of (storage_class option * ty_exp * id * exp)

	and fun_dec =
	     FunPrototypeDec of (id * field list * ty_exp)
	  |  FunDec          of (id * field list * ty_exp * block)
	  |  FunStaticDec    of (id * field list * ty_exp * block)

	and decl =
	    Var of (var_dec)
	  | Fun of (fun_dec) 
	  | Ty  of (ty_dec)
	  | Com of (string)
	  | TagTable of (string * int) list
	and const_exp = I of (int) | C of (char) | E of (id)
	  | Void | NULL  | A of id | S of (string)

	and unary_op  = NEG | NOT | DEREF  | ADDR

	and binary_op =
	    BLSHIFT | BRSHIFT | BAND  | BOR | BXOR | BNOT
	  | PLUS    | SUB     | MUL   | DIV | MOD
	  | EQ      | GT      | LT    | NEQ | GEQ  | LEQ
	  | LAND    | LOR
	    
	and exp =
	    Constant of (const_exp)
	  | Variable of (id)
	  | Call     of (exp * exp list)
	  | Assign   of (exp * exp)
	  | Unop     of (unary_op * exp)
	  | Binop    of (binary_op * exp * exp)
	  | Cast     of (ty_exp * exp)
	  | AggarSub of (exp * id)
	  | ArraySub of (exp * exp)
	  | Comment  of (string)
	  | Sizeof   of (exp)
	  | SizeofT  of (ty_exp)
	  | ExpSeq   of (exp list)
	  | IfExp    of {test:exp,then_exp:exp,else_exp:exp}
	  | ExpGroup of (exp)

	and stmt =
	    Nop
	  | Break
	  | Continue
	  | Exp    of (exp)
	  | If     of {test:exp,then_stmt:stmt,else_stmt:stmt}
	  | For    of {init:exp,test:exp,step:exp,body:stmt}
	  | While  of {test:exp,body:stmt}
	  | Label  of (id)
	  | Goto   of (id)
	  | Return of (exp)
	  | Block  of (block)
	  | Switch of {test:exp,
		       body:switch_arm list,
		       default:stmt}
	    
	and switch_arm =
	    CaseInt of (int * stmt list)
	  | CaseEnum of (id * stmt list)
	
	withtype field   = {name:id,ty:ty_exp}
	    and  enumer  = {name:id,value:int option}
	    and  block   = {ty_decs:ty_dec list,
			   var_decs:var_dec list,
			    stmts:stmt list}

        include LANG_AST where type decls = decl list
    end



