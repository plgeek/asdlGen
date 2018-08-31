(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure OOAst: OO_AST =
    struct
	open LangIds

	datatype ty_exp =
	    TyVoid
	  | TyId         of (ty_id)
	  | TyArray      of (ty_exp * int option)
	  | TyReference  of (ty_exp)
	  | TyOption     of (ty_exp)
	  | TySequence   of (ty_exp)

	and ty_decl =
	    DeclAbstractClass of
	    {name: ty_id,
	   idecls: inner_decl list,
	    scope: scope,
	 inherits: ty_id option,
	   fields: mfield list, 
	     mths: mth list}

	  | DeclClass of
	    {name: ty_id,
	    final: bool,
	   idecls: inner_decl list,
	    scope: scope,
         inherits: ty_id option,
           cnstrs: cnstr list,
           fields: mfield list, 
             mths: mth list}

	  | DeclFun of
	    {name:id,
	     inline:bool,
	     public:bool,
	     args:field list,
	     ret:ty_exp,
	     body:block}

	  | DeclConst of {field:field,public:bool,value:exp}

	and inner_decl =
	    IDeclEnum of {name:ty_id,enums:enumer list}

	and mth = 
	    MthAbstract of
	    {name:id,
	     mods:modifiers,
	     args:field list,
	      ret:ty_exp}
	  | Mth of
	    {name:id,
           inline:bool,
	     mods:modifiers,
	     args:field list,
	      ret:ty_exp,
	     body:block}

	and const =
	    IntConst of (int)
	  | EnumConst of (ty_id * id)
	  | VarConst of (id)

	and exp =
	    NilPtr
	  | This
	  | Id         of (id)
	  | Const      of (const)
	  | ThisId     of (id)
	  | MthCall    of (exp * exp list)
	  | SMthCall   of (ty_id * id * exp list)
	  | FunCall    of (id * exp list)
	  | FieldSub   of (exp * id)
	  | ArraySub   of (exp * exp) 
	  | DeRef      of (exp)
	  | PlusOne    of (exp)
	  | MinusOne   of (exp)
	  | NotEqConst of (exp * const)
	  | NotNil     of (exp)
	  | NotZero    of (exp)
	  | Less       of (exp * exp)
	  | Cast       of (ty_exp * exp)
	  | New        of (ty_id * exp list)

	  | SeqNew     of {elm_ty:ty_exp, len:exp}
	  | SeqLen     of {elm_ty:ty_exp, seq:exp}
	  | SeqGet     of {elm_ty:ty_exp, seq:exp, idx:exp}
	  | SeqSet     of {elm_ty:ty_exp, seq:exp, idx:exp, v:exp}

	  | OptNone    of (ty_exp)
	  | OptSome    of (ty_exp * exp)
	  | OptIsSome  of (ty_exp * exp)
	  | OptGetVal  of (ty_exp * exp)

	and stmt =
	    Nop
	  | Expr    of (exp)
  	  | Assign  of (exp * exp)
	  | If      of {test:exp,then_stmt:stmt,else_stmt:stmt}
	  | While   of {test:exp,body:stmt}
	  | Case    of {test:exp,clauses:clause list,default:stmt}
	  | Block   of (block)
	  | Return  of (exp)
	  | Die     of (string)

	and scope = Public | Private | Protected

	withtype enumer  = {name:id,value:int option}
           and field     = {name:id,ty:ty_exp}
	   and block     = {vars:field list,body:stmt list}
	   and cnstr     = {inline:bool,scope:scope,
			    args:field list,body:block}
	   and modifiers = {scope:scope,static:bool,final:bool}
	   and mfield    = {mods:modifiers,field:field}
	   and clause    = {tag:const,body:stmt}

	structure T =  mkLangAst(type decls = ty_decl list)
	open T
	structure Env =
	    SplayMapFn(struct
			   type ord_key = TypeId.id
			   val compare = TypeId.compare
		       end)
	fun add_methods x =
	    let
		val env =
		    List.foldl (fn ((tid,mth),env)   =>
				case (Env.find(env,tid)) of
				    NONE => Env.insert(env,tid,[mth])
				  | SOME mths => 
					Env.insert(env,tid,mth::mths))
		    Env.empty x

		fun add_mths
		    (c as  (DeclAbstractClass
			    {name,idecls,scope,inherits,fields,mths}),rest) =
		    (case (Env.find (env,name)) of
			 NONE => c
		       | SOME m =>
			     (DeclAbstractClass
			      {name=name,idecls=idecls,
			       scope=scope,inherits=inherits,
			       fields=fields,mths=m@mths}))::rest
		  | add_mths
			 (c as (DeclClass
				{name,final,idecls,scope,inherits,
				 cnstrs,fields,mths}),rest) =
			 (case (Env.find (env,name)) of
			      NONE => c
			    | SOME m =>
				  (DeclClass
				   {final=final,
				    cnstrs=cnstrs,
				    name=name,idecls=idecls,
				    scope=scope,inherits=inherits,
				    fields=fields,mths=m@mths}))::rest
		  | add_mths (x,rest) = x::rest
	    in
		 List.foldr add_mths []
	    end

    end