(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
Here is a straight forward implementation of the [[STMT_EXP]] signature.
 **)
structure StmtExp :> STMT_EXP =
  struct
    datatype ('ty,'id,'exp,'stmt) stmt_exp =
      RET  of 'exp
    | STMT of 'stmt
    | EXPR of ('id * 'ty) option -> 'stmt
    | EVAL of  (('ty,'id,'exp,'stmt) stmt_exp * 'ty *
		('exp -> ('ty,'id,'exp,'stmt) stmt_exp))
    | BIND of {vars: ('id * 'ty) list,
	       exps: ('ty,'id,'exp,'stmt) stmt_exp list,
	       body: 'id list -> ('ty,'id,'exp,'stmt) stmt_exp list}

    type ('ty,'id,'exp,'stmt) info =
                        {tmpId : unit -> 'id,
			isPure : 'exp -> bool,
			 expId : 'exp -> 'id option,
			 setId : 'id * 'exp -> 'stmt,
			 getId : 'id -> 'exp,
		      stmtScope: (('id * 'ty) list * 'stmt list) -> 'stmt}


(**:[[structure StmtExp]] [[flatten]] function:
The [[RET]] and [[STMT]] cases are easy. For [[RET]] assign
the expression to the variable proivded as an argument. A [[STMT]]
has no return value so it can just ignore it. 
**)
    fun flatten (info:('a,'b,'c,'d) info) (SOME (id,_)) (RET e) =
      ([],[(#setId info)(id,e)])
      | flatten info _ (STMT s) = ([],[s])
(**:[[structure StmtExp]] [[flatten]] function:
The [[EXPR]] case is easy too. Just apply the return argument to the
function carried by [[EXPR]].
**)   
      | flatten info ret (EXPR s) = ([],[s ret])
(**:[[structure StmtExp]] [[flatten]] function:
For [[EVAL]] check if we what we are evaluating is a pure expression.
If it is then use the pure expression as the value and flatten the
result. Otherwise call the helper [[flatten_eval]] which generates a
temporary variable to be use instead.
**)       
      | flatten (info as {isPure,...}) ret (EVAL (arg as (RET e,ty,b))) = 
      if (isPure e) then  flatten info ret (b e)
      else flatten_eval info ret arg
      | flatten info ret (EVAL arg) = flatten_eval info ret arg
(**:[[structure StmtExp]] [[flatten]] function:
The case for [[BIND]] is straight forward. It tries to avoid
generating extra temporaries by reusing variables when it
can. Ideally it should also try to fix any name capture problems.
It uses [[stmtScope]] to wrap each initialization expression in
it's own scope. 
**) 
    (* TODO play games to avoid name capture *)
      | flatten info res (BIND{vars,exps,body}) =
	let
	  val {stmtScope,expId,...} = info
	  fun getVar (RET e) = expId e
	    | getVar _ = NONE

	  fun mk_init ((var as (id,ty),exp),(ids,vars,stmts)) =
	    case (getVar exp) of
	      NONE => (id::ids,var::vars,
		       (stmtScope (flatten info (SOME var) exp))::stmts)
	    | (SOME id) => (id::ids,vars,stmts)

	  fun do_stmt e = stmtScope (flatten info res e)
	    
	  val (ids,vars,inits) =
	    List.foldr mk_init ([],[],[])  (ListPair.zip (vars,exps))
	  val stmts = inits@(List.map do_stmt (body ids))
	in
	  (vars,stmts) 
	end
      (* should check that e is pure before ignoring *)
      | flatten  _ NONE (RET e) = ([],[])
(**)
(**:[[structure StmtExp]] [[flatten_eval]] internal function:
[[flatten_eval]] is an internal function that introduces new temporaries.
**)
    and flatten_eval (info as {tmpId,getId,...}) ret (e,ty,b) =
      let
	val id = tmpId ()
	val (vars,stmt) = flatten info (SOME (id,ty)) e
	val (vars',stmt') = flatten info ret (b (getId id))
      in
	(((id,ty)::vars)@vars',stmt@stmt')
      end
(**)
  end
