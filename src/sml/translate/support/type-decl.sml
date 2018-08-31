(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(**::
The functor [[mkTypeDecl]] generates a structure that matches the
[[TYPE_DECL]] signature. The functor is parameterized by all the
target language specific details.
**)
functor mkTypeDecl(structure TypeId : SOURCE_ID
		   structure VarId : SOURCE_ID
		   type ty_exp
		   type tag
		   type exp) : TYPE_DECL =
  struct
    structure TypeId = TypeId
    structure VarId = VarId
    structure Env = SplayMapFn
      (struct type ord_key = TypeId.id
	      val compare = TypeId.compare
      end)
    type id = VarId.id
    type ty_id = TypeId.id
    type ty_exp = ty_exp
    type tag = tag
    type exp  = exp
    datatype info =
      Rd of string * exp
    | Wr of string * (exp -> exp)
    type ty_info = info list
    datatype ty =
      Prim of {ty : ty_exp,
	     name : string,
	     info : ty_info}
    | Prod of {ty : ty_exp,
	     info : ty_info,
	   fields : field list,
	    cnstr : exp list -> exp,
	    match : (match list -> exp) -> exp -> exp}
    | Sum  of {ty : ty_exp,
	     info : ty_info,
       num_attrbs : int,
	   cnstrs : con list,
	   match  : (choice -> exp) -> exp -> exp}
    | App   of (ty_con * ty_id)
    | Alias of (ty_id)

    withtype field   = {label : id option,label': id, tid : ty_id}
         and match   = (field * exp)
         and choice  = (tag * match list)
         and con = {tag : tag,
		  fields: field list,
                  cnstr : exp list -> exp}
         and ty_decl = (ty_id * ty)
         and ty_con =  ty_decl -> (ty_exp * ty_info)
    type env = ty Env.map
    exception Bad
    fun find get v [] = NONE
      | find get v (x::xs) = ((SOME (get(v,x))) handle Bad => find get v xs)
      
    fun add f x xs = (f x)::xs
    fun get_rd (s,Rd(s',x)) = if s = s' then x else raise Bad
      | get_rd _ = raise Bad

    fun get_wr (s,Wr(s',x)) = if s = s' then x else raise Bad
      | get_wr _ = raise Bad

    val getRd = find get_rd
    val getWr = find get_wr

    val addWr = add Wr
    val addRd = add Rd
    fun addRdWr s {rd=NONE,wr=NONE} x = x
      | addRdWr s {rd=SOME rd,wr=SOME wr} x = (addRd(s,rd) (addWr(s,wr) x))
      | addRdWr s {rd=NONE,wr=SOME wr} x = (addWr(s,wr) x)
      | addRdWr s {rd=SOME rd,wr=NONE} x = (addRd(s,rd) x)

    fun mk_env x = List.foldl Env.insert' Env.empty x
    fun lookup (e,x) = Env.find(e,x)
    fun add_env x = Env.insert' x
    val noInfo = []:ty_info
    fun merge (x,y) = x @ y
  end



