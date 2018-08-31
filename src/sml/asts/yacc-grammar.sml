(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
structure YaccGrammar :> YACC_GRAMMAR =
  struct
    open LangIds
    datatype rule_atom = Term of terminal
                       | NonTerm of ty_id

    withtype action = string
      and terminal = id
      and rule = rule_atom list * action option
      and production = (ty_id * rule list)
      and grammar = production list

    structure T = mkLangAst(type decls = grammar)
      open T

      structure S = SplaySetFn
	(struct
	  type ord_key = VarId.id
	  val compare = VarId.compare
	end)

      fun terminals (productions:grammar) =
	let
	  fun add_rule_atom (Term t,s) = S.add(s,t) 
	    | add_rule_atom (_,s) = s
	  fun add_rule ((ras,_),s) =  List.foldl add_rule_atom s ras
	  fun add_production ((_,rs),s) =
	    List.foldl add_rule s rs
	  val terms =
	    List.foldl add_production S.empty productions
	in
	  S.listItems terms
	end

   end



