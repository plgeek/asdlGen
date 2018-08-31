(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature YACC_GRAMMAR =
  sig
    include LANG_IDS
    datatype rule_atom = Term of terminal
                       | NonTerm of ty_id
    withtype action = string
      and terminal = id
      and rule = rule_atom list * action option
      and production = (ty_id * rule list)
      and grammar = production list
    include LANG_AST where type decls = grammar

    val terminals : grammar -> terminal list
   end



