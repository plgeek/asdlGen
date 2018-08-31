(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature YACC_GRAMMAR_PP =
  sig
    structure Ast : YACC_GRAMMAR
      include CODE_PP where type code = Ast.decls
  end

structure YaccGrammarPP : YACC_GRAMMAR_PP =
    struct 
	structure Ast  = YaccGrammar
	structure PP = PPUtil
	type code = Ast.decls
	val opts = CommandOptions.empty
	fun mkComment _ = PP.empty
	fun mkDeps _ = PP.empty
	open Ast

	val pp_tid = PP.wrap (TypeId.toString' "_")
	val pp_id = PP.wrap (VarId.toString' "_")
	val bar_sep = PP.cat [PP.ws, PP.s "| "]
	fun pp_terminal t = pp_id t
	and pp_rule_atom (Term t) = pp_terminal t
	  | pp_rule_atom (NonTerm ty) = pp_tid ty
	and pp_rule (ra,_) =
	  PP.box 4 
	  [PP.seq' {fmt=pp_rule_atom,sep=PP.ws,empty=PP.s "/* empty */"} ra]
	and pp_production (ty,rule) =
	  PP.cat [pp_tid ty,PP.s ":",
		  PP.box 4 [PP.seq {fmt=pp_rule,sep=bar_sep} rule,PP.s ";"]]
	and pp_grammar grammar =
	    let
	      val terminals = terminals grammar
	      val terminals = List.map (VarId.toString' "_") terminals
	      val terminals = ListMergeSort.sort String.< terminals
	      fun pp_term_decl t = PP.s ("%token "^t)
	    in
	      PP.cat [PP.seq_term{fmt=pp_term_decl,sep=PP.nl} terminals,
		      PP.s "%%",PP.nl,
		      PP.seq_term {fmt=pp_production,sep=PP.nl} grammar,
		      PP.s "%%",PP.nl]
	    end
	  
	fun pp_code _ (decls) =
	  let
	    fun file_name () =
	      OS.Path.joinBaseExt{base="out",ext=SOME "yacc"}
	  in [FileSet.mkFile
	      {name=file_name () ,body=pp_grammar decls,depends=[]}]
	  end

    end







