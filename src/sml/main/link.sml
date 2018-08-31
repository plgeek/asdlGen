(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
structure Link =
    struct
      structure LF = LibFiles
      structure HTMLGen =
	mkTranslateFromFn
	(val set_dir = true
	 val opts = CommandOptions.empty
	 val do_it = FormatTranslator.do_it
	 structure G =
	   mkSourceFileOutput(structure PP = HTMLPP
			      val file_set = FileSet.empty))
	   
      structure HTML =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = HTMLGen
	       val dflt_view = "Doc")

      structure XMLDTDGen =
	mkTranslateFromTranslator
	(structure T = XMLDTDTranslator
	 structure G = mkSourceFileOutput(structure PP = XMLDTDPP
					  val file_set = FileSet.empty))
	   
      structure XMLDTD =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = XMLDTDGen
	       val dflt_view = "DTD")
	
      structure YaccGrammarGen =
	mkTranslateFromTranslator
	(structure T = YaccGrammarTranslator
	 structure G = mkSourceFileOutput(structure PP = YaccGrammarPP
					  val file_set = FileSet.empty))

      structure YaccGrammar =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = YaccGrammarGen
	       val dflt_view = "Yacc")
	
      structure SMLAlgebraicSpec =
	mkAlgebraicSpec(structure Ty = AlgebraicTy
			val get_attribs = true
			val ignore_labels = false
			val prim_ty = {intT="int",stringT="string"}
			val streams_ty = NONE
			val monad_cons = NONE)
	
      structure SMLTranslator =
	mkAlgebraicSemantTranslator
	(structure Spec = SMLAlgebraicSpec
	 val fix_fields = false)
	   
      structure SMLGen =
	mkTranslateFromTranslator
	(structure T = SMLTranslator
	 structure G = mkSourceFileOutput(structure PP = SMLPP
					  val file_set = LF.sml))

      structure SML =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = SMLGen 
	       val dflt_view = "SML")

      structure OCamlAlgebraicSpec =
	mkAlgebraicSpec(structure Ty = AlgebraicTy
			val get_attribs = false
			val ignore_labels = true
			val prim_ty = {intT="int",stringT="string"}
			val streams_ty = NONE
			val monad_cons = NONE)
	
      structure OCamlTranslator =
	mkAlgebraicSemantTranslator
	(structure Spec = OCamlAlgebraicSpec
	 val fix_fields = false)
	   
      structure OCamlGen =
	mkTranslateFromTranslator
	(structure T = OCamlTranslator
	 structure G = mkSourceFileOutput(structure PP = OCamlPP
					  val file_set = LF.ocaml))

      structure OCaml =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = OCamlGen 
	       val dflt_view = "OCaml")
	
       structure HaskellAlgebraicSpec =
	 mkAlgebraicSpec(structure Ty = AlgebraicTy
			 val get_attribs = false
			 val ignore_labels = true
			 val prim_ty = {intT="Int",stringT="String"}
			 val streams_ty = 
			   SOME {ins="Instream",outs="Outstream"}
			 val monad_cons = SOME {inm="InIO",outm="OutIO"})
	 
       structure HaskellTranslator =
	 mkAlgebraicSemantTranslator
	 (structure Spec = HaskellAlgebraicSpec
	  val fix_fields = true)

       structure HaskellGen =
	   mkTranslateFromTranslator
	   (structure T = HaskellTranslator
	    structure G = mkSourceFileOutput(structure PP = HaskellPP
					     val file_set = LF.hs))

       structure Haskell =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = HaskellGen
		val dflt_view = "Haskell")
	 
       structure AnsiCAlgolSpec =
	 mkAlgolSpec(structure Ty = AlgolTy)
       structure AnsiCTranslator =
	 mkAlgolSemantTranslator(structure Spec = AnsiCAlgolSpec)
	 
       structure AnsiCGen =
	 mkTranslateFromTranslator
	 (structure T = AnsiCTranslator
	  structure G = mkSourceFileOutput(structure PP = AnsiCPP
					   val file_set = LF.c))

       structure AnsiC =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = AnsiCGen
		val dflt_view = "C")
	 
       structure JavaOOSpec =
	 mkOOSpec(structure Ty = OOTy
		  val streams_ty =
		    SOME {ins="java.io.InputStream",
			  outs="java.io.OutputStream"}
		  val int_ty = "java_int"
		  val int_kind = true)
       structure JavaTranslator =
	 mkOOSemantTranslator(structure Spec = JavaOOSpec)

       structure JavaGen =
	   mkTranslateFromTranslator
	   (structure T = JavaTranslator
	    structure G = mkSourceFileOutput(structure PP = JavaPP
					     val file_set = LF.java))
	      
       structure Java =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = JavaGen
		val dflt_view = "Java")
	 
       structure CPlusPlusOOSpec = 
	 mkOOSpec(structure Ty = OOTy
		  val streams_ty = NONE
		  val int_ty = "int"
		  val int_kind = false)

       structure CPlusPlusTranslator =
	 mkOOSemantTranslator(structure Spec = CPlusPlusOOSpec)
	 
       structure CPlusPlusGen =
	   mkTranslateFromTranslator
	   (structure T = CPlusPlusTranslator
	    structure G = mkSourceFileOutput(structure PP = CPlusPlusPP
					     val file_set = LF.cxx))

       structure CPlusPlus =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = CPlusPlusGen
		val dflt_view = "Cxx")	   

       structure IconDynamicSpec =
	 mkDynamicSpec(structure Ty = DynamicTy)

      structure IconTranslator =
	mkDynamicSemantTranslator(structure Spec = IconDynamicSpec)
	   
      structure IconGen =
	mkTranslateFromTranslator
	(structure T = IconTranslator
	 structure G = mkSourceFileOutput(structure PP = IconPP
					     val file_set = LF.icon))

      structure Icon =
	mkMain(structure S = Semant
	       structure Parser = AsdlParser
	       structure Gen = IconGen 
	       val dflt_view = "Icon")


(*
       structure Check =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen =  mkDependGen(structure S = S)
		val dflt_view = "Check")	   

       structure TypePickler =
	 mkMain(structure S = Semant
		structure Parser = AsdlParser
		structure Gen = GenPickleTranslator
		val dflt_view = "Typ")	   


*)
    end
