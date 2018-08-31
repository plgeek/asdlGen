(**::
Here's the glue code for the lexer and parser built by
[[ml-lex]] and [[ml-yacc]]. The [[structure AsdlParser]] exports one
function [[parse]] which takes a list of input files parses them and
returns a list of toplevel declarations tagged with the file from
which it came. 
**)
(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
signature ASDL_PARSER =
  sig
    val parse: string list -> {file:string,decl:Asdl.decl} list
  end 
structure AsdlParser :> ASDL_PARSER =
  struct
    structure T = Asdl
    structure AsdlLrVals =
      AsdlLrValsFun(structure Token = LrParser.Token
		    structure T = T)
    structure AsdlLex =
      AsdlLexFun(structure Tokens = AsdlLrVals.Tokens)
    structure Parser =
      JoinWithArg(structure Lex= AsdlLex
		  structure LrParser = LrParser
		  structure ParserData = AsdlLrVals.ParserData)
    fun parse_one (f,acc) =
      let
	val f = OS.Path.mkCanonical f
	val (cls,ins) = 
	  if ((String.size f) > 0) then (TextIO.closeIn,TextIO.openIn f)
	  else ((fn _ => ()),TextIO.stdIn) (* empty file is stdIn *)
	val sm = SourceMap.newmap(1,{fileName=f,line=1,column=0})

	fun err (x,y) s =
	  let
	    val xpos = SourceMap.filepos sm x
	    val ypos = SourceMap.filepos sm y
	    fun pos2str {fileName,line,column} =
	      String.concat [Int.toString line,".", Int.toString column]
	    fun pos2fname {fileName,line,column} = fileName
	    val msg = String.concat
	      [pos2fname xpos,":",pos2str xpos, "-",
	       pos2str ypos,": ",s,"\n"]
	  in TextIO.output(TextIO.stdErr,msg)
	  end

	val stream = Parser.makeLexer
	  (fn i => TextIO.inputN(ins,i)) {sourceMap=sm, err=err}
	fun error (e,x,y) = err (x,y) e

	val (decls,_) = Parser.parse(30,stream,error,())
(*	val _ = List.app (fn d => AsdlUtil.sexp_wr_decl d TextIO.stdErr) decls
*)
      in cls ins; List.foldl (fn (d,ds) => {file=f,decl=d}::ds) acc decls
      end
    fun parse f = List.foldl parse_one [] f
  end
