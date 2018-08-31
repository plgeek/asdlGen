(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure Export =
    struct

	structure O = CommandOptions
	val (opts,version)  = O.boolFlag O.empty
	  {name="version",
	   flags="v",
	   v=true,
	   dflt=false,
	   doc="print version and exit"}

	val (opts,help)  = O.boolFlag opts
	  {name="help",
	   flags="h",
	   v=true,
	   dflt=false,
	   doc="print help and exit"}
	  
	val (opts,lang_name)  =  O.stringParam opts
	  {name="lang",
	   flags="l",
	   arg_dflt=NONE,
	   dflt="",
	   advice="driver",
	   doc="output language"}

	type prog = (string * O.args_spec * (O.args -> string list))
	local
	open Link
	in
	  val langs = [("doc",HTML.opts,HTML.do_it),
		       ("dtd",XMLDTD.opts,XMLDTD.do_it),
		       ("yacc",YaccGrammar.opts,YaccGrammar.do_it),
		       ("sml",SML.opts,SML.do_it),
		       ("ocaml",OCaml.opts,OCaml.do_it),
		       ("ansi-c",AnsiC.opts,AnsiC.do_it),
		       ("java",Java.opts,Java.do_it),
		       ("haskell",Haskell.opts,Haskell.do_it),
		       ("icon",Icon.opts,Icon.do_it),
		       ("cxx",CPlusPlus.opts,CPlusPlus.do_it)]
	  val opts = List.foldl (fn ((_,opts,_),opts') => O.layer(opts,opts'))
	             opts langs
	  fun usage () =
	    let val msg = O.mkUsage opts
	      (String.concat ["Usage: asdlGen [options] files ...\n",
			      "Generate code from ASDL specifications\n"])
		val rest =
		  ListFormat.fmt{init="\nDrivers:\n  ",
				 fmt=(fn x => x),
				 sep=", ",
				 final="\n"} (List.map #1 langs)
	    in print msg;print rest
	    end
	  fun get_prog args =
	    let
	      val l = lang_name args
	      fun loop ((x,_,f)::xs) = 
		if x = l then (f args) else loop xs
		| loop [] = []
	    in if (help args) then (usage();[])
	       else if (version args) then
		 (print (ConfigPaths.version^"\n");[])
	        else (loop langs)
	    end
	end

	fun run_it (name, args) =
	  case (O.mkCmd opts get_prog (name,args)) of
	    NONE => (usage();[])
	  | SOME x => x

	fun asdlGen (name,args) =
	  (run_it (name,args);OS.Process.success) handle e =>
	    (Error.say ("Error: "^(exnMessage e)^"\n");
	     OS.Process.failure)

    end


