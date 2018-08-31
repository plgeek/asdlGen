(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


signature EXTERNAL_PROGRAMS =
    sig

	val cc: {include_path: string list,
		 library_path: string list,
		       inputs: string list,
		         rest: string list} -> OS.Process.status

	val cxx:{include_path: string list,
		 library_path: string list,
		       inputs: string list,
		         rest: string list} -> OS.Process.status

	val ocaml_batch: {inputs: string list,
		     search_path: string list} -> OS.Process.status

	val sml_batch: {inputs: string list,
		       cm_path: string list} -> OS.Process.status

	val javac: {class_path: string list,
		       inputs: string list,
		         rest: string list} -> OS.Process.status

	val icont: {ipath: string list,
		   inputs: string list,
		     rest: string list} -> OS.Process.status

	val haskell: {haskell_path: string list,
		        inputs: string list} -> OS.Process.status


	val rm: string list -> OS.Process.status

    end

signature SUPPORT_FILES =
    sig
	val c_includes     : string list
	val c_libs         : string list

	val cxx_includes   : string list
	val cxx_libs       : string list

	val java_classes   : string list
	val cm_path        : string list
	val haskell_path   : string list
	val ocaml_path     : string list
	val icon_ucode     : string list
    end

structure SupportFiles: SUPPORT_FILES =
    struct

	fun mk_path x =
	    [OS.Path.toString {isAbs=false,
			      vol="",
			      arcs=OS.Path.parentArc::x}]

	val c_includes =  (mk_path ["c"])@   ["/usr/local/include"]
	val c_libs =  mk_path ["c"]@ ["/usr/local/lib"]

	val cxx_includes = mk_path ["c"]
	val cxx_libs = mk_path ["cxx"]

	val java_classes = [] (*mk_path ["java"]*)

	val cm_path = [] (* mk_path ["sml","base"]*)
	val icon_ucode = [] (* mk_path ["icon"] *)
	val haskell_path = (* (mk_path ["haskell"])@*)
	  ["/usr/local/share/hugs/lib"]
	val ocaml_path = [] (* (mk_path ["ocaml"]) *)
    end

structure UnixExternalProgs : EXTERNAL_PROGRAMS =
    struct
	val cc_prg  = "gcc"
	val cxx_prg = "g++"
	val javac_prg = "javac"
	val haskell_prg = "hugs +q -c1000"
	val icont_prg = "icont"
	val sml_prg = "sh ../misc/sml-batch"
	val ocaml_prg = "sh ../misc/ocaml-batch"
	fun prefix x s = x^s

	fun run cmd = OS.Process.system ((*"echo " ^ *)cmd )

	val shpath = ListFormat.fmt
	    {init="",final="",sep=":",fmt=(fn x => x)} 

	fun join s =
	    String.concat (List.map (prefix " ") s)

	fun cc_compiler prog {include_path,library_path,
			      inputs,rest} =
	    let
		val dash_I = List.map (prefix "-I") include_path
		val dash_L = List.map (prefix "-L") library_path
		val cmd = join (prog::(dash_I@dash_L@rest@inputs))
	    in
		run cmd
	    end

	val  cc = cc_compiler cc_prg
	val cxx = cc_compiler cxx_prg

	(* call a hacked shell script that does the right thing *)
	fun sml_batch {cm_path,inputs} =
	    let
		val cm_path = (shpath cm_path)
		val cmd = (join (sml_prg::inputs))
	    in run cmd
	    end

	fun ocaml_batch {search_path,inputs} =
	  let val dash_I = String.concat (List.map (prefix " -I ") search_path)
	    val cmd = (join (ocaml_prg::("'"^dash_I^"'")::inputs))
	    in run cmd
	  end

	fun haskell {haskell_path,inputs} =
	    let
		val haskell_path = "-P"^(shpath haskell_path)
		    
		val cmd = (join (haskell_prg::"+."::haskell_path::inputs))
	    in run (cmd ^ " </dev/null")
	    end

	fun javac {class_path,inputs,rest} = 
	    let
		val env = "env CLASSPATH="^
		    (shpath (class_path@["${CLASSPATH}"]))
		val cmd =
		    join (env::javac_prg::(inputs@rest))
	    in	run cmd
	    end
	fun icont {ipath,inputs,rest} = 
	    let
		val env = "env IPATH="^
		    (shpath (ipath@["${IPATH}"]))
		val cmd =
		    join (env::icont_prg::(rest@inputs))
	    in run cmd
	    end

	fun rm s = run (join ("rm -rf"::s))
    end

structure Test =
    struct
	structure P = UnixExternalProgs
	structure S = SupportFiles

	structure Set = ListSetFn(struct
				      type ord_key = String.string
				      val compare = String.compare
				  end)

	fun cmp_paths ([],[]) = EQUAL
	  | cmp_paths (x::xs,[]) = GREATER
	  | cmp_paths ([],y::ys) = LESS
	  | cmp_paths (x::xs,y::ys) =
	    case (String.compare (x,y)) of
		EQUAL => (cmp_paths(xs,ys)) 
	      | x => x
		
	fun remove_dups outs = ListMergeSort.uniqueSort String.compare outs

	fun get_files  "" outs = remove_dups outs
	  | get_files s outs =
	    let
		fun is_type x =
		    case ((OS.Path.ext o OS.Path.file) x) of
			NONE => false
		      | SOME x => x = s
	    in remove_dups (List.filter is_type outs)
	    end
	
	fun java_comp i =
	    let
		val outs = get_files "java" i
		val dirs = Set.addList (Set.empty,List.map OS.Path.dir outs)
		val class_path = (Set.listItems dirs)@S.java_classes
	    in P.javac{class_path=class_path,inputs=outs,
		       rest=["-nowrite"]}
	    end
	fun icon_comp i =
	    let
		val outs = get_files "icn" i
		val dirs = Set.addList (Set.empty,List.map OS.Path.dir outs)
		val ipath = (Set.listItems dirs)@S.java_classes
	    in P.icont{ipath=ipath,inputs=outs,
		       rest=["-o /dev/null"]}
	    end

	fun sml_comp i =
	    let
		val strs = get_files "sig" i
		val sigs = (get_files "sml" i)
		val outs = strs@sigs
		val cm_path = S.cm_path
	    in P.sml_batch{cm_path=cm_path,inputs="smlnj-lib.cm"::outs}
	    end

	fun ocaml_comp i =
	  let
	    val outs = get_files "" i
	    val dirs = Set.addList (Set.empty,List.map OS.Path.dir outs)
	    val search_path  = (Set.listItems dirs)@S.ocaml_path
	  in P.ocaml_batch{search_path=search_path,inputs=outs}
	  end

	fun haskell_comp i =
	    let
		val outs = get_files "" i
		val haskell_path = S.haskell_path
	    in
		P.haskell{haskell_path=haskell_path,inputs=outs}
	    end
	fun c_comp i =
	    P.cc{include_path=S.c_includes,
		 library_path=S.c_libs,
		 inputs=(get_files "c" i),
		 rest=["-fsyntax-only",
		       "-ansi",
		       "-pedantic","-Wall"]}

	fun cxx_comp i =
	    P.cxx{include_path=S.cxx_includes,
		  library_path=S.cxx_libs,
		  inputs= (get_files "cxx" i),
		  rest=["-fsyntax-only","-Wall"]}

	fun do_lang (f,s) args =
	  (f  (Export.run_it("",("--lang="^s)::args)))
	    
	val do_java = do_lang(java_comp,"java")
	val do_c =    do_lang(c_comp,"ansi-c")
	val do_cxx =  do_lang(cxx_comp,"cxx")
	val do_sml =  do_lang(sml_comp,"sml")
	val do_haskell =  do_lang(haskell_comp,"haskell")
	val do_icon =  do_lang(icon_comp,"icon")
	val do_ocaml = do_lang(ocaml_comp,"ocaml")
	val keep_going = ref false
	fun test (name,f,i) () = (name,((f i) = OS.Process.success) orelse
				  (!keep_going))

	fun test_all n i =
	  let val i = ("-d"::"../asdl/tests/work"::i)
	  in
	    [test (n^"-ocaml",do_ocaml,"--view=OCaml"::i),
	     test (n^"-ml",do_sml,"--view=SML"::i),
	     test (n^"-hs",do_haskell,"--view=Haskell"::i),
	     test (n^"-c",do_c,"--view=C"::i),
	     test (n^"-cxx",do_cxx,"--view=Cxx"::i),
	     test (n^"-java",do_java,"--view=Java"::i),
	     test (n^"-icon",do_icon,"--view=Icon"::i)]
	  end
	    
	fun run_test s =
	    let
		fun check f =
		    let
			val (n,t) = f ()
			val msg =
			    [if t then "ok    : " else "**fail: ",n,"\n"]
		    in
			if t then Error.say (String.concat msg)
			else raise Error.error msg
		    end
	    in List.app check s
	    end

	fun mk_path x =
	    OS.Path.toString {isAbs=false,
			      vol="",
			      arcs=OS.Path.parentArc::
			           "asdl"::"tests"::x}
	val modTest1 =
	    test_all "all test" [mk_path ["modTest","all.asdl"]]
	val modTest2 =
	    (test_all "modTest"
	     [mk_path ["modTest","stm.asdl"],
	      mk_path ["modTest","exp.asdl"],
	      mk_path ["modTest","op.asdl"],
	      mk_path ["modTest","pos.asdl"]]) 

	val big_int_test =
	    (test_all "generic.asdl"  [mk_path ["generic.asdl"]])
	val asdl_test =
	    (test_all "asdl.asdl"  [mk_path ["asdl.asdl"]])

	val pattern_test =
	    (test_all "pattern.asdl"  [mk_path ["pattern.asdl"]])

	val slp_test =
	    (test_all "slp.asdl"  [mk_path ["slp.asdl"]])

	val slp3_test =
	    (test_all "slp3.asdl"  [mk_path ["slp3.asdl"]])

	val views_test =
	    (test_all "views.asdl"  [mk_path ["views.asdl"]])

	val views_test' =
	    (test_all "views.asdl'"  ["--base_include=foo.h",
				      mk_path ["views.asdl"]])

	val zsuif_test =
	    (test_all "zsuif.asdl"  [mk_path ["zsuif.asdl"],
				     mk_path ["..","std-types.asdl"]])
	val rcc_test =
	    (test_all "rcc.asdl"  [mk_path ["rcc.asdl"]])

	val cii_test =
	    [test ("cii",do_c,["--view=C",
				mk_path ["slp3.asdl"]])]

	val toplevel_test =
	  (test_all "toplevel"  [mk_path ["toplevel.asdl"]])

	val tests =
	    toplevel_test@
	    big_int_test@
	    slp_test@
	    slp3_test@
	    asdl_test@
	    modTest2@
	    pattern_test@
	    rcc_test@
	    views_test(*@zsuif_test*)

	fun do_it () = run_test tests
(*    	val _ = do_it()*)
    end
    
	

