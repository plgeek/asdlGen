(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature CODE_PP =
  sig
    type code
    val opts     : CommandOptions.args_spec
    val mkComment: string list -> PPUtil.pp
    val mkDeps   : (string list * string list list) list -> PPUtil.pp
    val pp_code: CommandOptions.args -> code -> FileSet.file list
  end

functor mkSourceFileOutput (structure PP : CODE_PP
			    val file_set : FileSet.file_set) : TRANSLATE =
    struct
	structure Out = TextIOFileOutput
	structure O = CommandOptions
	type input = PP.code list 
	type output = Out.output
	val opts = O.merge (PP.opts,Out.opts)
	val (opts,width) = O.intParam opts
	  {name="line-width",
	   flags="w",
	   arg_dflt=NONE,
	   dflt=74,
	   advice="num",
	   doc="line width for pretty printer"}

	val (opts,depends) = O.stringParam opts
	  {name="depends",
	   flags="M",
	   arg_dflt=SOME "-",
	   dflt="",
	   advice="file",
	   doc="output dependency information"}

	val (opts,lib_dir) = O.stringParam opts
	  {name="lib-dir",
	   flags="",
	   arg_dflt=NONE,
	   dflt="",
	   advice="directory",
	   doc="directory to place library code"}

	val (opts,no_libs) = O.boolFlag opts
	  {name="no-libs",
	   flags="", 
	   v=true,
	   dflt=false,
	   doc="don't output library files"}

	val (opts,dump_libs) = O.boolFlag opts
	  {name="dump-libs",
	   flags="", 
	   v=true,
	   dflt=false,
	   doc="only output library files"}

	fun translate p arg =
	  let
	    val wd = width p
	    fun mkpp x =
	      PPUtil.cat
	      [PP.mkComment
	       (" Machine generated. Edit at your own risk "::
		" Reproduce with the following"::
		(O.toArgList p)), PPUtil.ws,x]
	      
	    fun cvt {name,depends,body} =
	      (name,(fn s => PPUtil.pp_to_outstream s wd (mkpp body)))
	    fun do_code (code,fs) =
	      List.foldl FileSet.addFile fs (PP.pp_code p code)

	    fun get_depends {name,depends,body} = (name,depends)
	    val ldir = case lib_dir p of  "" => [] | x => [x]
	    val opt = FileSet.LAZY_LIBS
	    val opt = if (no_libs p) then FileSet.NO_LIBS
	              else opt
	    val (opt,file_set) =
	      if (dump_libs p) then  (FileSet.DUMP,file_set)
	      else (opt,List.foldl do_code file_set arg)
	    val files = FileSet.export (opt,ldir,file_set)
	    val deps = PP.mkDeps (List.map get_depends files)
	    val files = List.map cvt files
	    val files =
	      case depends p of
		"" => files
	      | "-" => ((PPUtil.pp_to_outstream TextIO.stdOut wd deps); files)
	      | p => ([p],(fn s => PPUtil.pp_to_outstream s wd deps))::files
	  in  Out.translate p files
	  end
    end


