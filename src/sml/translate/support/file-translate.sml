(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)

signature TRANSLATE_TO_FILES =
    sig
	type outstream
	type input = (string list * (outstream -> unit)) list
	type output = string list
	    
	val opts      : CommandOptions.args_spec
	val translate : CommandOptions.args -> input -> output
    end
			
functor mkFileOutput(type outstream
		     val openOut : string -> outstream
		     val closeOut: outstream -> unit): TRANSLATE_TO_FILES =
  struct
    
    type outstream = outstream
    type input = (string list * (outstream -> unit)) list
    type output = string list
    structure FS = FSUtils
    structure O = CommandOptions
    val (opts,output_dir) = O.stringParam O.empty
      {name="output-dir",
       flags="d",
       arg_dflt=NONE,
       dflt=OS.Path.currentArc,
       advice="directory",
       doc="directory for output"}
      
    val (opts,no_action) = O.boolFlag opts
      {name="no-action",
       flags="n",
       v=true,
       dflt=false,
       doc="dry run without output"}

    val (opts,no_cpif) = O.boolFlag opts
      {name="no-cpif",
       flags="",
       v=true,
       dflt=false,
       doc="overwrite unchanged files"}

    fun msg x = TextIO.output(TextIO.stdErr,x)
    fun translate p args =
      let
	fun do_file (arcl,f) =
	  let
	    val dir = output_dir p
	    val {isAbs,vol,arcs} = OS.Path.fromString dir
	    fun copy outname f =
	      let
		val tmpf = FS.tmpName()
		val outs = openOut tmpf
		val _ = (f outs;closeOut outs)
	      in if (FS.cmp (tmpf,outname)) then
		(msg (outname^" is unchanged\n"); FS.rm tmpf)
		 else FS.mv (tmpf,outname)
	      end
	    fun mkPath arcs = OS.Path.toString{isAbs=isAbs,vol=vol,arcs=arcs}
	    fun ensure_path ([x],_) = ()
	      | ensure_path (x::xs,pre) =
	      let val p = pre@[x]
		  val pname = mkPath p
	      in
		(if (OS.FileSys.isDir pname) then
		   ensure_path (xs,p)
		 else  raise
		   (Error.error ["Invalid Path ",pname]))
		   handle (OS.SysErr _)=>
		     (OS.FileSys.mkDir pname;
		      msg ("Created Directory: "^pname^"\n");
		      ensure_path (xs,p))
	      end
	      | ensure_path _ = ()
	    val path = arcs@arcl
	    val outname = mkPath path
	    val {arcs,...} = OS.Path.fromString outname
	  in if (no_action p) then msg (outname^"\n")
	     else (ensure_path (arcs,[]);
		   if no_cpif p then
		     (let val outs = openOut outname
		     in (f outs) before (closeOut outs)
		     end)
		   else  (copy outname f));
	       outname
	  end
      in List.map do_file args
      end
  end

structure TextIOFileOutput =
    mkFileOutput( type outstream = TextIO.outstream
		  val openOut = TextIO.openOut
		  val closeOut = TextIO.closeOut)
		      
structure BinIOFileOutput =
    mkFileOutput( type outstream = BinIO.outstream
		  val openOut = BinIO.openOut
		  val closeOut = BinIO.closeOut)



