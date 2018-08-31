signature FILE_OPS_BUILD =
  sig
    structure B : CORE_BUILD

    type file = Paths.file_path
    type dir = Paths.dir_path
    type attrbs = {r:bool,w:bool,x:bool}
    val copy_file    : {src:file,dst:file} -> unit B.cmd
    val delete_file  : {force:bool,file:file} -> unit B.cmd
    val ensure_dir   : dir -> unit B.cmd
    val set_attrbs   : (file * attrbs) -> unit B.cmd
      
    val install_exe_file  : {src:file,dst:file} -> B.rule list
    val install_data_file : {src:file,dst:file} -> B.rule list
  end

signature FILE_OPS_BUILD_PARAMS =
  sig
    structure B            : CORE_BUILD
    val copy               : B.var
    val delete             : B.var
    val delete_force_arg   : string
    val mkDir              : B.var
    val existsDir          : B.var
    val attrb_set          : B.var
    val attrbs2str         : {r:bool,w:bool,x:bool} -> string
  end
functor FileOpsBuild(structure Params : FILE_OPS_BUILD_PARAMS )
  : FILE_OPS_BUILD =
  struct
    structure B = Params.B
    type file = Paths.file_path
    type dir = Paths.dir_path
    type attrbs = {r:bool,w:bool,x:bool}

    fun copy_file {src,dst} =
      (B.EXEC (Params.copy,
	       [B.STR(Paths.fileToNative src),
		B.STR(Paths.fileToNative dst)]))
    fun delete_file {force=true,file} =
      (B.EXEC (Params.delete,
	       [B.STR(Params.delete_force_arg),
		B.STR(Paths.fileToNative file)]))
      | delete_file {force=false,file} =
      (B.EXEC (Params.delete,
	       [B.STR(Paths.fileToNative file)]))

    fun ensure_dir dir =
      let
	val dirn = Paths.dirToNative dir
	val msg = B.STR ("Created directory "^dirn)
	fun make_path (NONE,cmds) = cmds
	  | make_path (SOME d,cmds) =
	  let
	    val arg = [B.STR (Paths.dirToNative d)]
	    val cmd =
	      B.OR [B.EXEC (Params.existsDir,arg),
		    B.EXEC(Params.mkDir,arg)]
	  in
	   (make_path (Paths.getDirParent d,cmd::cmds))
	  end
	val cmds = make_path (SOME dir,[B.WRITE msg])
      in
	B.OR [B.EXEC (Params.existsDir,[B.STR dirn]),
	      B.SEQ cmds]
      end
    fun set_attrbs (f,a) =
      B.EXEC(Params.attrb_set,[B.STR (Params.attrbs2str a),
			       B.STR (Paths.fileToNative f)])
    fun install_exe_file {src,dst} =
      [B.RULE{valid=B.VALIDATE{targets=[dst],depends=[src]},
	     update=B.AND [ensure_dir (Paths.getFileDir dst),
			   B.IGN (delete_file {force=true,file=dst}),
			   copy_file {src=src,dst=dst},
			   set_attrbs(dst,{r=true,w=false,x=true})]}]

    fun install_data_file {src,dst} =
      [B.RULE{valid=B.VALIDATE{targets=[dst],depends=[src]},
	     update=B.AND [ensure_dir (Paths.getFileDir dst),
			   B.IGN (delete_file {force=true,file=dst}),
			   copy_file {src=src,dst=dst},
			   set_attrbs(dst,{r=true,w=false,x=false})]}]
	       
  end