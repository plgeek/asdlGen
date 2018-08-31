signature CC_BUILD =
  sig
    structure B : CORE_BUILD
    type src_file
    type obj_file
    type lib_file
    type exe_file

    type 'a res = ('a * B.rule list)
    type comp_env
    type link_env

    val mk_src_file  : Paths.file_path -> src_file
    val mk_comp_env  : {ipath:Paths.dir_path list,
			debug:bool,
			  opt:int,
		      defines:(string * string option) list} -> comp_env

    val mk_link_env  : {lpath:Paths.dir_path list,
		       static:bool} -> link_env

    val comp_cxx_srcs : {cenv:comp_env,
			 srcs:src_file list} -> obj_file list res

    val comp_srcs    : {cenv:comp_env,
			srcs:src_file list} -> obj_file list res

    val link_objs    : {lenv:link_env,
			name:Paths.file_path,
			objs:obj_file list,
			libs:lib_file list} -> exe_file res

    val make_lib     : {name:Paths.file_path,
	              static:bool,
			objs:obj_file list} -> lib_file res

    val src_path  : src_file -> Paths.file_path
    val obj_path  : obj_file -> Paths.file_path
    val exe_path  : exe_file -> Paths.file_path
    val lib_path  : lib_file -> Paths.file_path
  end

signature CC_BUILD_PARAMS =
  sig
    structure B    : CORE_BUILD
    val comp_cxx   : B.var
    val comp       : B.var
    val link       : B.var
    val mklib      : B.var

    val comp_args  : string B.cmd list
    val link_args  : string B.cmd list
    val mklib_args : string B.cmd list

    val debugFlag  : bool    -> string B.cmd
    val optFlag    : int     -> string B.cmd
    val incFlag    : string  -> string B.cmd
    val libFlag    : string  -> string B.cmd
    val defFlag    : (string * string option) -> string B.cmd

    val  compOutFlag: string -> string B.cmd
    val  linkOutFlag: string -> string B.cmd
    val mklibOutFlag: string -> string B.cmd

    val obj_suffix : string option
    val lib_suffix : string option
    val exe_suffix : string option
  end

functor CCBuild(structure Params:CC_BUILD_PARAMS) : CC_BUILD =
  struct
    structure B = Params.B
    type src_file = Paths.file_path 
    type obj_file = Paths.file_path 
    type exe_file = Paths.file_path 
    type lib_file = Paths.file_path
    type 'a res = ('a * B.rule list)
    val mk_src_file = (fn x => x)
    val src_path = (fn x => x)
    val obj_path = (fn x => x)
    val exe_path = (fn x => x)
    val lib_path = (fn x => x)

    fun mkRule (ts,ds,cmd) =
      B.RULE{valid=B.VALIDATE{targets=ts,
			      depends=ds},update=cmd}

    type comp_env = string B.cmd list
    type link_env = string B.cmd list
    fun prefix x s = x^s
    fun mk_comp_env {ipath,debug,opt,defines} =
      let
	val paths =  List.map (Params.incFlag o Paths.dirToNative) ipath
	val flags =
	  (Params.debugFlag debug)::
	  (Params.optFlag opt)::
	  (List.map Params.defFlag defines)
      in
	flags@paths
      end
    fun mk_link_env {lpath,static} =
      let
	val paths =  List.map (Params.incFlag o Paths.dirToNative) lpath
      in
	paths
      end
    fun comp_srcs {cenv,srcs} =
      let
	val flags = cenv@Params.comp_args
	fun comp src =
	  let
	    val obj = Paths.setFileExt src Params.obj_suffix
	  in
	    (obj,
	     mkRule([obj],[src],B.EXEC(Params.comp,flags@
		   [Params.compOutFlag (Paths.fileToNative obj),
		    B.STR (Paths.fileToNative src)])))
	  end
	val res = List.map comp srcs
	val objs = List.map #1 res
	val rules = List.map #2 res
      in
	(objs,rules)
      end

    fun comp_cxx_srcs {cenv,srcs} =
      let
	val flags = cenv@Params.comp_args
	fun comp src =
	  let
	    val obj = Paths.setFileExt src Params.obj_suffix
	  in
	    (obj,
	     mkRule([obj],[src],B.EXEC(Params.comp_cxx,flags@
		   [Params.compOutFlag (Paths.fileToNative obj),
		    B.STR (Paths.fileToNative src)])))
	  end
	val res = List.map comp srcs
	val objs = List.map #1 res
	val rules = List.map #2 res
      in
	(objs,rules)
      end

    fun link_objs {lenv,name,objs,libs} =
      let
	val exec = Paths.setFileExt name Params.exe_suffix
	val flags =
	  (Params.linkOutFlag
	   (Paths.fileToNative exec))::(Params.link_args@lenv)
	val args = List.map (B.STR o Paths.fileToNative) (objs@libs)
      in
	(exec,[mkRule ([exec],objs@libs,B.EXEC (Params.link,flags@args))])
      end

    fun make_lib {name,static,objs} =
      let
	val lib = Paths.setFileExt name Params.lib_suffix
	val flags = (Params.mklib_args)@[Params.mklibOutFlag
					(Paths.fileToNative lib)]
	val args = List.map (B.STR o Paths.fileToNative) objs
      in
	(lib,[mkRule ([lib],objs,B.EXEC (Params.mklib,flags@args))])
      end
  end





