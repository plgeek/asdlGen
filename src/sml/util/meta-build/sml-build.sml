signature SML_BUILD =
  sig
    structure B : CORE_BUILD
    type cm_file
    type heap_file
    type comp_env 

    type 'a res = ('a * B.rule list)

    val mk_cm_file  : Paths.file_path -> cm_file
    val mk_comp_env : {lpath:Paths.dir_path list} -> comp_env

    val dump_heap   : {name:Paths.file_path,
		       cenv:comp_env,
		       root:cm_file,
		       main:string} -> heap_file res

(*  TODO
    type cm_lib
    type exe_file

    val build_exe {heap:heap_file,name:Paths.file_path list} -> exe_file
    val build_lib {root:cm_file,alias:string option} -> cm_lib
    val root_srcs     : {cenv:comp_env,root:cm_file} -> Paths.file_path list
    val root_binfiles : {cenv:comp_env,root:cm_file} -> Paths.file_path list

    val exe_path  : exe_file -> Paths.file_path
    val cm_lib_files : cm_lib -> Paths.file_path list
*) 

   val execute_heap  : {heap:heap_file,args:string B.cmd list} -> unit B.cmd
   val heap_path     : heap_file -> Paths.file_path
  end
signature SML_BUILD_PARAMS =
  sig
    structure B : CORE_BUILD
    val run_cm      : B.var
    val run_heap    : B.var
    val heap_suffix : string
    val heap_arg    : string -> string
  end

functor SMLBuild(structure Params : SML_BUILD_PARAMS) : SML_BUILD =
  struct
    open Params
    type cm_file = Paths.file_path
    type heap_file = Paths.file_path
    type comp_env = Paths.dir_path list
    type 'a res = ('a * B.rule list)
    val mk_cm_file = (fn x => x)
    fun mk_comp_env {lpath} = lpath
    fun dump_heap {name,cenv,root,main} =
      let
	(* ignoring CMPATH  for now *)
	val heap = Paths.setFileExt name (SOME heap_suffix)
	val heap_file = Paths.fileToNative heap
	val root_file = Paths.fileToNative root
	val inp =
	  String.concat
	  ["CM.make'(\"",String.toString root_file,"\");",
	   "SMLofNJ.exportFn(\"",String.toString heap_file,"\",",
	   main,");"]
	val cmd = B.EXEC_WITH_INPUT(run_cm,[],B.STR inp)
	(* some what bogus dependency *)
	val valid = B.VALIDATE{targets=[heap],depends=[root]}
      in
	(heap,[B.RULE {valid=valid,update=cmd}])
      end
    fun execute_heap {heap,args} =
      B.EXEC(run_heap,(B.STR (heap_arg (Paths.fileToNative heap)))::args)
    fun heap_path x = x
  end



