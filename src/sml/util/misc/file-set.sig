signature FILE_SET =
  sig
    type file_set
    type file
    datatype opt =
      NO_LIBS   (* don't include libraries *)
    | DUMP      (* include the entire file set *)
    | LAZY_LIBS (* only include libraries that are referenced *)

    val empty   : file_set

    val mkFile  : {name:string,
		depends:string list,
		   body:PPUtil.pp} -> file

    val mkLib   : {name:string,
		depends:string list,
		   impl:file_set} -> file

    val addFile : file * file_set -> file_set
    val export  : (opt * string list * file_set) ->
      {name:string list,depends:string list list,body:PPUtil.pp} list

  end