signature MK_FILE_SET =
  sig
    val file_set : Paths.path ->  FileSet.file_set ->
      (string * string list) list -> FileSet.file_set
  end
structure MkFileSet :> MK_FILE_SET =
  struct
    fun file_set root init rest =
      let
	fun do_one ((name,depends),fs) =
	  let
	    val path = Paths.pathConcat (root,Paths.pathFromArcs [name])
	    val fname = (Paths.pathToNative path)
	    val ins = TextIO.openIn (Paths.pathToNative path)
	    val file = TextIO.inputAll ins
	    val _ = (TextIO.closeIn ins;print ("Read file:"^fname^"\n"))
	    val body = PPUtil.s file
	    val files = FileSet.mkFile{name=name,depends=depends,
					  body=body}
	  in FileSet.addFile (files,fs)
	  end
      in List.foldl do_one init rest
      end
  end