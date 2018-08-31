signature PATHS =
    sig
	exception PathsError
	type path
	type file_path
	type dir_path
	type search_path 

	val pathFromNative : string -> path
	val pathFromArcs   : string list -> path
	    
	val pathToNative   : path -> string 
	val pathConcat     : (path * path) -> path

	val dirFromPath : path -> dir_path
	val fileFromPath: path -> file_path

	val fileToNative  : file_path -> string
	val dirToNative   : dir_path -> string

	val searchPathFromDirs : dir_path list -> search_path

	val existsDir   : dir_path -> bool
	val existsFile  : file_path -> bool
	val ensureDir   : dir_path -> unit
	    
	val getDirParent: dir_path -> dir_path option

	val getFileDir  : file_path -> dir_path
	val getFileBase : file_path -> string
	val getFileExt  : file_path -> string option

	val setFileDir  : file_path -> dir_path -> file_path 
	val setFileBase : file_path -> string -> file_path
	val setFileExt  : file_path -> string option -> file_path 

	val combineSearchPath : search_path -> search_path -> search_path
	val resolveFile : search_path -> file_path -> file_path option

    end
    
structure Paths :> PATHS =
    struct
	exception PathsError
	type path = string
	type dir_path = {isAbs:bool,arcs:string list,vol: string}
	type file_path = {dir:dir_path,base:string,ext:string option}

	type search_path = dir_path list

	val pathFromNative = OS.Path.mkCanonical
	fun pathFromArcs x = OS.Path.toString{isAbs=false,vol="",arcs=x}
	val pathToNative = OS.Path.mkCanonical

	fun pathConcat (x,y) = OS.Path.concat(x,y)

	val dirFromPath = OS.Path.fromString 

	fun fileFromPath x =
	    let
		val {dir,file} = OS.Path.splitDirFile x
		val dir = dirFromPath dir
		val {base,ext} = OS.Path.splitBaseExt file
	    in
		{dir=dir,base=base,ext=ext}
	    end

	fun searchPathFromDirs x = x

	fun getFileDir {dir,base,ext} = dir
	fun setFileDir {dir,base,ext}  x = {dir=x,base=base,ext=ext}

	fun getFileBase {dir,base,ext} = base
	fun setFileBase {dir,base,ext} x = {dir=dir,base=x,ext=ext}

	fun getFileExt {dir,base,ext} = ext
	fun setFileExt {dir,base,ext} x = {dir=dir,base=base,ext=x}
	fun getDirParent {isAbs,arcs=[],vol} = NONE
	  | getDirParent {isAbs,arcs,vol} =
	  SOME
	  {isAbs=isAbs,arcs=List.take(arcs,(List.length arcs) - 1),vol=vol}
	fun dirToNative d = OS.Path.toString d
	fun fileToNative {dir,base,ext} =
	    OS.Path.joinDirFile
	    {dir=(dirToNative dir),
	     file=OS.Path.joinBaseExt{base=base,ext=ext}}

	fun existsDir d =
	    let
		val dir = dirToNative d 
	    in
		(OS.FileSys.isDir dir) handle (OS.SysErr (s,err)) => false
	    end

	fun checkFile f =
	    ((OS.FileSys.access(f,[OS.FileSys.A_READ]))
		handle (OS.SysErr (s,err)) => false)

	val existsFile = checkFile o fileToNative

	fun ensureDir (d as {isAbs,arcs=[],vol}) =
	    if existsDir d then ()
	    else (raise Fail "Can't create directory")
	  | ensureDir (d as {isAbs,arcs,vol}) =
		if existsDir d then ()
		else
		    let
			val parent_arcs =
			    List.take (arcs,(List.length arcs)-1)
		    in
			ensureDir {isAbs=isAbs,arcs=parent_arcs,vol=vol};
			OS.FileSys.mkDir (dirToNative d)
		    end
	fun isAbs (x:file_path) = (#isAbs o #dir) x
	fun resolveFile s f =
	    let
		val fname = fileToNative f
		fun loop (x::xs) =
		    let val tryfile =
			(OS.Path.concat (dirToNative x,fname))
		    in
			if checkFile tryfile then
			    (SOME(fileFromPath (pathFromNative tryfile)))
			else loop(xs)
		    end
		  | loop [] =
		    if checkFile fname then (SOME f)
		    else NONE
	    in
		if isAbs f then
		    (if checkFile fname then (SOME f)
		    else NONE)
		else loop(s)
	    end
	fun combineSearchPath x y = x @ y
    end

