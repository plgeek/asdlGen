structure FileSet :> FILE_SET =
  struct
    structure Key =
      struct
	type ord_key = string
	val compare = String.compare
      end

    structure OM = ListMapFn(Key)
    structure Scc = SCCUtilFun(structure Node = Key)

    datatype file = L of {name:string,
		       depends:string list,
			  impl:file OM.map}
                  | F of {name:string,
		       depends:string list,
			  body:PPUtil.pp}
    type file_set = file OM.map

    datatype opt =
      NO_LIBS  
    | DUMP     
    | LAZY_LIBS

    val mkFile = F
    val mkLib = L
      
    fun addFile (F x,fs) =
      if OM.inDomain (fs, #name x) then 
	(raise Fail "duplicate file in file set")
      else OM.insert' ((#name x,F x),fs)
      | addFile (L x,fs) = 
      if OM.inDomain (fs, #name x) then 
	(raise Fail "duplicate file in file set")
      else OM.insert' ((#name x,L x),fs)

    fun addFile' (F x,fs) =
      OM.insert' ((#name x,F x),fs)
      | addFile' (L x,fs) =  OM.insert' ((#name x,L x),fs)
    val empty = OM.empty
    fun export (opt,d,fs:file_set) =
      let
	fun get_root (F x,xs) = (#name x)::xs
	  | get_root (L x,xs) =
	  (case opt of DUMP => (#name x)::xs | _ => xs)
	val roots = OM.foldl get_root [] fs
	fun follow "" = roots
	  | follow x  =
	  case OM.find (fs,x) of
	  NONE => []
	| SOME (F x) => (#depends x)
	| SOME (L x) => (#depends x)
	fun do_depend (x,xs) =
	   case OM.find (fs,x) of
	    NONE => xs
	  | SOME (F x) =>
	      List.foldl do_depend  (addFile' (F x,xs)) (#depends x)
	  | SOME (L x) =>
	      List.foldl do_depend  (addFile' (L x,xs)) (#depends x)

	fun do_file (L x,xs) =
	  (case opt of 
	     LAZY_LIBS => xs
	   | _ => List.foldl do_depend (addFile' (L x,xs)) (#depends x))
	  | do_file (F x,xs) = 
	  List.foldl do_depend (addFile' (F x,xs)) (#depends x)
	val fs' =  OM.foldl do_file empty fs
	fun libPath x = d@x
	fun get_node "" = NONE
	  | get_node x = OM.find(fs',x)
	fun fix_dep x = case OM.find(fs',x) of 
	  (SOME (L _)) => libPath [x] | _ => [x]
	fun cvt (F x) = {name=[#name x],
			 depends=List.map fix_dep (#depends x),
			 body=(#body x)}
	  | cvt _ = raise (Fail "impossible")

	fun prefix_it {name,depends,body} =
	  {name=libPath name,depends=List.map libPath depends,body=body}
	fun no_cycle (Scc.SIMPLE x,xs) =
	  (case (get_node x) of
	     SOME (L x) =>
	       (case opt of
		  NO_LIBS => xs
		| _ => ((List.map prefix_it
			 (export (opt,d,#impl x)))@xs))
	   | SOME (F x) => (cvt (F x)::xs)
	   | _ => xs)
	  | no_cycle (Scc.RECURSIVE _,xs) = raise (Fail "cyclic dependency")
	val components = Scc.topOrder {root="",follow=follow}
      in List.foldl no_cycle [] components
      end
  end