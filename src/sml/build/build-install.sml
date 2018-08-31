functor BuildInstall(structure BuildC   : BUILD_IT
		     structure BuildCXX : BUILD_IT
		     structure BuildSML : BUILD_IT
		     structure BuildDoc : BUILD_DOC
		     structure FileOps  : FILE_OPS_BUILD
		     val install_dir    : Paths.path
		     sharing BuildC.BU.B = 
		             BuildCXX.BU.B =
		             BuildSML.BU.B = 
			     FileOps.B = BuildDoc.B) =
  struct
    structure P = Paths
    structure F = FileOps
    structure B = F.B
    fun mk_abs root arcs = P.pathConcat(root,P.pathFromArcs arcs)

    val pkg_lib_dir = P.dirFromPath(mk_abs install_dir ["lib","asdlGen"])

    val pkg_heap_dir =
      P.dirFromPath(mk_abs install_dir ["lib","asdlGen","heaps"])

    val pkg_share_dir =
      P.dirFromPath(mk_abs install_dir ["share","asdlGen"])

    val pkg_doc_dir =
      P.dirFromPath(mk_abs install_dir ["doc","asdlGen"])

    val pkg_inc_dir = P.dirFromPath(mk_abs install_dir ["include","asdlGen"])

    fun install_includes (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_inc_dir})@r

    fun install_libs (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_lib_dir})@r
      
    fun install_heap (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_heap_dir})@r

    fun install_share (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_share_dir})@r

    fun install_doc (f,r) =
      (F.install_data_file{src=f,dst=P.setFileDir f pkg_doc_dir})@r

    fun do_pkg rules f r root =
      let
	val ({lib,includes,cleanable,share,doc,bin},r) =
	  f (P.pathFromArcs root) r
	val rules = List.foldl install_includes rules includes
	val rules = List.foldl install_libs rules lib
	val rules = List.foldl install_doc rules doc
	val rules = List.foldl install_share rules share
      in rules 	
      end
      
    val rules = []
(*  val rules = do_pkg rules BuildC.BU.getRules BuildC.rules ["..","c"] 
    val rules = do_pkg rules BuildCXX.BU.getRules BuildCXX.rules ["..","cxx"] 
 *)
    val rules = do_pkg rules BuildSML.BU.getRules BuildSML.rules ["..","sml"] 
(*    val rules = List.foldl install_doc rules BuildDoc.docs *)
  end
  


