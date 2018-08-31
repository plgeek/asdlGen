functor BuildClean(structure BuildC   : BUILD_IT
		   structure BuildCXX : BUILD_IT
		   structure BuildSML : BUILD_IT
		   structure BuildDoc : BUILD_DOC
		   structure FileOps  : FILE_OPS_BUILD
		   sharing BuildC.BU.B = 
		     BuildCXX.BU.B =
		     BuildSML.BU.B = 
		     FileOps.B = BuildDoc.B) =
  struct
    structure P = Paths
    structure F = FileOps
    structure B = F.B
    fun mk_abs root arcs = P.pathConcat(root,P.pathFromArcs arcs)
    fun do_pkg rules f r root =
      let
	val ({lib,includes,cleanable,share,doc,bin},r) =
	  f (P.pathFromArcs root) r
	fun clean (x,r) =
	  (B.RULE{valid=B.INVALID,
		 update=FileOps.delete_file {force=true,file=x}})::r
	val rules = List.foldl clean rules cleanable 
      in rules 	
      end
    val rules = []
    val rules = do_pkg rules BuildC.BU.getRules BuildC.rules ["..","c"] 
    val rules = do_pkg rules BuildCXX.BU.getRules BuildCXX.rules ["..","cxx"] 
    val rules = do_pkg rules BuildSML.BU.getRules BuildSML.rules ["..","sml"] 
(*    val rules = List.foldl install_doc rules BuildDoc.docs *)
  end
  


