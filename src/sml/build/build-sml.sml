functor BuildSML (structure SML : SML_BUILD
		  structure BU : BUILD_UTIL
		  sharing SML.B = BU.B
		  val debug : bool) : BUILD_IT =
  struct
    structure BU = BU

    fun dump_heap {root,main,name} =
      let fun dump (f,root) =
	SML.dump_heap {name=f name,
		       cenv=SML.mk_comp_env {lpath=[]},
		       root=root,
		       main=main}
      in (BU.mkRule' dump) (BU.mkInput SML.mk_cm_file root)
      end

    val asdl_heap = dump_heap
      {root="sources.cm",
       main="Export.asdlGen",
       name="asdlGen"}

(* TODO add CM library and standalone binary output *)

    val rules = BU.mBind(asdl_heap,(fn heap =>
	       BU.mUnit {lib=[SML.heap_path heap],
	            includes=[]:Paths.file_path list,
		   cleanable=[]:Paths.file_path list,
		       share=[]:Paths.file_path list,
			 doc=[]:Paths.file_path list,
			 bin=[]:Paths.file_path list}))
  end
    



