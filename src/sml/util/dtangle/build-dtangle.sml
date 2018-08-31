signature BUILD_DTANGLE =
  sig
    structure BU : BUILD_UTIL
    datatype style = ML | C | SHELL | TEX | ADA | VERB
    val rules :
      {heap:Paths.file_path,
    dtangle:{inp:(style * int * Paths.file_path) list,
	     out:Paths.file_path} -> (Paths.file_path * BU.B.rule list)} BU.M

  end
functor BuildDTangle (structure SML : SML_BUILD
		      structure BU  : BUILD_UTIL
		      sharing SML.B = BU.B
		          val debug : bool) : BUILD_DTANGLE =
  struct
    structure BU = BU
    structure B = BU.B
    datatype style = ML | C | SHELL | TEX | ADA | VERB

    fun dump_heap {root,main,name} =
      let fun dump (f,root) =
	SML.dump_heap {name=f name,
		       cenv=SML.mk_comp_env {lpath=[]},
		       root=root,
		       main=main}
      in (BU.mkRule' dump) (BU.mkInput SML.mk_cm_file root)
      end

    val dtangle_heap = dump_heap {root="sources.cm",
				  main="DTangle.main",
				  name="dtangle"}

    fun dtangle_rule heap {inp,out} =
      let
	fun dtangle args = SML.execute_heap{heap=heap,args=args}
	fun lang2arg ML = "-lml"
	  | lang2arg C = "-lc"
	  | lang2arg TEX = "-ltex"
	  | lang2arg SHELL = "-lsh"
	  | lang2arg ADA = "-lada"
	  | lang2arg VERB = "-linc"
	fun do_inp ((s,tp,f),(deps,args)) =
	  let
	    val deps = f::deps
	    val args =
	      B.STR("-p"^(Int.toString tp))::
	      (B.STR(lang2arg s))::
	      (B.STR (Paths.fileToNative f))::args
	  in (deps,args)
	  end
	val out_arg = [B.STR ("-o"),B.STR(Paths.fileToNative out)]
	val (deps,args) = List.foldr do_inp  ([],[]) inp
	val valid =
	  B.VALIDATE{targets=[out],depends=(SML.heap_path heap)::deps}
      in (out,[B.RULE {valid=valid,update=dtangle (out_arg@args)}])
      end
    val rules = BU.mBind(dtangle_heap,(fn heap =>
		BU.mUnit {heap=SML.heap_path heap,
			  dtangle=dtangle_rule heap}))
  end
    




