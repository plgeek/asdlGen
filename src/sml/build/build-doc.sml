signature BUILD_DOC =
  sig
    structure B : CORE_BUILD
    val rules : B.rule list
    val docs : Paths.file_path  list
  end
    (* TODO clean this up *)
functor BuildDoc(structure DT : BUILD_DTANGLE
		 val dtangle_src : Paths.path
		 val src_root : Paths.path) : BUILD_DOC =
  struct
    fun mk_file arcs =
      Paths.fileFromPath (Paths.pathConcat(src_root,Paths.pathFromArcs arcs))
    structure B = DT.BU.B

    val ({heap,dtangle},rules) = DT.BU.getRules dtangle_src DT.rules

    fun mk_ins (prefix,style) =
      let val kp = (List.length prefix) + 1
      in List.foldr (fn (x,xs) => (style,kp,mk_file (prefix@[x]))::xs) []
      end
    val noweb = B.mkVAR{name=(SOME "NOWEB"),
			doc=["Process the output of dtangle"],
			init=B.STR("noweb")}
    fun mkTeX ctx ((out,inps),(outs,rules)) =
      let
	val inp = mk_ins ctx inps
	val out_nw = mk_file ["doc","internals",out]
	val out_tex = Paths.setFileExt out_nw (SOME "tex") 
	val noweb_valid =
	  B.VALIDATE {targets=[out_tex],depends=[out_nw]}
	val noweb_cmd =
	  B.EXEC(noweb,[B.STR ("-o"),B.STR (Paths.fileToNative out_nw)])
	val noweb_rule = B.RULE{valid=noweb_valid,update=noweb_cmd}
	val (out,dtangle_rules) = dtangle {inp=inp,out=out_nw}
      in (out::outs,noweb_rule::(dtangle_rules@rules))
      end


    fun do_dir ctx = List.foldr (mkTeX ctx) 
    fun mk s = (s^".nw",[s^".sig",s^".sml"])
    fun mk_sig s = (s^".nw",[s^".sig"])
    fun mk_sml s = (s^".nw",[s^".sml"])

    val ctx = (["sml","frontend"],DT.ML)      
    val res = do_dir ctx ([],rules)
      [mk_sml "parser",
       mk "semant",mk "semant-props"]

    val ctx = (["asdl"],DT.ADA)      
    val res = do_dir ctx res [("asdl.nw",["asdl.asdl"])]
      
    val ctx = (["sml","translate","support"],DT.ML)      
    val res = do_dir ctx res
      [mk "stmt-exp",
       mk "type-decl",
       mk_sig "semant-translator",
       mk_sml "semant-translate"]
    val ctx = (["sml","translate","picklers"],DT.ML)      
    val res = do_dir ctx res
      [mk "std-pickler"]

    val ctx = (["sml","translate","algebraic"],DT.ML)      
    val res = do_dir ctx res
      [mk "algebraic-spec",
       mk_sml "algebraic-semant-translator"]

    val ctx = (["sml","util","meta-build"],DT.ML)      
    val res = do_dir ctx res [mk_sig "meta-build"]
    val (docs,rules) = res
  end
