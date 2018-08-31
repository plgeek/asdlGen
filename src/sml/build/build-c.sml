functor BuildC (structure CC : CC_BUILD
		structure BU : BUILD_UTIL
		sharing CC.B = BU.B
		val include_dirs : Paths.dir_path list
		val debug : bool) : BUILD_IT =
  struct
    structure BU = BU
    structure CC = CC
    val comp_srcs =
      let fun mk_arg m =
	BU.getDir(m,(fn (srcs,src_dir) =>
		     BU.mUnit {cenv=CC.mk_comp_env
			       {ipath=src_dir::include_dirs,
				debug=debug, opt=1,defines=[]},
			       srcs=srcs}))
      in (BU.mkRule CC.comp_srcs) o mk_arg 
      end
    fun make_lib name =
      BU.mkRule' (fn (mkf,objs) =>
		  CC.make_lib {name=mkf name,static=false,objs=objs})
    fun mk_headers x = BU.mBindList
      (List.map (BU.mkInput (fn x => x)) x,BU.mUnit)
    fun mk_srcs s =  BU.mBindList
      (List.map (BU.mkInput CC.mk_src_file) s,BU.mUnit)
    fun merge_objs (x,y) =
      BU.mBind(x,(fn xs =>
      BU.mBind(y,(fn ys => BU.mUnit (xs @ ys)))))
      
(* ignore the cruft above all the work happens below *)

    val headers =
      mk_headers
      ["pkl-int.h","share.h","StdTypes.h",
       "StdPrims.h","StdPkl.h",
       "XMLPrims.h","XMLPkl.h"]

    val common_srcs = mk_srcs ["pkl-int.c","share.c"]
    val common_objs = comp_srcs common_srcs

    val std_srcs = mk_srcs ["std_prims.c"]
    val std_objs = comp_srcs std_srcs
    val std_lib = make_lib "libasdl"
      (merge_objs (std_objs,common_objs))

    val xml_srcs = mk_srcs ["xml_prims.c"]
    val xml_objs = comp_srcs xml_srcs
    val xml_lib = make_lib "libxml"
      (merge_objs (xml_objs,common_objs))

    val cleanable = merge_objs(merge_objs (xml_objs,std_objs),common_objs)

    val rules = BU.mBind(xml_lib,(fn xml_lib =>
      	        BU.mBind(std_lib,(fn std_lib =>
	        BU.mBind(headers,(fn headers =>
	        BU.mBind(cleanable,(fn cleanable =>
		BU.mUnit{lib=[CC.lib_path xml_lib,
			      CC.lib_path std_lib],
			 cleanable=List.map CC.obj_path cleanable,
			 includes=headers,
			 share=[]:Paths.file_path list,
			 doc=[]:Paths.file_path list,
			 bin=[]:Paths.file_path list}))))))))

  end
    


